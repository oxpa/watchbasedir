-module(rpm_read).
-compile(export_all).
%-on_load(on_load/0).
%-include("rpm.types.hrl").
-include("rpmtags.hrl").
%% Definition from http://www.rpm.org/max-rpm/s1-rpm-file-format-rpm-file-format.html
-define(RPM_LEAD, <<Magic:32,Major:8,Minor:8,Type:16,Archnum:16,Name:528/bits,Osnum:16,Sign_type:16,Reserved:128>>).
-define(SIGNATURE, <<Magic:24,Header_version:8,Reserved:32,Index_count:32,Signature_bytes:32>>).
-define(INDEX,<<Tag:32,Data_type:32,Offset:32,Num_of_entries:32>>).

%% A helper function for C strings reading.
%% It's almost for sure have to be rewritten
read_till_zero(F, Max_Offset) -> read_till_zero(F, 1, Max_Offset ). 
read_till_zero(F, Count, Max_Offset) -> read_till_zero(F, Count, Max_Offset, [], 0, <<>>).
read_till_zero(_, 0, Max_Offset, Strings, Offset , _ ) -> {ok, {Offset, lists:reverse(Strings)}};
read_till_zero(F, Count, Max_Offset, Strings, Offset , Acc) when Offset =< Max_Offset ->
	case file:read(F,1) of
		{ok, <<0>>} -> read_till_zero(F, Count - 1, Max_Offset, [binary:bin_to_list(Acc)|Strings], Offset + 1, <<>>);
		{ok, Data} when is_binary(Data) -> 
			           read_till_zero(F, Count, Max_Offset,  Strings, Offset + 1, <<Acc/binary,Data/binary>> )
	end.

%% One more helper function due to rpm header structure
round_offset_by_eight(F,Off) ->
	case Off rem 8 of
		0 -> true;
		_ -> file:read(F,8 - (Off rem 8)),
			debug("Moved from ~B by ~B bytes",[Off, 8 - (Off rem 8)])
	end.

%% A helper for future integration with lager or other logger
debug(Message,Args) ->
	io:format(Message,Args),
	true.

%% A function used in shell to test module.
on_load(File) ->
	RPM = read_rpm(File),
	rpm_get_filelist(RPM).

read_rpm(RPM) ->
		{ok,F} = file:open(RPM,[read, binary, read_ahead]),
		{ok, Lead} = read_rpm_lead(F),
		{ok, {header_length, Max_Offset},Indexes} = read_rpm_signature_header(F),
		{ok, {Off,Signature}} = read_rpm_header_data(F,Indexes, Max_Offset),
		%% signature may end anywhere in the file. We should round the offset by 8 byte boundary
		round_offset_by_eight(F,Off),
		{ok,{header_length, Max_Header_Offset}, Head_indexes} = read_rpm_signature_header(F),
		{ok, {_,Header}} = read_rpm_header_data(F,Head_indexes, Max_Header_Offset),
		#rpm{filename=RPM, lead=Lead, signature=Signature, header=Header}.
		
read_rpm_lead(F) ->
	Header_length=96,
	{ok, ?RPM_LEAD} = file:read(F,Header_length),
	%?RPM_LEAD = Data,
	Magic=3987467995, % 16#edabeedb - rpm lead magic number
	{ok, #rpm_lead{ magic = Magic,	major = Major, minor = Minor, type = Type, archnum = Archnum,
        		name = Name, osnum = Osnum,	signature_type = Sign_type,
		   		reserved = 0 = Reserved 	% should always be zeroed.
    	   }
	}.

read_rpm_signature_header(F) ->
    Header_length=16,
    {ok,Data} = file:read(F,Header_length),
	debug("~w",[Data]),
	?SIGNATURE = Data, 
	% 9350632 = 16#8eade8 - header magic number
	Magic = 9350632,
	debug("Read magic: ~.16B , will read ~B index records with  ~B bytes of signature~n",[Magic,Index_count,Signature_bytes]),
	read_rpm_index_entry(F,Index_count, Signature_bytes).
	
read_rpm_index_entry(F, N, Max_Offset) 	-> read_rpm_index_entry(F,N, Max_Offset, []).
read_rpm_index_entry(_, 0, Max_Offset, Acc) ->
	%% return sorted by offset index list from Acc 
	{ok, {header_length, Max_Offset},
	 lists:sort(
		fun(A,B) when 
			A#rpm_sig_index.offset =< B#rpm_sig_index.offset	-> true; 
													(_, _)		-> false 
		end
		, Acc)
	};
read_rpm_index_entry(F, Num_of_indexes, Max_Offset, Acc) when Num_of_indexes > 0 -> 
	%% index is 4 fields of 4 bytes. Read them recursevly. Then proceed to data reading
    Index_length=16,
    {ok, Data} = file:read(F,Index_length),
	?INDEX=Data,
	debug("read tag ~B, ~B -th type at offset ~B. ~B entries.~n",[Tag, Data_type, Offset, Num_of_entries]),
	read_rpm_index_entry(F, Num_of_indexes - 1, Max_Offset, [#rpm_sig_index{tag=Tag,data_type=Data_type, offset=Offset,num_of_entries=Num_of_entries}|Acc]).
	

read_rpm_header_data(F, Indexes, Max_Offset) -> read_rpm_tag_data(F, Indexes, Max_Offset, {0, []}).


% reading ended. Return.
read_rpm_tag_data(_, [], _, {Off, Data}) -> 
	debug("read tag data till ~B",[Off]),
	{ok, {Off,Data}};

% read some data till next tag boundary
read_rpm_tag_data(F, [Index|Tail], Max_Offset, {Off,Data}) when Index#rpm_sig_index.offset - Off /= 0, Off =< Max_Offset -> 
	debug("Skipping through from ~B to ~B ~n",[Off, Index#rpm_sig_index.offset]),
	{ok, _} = file:read(F,Index#rpm_sig_index.offset - Off),
	read_rpm_tag_data(F, [Index|Tail], Max_Offset, {Index#rpm_sig_index.offset,Data});

% going to read a 6-th data type - a string
% or a list of string (8 and 9). 7 is binary and is read the other way
read_rpm_tag_data(F, [Index|Tail], Max_Offset, {Off, Data}) when Index#rpm_sig_index.data_type == 6; Index#rpm_sig_index.data_type > 7 ->
	{ok,{Offset,String}} =  read_till_zero(F, Index#rpm_sig_index.num_of_entries, Max_Offset ),
	debug("Read ~B bytes starting from ~B ~n",[Offset,Off]),
	Tag_data = {Index#rpm_sig_index.tag, String},
	read_rpm_tag_data(F, Tail, Max_Offset, {Offset+Off,[Tag_data|Data]});

% read a "simple" value which size can be calculated using Index information.
read_rpm_tag_data(F, [Index|Tail], Max_Offset, {Off, Data}) when Index#rpm_sig_index.data_type /= 6, Index#rpm_sig_index.data_type < 8  ->
	%Off=Index#rpm_sig_index.offset,
	Multiplier = proplists:get_value(Index#rpm_sig_index.data_type, rpm_types_sizes()),
	{ok, D} = file:read(F, Index#rpm_sig_index.num_of_entries * Multiplier), 
	debug("read ~B bytes by ~B bytes, starting from ~B~n",[Index#rpm_sig_index.num_of_entries * Multiplier, Multiplier, Off]),
	Tag_data = {Index#rpm_sig_index.tag, [ Bin || <<Bin:Multiplier/unit:8>> <= D ]},
	debug("~w, ~w~n",[D, Tag_data]),
	read_rpm_tag_data(F, Tail, Max_Offset, {Off + Index#rpm_sig_index.num_of_entries * Multiplier, [Tag_data|Data]}).

%% filelist is in 3 indexes: basenames, directories and "a link" between them, dir index.
%% length (Dir_index) == length( Base_names)
%% directories are numerated from 0, so have to add 1 in nth/2
%% if at least on of these arrays are not defined - it's an RPM without files (e.g. meta package with reqs only).

join_filelist(Dir_index, Base_names, Directories) 
	when Dir_index == undefined; Base_names == undefined; Directories == undefined -> {'file',''};

join_filelist(Dir_index, Base_names, Directories) -> 
        lists:zipwith(fun(A,B)-> {'file', lists:nth(A+1, Directories) ++ B} end, Dir_index,  Base_names).

%%% Functions to work with rpm description from read_rpm/1
rpm_get_filelist(RPM_DESC) ->
	case proplists:get_value(1027, RPM_DESC#rpm.header) of
		undefined -> join_filelist(proplists:get_value(1116, RPM_DESC#rpm.header),
								   proplists:get_value(1117, RPM_DESC#rpm.header),
				                   proplists:get_value(1118, RPM_DESC#rpm.header));
		A -> A
	end.

%% helper functions which does 90% of "get_tag_value" job...
rpm_get_header_parameter_by_id(RPM_DESC, Id) ->
	proplists:get_value(Id, RPM_DESC#rpm.header).

rpm_get_signature_parameter_by_id(RPM_DESC, Id) ->
	proplists:get_value(Id, RPM_DESC#rpm.signature).


rpm_get_name(RPM_DESC)-> {name, [rpm_get_header_parameter_by_id(RPM_DESC, 1000)]}.

rpm_get_arch(RPM_DESC)-> {arch, [rpm_get_header_parameter_by_id(RPM_DESC, 1022)]}.

rpm_get_version(RPM_DESC) -> 
	{version, [{epoch, rpm_get_header_parameter_by_id(RPM_DESC, 1003)},
				{version, rpm_get_header_parameter_by_id(RPM_DESC, 1001)},
				{release, rpm_get_header_parameter_by_id(RPM_DESC, 1002)}
			  ]}.

rpm_get_summary(RPM_DESC) -> {summary,[rpm_get_header_parameter_by_id(RPM_DESC, 1004)]}.

rpm_get_checksum(Filename) ->
	{ok, F} = file:open(Filename,[read, binary, read_ahead]),
	ShaContext = crypto:sha_init(),
	MdContext = crypto:md5_init(),
	rpm_get_checksum(F, MdContext, ShaContext).
rpm_get_checksum(F, MdContext, ShaContext) ->
	case file:read(F, 4096) of
		eof -> {checksum, rpm_finalize_cs(crypto:md5_final(MdContext)),
						rpm_finalize_cs(crypto:sha_final(ShaContext))};
		{ok, Data} -> rpm_get_checksum(F, crypto:md5_update(MdContext,Data), crypto:sha_update(ShaContext,Data))
	end.
	
rpm_finalize_cs(Sum) ->
		%debug("~s~n", Sum),
		lists:flatten(lists:map(fun(A) ->	io_lib:format("~.16B",[A]) end,	 
				binary:bin_to_list(<< <<D>> || <<D:4>> <= Sum>>))).
	
	%{checksum, {type, pkgid}, checksum}
%rpm_get_summary() ->
%rpm_get_description() ->
%packager
%url
%time file build
%size package installed archive
%location href
%format
%license
%vendor
%group
%buildhost
%sourcerpm
%header-range
%provides
%requires



