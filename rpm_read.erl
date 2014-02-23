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
read_till_zero(F) -> read_till_zero(F, 1 ). 
read_till_zero(F, Count) -> read_till_zero(F, Count, [], 0, <<>>).
read_till_zero(_, 0, Strings, Offset , _ ) -> {ok, {Offset, lists:reverse(Strings)}};
read_till_zero(F, Count, Strings, Offset , Acc) ->
	case file:read(F,1) of
		{ok, <<0>>} -> read_till_zero(F, Count - 1, [binary:bin_to_list(Acc)|Strings], Offset + 1, <<>>);
		{ok, Data} when is_binary(Data) -> 
			           read_till_zero(F, Count, Strings, Offset + 1, <<Acc/binary,Data/binary>> )
	end.

%% One more helper function due to rpm header structure
round_offset_by_eight(F,Off) ->
	file:read(F,Off rem 8).

%% A helper for future integration with lager or other logger
debug(Message,Args) ->
	%io:format(Message,Args),
	true.

%% A function used in shell to test module.
on_load(File) ->
	RPM = read_rpm(File),
	rpm_get_filelist(RPM).

read_rpm(RPM) ->
		{ok,F} = file:open(RPM,[read, binary, read_ahead]),
		{ok, Lead} = read_rpm_lead(F),
		{ok, Indexes} = read_rpm_signature_header(F),
		{ok, {Off,Signature}} = read_rpm_header_data(F,Indexes),
		%% signature may end anywhere in the file. We should round the offset by 8 byte boundary
		round_offset_by_eight(F,Off),
		{ok, Head_indexes} = read_rpm_signature_header(F),
		{ok, {_,Header}} = read_rpm_header_data(F,Head_indexes),
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
	?SIGNATURE = Data, 
	% 9350632 = 16#8ead e8 - header magic number
	Magic = 9350632,
	debug("Read magic: ~.16B , will read ~B index records with  ~B bytes of signature~n",[Magic,Index_count,Signature_bytes]),
	read_rpm_index_entry(F,Index_count).
	
read_rpm_index_entry(F, N) 	-> read_rpm_index_entry(F,N, []).
read_rpm_index_entry(_, 0, Acc) ->
	%% return sorted by offset index list from Acc 
	{ok, 
	 lists:sort(
		fun(A,B) when 
			A#rpm_sig_index.offset =< B#rpm_sig_index.offset	-> true; 
													(_, _)		-> false 
		end
		, Acc)
	};
read_rpm_index_entry(F, Num_of_indexes, Acc) when Num_of_indexes > 0 -> 
	%% index is 4 fields of 4 bytes. Read them recursevly. Then proceed to data reading
    Index_length=16,
    {ok, Data} = file:read(F,Index_length),
	?INDEX=Data,
	debug("read tag ~B, ~B -th type at offset ~B. ~B entries.~n",[Tag, Data_type, Offset, Num_of_entries]),
	read_rpm_index_entry(F, Num_of_indexes - 1, [#rpm_sig_index{tag=Tag,data_type=Data_type, offset=Offset,num_of_entries=Num_of_entries}|Acc]).
	

read_rpm_header_data(F, Indexes) -> read_rpm_tag_data(F, Indexes, {0, []}).


% reading ended. Return.
read_rpm_tag_data(_, [], {Off, Data}) -> {ok, {Off,Data}};

% read some data till next tag boundary
read_rpm_tag_data(F, [Index|Tail], {Off,Data}) when Index#rpm_sig_index.offset - Off /= 0 -> 
	debug("Skipping through from ~B to ~B ~n",[Off, Index#rpm_sig_index.offset]),
	{ok, _} = file:read(F,Index#rpm_sig_index.offset - Off),
	read_rpm_tag_data(F, [Index|Tail], {Index#rpm_sig_index.offset,Data});

% going to read a 6-th data type - a string
% or a list of string (8 and 9). 7 is binary and is read the other way
read_rpm_tag_data(F, [Index|Tail], {Off, Data}) when Index#rpm_sig_index.data_type == 6; Index#rpm_sig_index.data_type > 7 ->
	{ok,{Offset,String}} =  read_till_zero(F, Index#rpm_sig_index.num_of_entries),
	debug("Read ~B bytes starting from ~B ~n",[Offset,Off]),
	Tag_data = {Index#rpm_sig_index.tag, String},
	read_rpm_tag_data(F, Tail, {Offset+Off,[Tag_data|Data]});

% read a "simple" value which size can be calculated using Index information.
read_rpm_tag_data(F, [Index|Tail], {Off, Data}) when Index#rpm_sig_index.data_type /= 6, Index#rpm_sig_index.data_type < 8  ->
	%Off=Index#rpm_sig_index.offset,
	Multiplier = proplists:get_value(Index#rpm_sig_index.data_type, rpm_types_sizes()),
	{ok, D} = file:read(F, Index#rpm_sig_index.num_of_entries * Multiplier), 
	debug("read ~B bytes by ~B bytes, starting from ~B~n",[Index#rpm_sig_index.num_of_entries * Multiplier, Multiplier, Off]),
	Tag_data = {Index#rpm_sig_index.tag, [ Bin || <<Bin:Multiplier/unit:8>> <= D ]},
	debug("~w, ~w~n",[D, Tag_data]),
	read_rpm_tag_data(F, Tail, {Off + Index#rpm_sig_index.num_of_entries * Multiplier, [Tag_data|Data]}).

%%% Functions to work with rpm description from read_rpm/1
rpm_get_filelist(RPM_DESC) ->
	join_filelist(proplists:get_value(1116, RPM_DESC#rpm.header),
				proplists:get_value(1117, RPM_DESC#rpm.header),
				proplists:get_value(1118, RPM_DESC#rpm.header)).

%% filelist is in 3 indexes: basenames, directories and "a link" between them, dir index.
%% length (Dir_index) == length( Base_names)
%% directories 
join_filelist(Dir_index, Base_names, Directories) -> %when is_binary(Dir_indexes) ->
	%lists:map(fun(A) -> io:format("[~s],~n",[A]) end,Base_names ++ Directories),
	%io:format("~w~n ~s~n ~s~n", [Dir_index, Base_names, Directories]),
	lists:map(fun({A,B})-> A++B end, 
			  lists:keymap( fun(Index)->lists:nth(Index + 1, Directories) end, 
							1, 
							lists:zip((Dir_index), Base_names)
			  )
	).
	





