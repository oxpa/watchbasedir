-module(rpm_read).
-compile(export_all).
%-on_load(on_load/0).
%-include("bin_to.erl").
-include("rpmtags.hrl").
-include_lib("kernel/include/file.hrl").

%% Definition from http://www.rpm.org/max-rpm/s1-rpm-file-format-rpm-file-format.html
-define(RPM_LEAD, <<Magic:32,Major:8,Minor:8,Type:16,Archnum:16,Name:528/bits,Osnum:16,Sign_type:16,Reserved:128>>).
-define(SIGNATURE, <<Magic:24,_Header_version:8,_Reserved:32,Index_count:32,Signature_bytes:32>>).
-define(INDEX,<<Tag:32,Data_type:32,Offset:32,Num_of_entries:32>>).

%% A helper function for C strings reading.
%% It's almost for sure have to be rewritten
read_till_zero(F, Max_Offset) -> read_till_zero(F, 1, Max_Offset ). 
read_till_zero(F, Count, Max_Offset) -> read_till_zero(F, Count, Max_Offset, [], 0, <<>>).
read_till_zero(_, 0, _, Strings, Offset , _ ) -> {ok, {Offset, lists:reverse(Strings)}};
read_till_zero(F, Count, Max_Offset, Strings, Offset , Acc) when Offset =< Max_Offset ->
	case file:read(F,1) of
		{ok, <<0>>} -> read_till_zero(F, Count - 1, Max_Offset, [binary:bin_to_list(Acc)|Strings], Offset + 1, <<>>);
		{ok, Data} when is_binary(Data) -> 
			           read_till_zero(F, Count, Max_Offset,  Strings, Offset + 1, <<Acc/binary,Data/binary>> )
	end.

%% One more helper function due to rpm header structure
round_offset_by_eight(F,Off) ->
	debug("current offset is ~p in file and ~B bytes read", [file:position(F,cur), Off]),
	case Off rem 8 of
		0 -> file:position(F,cur);
		_ -> file:position(F,{cur,8 - (Off rem 8)})
	end.

%% A helper for future integration with lager or other logger
debug(Message,Args) ->
	io:format(Message,Args),
	true.

%% A function used in shell to test module.
on_load(File) ->
	RPM = read_rpm(File),
	rpm_get_filelist(RPM).

%% A function that reads and parses RPM, fulfills an RPM description. 
%% This is to be used, if you have to work with RPMs
read_rpm(RPM) ->
		{ok,F} = file:open(RPM,[read, binary, read_ahead]),
		{ok, Lead} = read_rpm_lead(F),
		{ok, {header_iValues_length, Max_Offset},Indexes} = read_rpm_header(F),
		{ok, {SOff,Signature}} = read_rpm_header_data(F,Indexes, Max_Offset),
		%% signature may end anywhere in the file. We should round the offset by 8 byte boundary
		{ok, Header_start} = round_offset_by_eight(F,SOff),
		{ok,{header_iValues_length, Max_Header_Offset}, Head_indexes} = read_rpm_header(F),
		{ok, {HOff,Header}} = read_rpm_header_data(F,Head_indexes, Max_Header_Offset),
		{ok, Header_end} = round_offset_by_eight(F,HOff),
		{ok, Fileprops} = file:read_file_info(RPM), 
		#rpm{filename=RPM, lead=Lead, signature=Signature, header=[{header_range,[Header_start,Header_end]}|Header], fileprops=Fileprops}.
		
read_rpm_lead(F) ->
	Header_length=96,
	{ok, ?RPM_LEAD} = file:read(F,Header_length),
	%?RPM_LEAD = Data,
	if 
		Magic == 3987467995 -> % 16#edabeedb - rpm lead magic number.
			{ok, #rpm_lead{ magic = Magic,	major = Major, minor = Minor, type = Type, archnum = Archnum,
							name = Name, osnum = Osnum,	signature_type = Sign_type,
							reserved = Reserved }
			};
		true -> {error, "Wrong LEAD magic."}
	end.

read_rpm_header(F) ->
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
	%% we'll read index values in this order later.
	{ok, {header_iValues_length, Max_Offset},
	 lists:sort(
		fun(A,B) when 
			A#rpm_tag_index.offset =< B#rpm_tag_index.offset	-> true; 
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
	read_rpm_index_entry(F, Num_of_indexes - 1, Max_Offset, [#rpm_tag_index{tag=Tag,data_type=Data_type, offset=Offset,num_of_entries=Num_of_entries}|Acc]).
	

read_rpm_header_data(F, Indexes, Max_Offset) -> read_rpm_index_value(F, Indexes, Max_Offset, {0, []}).


% reading ended. Return.
read_rpm_index_value(_, [], _, {Off, Data}) -> 
	debug("read tag data till ~B",[Off]),
	{ok, {Off,Data}};

% read some data till next tag boundary
read_rpm_index_value(F, [Index|Tail], Max_Offset, {Off,Data}) when Index#rpm_tag_index.offset - Off /= 0, Off =< Max_Offset -> 
	debug("Skipping through from ~B to ~B ~n",[Off, Index#rpm_tag_index.offset]),
	{ok, _} = file:read(F,Index#rpm_tag_index.offset - Off),
	read_rpm_index_value(F, [Index|Tail], Max_Offset, {Index#rpm_tag_index.offset,Data});

% going to read a 6-th data type - a string
% or a list of string (8 and 9). 7 is binary and is read the other way
read_rpm_index_value(F, [Index|Tail], Max_Offset, {Off, Data}) when Index#rpm_tag_index.data_type == 6; Index#rpm_tag_index.data_type > 7 ->
	{ok,{Offset,String}} =  read_till_zero(F, Index#rpm_tag_index.num_of_entries, Max_Offset ),
	debug("Read ~B bytes starting from ~B ~n",[Offset,Off]),
	Tag_data = {Index#rpm_tag_index.tag, String},
	read_rpm_index_value(F, Tail, Max_Offset, {Offset+Off,[Tag_data|Data]});

% read a "simple" value which size can be calculated using Index information.
read_rpm_index_value(F, [Index|Tail], Max_Offset, {Off, Data}) when Index#rpm_tag_index.data_type /= 6, Index#rpm_tag_index.data_type < 8  ->
	%Off=Index#rpm_sig_index.offset,
	Multiplier = proplists:get_value(Index#rpm_tag_index.data_type, rpm_types_sizes()),
	{ok, D} = file:read(F, Index#rpm_tag_index.num_of_entries * Multiplier), 
	debug("read ~B bytes by ~B bytes, starting from ~B~n",[Index#rpm_tag_index.num_of_entries * Multiplier, Multiplier, Off]),
	Tag_data = {Index#rpm_tag_index.tag, [ Bin || <<Bin:Multiplier/unit:8>> <= D ]},
	debug("~w, ~w~n",[D, Tag_data]),
	read_rpm_index_value(F, Tail, Max_Offset, {Off + Index#rpm_tag_index.num_of_entries * Multiplier, [Tag_data|Data]}).


%%%
%%% Here goes functions that get data from RPM description read above
%%% RPM_DESC is an rpm_read function output
%%%


%% helper functions which does 90% of "get_tag_value" job...
rpm_get_header_parameter_by_id(RPM_DESC, Id) ->
	proplists:get_value(Id, RPM_DESC#rpm.header, []).

rpm_get_signature_parameter_by_id(RPM_DESC, Id) ->
	proplists:get_value(Id, RPM_DESC#rpm.signature, []).

rpm_get_file_parameter_by_id(RPM_DESC, Id)  ->
	case Id of
					% 719528*86400 is seconds from 1 Jan 0 to Epoch (1 Jan 1970)
		mtime	-> calendar:datetime_to_gregorian_seconds(RPM_DESC#rpm.fileprops#file_info.mtime) - 719528*86400; 
		size	-> RPM_DESC#rpm.fileprops#file_info.size
	end.

rpm_get_checksum(Filename) ->
	{ok, F} = file:open(Filename,[read, binary, read_ahead]),
	ShaContext = crypto:sha_init(),
	MdContext = crypto:md5_init(),
	rpm_get_checksum(F, MdContext, ShaContext).

%% filelist is in 3 indexes: basenames, directories and "a link" between them, dir index.
%% length (Dir_index) == length( Base_names)
%% directories are numerated from 0, so have to add 1 in nth/2
%% if at least on of these arrays are not defined - it's an RPM without files (e.g. meta package with reqs only).

join_filelist(Dir_index, Base_names, Directories) 
	when Dir_index == undefined; Base_names == undefined; Directories == undefined -> {'file',''};

join_filelist(Dir_index, Base_names, Directories) -> 
		% there is no special meaning for [] here. It's a convience element for xml encoding later
        lists:zipwith(fun(A,B)-> {'file', [], lists:nth(A+1, Directories) ++ B} end, Dir_index,  Base_names).

%%% Functions to work with rpm description from read_rpm/1
rpm_get_filelist(RPM_DESC) ->
	case proplists:get_value(1027, RPM_DESC#rpm.header) of
		undefined -> join_filelist(proplists:get_value(1116, RPM_DESC#rpm.header),
								   proplists:get_value(1117, RPM_DESC#rpm.header),
				                   proplists:get_value(1118, RPM_DESC#rpm.header));
		A -> A
	end.

is_sublist(_Sublist,[]) -> false;
is_sublist(Sublist,L=[_H|T]) ->
	lists:prefix(Sublist,L) orelse is_sublist(Sublist,T).

rpm_get_primary_filelist(RPM_DESC) ->
	lists:filter( fun({_,_,Name}) -> lists:prefix("/etc/",Name) orelse ("/usr/lib/sendmail" == Name) orelse is_sublist("bin/", Name) end, 
				 rpm_get_filelist(RPM_DESC)).

rpm_get_name(RPM_DESC)-> {name, [], [rpm_get_header_parameter_by_id(RPM_DESC, 1000)]}.
rpm_get_arch(RPM_DESC)-> {arch, [], [rpm_get_header_parameter_by_id(RPM_DESC, 1022)]}.
rpm_get_version(RPM_DESC) -> 
	{version, [{epoch, io_lib:format("~b", rpm_get_header_parameter_by_id(RPM_DESC, 1003))},
				{version, rpm_get_header_parameter_by_id(RPM_DESC, 1001)},
				{release, rpm_get_header_parameter_by_id(RPM_DESC, 1002)}
			  ]}.
rpm_get_summary(RPM_DESC) -> {summary,[rpm_get_header_parameter_by_id(RPM_DESC, 1004)]}.
rpm_get_checksum(F, MdContext, ShaContext) ->
	% there is no proper way of converting to textual hex.
	% my hex is from erlang@c.j.r. It's fast and simple.
	case file:read(F, 4096) of
		eof -> {checksum, bin_to:hex(crypto:md5_final(MdContext)),
						bin_to:hex(crypto:sha_final(ShaContext))};
		{ok, Data} -> rpm_get_checksum(F, crypto:md5_update(MdContext,Data), crypto:sha_update(ShaContext,Data))
	end.
rpm_get_description(RPM_DESC) -> {description,[rpm_get_header_parameter_by_id(RPM_DESC, 1005)]}.
rpm_get_packager(RPM_DESC) -> {packager,[rpm_get_header_parameter_by_id(RPM_DESC, 1015)]}.
rpm_get_license(RPM_DESC) -> {license,[rpm_get_header_parameter_by_id(RPM_DESC, 1014)]}.
rpm_get_url(RPM_DESC) -> {url,[rpm_get_header_parameter_by_id(RPM_DESC, 1020)]}.
% http://lists.baseurl.org/pipermail/rpm-metadata/2010-April/001159.html 
% "file" time in repo is mtime of the package
rpm_get_filetime(RPM_DESC) -> {file, rpm_get_file_parameter_by_id(RPM_DESC, mtime)}.
rpm_get_buildtime(RPM_DESC) -> {build,[rpm_get_header_parameter_by_id(RPM_DESC, 1006)]}.
rpm_get_archive_size(RPM_DESC) -> {archive,[rpm_get_signature_parameter_by_id(RPM_DESC, 1007)]}.
rpm_get_installed_size(RPM_DESC) -> {installed,[rpm_get_header_parameter_by_id(RPM_DESC, 1009)]}.
%location href
%calculated relative package path
rpm_get_vendor(RPM_DESC) -> {vendor,[rpm_get_header_parameter_by_id(RPM_DESC, 1011)]}.
rpm_get_group(RPM_DESC) -> {group,[rpm_get_header_parameter_by_id(RPM_DESC, 1016)]}.
rpm_get_buildhost(RPM_DESC) -> {buildhost,[rpm_get_header_parameter_by_id(RPM_DESC, 1007)]}.
rpm_get_src(RPM_DESC) -> {sourcerpm,[rpm_get_header_parameter_by_id(RPM_DESC, 1044)]}.
rpm_get_header_range(RPM_DESC) ->{rpm_get_header_parameter_by_id(RPM_DESC, header_range)}.

rpm_normalize_version([]) -> [];
rpm_normalize_version(Version) ->
	 case re:split(Version,"[:-]") of 
		[A,B,C] -> [{epoch,A},{ver,B},{rel,C}];
		[B,C] -> [{epoch, "0"},{ver,B},{rel,C}]
	 end.



rpm_get_provides(RPM_DESC) -> {provides, lists:zipwith3( fun(Name,Flags, Version) ->
								{'rpm:entry',[{name,Name},
											rpm_sens_values_to_text(Flags),
											rpm_normalize_version(Version)]} end,
								rpm_get_header_parameter_by_id(RPM_DESC, 1047),
								rpm_get_header_parameter_by_id(RPM_DESC, 1112),
								rpm_get_header_parameter_by_id(RPM_DESC, 1113)
								)}.
rpm_get_requires(RPM_DESC) -> {requires, 
									lists:filter( fun({'rpm:entry',[{name,Name}|_]}) -> not lists:prefix("rpmlib(", Name)  end, 
										lists:zipwith3( fun(Name,Flags, Version) -> 
											{'rpm:entry',[{name,Name},
												rpm_sens_values_to_text(Flags),
												rpm_normalize_version(Version)]} end,
											rpm_get_header_parameter_by_id(RPM_DESC, 1049),
											rpm_get_header_parameter_by_id(RPM_DESC, 1048),
											rpm_get_header_parameter_by_id(RPM_DESC, 1050)
								))}.
rpm_get_conflicts(RPM_DESC) -> {conflicts, lists:zipwith3( fun(Name,Flags, Version) ->
                                {'rpm:entry',[{name,Name},
                                            rpm_sens_values_to_text(Flags),
                                            rpm_normalize_version(Version)]} end,
								rpm_get_header_parameter_by_id(RPM_DESC, 1054),
								rpm_get_header_parameter_by_id(RPM_DESC, 1053),
								rpm_get_header_parameter_by_id(RPM_DESC, 1055)
								)}.
rpm_get_obsoletes(RPM_DESC) -> {obsoletes, lists:zip3( 
								rpm_get_header_parameter_by_id(RPM_DESC, 1090),
								rpm_get_header_parameter_by_id(RPM_DESC, 1114),
								rpm_get_header_parameter_by_id(RPM_DESC, 1115)
								)}.

rpm_sens_values_to_text(Value) when is_integer(Value) ->
	BValue = binary:encode_unsigned(Value),
	NBValue = case byte_size(BValue) of
		1 -> <<0,0,0,BValue/binary>>;
		2 -> <<0,0,BValue/binary>>;
		3 -> <<0,BValue/binary>>;
		4 -> <<BValue/binary>>;
		true -> {error, "unknown rpm sense"}
	end,
	rpm_sens_values_to_text(NBValue);
	
% a little bit ugly, but it will help to render value a little bit easier
rpm_sens_values_to_text(Value) when is_binary(Value) ->
	<<_:7,_Lib:1,_:11,_Postun:1,_Preun:1,_Post:1,Pre:1,_Interp:1, _:1,_Prereq:1,_:2,_Eq:1,_Gt:1,_Lt:1,_:1>> = Value,
	Pre_Value = if	
		Pre==1	-> {pre,1};
		true	-> []
	end,
	Main_flags = binary:last(Value),
	%% TODO: should parse all flags, not only from primary.xml ;)
	Main_flags_value = if 
		Main_flags == 2		-> {flags, "LT"};
		Main_flags == 4		-> {flags, "GT"};
		Main_flags == 8		-> {flags, "EQ"};
		Main_flags == 10	-> {flags, "LE"};
		Main_flags == 12	-> {flags, "GE"};
		true				-> []
	end,
	[Main_flags_value|Pre_Value].


%%% XML part. Should be placed into a separate file later
%-module("duexml"). %damn useless erlang xml encoder ;)
%

xml_compiled_REs() ->
	% {RE,Replacement}
	REs = [{"&", "\\&amp;"}, {"\"","\\&quot;"}, {"'","\\&apos;"}, {"<","\\&lt;"}, {">","\\&gt;"}],
	lists:foldr(fun({RE,Repl},Acc) -> {ok, RE_C} = re:compile(RE), [{RE_C,Repl}|Acc] end, [], REs).


escape_chars(Text, []) -> Text;
escape_chars(Text, [{Re,Repl}|Replacements]) ->
	escape_chars(re:replace(Text, Re, Repl,[global]), Replacements).


encode_attrs(Attrs) ->
	encode_attrs(Attrs, []).
encode_attrs([], Accum) -> Accum;
encode_attrs(Attrs, Accum) ->
	%TODO:remove compiling REs from here
	REs = xml_compiled_REs(),
	lists:foldr(fun
					({Atom,Value}, Acc) -> [io_lib:format(' ~s="~s"',[Atom,escape_chars(Value,REs)])|Acc]; 
					(List, Acc) when is_list(List) -> [encode_attrs(List)|Acc] end,
				Accum, 
				Attrs).


% a regular element
encode_element({Element_name, Attrs, Text}) when is_atom(Element_name), is_list(Attrs), is_list(Text) ->
	%TODO: remove compiling REs from here
	REs = xml_compiled_REs(),
	io_lib:format("<~s~s>~s</~s>~n", [Element_name, encode_attrs(Attrs), escape_chars(Text,REs), Element_name]);

% a simple element
encode_element({Element_name, Attrs}) when is_atom(Element_name), is_list(Attrs) ->
	io_lib:format("<~s~s/>~n", [Element_name, encode_attrs(Attrs)]);

% for a list of elements
encode_element(A_List) when is_list(A_List) ->
	lists:foldr( fun(Elem, Acc) -> [encode_element(Elem)| Acc] end, [], A_List).

default_start_xml() -> '<xml version="1.0" encoding="UTF-8"?>'.

%%% end of duexml module.

%get_package_primary_xml(RPMD#rpm) ->
%	rpm_get_name(RPMD)





















