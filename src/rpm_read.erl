-module(rpm_read).
-compile(export_all).
%-on_load(on_load/0).
%-include("bin_to.erl").
-include("../include/rpmtags.hrl").
-include_lib("kernel/include/file.hrl").

%% Definition from http://www.rpm.org/max-rpm/s1-rpm-file-format-rpm-file-format.html
-define(RPM_LEAD, <<Magic:32,Major:8,Minor:8,Type:16,Archnum:16,Name:528/bits,Osnum:16,Sign_type:16,Reserved:128>>).
-define(SIGNATURE, <<Magic:24,_Header_version:8,_Reserved:32,Index_count:32,Signature_bytes:32>>).
-define(INDEX,<<Tag:32,Data_type:32,Offset:32,Num_of_entries:32>>).

%% One more helper function due to rpm header structure
round_offset_by_eight(F,Off) ->
	%debug("current offset is ~p in file and ~B bytes read", [file:position(F,cur), Off]),
	case Off rem 8 of
		0 -> file:position(F,cur);
		_ -> file:position(F,{cur,8 - (Off rem 8)})
	end.

%String=lists:sublist((binary:split(Read_Data,<<0>>,[global])),Index#rpm_tag_index.num_of_entries),
get_strings(Data, Count) -> get_strings(Data, Count, <<>>, []).
get_strings(_Data, 0, _Str, Acc) -> lists:reverse(Acc);
get_strings(<<0,Data/binary>>, Count, Str, Acc) -> get_strings(Data, Count-1, <<>>, [Str|Acc]);
get_strings(<<C,Data/binary>>, Count, Str, Acc) -> get_strings(Data, Count, <<Str/binary,C>>, Acc).
	

%% A helper for future integration with lager or other logger
debug(Message,Args) ->
	%io:format(Message,Args),
	true.

%% A function used in shell to test module.
on_load(File) ->
	RPM = read_rpm(File),
	rpm_get_filelist(RPM).

%% A function that reads and parses RPM, fulfills an RPM description. 
%% This is to be used, if you have to work with RPMs
read_rpm(RPM) ->
		{ok,F} = file:open(RPM,[read, binary, read_ahead,raw]),
		{ok, Lead} = read_rpm_lead(F),
		{ok, {header_values_length, Max_Offset},Indexes} = read_rpm_header(F),
		{ok, {SOff,Signature}} = read_rpm_header_data(F,Indexes, Max_Offset),
		%% signature may end anywhere in the file. We should round the offset by 8 byte boundary
		{ok, Header_start} = round_offset_by_eight(F,SOff),
		{ok,{header_values_length, Max_Header_Offset}, Head_indexes} = read_rpm_header(F),
		{ok, {HOff,Header}} = read_rpm_header_data(F,Head_indexes, Max_Header_Offset),
		{ok, Header_end} = file:position(F,cur),
		% {ok, _} = round_offset_by_eight(F,HOff),
			{ok, Fileprops} = file:read_file_info(RPM,[{time,posix}]), 
			Checksum = rpm_get_checksum(RPM),
			file:close(F),
			#rpm{filename=RPM, 
				 lead=Lead, 
				signature=Signature, 
				header=[{href,RPM}|[{header_range,[{start,integer_to_list(Header_start)},{'end',integer_to_list(Header_end)}]}|Header]], 
				fileprops=Fileprops, 
				checksum=Checksum}.
			
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

	read_rpm_header(FileName) ->
		Header_length=16,
		{ok,Data} = file:read(FileName,Header_length),
		%debug("~w",[Data]),
		?SIGNATURE = Data, 
		% 9350632 = 16#8eade8 - header magic number
		Magic = 9350632,
		%debug("Read magic: ~.16B , will read ~B index records with  ~B bytes of signature~n",[Magic,Index_count,Signature_bytes]),
		read_rpm_index_entry(FileName,Index_count, Signature_bytes).
		
	read_rpm_index_entry(FileName, N, Max_Offset) 	-> read_rpm_index_entry(FileName,N, Max_Offset, []).
	read_rpm_index_entry(_, 0, Max_Offset, Acc)		->
		%% return sorted by offset index list from Acc 
		%% we'll read index values in this order later.
		debug("~p", [Acc]),
		{ok, {header_values_length, Max_Offset},
		 lists:sort(
			fun(A,B) when 
				A#rpm_tag_index.offset =< B#rpm_tag_index.offset	-> true; 
														(_, _)		-> false 
			end
			, Acc)
		};
	read_rpm_index_entry(FileName, Num_of_indexes, Max_Offset, Acc) when Num_of_indexes > 0 -> 
		%% index is 4 fields of 4 bytes. Read them recursevly. Then proceed to data reading
		Index_length=16,
		{ok, Data} = file:read(FileName,Index_length*Num_of_indexes),
		Indexes = [#rpm_tag_index{tag=Tag,data_type=Data_type, offset=Offset,num_of_entries=Num_of_entries} ||
									 <<Tag:32,Data_type:32,Offset:32,Num_of_entries:32>> <= Data ],
		
		read_rpm_index_entry(FileName, 0, Max_Offset, Indexes).
		

	read_rpm_header_data(F, Indexes, Max_Offset) -> read_rpm_index_value(F, Indexes, Max_Offset, {0, []}).

	% reading ended. Return.
	read_rpm_index_value(_, [], _, {Off, Data}) -> 
		%debug("read tag data till ~B",[Off]),
		{ok, {Off,Data}};

	% read some data till next tag boundary
	read_rpm_index_value(F, [Index|Tail], Max_Offset, {Off,Data}) when Index#rpm_tag_index.offset - Off /= 0, Off =< Max_Offset -> 
		%debug("Skipping through from ~B to ~B ~n",[Off, Index#rpm_tag_index.offset]),
		{ok, _} = file:read(F,Index#rpm_tag_index.offset - Off),
		read_rpm_index_value(F, [Index|Tail], Max_Offset, {Index#rpm_tag_index.offset,Data});

	% going to read a 6-th data type - a string
	% or a list of string (8 and 9). 7 is binary and is read the other way
	read_rpm_index_value(F, [Index|Tail], Max_Offset, {Off, Data}) when Index#rpm_tag_index.data_type == 6; Index#rpm_tag_index.data_type > 7 ->
		[Head|_]= Tail,
		{ok, Read_Data} = file:read(F, Head#rpm_tag_index.offset - Index#rpm_tag_index.offset),
		% notice that empty fileds would be trimmed off here. Also, we may read some <<0>> padding here.
		%String=get_strings(Read_Data,Index#rpm_tag_index.num_of_entries),
		String=lists:sublist((binary:split(Read_Data,<<0>>,[global])),Index#rpm_tag_index.num_of_entries),
		Tag_data = {Index#rpm_tag_index.tag, String},
		read_rpm_index_value(F, Tail, Max_Offset, {Head#rpm_tag_index.offset - Index#rpm_tag_index.offset +Off,[Tag_data|Data]});

	% a binary worth it's own implementation, cause doesn't require any transformation
	read_rpm_index_value(F, [Index|Tail], Max_Offset, {Off, Data}) when Index#rpm_tag_index.data_type == 7  ->
		Multiplier = proplists:get_value(Index#rpm_tag_index.data_type, rpm_types_sizes()),
		{ok, D} = file:read(F, Index#rpm_tag_index.num_of_entries * Multiplier),
		%debug("read ~B bytes by ~B bytes, starting from ~B~n",[Index#rpm_tag_index.num_of_entries * Multiplier, Multiplier, Off]),
		read_rpm_index_value(F, Tail, Max_Offset, {Off + Index#rpm_tag_index.num_of_entries * Multiplier, [D|Data]});

	% read a "simple" value which size can be calculated using Index information.
read_rpm_index_value(F, [Index|Tail], Max_Offset, {Off, Data}) when Index#rpm_tag_index.data_type < 6  ->
	%Off=Index#rpm_sig_index.offset,
	Multiplier = proplists:get_value(Index#rpm_tag_index.data_type, rpm_types_sizes()),
	{ok, D} = file:read(F, Index#rpm_tag_index.num_of_entries * Multiplier), 
	%debug("read ~B bytes by ~B bytes, starting from ~B~n",[Index#rpm_tag_index.num_of_entries * Multiplier, Multiplier, Off]),
	%debug("~p,",[D]),
	Tag_data = {Index#rpm_tag_index.tag, [ Bin || <<Bin:Multiplier/unit:8>> <= D ]},
	%debug("~w~n",[Tag_data]),
	read_rpm_index_value(F, Tail, Max_Offset, {Off + Index#rpm_tag_index.num_of_entries * Multiplier, [Tag_data|Data]}).


%%%
%%% Here goes functions that get data from RPM description read above
%%% RPM_DESC is an rpm_read function output
%%%


%% helper functions which does 90% of "get_tag_value" job...
rpm_get_header_parameter_by_id(RPM_DESC, Id, Default) ->
	proplists:get_value(Id, RPM_DESC#rpm.header, Default).
rpm_get_header_parameter_by_id(RPM_DESC, Id) ->
	proplists:get_value(Id, RPM_DESC#rpm.header, []).

rpm_get_signature_parameter_by_id(RPM_DESC, Id) ->
	proplists:get_value(Id, RPM_DESC#rpm.signature, []).

rpm_get_file_parameter_by_id(RPM_DESC, Id)  ->
	case Id of
					% 719528*86400 is seconds from 1 Jan 0 to Epoch (1 Jan 1970)
		mtime	-> integer_to_list(RPM_DESC#rpm.fileprops#file_info.mtime); %io_lib:write(calendar:datetime_to_gregorian_seconds(RPM_DESC#rpm.fileprops#file_info.mtime) - 719528*86400); 
		size	-> integer_to_list( RPM_DESC#rpm.fileprops#file_info.size)
	end.

rpm_get_checksum(RPM_DESC) when is_tuple(RPM_DESC)->
	RPM_DESC#rpm.checksum;
rpm_get_checksum(Filename) ->
	{ok, F} = file:open(Filename,[read, binary, read_ahead,raw]),
	ShaContext = crypto:hash_init(sha256),
	MdContext = crypto:hash_init(md5),
	rpm_get_checksum(F, MdContext, ShaContext).
rpm_get_checksum(F, MdContext, ShaContext) ->
	% there is no proper way of converting to textual hex.
	% my hex is from erlang@c.j.r. It's fast and simple.
	case file:read(F, 1048576) of
		eof -> file:close(F), {checksum,% bin_to:hex(crypto:md5_final(MdContext)),
						[{type, sha256},{pkgid,'YES'}], bin_to:hex(crypto:hash_final(ShaContext))};
		{ok, Data} -> rpm_get_checksum(F, crypto:hash_update(MdContext,Data), crypto:hash_update(ShaContext,Data))
	end.


%% filelist is in 3 indexes: basenames, directories and "a link" between them, dir index.
%% length (Dir_index) == length( Base_names)
%% directories are numerated from 0, so have to add 1 in nth/2
%% if at least on of these arrays are not defined - it's an RPM without files (e.g. meta package with reqs only).

ets_insert_list(Tab, [H|T], Count) -> ets:insert(Tab,{Count,H}), ets_insert_list(Tab, T, Count +1 );
ets_insert_list(Tab, [], _Count) -> ok.

join_filelist(_, Dir_index, Base_names, Directories) 
	when Dir_index == undefined; Base_names == undefined; Directories == undefined -> [];

join_filelist(RPM_DESC, Dir_index, Base_names, Directories) -> 
		case ets:lookup(dirs,get_package_id(RPM_DESC)) of 
			[{_,List}] -> List;
			[] ->
				ets_insert_list(dirs,Directories,0),
				List=lists:zipwith(fun(A,B)-> << (ets:lookup_element(dirs, A, 2))/binary, B/binary>>  end, Dir_index,  Base_names),
				ets:insert(dirs,{get_package_id(RPM_DESC), List}), 
				List
		end.

%%% Functions to work with rpm description from read_rpm/1
rpm_get_filelist(RPM_DESC) ->
	case proplists:get_value(1027, RPM_DESC#rpm.header) of
		undefined -> join_filelist(RPM_DESC, 
								   proplists:get_value(1116, RPM_DESC#rpm.header),
								   proplists:get_value(1117, RPM_DESC#rpm.header),
				                   proplists:get_value(1118, RPM_DESC#rpm.header));
		A -> A
	end.

is_sublist(_Sublist,[]) -> false;
is_sublist(Sublist,L=[_H|T]) ->
	lists:prefix(Sublist,L) orelse is_sublist(Sublist,T).

is_subbinary(_, <<>>) -> false;
is_subbinary(Subbin, <<Subbin,_A/binary>>) -> true;
is_subbinary(Subbin, <<_H,A/binary>>) -> is_subbinary(Subbin,A).


rpm_get_primary_filelist(RPM_DESC) ->
	lists:filter( fun ({_,_,[<<"/etc/",_Name/binary>>]}) -> true;
					  ({_,_,[<<"/usr/lib/sendmail">>]}) -> true;
					  ({_,_,[<<Name/binary>>]}) -> nomatch /= binary:match(Name,<<"bin/">>)
				  end,
				 rpm_get_files_with_types(RPM_DESC)).

rpm_get_name(RPM_DESC)-> rpm_get_header_parameter_by_id(RPM_DESC, 1000).
rpm_get_arch(RPM_DESC)-> rpm_get_header_parameter_by_id(RPM_DESC, 1022).
rpm_get_version(RPM_DESC) -> 
	{version, [{epoch, lists:map(fun (A) -> integer_to_list(A) end, rpm_get_header_parameter_by_id(RPM_DESC, 1003, [0]))},
				{ver, rpm_get_header_parameter_by_id(RPM_DESC, 1001)},
				{rel, rpm_get_header_parameter_by_id(RPM_DESC, 1002)}
			  ]}.
rpm_get_summary(RPM_DESC) -> {summary,[],[lists:nth(1,rpm_get_header_parameter_by_id(RPM_DESC, 1004))]}.
rpm_get_description(RPM_DESC) -> {description, [], [lists:nth(1,rpm_get_header_parameter_by_id(RPM_DESC, 1005))]}.
rpm_get_packager(RPM_DESC) -> {packager, [], rpm_get_header_parameter_by_id(RPM_DESC, 1015)}.
rpm_get_license(RPM_DESC) -> {'rpm:license',[],rpm_get_header_parameter_by_id(RPM_DESC, 1014)}.
rpm_get_url(RPM_DESC) -> case rpm_get_header_parameter_by_id(RPM_DESC, 1020) of
						 [] ->  {url, []};
						 A -> {url,[],A}
						end.
% http://lists.baseurl.org/pipermail/rpm-metadata/2010-April/001159.html 
% "file" time in repo is mtime of the package
rpm_get_filetime(RPM_DESC) -> {file, rpm_get_file_parameter_by_id(RPM_DESC, mtime)}.
rpm_get_buildtime(RPM_DESC) -> {build,rpm_get_header_parameter_by_id(RPM_DESC, 1006)}.
rpm_get_archive_size(RPM_DESC) -> {archive,integer_to_list(lists:nth(1,rpm_get_signature_parameter_by_id(RPM_DESC, 1007)))}.
rpm_get_installed_size(RPM_DESC) -> {installed, integer_to_list(lists:nth(1,rpm_get_header_parameter_by_id(RPM_DESC, 1009)))}.
rpm_get_location(RPM_DESC) -> {location, [{href,rpm_get_header_parameter_by_id(RPM_DESC, href)}]}.
rpm_get_vendor(RPM_DESC) -> {'rpm:vendor',[],[rpm_get_header_parameter_by_id(RPM_DESC, 1011)]}.
rpm_get_group(RPM_DESC) -> {'rpm:group',[],[rpm_get_header_parameter_by_id(RPM_DESC, 1016)]}.
rpm_get_buildhost(RPM_DESC) -> {'rpm:buildhost',[],[rpm_get_header_parameter_by_id(RPM_DESC, 1007)]}.
rpm_get_src(RPM_DESC) -> {'rpm:sourcerpm',[],[rpm_get_header_parameter_by_id(RPM_DESC, 1044)]}.
rpm_get_header_range(RPM_DESC) -> {'rpm:header-range',rpm_get_header_parameter_by_id(RPM_DESC, header_range)}.
rpm_get_files_flags(RPM_DESC) -> rpm_get_header_parameter_by_id(RPM_DESC, 1037).
rpm_get_files_md5(RPM_DESC) -> rpm_get_header_parameter_by_id(RPM_DESC, 1035).
rpm_get_files_modes(RPM_DESC) -> rpm_get_header_parameter_by_id(RPM_DESC, 1030).
rpm_get_files_with_types(RPM_DESC) -> lists:zipwith3(fun (Name,Mode, _) when Mode band 16#4000 /= 0 -> % stat.h -> s_isdir 
																{file,[{type,dir}],[Name]}; 
														(Name,_, Flags) when Flags band 16#40 /= 0 -> 
																{file,[{type,ghost}],[Name]}; 
														(Name,_, _) -> 
																{file,[],[Name]} end, 
												rpm_get_filelist(RPM_DESC), rpm_get_files_modes(RPM_DESC),rpm_get_files_flags(RPM_DESC)).


rpm_normalize_version(<<>>) -> [];
rpm_normalize_version(A) -> helper_parse_version(A,<<>>,[],<<>>).%helper_find_epoch(A, <<>>).

helper_find_epoch(<<>>,				Acc)	-> [{epoch,"0"},{ver,Acc}];
helper_find_epoch(<<":",T/binary>>, Acc)	-> helper_find_version(T,<<>>, [{epoch,Acc}]);
helper_find_epoch(<<"-",T/binary>>, Acc)	-> [{epoch,"0"},{ver,Acc},{rel,T}];
helper_find_epoch(<<H,T/binary>>,	Acc)	-> helper_find_epoch(T, <<Acc/binary,H>>).

helper_find_version(<<>>,Acc,Return_value) -> [Return_value, {ver,Acc}];
helper_find_version(<<"-",T/binary>>,Acc, Return_value) -> [Return_value,{ver,Acc},{rel,T}];
helper_find_version(<<H,T/binary>>,Acc, Return_value) -> helper_find_version(T,<<Acc/binary,H>>, Return_value).

%helper_parse_version(Version, Acc, Result, State)
%states: initial, epoch, first, second (first and second - dashes).
helper_parse_version(<<>>, Acc, [], <<>>)	 			    -> [{epoch, "0"}, {ver, Acc}];
helper_parse_version(<<>>, Acc, [], Acc2)	 			    -> [{epoch, "0"}, {ver, Acc2}, {rel, Acc}];
helper_parse_version(<<>>, Acc, Result, Acc2)			    -> lists:reverse([{rel,Acc}|[{ver, Acc2}|Result]]);
helper_parse_version(<<>>, Acc, Result, <<>>)			    -> lists:reverse([{ver, Acc}|Result]);
helper_parse_version(<<":",T/binary>>, Acc, [],   <<>>)	-> helper_parse_version(T, <<>>, [{epoch, Acc}], <<>>);
helper_parse_version(<<"-",T/binary>>, Acc, Result, <<>>)	-> helper_parse_version(T, <<>>, Result, Acc);
helper_parse_version(<<"-",T/binary>>, Acc, Result, Acc2)   -> lists:reverse([{rel,T}|[ {ver,[Acc2, "-", Acc]}|Result]]);
helper_parse_version(<<H,T/binary>>,   Acc, Result, Acc2)	-> helper_parse_version(T, <<Acc/binary,H>>,  Result, Acc2).

	

zipwith3_wrapper(RPM_DESC, {ID1, ID2, ID3}) -> 
	lists:zipwith3( fun(Name,Flags, Version) ->								
						{'rpm:entry',[{name,Name},
                        rpm_sense_values_to_text(Flags),
                        rpm_normalize_version(Version)]} end,
					rpm_get_header_parameter_by_id(RPM_DESC, ID1),
					rpm_get_header_parameter_by_id(RPM_DESC, ID2),
					rpm_get_header_parameter_by_id(RPM_DESC, ID3)
	   			  ).
	  
rpm_get_provides(RPM_DESC) -> 
	case zipwith3_wrapper(RPM_DESC, {1047, 1112, 1113}) of
					[] -> [];
		Non_empty_list -> {'rpm:provides', [], Non_empty_list}
	end.
							
rpm_get_requires(RPM_DESC) -> 
case zipwith3_wrapper(RPM_DESC, {1049, 1048, 1050}) of
	[] -> [];
	Non_empty_list -> {'rpm:requires', [], Non_empty_list}
end.
	%case lists:filter(	fun ({'rpm:entry',[{name,<<"rpmlib(",_Name/binary>>}|_]}) -> false;
%							({'rpm:entry',[{name,Name}|_]}) -> true
%						end, 
%							%nomatch == binary:match(Name,<<"rpmlib(">>)  end, 
%						zipwith3_wrapper(RPM_DESC, {1049, 1048, 1050})) of
%					[] -> [];
%		Non_empty_list -> {requires, [], Non_empty_list}
%	end.

rpm_get_conflicts(RPM_DESC) -> 
	case zipwith3_wrapper(RPM_DESC, {1054, 1053, 1055}) of
					[] -> [];
		Non_empty_list -> {'rpm:conflicts', [], Non_empty_list}
	end.

rpm_get_obsoletes(RPM_DESC) -> 
	case zipwith3_wrapper(RPM_DESC, {1090, 1114, 1115}) of
					[] -> [];
		Non_empty_list -> {'rpm:obsoletes', [], Non_empty_list}
	end.

rpm_sense_values_to_text(Value) when is_list(Value) ->
	rpm_sense_values_to_text(list_to_integer(Value));
rpm_sense_values_to_text(Value) when is_integer(Value) ->
	BValue = binary:encode_unsigned(Value),
	NBValue = case byte_size(BValue) of
		1 -> <<0,0,0,BValue/binary>>;
		2 -> <<0,0,BValue/binary>>;
		3 -> <<0,BValue/binary>>;
		4 -> <<BValue/binary>>;
		true -> {error, "unknown rpm sense"}
	end,
	rpm_sense_values_to_text(NBValue);
	
% a little bit ugly, but it will help to render value a little bit easier
rpm_sense_values_to_text(Value) when is_binary(Value) ->
%	<<_:7,_Lib:1,_:11,_Postun:1,_Preun:1,_Post:1,Pre:1,_Interp:1, _:1,_Prereq:1,_:2,_Eq:1,_Gt:1,_Lt:1,_:1>> = Value,
%	Pre_Value = if	
%		Pre==1	-> {pre, "1"};
%		true	-> []
%	end,
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
	[Main_flags_value].%,Pre_Value].

rpm_get_chagelog_entries(RPMD) ->
	lists:zipwith3( fun (A,B,C) -> {changelog,[{author,A},{date,B}], [C]} end,
					rpm_read:rpm_get_header_parameter_by_id(RPMD,1081),
					rpm_read:rpm_get_header_parameter_by_id(RPMD,1080),
					rpm_read:rpm_get_header_parameter_by_id(RPMD,1082)
				).


get_package_id(RPMD) ->
	{_,_,Id}=rpm_get_checksum(RPMD), Id.

get_package_primary_xml(RPMD) ->
	["<package type=\"rpm\">\n", 
	duexml:encode_element({name, [], [rpm_get_name(RPMD)]}),
	duexml:encode_element({arch,[],[rpm_get_arch(RPMD)]}),
	duexml:encode_element(rpm_get_version(RPMD)),
	duexml:encode_element(rpm_get_checksum(RPMD)),
	duexml:encode_element(rpm_get_summary(RPMD)),
	duexml:encode_element(rpm_get_description(RPMD)),
	duexml:encode_element(rpm_get_packager(RPMD)),
	duexml:encode_element(rpm_get_url(RPMD)),
	duexml:encode_element({time,[rpm_get_filetime(RPMD), rpm_get_buildtime(RPMD)]}),
	duexml:encode_element({size,[{package, rpm_get_file_parameter_by_id(RPMD, size)},rpm_get_installed_size(RPMD),rpm_get_archive_size(RPMD)]}),
	duexml:encode_element(rpm_get_location(RPMD)),
	<<"<format>\n">>,
	duexml:encode_element(rpm_get_license(RPMD)),
	duexml:encode_element(rpm_get_vendor(RPMD)),
	duexml:encode_element(rpm_get_group(RPMD)),
	duexml:encode_element(rpm_get_buildhost(RPMD)),
	duexml:encode_element(rpm_get_src(RPMD)),
	duexml:encode_element(rpm_get_header_range(RPMD)),
	duexml:encode_element(rpm_get_provides(RPMD)),
	duexml:encode_element(rpm_get_requires(RPMD)),
    duexml:encode_element(rpm_get_conflicts(RPMD)),
    duexml:encode_element(rpm_get_obsoletes(RPMD)),
	duexml:encode_element(rpm_get_primary_filelist(RPMD)),
	<<"</format>\n">>,
	<<"</package>\n">>
	].

get_package_filelist_xml(RPMD) -> 
	duexml:encode_element({package,[{pkgid, get_package_id(RPMD)}, {name,rpm_get_name(RPMD)}, {arch,rpm_get_arch(RPMD)}],
			[rpm_get_version(RPMD),
			 rpm_get_files_with_types(RPMD)
			]
	}).	


get_package_other_xml(RPMD) ->
    duexml:encode_element({package,[{pkgid, get_package_id(RPMD)}, {name,rpm_get_name(RPMD)}, {arch,rpm_get_arch(RPMD)}],
						[rpm_get_version(RPMD),
						lists:reverse(lists:sublist(rpm_get_chagelog_entries(RPMD),10))
						]
					}
				  ).






generate_repo(DirName) ->
	ets:info(dirs) /= undefined orelse ets:new(dirs,[named_table, {read_concurrency,true}]),
	ets:info(atoms) /= undefined orelse ets:new(atoms,[named_table, {read_concurrency,true}]),
	%ets:i(),
	{ok,DirList} = file:list_dir(DirName),
	RPMDS=lists:filtermap( fun(Elem) -> 
						case lists:suffix(".rpm",Elem) of
							true -> {true, read_rpm(filename:join([DirName,Elem]))};
							false -> false
						end end,
					DirList),
	generate_primary_xml(RPMDS,filename:join([DirName,"primary.xml"])),
	generate_filelist_xml(RPMDS,filename:join([DirName,"filelist.xml"])),
	generate_other_xml(RPMDS,filename:join([DirName,"other.xml"])).

generate_primary_xml(RPMDS,Filename) ->
	{ok,Primary}=file:open(Filename, [raw,write]),
	file:write(Primary, [ "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n<metadata xmlns=\"http://linux.duke.edu/metadata/common\"",
						  " xmlns:rpm=\"http://linux.duke.edu/metadata/rpm\" packages=\"",
							integer_to_list(length(RPMDS)),"\">\n"]),
	lists:foreach(fun(Elem) -> %io:format("~s~n",[rpm_get_header_parameter_by_id(Elem, href)]),
							%io:format("~s~n", [io:write(get_package_primary_xml(Elem))]),
						  file:write(Primary,get_package_primary_xml(Elem)) end, 
			  RPMDS),
	file:write(Primary,["</metadata>"]),
	file:close(Primary).

generate_filelist_xml(RPMDS,Filename ) ->
	{ok,Filelist}=file:open(Filename, [raw, write]),
	file:write(Filelist, ["<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n<filelists xmlns=\"http://linux.duke.edu/metadata/filelists\" packages=\"",
							integer_to_list(length(RPMDS)),"\">\n"]),
	lists:foreach(fun(Elem) -> file:write(Filelist,get_package_filelist_xml(Elem)) end, RPMDS),
	file:write(Filelist,["</filelist>"]),
	file:close(Filelist).

generate_other_xml(RPMDS, Filename) ->
	{ok,Other}=file:open(Filename, [raw, write]),
	file:write(Other,["<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n<otherdata xmlns=\"http://linux.duke.edu/metadata/other\" packages=\"",
						integer_to_list(length(RPMDS)),"\">\n"]),
	lists:foreach(fun(Elem) -> file:write(Other,get_package_filelist_xml(Elem)) end, RPMDS),
	file:write(Other,["</otherdata>"]),
	file:close(Other).




