-module(rpm_read).
-compile(export_all).
%-on_load(on_load/0).
-include("rpm.types.hrl").
%% Definition from http://www.rpm.org/max-rpm/s1-rpm-file-format-rpm-file-format.html
-define(RPM_LEAD, <<Magic:32,Major:8,Minor:8,Type:16,Archnum:16,Name:528/bits,Osnum:16,Sign_type:16,Reserved:128>>).
-define(SIGNATURE, <<Magic:24,Header_version:8,Reserved:32,Index_count:32,Signature_bytes:32>>).
-define(INDEX,<<Tag:32,Data_type:32,Offset:32,Num_of_entries:32>>).

%% A helper function for C strings reading.
%% It's almost for sure have to be rewritten
read_till_zero(F) -> read_till_zero(F, 1 ). 
read_till_zero(F, Count) -> read_till_zero(F, Count, [], 0, <<>>).
read_till_zero(F, 0, Strings, Offset , _ ) -> {ok, {Offset, Strings}};
read_till_zero(F, Count, Strings, Offset , Acc) ->
	case file:read(F,1) of
		{ok, <<0>>} -> read_till_zero(F, Count - 1, [{binary:bin_to_list(Acc)}|Strings], Offset + 1, <<>>);
		{ok, Data} when is_binary(Data) -> 
			read_till_zero(F, Count, Strings, Offset + 1, <<Acc/binary,Data/binary>> )
	end.

%% One more helper function due to rpm header structure
round_offset_by_eight(F,Off) ->
	file:read(F,Off rem 8).

%% A function used in shell to test module.

read_rpm_desc(RPM) ->
		{ok,F} = file:open(RPM,[read, raw, binary, read_ahead]),
		read_rpm_lead(F),
		{ok, Indexes} = read_rpm_signature_header(F),
		{ok, {Off,Signature}} = read_rpm_header_data(F,Indexes),
		%% signature may end anywhere in the file. We should round the offset by 8 byte boundary
		round_offset_by_eight(F,Off),
		{ok, Head_indexes} = read_rpm_signature_header(F),
		{ok, Header} = read_rpm_header_data(F,Head_indexes).
		
read_rpm_lead(F) ->
	Header_length=96,
	{ok, ?RPM_LEAD} = file:read(F,Header_length),
	%?RPM_LEAD = Data,
	#rpm_lead{ magic = Magic,	major = Major, minor = Minor, type = Type, archnum = Archnum,
        		name = Name, osnum = Osnum,	signature_type = Sign_type,
		   		reserved = 0 = Reserved 	% should always be zeroed.
			   }.

read_rpm_signature_header(F) ->
    Header_length=16,
    {ok,Data} = file:read(F,Header_length),
	?SIGNATURE = Data, Magic = 9350632,
	io:fwrite("~.16B ~B ~B~n",[Magic,Index_count,Signature_bytes]),
	read_rpm_index_entry(F,Index_count).
	
read_rpm_index_entry(F, N) 	-> read_rpm_index_entry(F,N, []).
read_rpm_index_entry(F, 0, Acc) ->
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
	% io:fwrite("~B ~B ~B ~B~n",[Tag, Data_type, Offset, Num_of_entries]),
	read_rpm_index_entry(F, Num_of_indexes - 1, [#rpm_sig_index{tag=Tag,data_type=Data_type, offset=Offset,num_of_entries=Num_of_entries}|Acc]).
	

read_rpm_header_data(F, Indexes) -> read_rpm_tag_data(F, Indexes, {0, []}).


% reading ended. Return.
read_rpm_tag_data(F, [], {Off, Data}) -> {ok, {Off,Data}};

% read some data till next tag boundary
read_rpm_tag_data(F, [Index|Tail], {Off,Data}) when Index#rpm_sig_index.offset - Off /= 0 -> 
	% io:fwrite("reading through from ~B to ~B ~n",[Off, Index#rpm_sig_index.offset]),
	{ok, _} = file:read(F,Index#rpm_sig_index.offset - Off),
	read_rpm_tag_data(F, [Index|Tail], {Index#rpm_sig_index.offset,Data});

% going to read a 6-th data type - a string
% or a list of string (8 and 9). 7 is binary and is read the other way
read_rpm_tag_data(F, [Index|Tail], {Off, Data}) when Index#rpm_sig_index.data_type == 6; Index#rpm_sig_index.data_type > 7 ->
	{ok,{Offset,String}} =  read_till_zero(F, Index#rpm_sig_index.num_of_entries),
			% io:fwrite("starting from ~B read ~B ~n",[Off,Offset]),
	% TODO: Need to make distinction here which proplist to use: signature or header
	% Tag_data = {proplists:get_value(Index#rpm_sig_index.tag, rpm_tags()), String},
	Tag_data = {Index#rpm_sig_index.tag, String},
	read_rpm_tag_data(F, Tail, {Offset+Off,[Tag_data|Data]});

% read a "simple" value which size can be calculated using Index information.
read_rpm_tag_data(F, [Index|Tail], {Off, Data}) when Index#rpm_sig_index.data_type /= 6, Index#rpm_sig_index.data_type < 8  ->
	Multiplier = proplists:get_value(Index#rpm_sig_index.data_type, rpm_types_sizes()),
	{ok, D} = file:read(F, Index#rpm_sig_index.num_of_entries * Multiplier), 
	% io:fwrite("~B~n",[Off]),
	% Tag_data = {proplists:get_value(Index#rpm_sig_index.tag, rpm_tags()), D},
	Tag_data = {Index#rpm_sig_index.tag, D},
	read_rpm_tag_data(F, Tail, {Off + Index#rpm_sig_index.num_of_entries * Multiplier, [Tag_data|Data]}).







