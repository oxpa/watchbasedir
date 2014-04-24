-module(duexml). % damn useless erlang xml encoder
-compile(export_all).
%-export([encode_element/1, encode_attrs/1]).
%-on_load(init/0).

%init() ->
%:w	ets:info(atoms) /= undefines orelse ets:new(atoms,[]), ok.
%xml_compiled_REs() ->
	% {RE,Replacement}
	%REs = [{"&", "\\&amp;"}, {"\"","\\&quot;"}, {"'","\\&apos;"}, {"<","\\&lt;"}, {">","\\&gt;"}],
%	REs = [{"&", "\\&amp;"},{"<","\\&lt;"}, {">","\\&gt;"}],
%	lists:foldr(fun({RE,Repl},Acc) -> {ok, RE_C} = re:compile(RE,[no_auto_capture, firstline, ungreedy]), [{RE_C,Repl}|Acc] end, [], REs).


%escape_chars(Text, Acc) when is_atom(Text) ->
%		escape_chars(io_lib:format("~s", [Text]), Acc);
%escape_chars(Text, []) -> Text;
%escape_chars(Text, [{Re,Repl}|Replacements]) ->
%	io:format("~p~n", [Text]),
%	escape_chars(Text, Replacements).%re:replace(Text, Re, Repl,[global,notempty]), Replacements).

escape_chars(Text) when is_atom(Text) ->
		case ets:lookup(atoms,Text) of
			[] -> ets:insert(atoms, {Text,escape_chars(io_lib:format("~s", [Text]))}), escape_chars(Text);
		    [{Text,Encoded}] -> Encoded
		end;
escape_chars(Text) when is_integer(Text) ->
		integer_to_list(Text);
escape_chars(Text) when is_list(Text) ->
		escape_chars(Text,[]);
escape_chars(<<T/binary>>) -> escape_chars(T,<<>>).

%escape_chars("'") -> "&apos;";
%escape_chars("\"") -> "&quot;";
escape_chars(["<"|T], Acc) -> escape_chars(T,["&lt;"|Acc]);
escape_chars([">"|T], Acc) -> escape_chars(T,["&gt;"|Acc]);
escape_chars(["&"|T], Acc) -> escape_chars(T,["&amp;"|Acc]);
escape_chars([H|T], Acc) -> escape_chars(T,[H|Acc]);
escape_chars([], Acc) -> lists:reverse(Acc);

escape_chars(<<"<",T/binary>>,Acc) -> escape_chars(T,<<Acc/binary,"&lt;">>);
escape_chars(<<">",T/binary>>,Acc) -> escape_chars(T,<<Acc/binary,"&gt;">>);
escape_chars(<<"&",T/binary>>,Acc) -> escape_chars(T,<<Acc/binary,"&amp;">>);
escape_chars(<<H,T/binary>>,Acc) -> escape_chars(T,<<Acc/binary,H>>);
escape_chars(<<>>,Acc) -> <<Acc/binary>>.


encode_attrs(Attrs) ->
	encode_attrs(Attrs, []).
encode_attrs([], Accum) -> Accum;
encode_attrs(Attrs, Accum) ->
	lists:foldr(fun
					({Atom,Value}, Acc) -> %debug("encoding ~p |~p|~n",[Atom,Value]), 
										    case ets:lookup(atoms,Atom) of
												[]			->  Text=io_lib:format("~s",[Atom]), ets:insert(atoms, {Atom,Text});
												[{Atom, T}]	->	Text=T
											end,
											[[" ", Text, "=\"", escape_chars(Value), "\"" ] |Acc]; 
					(List, Acc) when is_list(List) -> encode_attrs(List,Acc) end,
				Accum, 
				Attrs).


% conventional functions for nested elements
encode_element({Element_name, Attrs, Elements=[{B, C, _}|_]},Accum) when is_atom(Element_name), is_atom(B), is_list(C) or is_binary(C) ->
	Text = encode_element(Elements),
	case ets:lookup(atoms,Element_name) of
		[] -> Name=io_lib:format('~s',[Element_name]), ets:insert(atoms, {Element_name,Name});
		[{Element_name, Value}] -> Name = Value
	end,
	["<", Name, encode_attrs(Attrs), ">\n", Text, "</", Name,">\n", Accum];

encode_element({Element_name, Attrs, Elements=[{B, C}|_]},Accum) when is_atom(Element_name), is_atom(B), is_list(C) or is_binary(C) ->
	Text = encode_element(Elements),
	case ets:lookup(atoms,Element_name) of
		[] -> Name=io_lib:format('~s',[Element_name]), ets:insert(atoms, {Element_name,Name});
		[{Element_name, Value}] -> Name = Value
	end,
	[["<", Name, encode_attrs(Attrs), ">\n", Text, "</", Name, ">\n"] | Accum];

% a regular element
encode_element({Element_name, Attrs, Text},Accum) when is_atom(Element_name), is_list(Attrs), is_list(Text) ->
	case ets:lookup(atoms,Element_name) of
		[] -> Name=io_lib:format('~s',[Element_name]), ets:insert(atoms, {Element_name,Name});
		[{Element_name, Value}] -> Name = Value
	end,
	["<",Name, encode_attrs(Attrs), ">", escape_chars(Text), "</", Name, ">\n", Accum];

% a simple element
encode_element({Element_name, Attrs},Accum) when is_atom(Element_name), is_list(Attrs) ->
	case ets:lookup(atoms,Element_name) of
		[] -> Name=io_lib:format('~s',[Element_name]), ets:insert(atoms, {Element_name,Name});
		[{Element_name, Value}] -> Name = Value
	end,
	["<", Name, encode_attrs(Attrs), "/>\n", Accum];


% for a list of elements
encode_element([], _) -> [];
encode_element(A_List, Accum) when is_list(A_List) ->
	lists:foldr( fun(Elem, Acc) -> encode_element(Elem, Acc) end, Accum, A_List).

% an entry point for all encode_element functions
encode_element(Element) ->
	 encode_element(Element, []).

default_start_xml() -> '<xml version="1.0" encoding="UTF-8"?>'.

%%% end of duexml module.


