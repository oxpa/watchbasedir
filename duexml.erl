-module(duexml). % damn useless erlang xml encoder
-export([encode_element/1, encode_attrs/1]).



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
		escape_chars(io_lib:format("~s", [Text]));
escape_chars("<") -> "&lt;";
escape_chars(">") -> "&gt;";
escape_chars("&") -> "&amp;";
%escape_chars("'") -> "&apos;";
%escape_chars("\"") -> "&quot;";
escape_chars(Text) when is_list(Text) ->
		%io:format("debugging a list ~s~n", [Text]),
		lists:map(fun escape_chars/1, Text);
escape_chars(<<T/binary>>) -> escape_chars(T,<<>>);
escape_chars(Text) when is_integer(Text) andalso Text > 254 ->
		io_lib:write(Text);
escape_chars(A) -> A.

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
											%[[" "], io_lib:write(Atom), ["=\""], [escape_chars(Value)], ["\""], Acc]; 
											[[" "], io_lib:format("~s",[Atom]), ["=\""], [escape_chars(Value)], ["\""], Acc]; 
					(List, Acc) when is_list(List) -> encode_attrs(List,Acc) end,
				Accum, 
				Attrs).


% conventional functions for nested elements
encode_element({Element_name, Attrs, Elements=[{B, C, _}|_]},Accum) when is_atom(Element_name), is_atom(B), is_list(C) or is_binary(C) ->
	Text = encode_element(Elements),
	Name=io_lib:format('~s',[Element_name]),
	["<", Name, [encode_attrs(Attrs)], ">\n", Text, "</", Name,">\n", Accum];
encode_element({Element_name, Attrs, Elements=[{B, C}|_]},Accum) when is_atom(Element_name), is_atom(B), is_list(C) or is_binary(C) ->
	Text = encode_element(Elements),
	Name=io_lib:format('~s',[Element_name]),
	["<", Name, encode_attrs(Attrs), ">\n", Text, "</", Name, ">\n", Accum];

% a regular element
encode_element({Element_name, Attrs, Text},Accum) when is_atom(Element_name), is_list(Attrs), is_list(Text) ->
	Name=io_lib:format('~s',[Element_name]),
	["<",Name, encode_attrs(Attrs), ">", [escape_chars(Text)], "</", Name, ">\n", Accum];

% a simple element
encode_element({Element_name, Attrs},Accum) when is_atom(Element_name), is_list(Attrs) ->
	Name=io_lib:format('~s',[Element_name]),
	["<", Name, encode_attrs(Attrs), "/>\n", Accum];


% for a list of elements
encode_element(A_List, Accum) when is_list(A_List) ->
	lists:foldr( fun(Elem, Acc) -> encode_element(Elem, Acc) end, Accum, A_List).

% an entry point for all encode_element functions
encode_element(Element) ->
	 encode_element(Element, []).

default_start_xml() -> '<xml version="1.0" encoding="UTF-8"?>'.

%%% end of duexml module.


