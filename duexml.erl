-module(duexml). % damn useless erlang xml encoder
-export([encode_element/1, encode_attrs/1]).


xml_compiled_REs() ->
	% {RE,Replacement}
	REs = [{"&", "\\&amp;"}, {"\"","\\&quot;"}, {"'","\\&apos;"}, {"<","\\&lt;"}, {">","\\&gt;"}],
	lists:foldr(fun({RE,Repl},Acc) -> {ok, RE_C} = re:compile(RE), [{RE_C,Repl}|Acc] end, [], REs).


escape_chars(Text, Acc) when is_atom(Text) ->
		escape_chars(io_lib:format("~s", [Text]), Acc);
escape_chars(Text, Acc) when is_integer(Text) ->
		escape_chars(io_lib:format("~b", [Text]), Acc);
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
					%({Atom,Value}, Acc) -> [io_lib:format(' ~s="~s"',[Atom,escape_chars(Value,REs)])|Acc]; 
					({Atom,Value}, Acc) -> %debug("encoding ~p |~p|~n",[Atom,Value]), 
											[[" "], io_lib:format("~s",[Atom]), ["=\""], [escape_chars(Value,REs)], ["\""], Acc]; 
					(List, Acc) when is_list(List) -> encode_attrs(List,Acc) end,
				Accum, 
				Attrs).


% conventional functions for nested elements
encode_element({Element_name, Attrs, Elements=[{B, C, _}|_]},Accum) when is_atom(Element_name), is_atom(B), is_list(C) or is_binary(C) ->
	Text = encode_element(Elements),
	[io_lib:format("<~s", [Element_name]), [encode_attrs(Attrs)], ">\n", Text, io_lib:format("</~s", [Element_name]),">\n", Accum];
encode_element({Element_name, Attrs, Elements=[{B, C}|_]},Accum) when is_atom(Element_name), is_atom(B), is_list(C) or is_binary(C) ->
	Text = encode_element(Elements),
	[io_lib:format("<~s", [Element_name]), [encode_attrs(Attrs)], ">\n", Text, io_lib:format("</~s", [Element_name]),">\n", Accum];

% a regular element
encode_element({Element_name, Attrs, Text},Accum) when is_atom(Element_name), is_list(Attrs), is_list(Text) ->
	%TODO: remove compiling REs from here
	REs = xml_compiled_REs(),
	%io_lib:format("<~s~s>~s</~s>~n", [Element_name, encode_attrs(Attrs), escape_chars(Text,REs), Element_name]) ++ Accum;
	[io_lib:format("<~s", [Element_name]), [encode_attrs(Attrs)], ">", [escape_chars(Text,REs)], io_lib:format("</~s", [Element_name]),">\n", Accum];

% a simple element
encode_element({Element_name, Attrs},Accum) when is_atom(Element_name), is_list(Attrs) ->
	%io_lib:format("<~s~s/>~n", [Element_name, encode_attrs(Attrs)]) ++ Accum;
	[io_lib:format("<~s",[Element_name]), [encode_attrs(Attrs)], "/>\n", Accum];


% for a list of elements
encode_element(A_List, Accum) when is_list(A_List) ->
	lists:foldr( fun(Elem, Acc) -> encode_element(Elem, Acc) end, Accum, A_List).

% an entry point for all encode_element functions
encode_element(Element) -> encode_element(Element, []).

default_start_xml() -> '<xml version="1.0" encoding="UTF-8"?>'.

%%% end of duexml module.


