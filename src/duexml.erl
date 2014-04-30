-module(duexml). % damn useless erlang xml encoder
-compile(export_all).
%-export([encode_element/1, encode_attrs/1]).
%-on_load(init/0).

escape_chars(Text) when is_atom(Text) ->
		encode_atom(Text);
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
											Text = encode_atom(Atom),
											[[" ", Text, "=\"", escape_chars(Value), "\"" ] |Acc]; 
					(List, Acc) when is_list(List) -> encode_attrs(List,Acc);
					(A, Acc) -> error(badargs, A) end,
				Accum, 
				Attrs).

% a wrapper for atoms:
%    try get a string from an ets table
%	 if there is nothing - encode and put there for future use.
encode_atom(Name) when is_atom(Name) ->
	case ets:lookup(atoms,Name) of
		[] -> Elem_name=atom_to_binary(Name, latin1), ets:insert(atoms, {Name,Elem_name}), Elem_name;
		[{Element_name, Value}] -> Value
	end.

% conventional functions for nested elements
encode_element({Element_name, Attrs, Elements=[{B, C, _}|_]},Accum) when is_atom(Element_name), is_atom(B), is_list(C) or is_binary(C) ->
	Text = encode_element(Elements),
	Name = encode_atom(Element_name),
	["<", Name, encode_attrs(Attrs), ">\n", Text, "</", Name,">\n", Accum];

encode_element({Element_name, Attrs, Elements=[{B, C}|_]},Accum) when is_atom(Element_name), is_atom(B), is_list(C) or is_binary(C) ->
	Text = encode_element(Elements),
	Name = encode_atom(Element_name),
	[["<", Name, encode_attrs(Attrs), ">\n", Text, "</", Name, ">\n"] | Accum];

% a regular element
encode_element({Element_name, Attrs, Text},Accum) when is_atom(Element_name), is_list(Attrs), is_list(Text) ->
	Name = encode_atom(Element_name),
	["<",Name, encode_attrs(Attrs), ">", escape_chars(Text), "</", Name, ">\n", Accum];

% a simple element
encode_element({Element_name, Attrs},Accum) when is_atom(Element_name), is_list(Attrs) ->
	Name = encode_atom(Element_name),
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


