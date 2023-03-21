:- module test_parsing.

:- interface.
:- import_module io.
:- pred main(io::di, io::uo) is det.

:- implementation.

:- import_module parsing_utils, string, list, char, unit.

:- pred parse_standard_name(src::in, string::out, ps::in, ps::out) is semidet.
parse_standard_name(Src, Name, !PS) :-
    Letters = "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ",
    Digits = "0123456789",
    Parser = (pred(Src2::in, Char2::out, PS0::in, PS1::out) is semidet :-
		  char_in_class(Letters ++ "_" ++ Digits, Src2, Char2, PS0, PS1)),
    char_in_class(Letters ++ "_", Src, Char1, !PS),
    zero_or_more(Parser, Src, Rest, !PS),
    from_char_list([Char1 | Rest], Name).

:- pred parse_var_name(src::in, string::out, ps::in, ps::out) is semidet.
parse_var_name(Src, Name, PS0, PS3) :-
    (string_literal('"', Src, Name1, PS0, PS1) -> Name = "\"" ++ Name1 ++ "\"", PS3=PS1
    ;
    parse_standard_name(Src, Name, PS0, PS3)),
    %% Exclude if semicolon in Name
    not member(';', to_char_list(Name)).

:- pred parse_table_name(src::in, string::out, ps::in, ps::out) is semidet.
parse_table_name(Src, Name, PS0, PS4) :-
    parse_var_name(Src, SchemaName, PS0, PS1),
    next_char(Src, '.', PS1, PS2),
    parse_var_name(Src, TableName, PS2, PS3) ->
	Name = SchemaName ++ "." ++ TableName, PS4 = PS3
    ;
    parse_var_name(Src, Name, PS0, PS4).

%% We could extend this to parse for CREATE TABLE, INSERT and SELECT statements.
%% This is a large task:)

main(!IO) :- test1(!IO).

:- pred test1(io::di, io::uo) is det.
test1(!IO) :-
    some [!PS] (
	String = "_Abc\n\t \"temp\".def \"!!!\"",
	new_src_and_ps(String, Src, !:PS),
	(
	    parse_table_name(Src, Name, !PS),
	    whitespace(Src, _, !PS),
	    parse_table_name(Src, Name2, !PS),
	    whitespace(Src, _, !PS),
	    parse_table_name(Src, Name3, !PS),
	    eof(Src, _, !.PS, _)
	->
	    print_line(Name, !IO),
	    print_line(Name2, !IO),
	    print_line(Name3, !IO)
	;
	    print_line("fails", !IO)
	)).
