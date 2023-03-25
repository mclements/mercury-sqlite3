:- module test_sqlite3_impure.

:- interface.
:- import_module io.

:- pred main(io::di, io::uo) is det.

:- implementation.
:- import_module sqlite3_impure, maybe, list, pair, float, string, bool.

main(!IO) :-
    test(!IO).

%% :- func maybe_string(data_type) = maybe(string).
%% maybe_string(Value) = (if Value = text(String) then yes(String) else no).
%% :- func maybe_float(data_type) = maybe(float).
%% maybe_float(Value) = (if Value = float(Float) then yes(Float) else no).
%% :- func maybe_int(data_type) = maybe(int).
%% maybe_int(Value) = (if Value = int(Int) then yes(Int) else no).

%% :- func maybe_null(data_type) = maybe(data_type).
%% maybe_null(Value) = (if Value = null then yes(null) else no).

%% :- func reader(column_type) = (func(data) = T(column_type)).
%% reader(ColumnType) = F :-
%%     ColumnType = integer -> F = (func(Data) = maybe_int(Data))
%%     ;

%% :- func read3(maybe_error(list(data_type))) = {maybe(string), maybe(int), maybe(float)}.
%% read3(Value) =
%% (if Value = ok([A,B,C]) then {maybe_string(A),maybe_int(B),maybe_float(C)} else {no,no,no}).

%% :- type r ---> r(string,int,float).
%% :- func read3b(r) = list(data_type).
%% read3b(r(A,B,C)) = [text(A), int(B), float(C)].

:- pred test(io::di, io::uo) is det.
test(!IO) :-
    open_rw(":memory:", normal, MaybeDb, !IO),
    (MaybeDb = ok(Db) ->
	 (
	     Data = map(func(I) = [text("a"), float(float.float(I))], 1..10),
	     write_table(Db, "temp", ["s", "x"], Data, !IO),
	     create_example_function(Db, _, !IO),
	     Sql = "select s, count(*), sum(identity(x)) from temp group by s",
	     read_query(Db, Sql, Headers, Output, !IO),
	     print_line(Headers, !IO),
	     print_line(Output, !IO),
	     create_example_function2(Db, _, !IO),
	     Sql2 = "select s, count(*), sum(identity2(x)) from temp group by s",
	     read_query(Db, Sql2, Headers2, Output2, !IO),
	     print_line(Headers2, !IO),
	     print_line(Output2, !IO)),
	 close(Db, !IO)
    ;
    print_line("failed to open the database", !IO)).


