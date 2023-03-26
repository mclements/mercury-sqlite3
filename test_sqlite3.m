:- module test_sqlite3.

:- interface.
:- import_module io.

:- pred main(io::di, io::uo) is det.

:- implementation.
:- import_module sqlite3, maybe, list, pair, float, string, bool.

main(!IO) :-
    test(!IO).

:- func maybe_string(data_type) = maybe(string).
maybe_string(Value) = (if Value = text(String) then yes(String) else no).
:- func maybe_float(data_type) = maybe(float).
maybe_float(Value) = (if Value = float(Float) then yes(Float) else no).
:- func maybe_int(data_type) = maybe(int).
maybe_int(Value) = (if Value = int(Int) then yes(Int) else no).

:- func maybe_null(data_type) = maybe(data_type).
maybe_null(Value) = (if Value = null then yes(null) else no).

%% :- func reader(column_type) = (func(data) = T(column_type)).
%% reader(ColumnType) = F :-
%%     ColumnType = integer -> F = (func(Data) = maybe_int(Data))
%%     ;

:- func read3(maybe_error(list(data_type))) = {maybe(string), maybe(int), maybe(float)}.
read3(Value) =
(if Value = ok([A,B,C]) then {maybe_string(A),maybe_int(B),maybe_float(C)} else {no,no,no}).

:- type r ---> r(string,int,float).
:- func read3b(r) = list(data_type).
read3b(r(A,B,C)) = [text(A), int(B), float(C)].

:- pred test(io::di, io::uo) is det.
test(!IO) :-
    open_rw(":memory:", normal, MaybeDb, !IO),
    (MaybeDb = ok(Db) ->
	 (
	     Data = map(func(I) = [text("a"), float(float.float(I))], 1..5),
	     write_table(Db, "temp", ["s", "x"], Data, !IO),
	     read_query(Db, "select * from temp", Headers0, Output0, !IO),
	     print_line(Headers0, !IO),
	     print_line(Output0, !IO),
	     create_example_function(Db, _, !IO),
	     Sql = "select s, count(*), sum(identity(x)) from temp group by s",
	     read_query(Db, Sql, Headers, Output, !IO),
	     print_line(Headers, !IO),
	     print_line(Output, !IO),
	     create_example_function2(Db, _, !IO),
	     Sql2 = "select s, count(*), sum(identity2(x)) from temp group by s",
	     read_query(Db, Sql2, Headers2, Output2, !IO),
	     print_line(Headers2, !IO),
	     print_line(Output2, !IO),
	     create_example_function3(Db, _, !IO),
	     Sql3 = "select s, count(*), sum(identity3(x)) from temp group by s",
	     read_query(Db, Sql3, Headers3, Output3, !IO),
	     print_line(Headers3, !IO),
	     print_line(Output3, !IO)
	 ),
	 close(Db, !IO)
    ;
    print_line("failed to open the database", !IO)).


%-----------------------------------------------------------------------------%
% User-defined functions

%% hlc.gc grade requires that sqlite3.h be included again (otherwise: unknown type names)
:- pragma foreign_decl("C", "
    #include <sqlite3.h>
").

%% The following code creates an "identity" function in SQLite
%% create_example_function (foreign_proc) <- noopfunc (C)
:- pragma foreign_code("C", "
static void noopfunc(sqlite3_context *context, int argc, sqlite3_value **argv) {
  assert( argc==1 );
  sqlite3_result_value(context, argv[0]);
}
").

:- pred create_example_function(db(_)::in, string::out, io::di, io::uo) is det.
:- pragma foreign_proc("C",
    create_example_function(Db::in, Error::out, _IO0::di, _IO::uo),
    [promise_pure, thread_safe, tabled_for_io],
"
SQLITE3_CREATE_FUNCTION(Db,Error,identity,noopfunc)
"
).

%% create_example_function2 (foreign_proc) <- noopfunc2 (foreign_export) <- noopfunc2 (foreign_proc)
:- impure pred noopfunc2(context::in, int32::in, sqlite3_value_array::in, io::di, io::uo) is det.
:- pragma foreign_proc("C",
    noopfunc2(Context::in, Argc::in, Argv::in, _IO0::di, _IO1::uo),
    [will_not_call_mercury, tabled_for_io],
"
    assert( Argc==1 );
    sqlite3_result_value(Context, Argv[0]);
").
:- pragma foreign_export("C", noopfunc2(in, in, in, di, uo), "noopfunc2").

:- pred create_example_function2(db(_)::in, string::out, io::di, io::uo) is det.
:- pragma foreign_proc("C",
    create_example_function2(Db::in, Error::out, _IO0::di, _IO1::uo),
    [promise_pure, thread_safe, tabled_for_io],
"
SQLITE3_CREATE_FUNCTION(Db,Error,identity2,noopfunc2)
").

:- impure pred noopfunc3(context::in, int32::in, sqlite3_value_array::in) is det.
noopfunc3(Context, _Argc, Argv) :-
    sqlite3.value_array_get(Argv, 0i32, Arg),
    impure result_value(Context, Arg).
:- pragma foreign_export("C", noopfunc3(in, in, in), "noopfunc3").

:- pred create_example_function3(db(_)::in, string::out, io::di, io::uo) is det.
:- pragma foreign_proc("C",
    create_example_function3(Db::in, Error::out, _IO0::di, _IO::uo),
    [promise_pure, thread_safe, tabled_for_io],
"
  SQLITE3_CREATE_FUNCTION(Db,Error,identity3,noopfunc3)
").
