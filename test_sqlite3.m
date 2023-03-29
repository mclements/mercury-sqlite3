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

%% :- func read3(list(data_type)) = {maybe(string), maybe(int), maybe(float)}.
%% read3(Value) =
%% (if Value = [A,B,C] then {maybe_string(A),maybe_int(B),maybe_float(C)} else {no,no,no}).

:- type r ---> r(string,int,float).
:- func read3(list(data_type)) = maybe(r).
read3(Value) =
(if Value = [text(A),int(B),float(C)] then yes(r(A,B,C)) else no).

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
	     Data2 = append(map(func(I) = [text("Östersund"), float(float.float(I))], 1..500),
			    map(func(I) = [text("Göteborg"), float(float.float(I))], 1..1000)),
	     write_table(Db, "temp2", ["s", "x"], Data2, !IO),
	     create_sqlite3_function(Db, "identity", c_noopfunc, _, !IO),
	     create_sqlite3_function(Db, "identity2", c_noopfunc2, _, !IO),
	     create_sqlite3_function(Db, "identity3", c_noopfunc3, _, !IO),
	     create_sqlite3_function(Db, "mysum", c_mysum, c_myfinal, _, !IO),
	     Sql = "select s, count(*), sum(identity(x)) from temp2 group by s",
	     read_query(Db, Sql, Headers, Output, !IO),
	     print_line(Headers, !IO),
	     print_line(Output, !IO),
	     Sql2 = "select identity3(s), count(*), mysum(identity2(x)) from temp2 group by s",
	     read_query(Db, Sql2, Headers2, Output2, !IO),
	     print_line(Headers2, !IO),
	     Out2 = (ok(Out) = Output2 -> map(read3, Out) ; []),
	     print_line(Out2, !IO)
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
%% create_sqlite3_function <- noopfunc (C+ptr)
:- pragma foreign_code("C", "
static void noopfunc(sqlite3_context *context, int argc, sqlite3_value **argv) {
  assert( argc==1 );
  sqlite3_result_value(context, argv[0]);
}
").
:- func c_noopfunc = sqlite3_function.
:- pragma foreign_proc("C", c_noopfunc = (Ptr::out),
		       [thread_safe, promise_pure],
		       "Ptr = noopfunc;").

%% create_sqlite3_function <- noopfunc2 (foreign_export+ptr) <- noopfunc2 (impure pred)
:- impure pred noopfunc2(context::in, int32::in, sqlite3_value_array::in) is det.
noopfunc2(Context, _Argc, Argv) :-
    impure result_value(Context, Argv ^ elem(0)).
:- pragma foreign_export("C", noopfunc2(in, in, in), "noopfunc2").
:- func c_noopfunc2 = sqlite3_function.
:- pragma foreign_proc("C", c_noopfunc2 = (Ptr::out),
		       [thread_safe, promise_pure],
		       "Ptr = noopfunc2;").

:- impure pred noopfunc3(context::in, int32::in, sqlite3_value_array::in) is det.
noopfunc3(Context, _Argc, Argv) :-
    value_text(Argv ^ elem(0), Text),
    impure result_text(Context, "Region: " ++ Text).
:- pragma foreign_export("C", noopfunc3(in, in, in), "noopfunc3").
:- func c_noopfunc3 = sqlite3_function.
:- pragma foreign_proc("C", c_noopfunc3 = (Ptr::out),
		       [thread_safe, promise_pure],
		       "Ptr = noopfunc3;").

%% "Hello World" for user-defined SQL aggregates: sums:)
:- pragma foreign_code("C", "
static void mysum(sqlite3_context *context, int argc, sqlite3_value **argv) {
  double* p;
  assert(argc==1);
  p = sqlite3_aggregate_context(context, sizeof(*p));
  if (SQLITE_NULL != sqlite3_value_numeric_type(argv[0])) {
    *p = *p + sqlite3_value_double(argv[0]);
  }
}
static void myfinal(sqlite3_context *context) {
  double* p = sqlite3_aggregate_context(context, 0);
  sqlite3_result_double(context, *p);
}
").
:- func c_mysum = sqlite3_function.
:- pragma foreign_proc("C", c_mysum = (Ptr::out),
		       [thread_safe, promise_pure],
		       "Ptr = mysum;").
:- func c_myfinal = sqlite3_final.
:- pragma foreign_proc("C", c_myfinal = (Ptr::out),
		       [thread_safe, promise_pure],
		       "Ptr = myfinal;").
