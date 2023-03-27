% sqlite3 - Mercury interface with the SQLite library
% Based on pushpull - a bidirectional IMAP/Maildir synchronisation tool.
% Copyright (C) 2015 Peter Wang
% Copyright (C) 2023 Mark Clements

:- module sqlite3.
:- interface.

:- import_module array.
:- import_module assoc_list.
:- import_module bool.
:- import_module char.
:- import_module io.
:- import_module maybe.
:- import_module list.

:- import_module float.

%-----------------------------------------------------------------------------%

:- type rw ---> rw.
:- type ro ---> ro.

:- type db(RwRo).

:- type synchronous
    --->    off
    ;       normal
    ;       full.

:- type stmt.

:- type bind_index
    --->    num(int)
    ;       name(string).

:- type step_result
    --->    done
    ;       row
    ;       error(string).

:- inst step_result_nonerror
    --->    done
    ;       row.

:- type column
    --->    column(int).

:- type column_type
   --->  integer
   ;     float
   ;     text
   ;     blob
   ;     null.

:- type data_type
   --->  null
   ;     int(int)
   ;     float(float)
   ;     text(string)
   ;     blob(c_pointer, int).

:- type row_type == list(data_type).

:- type table_type == list(row_type).

:- type sqlite_error % exception type
    --->    sqlite_error(string).

%-----------------------------------------------------------------------------%

:- pred init_multithreaded(maybe_error::out, io::di, io::uo) is det.

:- pred synchronous(synchronous, string).
:- mode synchronous(in, out) is det.
:- mode synchronous(out, in) is semidet.

:- pred open_rw(string::in, synchronous::in, maybe_error(db(rw))::out,
    io::di, io::uo) is det.

:- pred open_ro(string::in, maybe_error(db(ro))::out, io::di, io::uo) is det.

:- pred close(db(RwRo)::in, io::di, io::uo) is det.

    % This is only good for temporarily treating a rw database connection
    % as a ro database connection.  It should be avoided.
    %
:- pred rw_db_to_ro_db(db(rw)::in, db(ro)::out) is det.

%-----------------------------------------------------------------------------%

    % Must be paired with end_transaction or rollback_transaction.
    %
:- pred begin_transaction(db(RwRo)::in, maybe_error::out,
    io::di, io::uo) is det.

:- pred end_transaction(db(RwRo)::in, maybe_error::out,
    io::di, io::uo) is det.

:- pred rollback_transaction(db(RwRo)::in, maybe_error::out,
    io::di, io::uo) is det.

:- pred exec(db(RwRo)::in, string::in, maybe_error::out,
    io::di, io::uo) is det.

%-----------------------------------------------------------------------------%

% Low-level interface

:- pred prepare(db(RwRo)::in, string::in, maybe_error(stmt)::out,
    io::di, io::uo) is det.

:- pred bind(db(RwRo)::in, stmt::in, bind_index::in, data_type::in,
    maybe_error::out, io::di, io::uo) is det.

:- pred bind_int(db(RwRo)::in, stmt::in, bind_index::in, int::in,
    maybe_error::out, io::di, io::uo) is det.

:- pred bind_float(db(RwRo)::in, stmt::in, bind_index::in, float::in,
    maybe_error::out, io::di, io::uo) is det.

    % This is "unsafe" in that the GC could collect the string while it is
    % still bound to the stmt.  You must keep a reference to the string while
    % it is still bound to the stmt.
    %
:- pred unsafe_bind_text(db(RwRo)::in, stmt::in, bind_index::in, string::in,
    maybe_error::out, io::di, io::uo) is det.

    % This is "unsafe" in that the GC could collect the object containing
    % the pointer address while the address is still bound to the stmt.
    % You must keep a reference to the object while the pointer is still
    % bound to the stmt.
    %
:- pred unsafe_bind_blob(db(RwRo)::in, stmt::in, bind_index::in,
    c_pointer::in, int::in, maybe_error::out, io::di, io::uo) is det.

:- pred bind_null(db(RwRo)::in, stmt::in, bind_index::in,
    maybe_error::out, io::di, io::uo) is det.

:- pred step(db(RwRo)::in, sqlite3.stmt::in, step_result::out,
    io::di, io::uo) is det.

:- pred reset(db(RwRo)::in, stmt::in, maybe_error::out,
    io::di, io::uo) is det.

:- pred finalize(stmt::in, io::di, io::uo) is det.

%-----------------------------------------------------------------------------%

:- pred column_is_null(stmt::in, column::in, bool::out,
    io::di, io::uo) is det.

:- pred column_int(stmt::in, column::in, int::out,
    io::di, io::uo) is det.

:- pred column_float(stmt::in, column::in, float::out,
    io::di, io::uo) is det.

:- pred column_text(stmt::in, column::in, string::out,
    io::di, io::uo) is det.

:- pred column_maybe_text(stmt::in, column::in, maybe(string)::out,
    io::di, io::uo) is det.

:- pred column_blob(stmt::in, column::in, c_pointer::out, int::out,
		    io::di, io::uo) is det.

:- pred column_type(stmt::in, column::in, int::out,
		    io::di, io::uo) is det.

:- pred column(stmt::in, column::in, data_type::out, io::di, io::uo) is det.

:- pred column_count(stmt::in, int::out, io::di, io::uo) is det.

:- pred column_name(stmt::in, column::in, string::out,
    io::di, io::uo) is det.

:- pred data_count(stmt::in, int::out, io::di, io::uo) is det.

:- pred db_handle(stmt::in, db(T)::out, io::di, io::uo) is det.

:- func column_type_id(column_type) = int.

%-----------------------------------------------------------------------------%

:- func escape_LIKE_argument(char, string) = string.

%-----------------------------------------------------------------------------%

% High-level interface

    % The bindings list is kept alive until the statement is finalized.
    %
:- pred with_stmt(
    pred(db(RwRo), stmt, T, io, io)::in(pred(in, in, out(TI), di, uo) is det),
    db(RwRo)::in, string::in, assoc_list(bind_index, data_type)::in,
    T::out(TI), io::di, io::uo) is det.

:- pred with_prepared_stmt(
    pred(db(RwRo), stmt, T, io, io)::in(pred(in, in, out(TI), di, uo) is det),
    db(RwRo)::in, stmt::in, assoc_list(bind_index, data_type)::in,
    T::out(TI), io::di, io::uo) is det.

:- pred with_stmt_acc(
    pred(db(RwRo), stmt, T, T, io, io)::in(pred(in, in, in, out, di, uo) is det),
    db(RwRo)::in, string::in, assoc_list(bind_index, data_type)::in,
    T::in, T::out, io::di, io::uo) is det.

:- pred with_stmt_acc3(
    pred(db(RwRo), stmt, maybe_error, A, A, B, B, C, C, io, io),
    db(RwRo), string, assoc_list(bind_index, data_type),
    maybe_error, A, A, B, B, C, C, io, io).
:- mode with_stmt_acc3(
    in(pred(in, in, out, in, out, in, out, in, out, di, uo) is det),
    in, in, in, out, in, out, in, out, in, out, di, uo) is det.
:- mode with_stmt_acc3(
    in(pred(in, in, out, in, out, in, out, array_di, array_uo, di, uo) is det),
    in, in, in, out, in, out, in, out, array_di, array_uo, di, uo) is det.

:- pred bind_checked(db(RwRo)::in, stmt::in,
    assoc_list(bind_index, data_type)::in, io::di, io::uo) is det.

:- pred step_ok(db(RwRo)::in, stmt::in, step_result::out(step_result_nonerror),
    io::di, io::uo) is det.

:- pred step_ok_keep_alive(db(RwRo)::in, stmt::in,
    assoc_list(bind_index, data_type)::in,
    step_result::out(step_result_nonerror), io::di, io::uo) is det.

:- pred insert_row(db(rw)::in, stmt::in, maybe_error::out,
    io::di, io::uo) is det.

:- pred get_header(db(rw)::in, stmt::in, maybe_error(list(string))::out,
    io::di, io::uo) is det.

:- pred get_row(db(rw)::in, stmt::in, maybe_error(row_type)::out,
    io::di, io::uo) is det.

:- pred get_rows(db(rw)::in, stmt::in, maybe_error(table_type)::out,
		 io::di, io::uo) is det.

:- pred get_cols(db(rw)::in, stmt::in, list(list(data_type))::out,
    io::di, io::uo) is det.

:- pred write_table(db(rw)::in, % Db
		    string::in, % TableName
		    list(string)::in, % Headers
		    list(list(data_type))::in, % Data
		    io::di, io::uo) is det.

:- pred read_query(db(rw)::in, % Db
		   string::in, % Query
		   maybe_error(list(string))::out, % Headers
		   maybe_error(list(list(data_type)))::out, % Data
		   io::di, io::uo) is det.


%-----------------------------------------------------------------------------%
%% utilities to support creating functions

:- type context.

:- type sqlite3_value.

:- type sqlite3_value_array.

:- type sqlite3_function.

:- func lookup(sqlite3_value_array, int) = sqlite3_value is det.
:- pred lookup(sqlite3_value_array::in, int::in, sqlite3_value::out) is det.
:- func elem(int, sqlite3_value_array) = sqlite3_value.
:- mode elem(in, in) = out is det.

:- impure pred result_value(context::in, sqlite3_value::in) is det.
:- impure pred result_double(context::in, float::in) is det.
:- impure pred result_int(context::in, int::in) is det.
:- impure pred result_blob(context::in, c_pointer::in, int::in) is det.
:- impure pred result_text(context::in, string::in) is det.

:- pred value_double(sqlite3_value::in, float::out) is det.
:- pred value_int(sqlite3_value::in, int::out) is det.
:- pred value_text(sqlite3_value::in, string::out) is det.
:- pred value_blob(sqlite3_value::in, c_pointer::out, int::out) is det.

:- pred create_sqlite3_function(db(_)::in, string::in, sqlite3_function::in,
				string::out, io::di, io::uo) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module exception.
:- import_module string.
:- import_module pair.
:- import_module int.
:- import_module cord.
:- import_module parsing_utils.
:- import_module unit.

:- include_module sqlite3.keep_alive.
:- import_module sqlite3.keep_alive.

%-----------------------------------------------------------------------------%

:- pragma foreign_decl("C", "
    #include <sqlite3.h>
").

:- pragma foreign_type("C", db(_), "sqlite3 *").

:- pragma foreign_type("C", stmt, "sqlite3_stmt *").

%-----------------------------------------------------------------------------%

:- pragma foreign_decl("C", "
MR_String _msqlite_copy_errmsg(sqlite3 *db);
").

:- pragma foreign_code("C", "
MR_String
_msqlite_copy_errmsg(sqlite3 *db)
{
    MR_String str;
    MR_make_aligned_string_copy(str, sqlite3_errmsg(db));
    return str;
}
").

%-----------------------------------------------------------------------------%

init_multithreaded(Res, !IO) :-
    init_multithreaded_2(Error, !IO),
    ( Error = "" ->
        Res = ok
    ;
        Res = error(Error)
    ).

:- pred init_multithreaded_2(string::out, io::di, io::uo) is det.

:- pragma foreign_proc("C",
    init_multithreaded_2(Error::out, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, thread_safe, tabled_for_io],
"
    int rc;

    rc = sqlite3_config(SQLITE_CONFIG_MULTITHREAD);
    if (rc != SQLITE_OK) {
        Error = MR_make_string(MR_ALLOC_ID,
            ""sqlite3_config returned: %d"", rc);
    } else {
        rc = sqlite3_initialize();
        if (rc != SQLITE_OK) {
            Error = MR_make_string(MR_ALLOC_ID,
                ""sqlite3_initialize returned: %d"", rc);
        } else {
            Error = MR_make_string_const("""");
        }
    }
").

%-----------------------------------------------------------------------------%

:- pragma foreign_type("C", context, "sqlite3_context *").

:- pragma foreign_type("C", sqlite3_value, "sqlite3_value *").

:- pragma foreign_type("C", sqlite3_value_array, "sqlite3_value **").

:- pragma foreign_decl("C", 
		       "typedef void (*sqlite3_function)(sqlite3_context*,int,sqlite3_value**);").
:- pragma foreign_type("C", sqlite3_function, "sqlite3_function").

:- pragma foreign_proc("C",
    lookup(Array::in, Index::in, Value::out),
    [promise_pure, will_not_call_mercury, thread_safe],
"
    Value = Array[(int32_t) Index];
").
lookup(Array, Index) = Value :-
    lookup(Array, Index, Value).
elem(Index, Array) = Elem :-
    lookup(Array, Index, Elem).

:- pragma foreign_proc("C",
    result_value(Context::in, Value::in),
    [will_not_call_mercury, thread_safe],
"
    sqlite3_result_value(Context, Value);
").

:- pragma foreign_proc("C",
    value_double(Value::in, Float::out),
    [promise_pure, will_not_call_mercury, thread_safe],
"
    Float = sqlite3_value_double(Value);
    /* printf(\"%f\\n\",Float); */
").

:- pragma foreign_proc("C",
    value_int(Value::in, Int::out),
    [promise_pure, will_not_call_mercury, thread_safe],
"
    Int = sqlite3_value_int(Value);
    /* printf(\"%i\\n\",Int); */
").

:- pragma foreign_proc("C",
    value_text(Value::in, Str::out),
    [promise_pure, will_not_call_mercury, thread_safe],
"
    const unsigned char *s =  sqlite3_value_text(Value);
    if (s) {
        MR_make_aligned_string_copy_msg(Str, (const char *)s, MR_ALLOC_ID);
    } else {
        Str = MR_make_string_const("""");
    }
    /* printf(\"%s\\n\", Str); */
").

:- pragma foreign_proc("C",
    value_blob(Value::in, Pointer::out, SizeBytes::out),
    [promise_pure, will_not_call_mercury, thread_safe],
"
    Pointer = (MR_Word) sqlite3_value_blob(Value);
    SizeBytes = sqlite3_value_bytes(Value);
").

:- pragma foreign_proc("C",
    result_double(Context::in, Value::in),
    [will_not_call_mercury, thread_safe],
"
    sqlite3_result_double(Context, Value);
").

:- pragma foreign_proc("C",
    result_int(Context::in, Value::in),
    [will_not_call_mercury, thread_safe],
"
    sqlite3_result_int(Context, Value);
").

:- pragma foreign_proc("C",
    result_blob(Context::in, Pointer::in, N::in),
    [will_not_call_mercury, thread_safe],
"
    sqlite3_result_blob(Context, (void *)Pointer, N, SQLITE_STATIC);
").

:- pragma foreign_proc("C",
    result_text(Context::in, Value::in),
    [will_not_call_mercury, thread_safe],
"
    sqlite3_result_text(Context, Value, strlen(Value), SQLITE_STATIC);
").

:- pragma foreign_proc("C",
    create_sqlite3_function(Db::in, SqlName::in, Func::in, Error::out, _IO0::di, _IO::uo),
    [promise_pure, thread_safe, tabled_for_io],
"
    int rc = sqlite3_create_function(Db, SqlName, 1, SQLITE_UTF8, NULL, Func, 
                                 NULL, NULL); 
    if (rc != SQLITE_OK) { 
        Error = MR_make_string(MR_ALLOC_ID,
            \"sqlite3_create_function returned: %d\", rc);
        } else {
            Error = MR_make_string_const(\"\");
        }
"
).

%-----------------------------------------------------------------------------%

synchronous(off,    "OFF").
synchronous(normal, "NORMAL").
synchronous(full,   "FULL").

%-----------------------------------------------------------------------------%

:- type open_mode
    --->    rw(synchronous)
    ;       ro.

open_rw(FileName, SynchronousValue, Res, !IO) :-
    open_2(FileName, rw(SynchronousValue), Res, !IO).

open_ro(FileName, Res, !IO) :-
    open_2(FileName, ro, Res, !IO).

:- pred open_2(string::in, open_mode::in, maybe_error(db(RwRo))::out,
    io::di, io::uo) is det.

open_2(FileName, OpenMode, Res, !IO) :-
    (
        OpenMode = rw(_),
        ReadWrite = yes
    ;
        OpenMode = ro,
        ReadWrite = no
    ),
    open_3(FileName, ReadWrite, Db, OpenError, !IO),
    ( OpenError = "" ->
        set_options_on_open(Db, OpenMode, Res, !IO)
    ;
        Res = error(OpenError)
    ),
    (
        Res = ok(_)
    ;
        Res = error(_),
        close(Db, !IO)
    ).

:- pred open_3(string::in, bool::in, db(RwRo)::out, string::out,
    io::di, io::uo) is det.

:- pragma foreign_proc("C",
    open_3(FileName::in, ReadWrite::in, Db::out, Error::out,
        _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, thread_safe, tabled_for_io],
"
    int flags;
    int rc;

    if (ReadWrite) {
        flags = SQLITE_OPEN_READWRITE | SQLITE_OPEN_CREATE;
    } else {
        flags = SQLITE_OPEN_READONLY;
    }
    rc = sqlite3_open_v2(FileName, &Db, flags, NULL);
    if (rc == SQLITE_OK) {
        Error = MR_make_string_const("""");
    } else if (Db == NULL) {
        Error = MR_make_string_const(""out of memory"");
    } else {
        MR_make_aligned_string_copy_msg(Error, sqlite3_errmsg(Db),
            MR_ALLOC_ID);
    }
").

:- pred set_options_on_open(db(RwRo)::in, open_mode::in,
    maybe_error(db(RwRo))::out, io::di, io::uo) is det.

set_options_on_open(Db, OpenMode, Res, !IO) :-
    Common = "",
    (
        OpenMode = rw(Synchronous),
        synchronous(Synchronous, SynchronousValue),
        PragmaJournalMode = "PRAGMA journal_mode=WAL;",
        PragmaSynchronous = "PRAGMA synchronous=" ++ SynchronousValue,
        Sql = Common ++ PragmaJournalMode ++ PragmaSynchronous
    ;
        OpenMode = ro,
        Sql = Common
    ),
    sqlite3.exec(Db, Sql, ExecRes, !IO),
    (
        ExecRes = ok,
        Res = ok(Db)
    ;
        ExecRes = error(Error),
        Res = error(Error)
    ).

%-----------------------------------------------------------------------------%

:- pragma foreign_proc("C",
    close(Db::in, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, thread_safe, tabled_for_io],
"
    if (Db) {
        sqlite3_close(Db);
    }
").

%-----------------------------------------------------------------------------%

:- pragma foreign_proc("C",
    rw_db_to_ro_db(DbRw::in, DbRo::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    DbRo = DbRw;
").

%-----------------------------------------------------------------------------%

begin_transaction(Db, Res, !IO) :-
    exec(Db, "BEGIN TRANSACTION", Res, !IO).

end_transaction(Db, Res, !IO) :-
    exec(Db, "END TRANSACTION", Res, !IO).

rollback_transaction(Db, Res, !IO) :-
    exec(Db, "ROLLBACK TRANSACTION", Res, !IO).

%-----------------------------------------------------------------------------%

exec(Db, Sql, Res, !IO) :-
    exec_2(Db, Sql, Rc, Error, !IO),
    ( Rc = 1 ->
        Res = ok
    ;
        Res = error(Error)
    ).

:- pred exec_2(db(RwRo)::in, string::in, int::out, string::out,
    io::di, io::uo) is det.

:- pragma foreign_proc("C",
    exec_2(Db::in, Sql::in, Rc::out, Error::out, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, thread_safe, tabled_for_io],
"
    char *errmsg;
    if (SQLITE_OK == sqlite3_exec(Db, Sql, NULL, NULL, &errmsg)) {
        Rc = 1;
        Error = MR_make_string_const("""");
    } else {
        Rc = -1;
        MR_make_aligned_string_copy_msg(Error, errmsg, MR_ALLOC_ID);
        sqlite3_free(errmsg);
    }
").

%-----------------------------------------------------------------------------%

prepare(Db, Sql, ResStmt, !IO) :-
    prepare_2(Db, Sql, Res, Stmt, Error, !IO),
    ( Res = 1 ->
        ResStmt = ok(Stmt)
    ;
        ResStmt = error(Error)
    ).

:- pred prepare_2(db(RwRo)::in, string::in, int::out, stmt::out, string::out,
    io::di, io::uo) is det.

:- pragma foreign_proc("C",
    prepare_2(Db::in, Sql::in, Res::out, Stmt::out, Error::out,
        _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, thread_safe, tabled_for_io],
"
    int rc = sqlite3_prepare_v2(Db, Sql, -1, &Stmt, NULL);
    if (rc == SQLITE_OK) {
        Res = 1;
        Error = MR_make_string_const("""");
    } else {
        Res = -1;
        Error = _msqlite_copy_errmsg(Db);
    }
").

%-----------------------------------------------------------------------------%

bind(Db, Stmt, IndexType, Value, Res, !IO) :-
    (
        Value = null,
        bind_null(Db, Stmt, IndexType, Res, !IO)
    ;
        Value = int(Int),
        bind_int(Db, Stmt, IndexType, Int, Res, !IO)
    ;
        Value = float(Float),
        bind_float(Db, Stmt, IndexType, Float, Res, !IO)
    ;
        Value = text(String),
        unsafe_bind_text(Db, Stmt, IndexType, String, Res, !IO)
    ;
        Value = blob(Pointer, SizeBytes),
        unsafe_bind_blob(Db, Stmt, IndexType, Pointer, SizeBytes, Res, !IO)
    ).

%-----------------------------------------------------------------------------%

bind_int(Db, Stmt, IndexType, Value, Res, !IO) :-
    (
        IndexType = num(Index)
    ;
        IndexType = name(Name),
        bind_parameter_index(Stmt, Name, Index, !IO)
    ),
    bind_int_2(Db, Stmt, Index, Value, Rc, Error, !IO),
    ( Rc = 1 ->
        Res = ok
    ;
        Res = error(Error)
    ).

:- pred bind_int_2(db(RwRo)::in, stmt::in, int::in, int::in, int::out,
    string::out, io::di, io::uo) is det.

:- pragma foreign_proc("C",
    bind_int_2(Db::in, Stmt::in, Index::in, Value::in, Rc::out, Error::out,
        _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, thread_safe, tabled_for_io],
"
    if (SQLITE_OK == sqlite3_bind_int(Stmt, Index, Value)) {
        Rc = 1;
        Error = MR_make_string_const("""");
    } else {
        Rc = -1;
        Error = _msqlite_copy_errmsg(Db);
    }
").

%-----------------------------------------------------------------------------%

%% :- pred bind_float(db(RwRo)::in, stmt::in, bind_index::in, float::in,
%%     maybe_error::out, io::di, io::uo) is det.

bind_float(Db, Stmt, IndexType, Value, Res, !IO) :-
    (
        IndexType = num(Index)
    ;
        IndexType = name(Name),
        bind_parameter_index(Stmt, Name, Index, !IO)
    ),
    bind_float_2(Db, Stmt, Index, Value, Rc, Error, !IO),
    ( Rc = 1 ->
        Res = ok
    ;
        Res = error(Error)
    ).

:- pred bind_float_2(db(RwRo)::in, stmt::in, int::in, float::in, int::out,
    string::out, io::di, io::uo) is det.

:- pragma foreign_proc("C",
    bind_float_2(Db::in, Stmt::in, Index::in, Value::in, Rc::out, Error::out,
        _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, thread_safe, tabled_for_io],
"
    if (SQLITE_OK == sqlite3_bind_double(Stmt, Index, Value)) {
        Rc = 1;
        Error = MR_make_string_const("""");
    } else {
        Rc = -1;
        Error = _msqlite_copy_errmsg(Db);
    }
").

%-----------------------------------------------------------------------------%

unsafe_bind_text(Db, Stmt, IndexType, Value, Res, !IO) :-
    (
        IndexType = num(Index)
    ;
        IndexType = name(Name),
        bind_parameter_index(Stmt, Name, Index, !IO)
    ),
    bind_text_2(Db, Stmt, Index, Value, Rc, Error, !IO),
    ( Rc = 1 ->
        Res = ok
    ;
        Res = error(Error)
    ).

:- pred bind_text_2(db(RwRo)::in, stmt::in, int::in, string::in,
    int::out, string::out, io::di, io::uo) is det.

:- pragma foreign_proc("C",
    bind_text_2(Db::in, Stmt::in, Index::in, Value::in, Rc::out, Error::out,
        _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, thread_safe, tabled_for_io],
"
    if (SQLITE_OK ==
        sqlite3_bind_text(Stmt, Index, Value, strlen(Value), SQLITE_STATIC))
    {
        Rc = 1;
        Error = MR_make_string_const("""");
    } else {
        Rc = -1;
        Error = _msqlite_copy_errmsg(Db);
    }
").

%-----------------------------------------------------------------------------%

unsafe_bind_blob(Db, Stmt, IndexType, Pointer, SizeBytes, Res, !IO) :-
    (
        IndexType = num(Index)
    ;
        IndexType = name(Name),
        bind_parameter_index(Stmt, Name, Index, !IO)
    ),
    bind_blob_2(Db, Stmt, Index, Pointer, SizeBytes, Rc, Error, !IO),
    ( Rc = 1 ->
        Res = ok
    ;
        Res = error(Error)
    ).

:- pred bind_blob_2(db(RwRo)::in, stmt::in, int::in,
    c_pointer::in, int::in, int::out, string::out, io::di, io::uo) is det.

:- pragma foreign_proc("C",
    bind_blob_2(Db::in, Stmt::in, Index::in, Pointer::in, SizeBytes::in,
        Rc::out, Error::out, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, thread_safe, tabled_for_io],
"
    if (SQLITE_OK ==
        sqlite3_bind_blob(Stmt, Index, (void *)Pointer, SizeBytes, SQLITE_STATIC))
    {
        Rc = 1;
        Error = MR_make_string_const("""");
    } else {
        Rc = -1;
        Error = _msqlite_copy_errmsg(Db);
    }
").

%-----------------------------------------------------------------------------%

bind_null(Db, Stmt, IndexType, Res, !IO) :-
    (
        IndexType = num(Index)
    ;
        IndexType = name(Name),
        bind_parameter_index(Stmt, Name, Index, !IO)
    ),
    bind_null_2(Db, Stmt, Index, Rc, Error, !IO),
    ( Rc = 1 ->
        Res = ok
    ;
        Res = error(Error)
    ).

:- pred bind_null_2(db(RwRo)::in, stmt::in, int::in, int::out, string::out,
    io::di, io::uo) is det.

:- pragma foreign_proc("C",
    bind_null_2(Db::in, Stmt::in, Index::in, Rc::out, Error::out,
        _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, thread_safe, tabled_for_io],
"
    if (SQLITE_OK == sqlite3_bind_null(Stmt, Index)) {
        Rc = 1;
        Error = MR_make_string_const("""");
    } else {
        Rc = -1;
        Error = _msqlite_copy_errmsg(Db);
    }
").

%-----------------------------------------------------------------------------%

:- pred bind_parameter_index(stmt::in, string::in, int::out, io::di, io::uo)
    is det.

:- pragma foreign_proc("C",
    bind_parameter_index(Stmt::in, Name::in, Index::out, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, thread_safe, tabled_for_io],
"
    Index = sqlite3_bind_parameter_index(Stmt, Name);
").

%-----------------------------------------------------------------------------%

step(Db, Stmt, Res, !IO) :-
    step_2(Db, Stmt, Rc, Error, !IO),
    ( Rc = 0 ->
        Res = done
    ; Rc = 1 ->
        Res = row
    ;
        Res = error(Error)
    ).

:- pred step_2(db(RwRo)::in, stmt::in, int::out, string::out,
    io::di, io::uo) is det.

:- pragma foreign_proc("C",
    step_2(Db::in, Stmt::in, Res::out, Error::out, _IO0::di, _IO::uo),
    [may_call_mercury, promise_pure, thread_safe, tabled_for_io],
"
    int rc = sqlite3_step(Stmt);
    if (rc == SQLITE_DONE) {
        Res = 0;
        Error = MR_make_string_const("""");
    } else if (rc == SQLITE_ROW) {
        Res = 1;
        Error = MR_make_string_const("""");
    } else {
        Res = -1;
        Error = _msqlite_copy_errmsg(Db);
    }
").

%-----------------------------------------------------------------------------%

reset(Db, Stmt, Res, !IO) :-
    reset_2(Db, Stmt, Rc, Error, !IO),
    ( Rc = 1 ->
        Res = ok
    ;
        Res = error(Error)
    ).

:- pred reset_2(db(RwRo)::in, stmt::in, int::out, string::out,
    io::di, io::uo) is det.

:- pragma foreign_proc("C",
    reset_2(Db::in, Stmt::in, Rc::out, Error::out, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, thread_safe, tabled_for_io],
"
    if (sqlite3_reset(Stmt) == SQLITE_OK) {
        Rc = 1;
        Error = MR_make_string_const("""");
    } else {
        Rc = -1;
        Error = _msqlite_copy_errmsg(Db);
    }
").

%-----------------------------------------------------------------------------%

:- pragma foreign_proc("C",
    finalize(Stmt::in, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, thread_safe, tabled_for_io],
"
    if (Stmt) {
        sqlite3_finalize(Stmt);
    }
").

%-----------------------------------------------------------------------------%

column_is_null(Stmt, column(Col), IsNull, !IO) :-
    column_is_null_2(Stmt, Col, IsNull, !IO).

:- pred column_is_null_2(stmt::in, int::in, bool::out, io::di, io::uo) is det.

:- pragma foreign_proc("C",
    column_is_null_2(Stmt::in, Col::in, IsNull::out, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, thread_safe, tabled_for_io],
"
    int type = sqlite3_column_type(Stmt, Col);
    IsNull = (type == SQLITE_NULL) ? MR_YES : MR_NO;
").

%-----------------------------------------------------------------------------%

column_int(Stmt, column(Col), Int, !IO) :-
    column_int_2(Stmt, Col, Int, !IO).

:- pred column_int_2(stmt::in, int::in, int::out,
    io::di, io::uo) is det.

:- pragma foreign_proc("C",
    column_int_2(Stmt::in, Col::in, Int::out, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, thread_safe, tabled_for_io],
"
    Int = sqlite3_column_int(Stmt, Col);
").

%-----------------------------------------------------------------------------%

column_float(Stmt, column(Col), Float, !IO) :-
    column_float_2(Stmt, Col, Float, !IO).

:- pred column_float_2(stmt::in, int::in, float::out,
    io::di, io::uo) is det.

:- pragma foreign_proc("C",
    column_float_2(Stmt::in, Col::in, Float::out, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, thread_safe, tabled_for_io],
"
    Float = sqlite3_column_double(Stmt, Col);
").

%-----------------------------------------------------------------------------%

column_text(Stmt, column(Col), Str, !IO) :-
    column_text_2(Stmt, Col, Str, !IO).

:- pred column_text_2(stmt::in, int::in, string::out,
    io::di, io::uo) is det.

:- pragma foreign_proc("C",
    column_text_2(Stmt::in, Col::in, Str::out, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, thread_safe, tabled_for_io],
"
    const unsigned char *s =  sqlite3_column_text(Stmt, Col);
    if (s) {
        MR_make_aligned_string_copy_msg(Str, (const char *)s, MR_ALLOC_ID);
    } else {
        Str = MR_make_string_const("""");
    }
").

column_maybe_text(Stmt, Col, MaybeStr, !IO) :-
    column_is_null(Stmt, Col, IsNull, !IO),
    (
        IsNull = yes,
        MaybeStr = no
    ;
        IsNull = no,
        column_text(Stmt, Col, Str, !IO),
        MaybeStr = yes(Str)
    ).

%-----------------------------------------------------------------------------%

column_blob(Stmt, column(Col), Pointer, SizeBytes, !IO) :-
    column_blob_2(Stmt, Col, Pointer, SizeBytes, !IO).

:- pred column_blob_2(stmt::in, int::in, c_pointer::out, int::out,
    io::di, io::uo) is det.

:- pragma foreign_proc("C",
    column_blob_2(Stmt::in, Col::in, Pointer::out, SizeBytes::out,
        _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, thread_safe, tabled_for_io],
"
    Pointer = (MR_Word) sqlite3_column_blob(Stmt, Col);
    SizeBytes = sqlite3_column_bytes(Stmt, Col);
").

:- pragma foreign_proc("C",
    column_type(Stmt::in, Col::in, Int::out, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, thread_safe, tabled_for_io],
"
    Int =  sqlite3_column_type(Stmt, Col);
").

column_type_id(integer) = 1.
column_type_id(float) = 2.
column_type_id(text) = 3.
column_type_id(blob) = 4.
column_type_id(null) = 5.

column(Stmt, column(Col), Res, !IO) :-
    column_type(Stmt, column(Col), Type, !IO),
    (Type = 1 -> column_int(Stmt, column(Col), Int, !IO),
		 Res = int(Int)
    ;
    Type = 2 -> column_float(Stmt, column(Col), Float, !IO),
		Res = float(Float)
    ;
    Type = 3 -> column_text(Stmt, column(Col), Text, !IO),
		Res = text(Text)
    ;
    Type = 4 -> column_blob(Stmt, column(Col), Pointer, SizeBytes, !IO),
		Res = blob(Pointer, SizeBytes)
    ;
    % Type = 5 ->
    Res = null
    ).

:- pragma foreign_proc("C",
    column_count(Stmt::in, Count::out, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, thread_safe, tabled_for_io],
"
    Count =  sqlite3_column_count(Stmt);
").

:- pragma foreign_proc("C",
    data_count(Stmt::in, Count::out, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, thread_safe, tabled_for_io],
"
    Count =  sqlite3_data_count(Stmt);
").

column_name(Stmt, column(Col), Str, !IO) :-
    column_name_2(Stmt, Col, Str, !IO).

:- pred column_name_2(stmt::in, int::in, string::out,
    io::di, io::uo) is det.

:- pragma foreign_proc("C",
    column_name_2(Stmt::in, Col::in, Str::out, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, thread_safe, tabled_for_io],
"
    const char *s =  sqlite3_column_name(Stmt, Col);
    if (s) {
        MR_make_aligned_string_copy_msg(Str, (const char *)s, MR_ALLOC_ID);
    } else {
        Str = MR_make_string_const("""");
    }
").


%-----------------------------------------------------------------------------%

escape_LIKE_argument(EscChar, S0) = S :-
    string.to_char_list(S0, Cs0),
    list.foldl(escape_LIKE_argument_2(EscChar), Cs0, [], RevCs),
    string.from_rev_char_list(RevCs, S).

:- pred escape_LIKE_argument_2(char::in, char::in,
    list(char)::in, list(char)::out) is det.

escape_LIKE_argument_2(EscChar, C, RevCs0, RevCs) :-
    ( C = EscChar ->
        RevCs = [EscChar, EscChar | RevCs0]
    ; C = ('%') ->
        RevCs = ['%', EscChar | RevCs0]
    ; C = ('_') ->
        RevCs = ['_', EscChar | RevCs0]
    ;
        RevCs = [C | RevCs0]
    ).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

% High-level interface

with_stmt(Pred, Db, Sql, Bindings, Result, !IO) :-
    sqlite3.prepare(Db, Sql, ResStmt, !IO),
    (
        ResStmt = ok(Stmt),
        promise_equivalent_solutions [Result, !:IO]
        (try [io(!IO)] (
            bind_checked(Db, Stmt, Bindings, !IO),
            Pred(Db, Stmt, Result, !IO),
            keep_alive(Bindings, !IO)
         )
         then
            sqlite3.finalize(Stmt, !IO)
         catch_any Excp ->
            sqlite3.finalize(Stmt, !IO),
            throw(Excp)
        )
    ;
        ResStmt = error(Error),
        throw(sqlite_error(Error))
    ).

with_prepared_stmt(Pred, Db, Stmt, Bindings, Result, !IO) :-
    promise_equivalent_solutions [Result, !:IO]
    (try [io(!IO)] (
        bind_checked(Db, Stmt, Bindings, !IO),
        Pred(Db, Stmt, Result, !IO),
        keep_alive(Bindings, !IO)
     )
     then
        sqlite3.reset(Db, Stmt, _, !IO)
     catch_any Excp ->
        sqlite3.reset(Db, Stmt, _, !IO),
        throw(Excp)
    ).

with_stmt_acc(Pred, Db, Sql, Bindings, !Acc, !IO) :-
    sqlite3.prepare(Db, Sql, ResStmt, !IO),
    (
        ResStmt = ok(Stmt),
        promise_equivalent_solutions [!:Acc, !:IO]
        (try [io(!IO)] (
            bind_checked(Db, Stmt, Bindings, !IO),
            Pred(Db, Stmt, !Acc, !IO),
            keep_alive(Bindings, !IO)
         )
         then
            sqlite3.finalize(Stmt, !IO)
         catch_any Excp ->
            sqlite3.finalize(Stmt, !IO),
            throw(Excp)
        )
    ;
        ResStmt = error(Error),
        throw(sqlite_error(Error))
    ).

with_stmt_acc3(Pred, Db, Sql, Bindings, Res, !A, !B, !C, !IO) :-
    sqlite3.prepare(Db, Sql, ResStmt, !IO),
    (
        ResStmt = ok(Stmt),
        promise_equivalent_solutions [Res, !:A, !:B, !:C, !:IO]
        (try [io(!IO)] (
            bind_checked(Db, Stmt, Bindings, !IO),
            Pred(Db, Stmt, Res, !A, !B, !C, !IO),
            keep_alive(Bindings, !IO)
         )
         then
            sqlite3.finalize(Stmt, !IO)
         catch_any Excp ->
            sqlite3.finalize(Stmt, !IO),
            throw(Excp)
        )
    ;
        ResStmt = error(Error),
        throw(sqlite_error(Error))
    ).

bind_checked(Db, Stmt, Bindings, !IO) :-
    (
        Bindings = []
    ;
        Bindings = [IndexType - Value | RestBindings],
        bind(Db, Stmt, IndexType, Value, BindRes, !IO),
        (
            BindRes = ok
        ;
            BindRes = error(Error),
            throw(sqlite_error(Error))
        ),
        bind_checked(Db, Stmt, RestBindings, !IO)
    ).

step_ok(Db, Stmt, StepResult, !IO) :-
    sqlite3.step(Db, Stmt, StepResult, !IO),
    (
        StepResult = done
    ;
        StepResult = row
    ;
        StepResult = error(Error),
        throw(sqlite_error(Error))
    ).

step_ok_keep_alive(Db, Stmt, KeepAlive, StepResult, !IO) :-
    sqlite3.step(Db, Stmt, StepResult, !IO),
    keep_alive(KeepAlive, !IO),
    (
        StepResult = done
    ;
        StepResult = row
    ;
        StepResult = error(Error),
        throw(sqlite_error(Error))
    ).

get_header(Db, Stmt, Res, !IO) :-
    step(Db, Stmt, StepResult, !IO),
    (
        StepResult = done,
        Res = error("expected row")
    ;
        StepResult = row,
	column_count(Stmt, Count, !IO),
	list.map_foldl((pred(I::in,Y::out,IO0::di,IO1::uo) is det :-
			    column_name(Stmt, column(int.(I-1)), Y, IO0, IO1)),
		       1..Count,
		       Val,
		       !IO),
             Res = ok(Val)
    ;
        StepResult = error(Error),
        Res = error(Error)
    ).

get_row(Db, Stmt, Res, !IO) :-
    step(Db, Stmt, StepResult, !IO),
    (
        StepResult = done,
        Res = error("expected row")
    ;
        StepResult = row,
	column_count(Stmt, Count, !IO),
	list.map_foldl((pred(I::in,Y::out,IO0::di,IO1::uo) is det :-
			    column(Stmt, column(int.(I-1)), Y, IO0, IO1)),
		       1..Count,
		       Val,
		       !IO),
             Res = ok(Val)
    ;
        StepResult = error(Error),
        Res = error(Error)
    ).

get_rows(Db, Stmt, Res, !IO) :-
    column_count(Stmt, Count, !IO),
    get_rows_helper(Db, Stmt, Count, init, Res, !IO).

:- pred get_rows_helper(db(rw)::in, stmt::in, int::in, cord(row_type)::in,
			maybe_error(table_type)::out,
		 io::di, io::uo) is det.

get_rows_helper(Db, Stmt, Count, Agg, Res, !IO) :-
    step(Db, Stmt, StepResult, !IO),
    (
        StepResult = done,
        Res = ok(to_list(Agg))
    ;
        StepResult = row,
	list.map_foldl((pred(I::in,Y::out,IO0::di,IO1::uo) is det :-
			    column(Stmt, column(int.(I-1)), Y, IO0, IO1)),
		       1..Count,
		       Val,
		       !IO),
        get_rows_helper(Db, Stmt, Count, snoc(Agg,Val), Res, !IO)
    ;
        StepResult = error(_),
        Res = error("get_rows_helper failed")
    ).

insert_row(Db, Stmt, Res, !IO) :-
    step(Db, Stmt, StepResult, !IO),
    (
        StepResult = done,
        Res = ok
    ;
        StepResult = row,
        Res = error("unexpected row")
    ;
        StepResult = error(Error),
        Res = error(Error)
    ).

:- pragma foreign_proc("C",
    db_handle(Stmt::in, Db::out, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, thread_safe, tabled_for_io],
"
    Db = sqlite3_db_handle(Stmt);
").

:- type cords == list(cord(data_type)).

get_cols(Db, Stmt, Res, !IO) :-
    column_count(Stmt, Count, !IO),
    Init = map(func(_) = cord.init, 1..Count),
    get_cols_helper(Db, Stmt, Count, Init, Cols, !IO),
    Res = map(to_list, Cols).

:- pred get_cols_helper(db(rw)::in, stmt::in, int::in, cords::in, cords::out,
		 io::di, io::uo) is det.

get_cols_helper(Db, Stmt, Count, Cords, Res, !IO) :-
    step(Db, Stmt, StepResult, !IO),
    (
        StepResult = done,
        Res = Cords
    ;
        StepResult = row,
	map_corresponding_foldl((pred(I::in,Cord0::in,Cord1::out,IO0::di,IO1::uo) is det :-
				     column(Stmt, column(int.(I-1)), Y, IO0, IO1),
				     Cord1 = snoc(Cord0,Y)),
				1..Count,
				Cords,
				Cords1,
				!IO),
        get_cols_helper(Db, Stmt, Count, Cords1, Res, !IO)
    ;
        StepResult = error(_),
        Res = []
    ).

%% infix function composition -- not exported 
:- func (func(T2) = T3) `o` (func(T1) = T2) = (func(T1) = T3) is det.
F `o` G = (func(X) = F(G(X))). % functional order: G first!

:- func data_to_column_type(data_type) = column_type.
data_to_column_type(null) = null.
data_to_column_type(int(_)) = integer.
data_to_column_type(float(_)) = float.
data_to_column_type(text(_)) = text.
data_to_column_type(blob(_,_)) = blob.

:- func column_type_to_string(column_type) = string.
column_type_to_string(null) = "NULL".
column_type_to_string(integer) = "INTEGER".
column_type_to_string(float) = "FLOAT".
column_type_to_string(text) = "TEXT".
column_type_to_string(blob) = "BLOB".

%% Should the data_type names reflect Sqlite or Mercury?

%% Using strings to generate SQL is generally a bad idea:(
%% This can be done in a more type-safe manner:)
write_table(Db, TableName, Headers, Data, !IO) :-
    Data = [Row1|_] ->
	%% valid_table_name(TableName) = yes, % Error: Unification with `bool.yes' can fail.
	%% TODO check Headers
	Types = map(column_type_to_string `o` data_to_column_type, Row1),
	ColumnDeclaration =
	  map_corresponding(func(Header,Type) = Header ++ " " ++ Type, Headers, Types),
	ColumnDeclarations = join_list(", ", ColumnDeclaration),
	format("create table %s (%s)", [s(TableName), s(ColumnDeclarations)], CreateSql),
	exec(Db, CreateSql, _, !IO),
	Placeholders = join_list(", ", map(func(_) = "?", Row1)),
	format("insert into %s values (%s)", [s(TableName), s(Placeholders)], InsertSql),
	prepare(Db, InsertSql, MaybeStmt, !IO),
	(MaybeStmt = ok(Stmt) ->
	     Ncol = length(Row1),
	     MakeRow = (func(Row) = map_corresponding(func(I,Datum) = num(I) - Datum, 1..Ncol, Row)),
	     foldl((pred(Row::in, IO0::di, IO1::uo) is det :-
			with_prepared_stmt(insert_row, Db, Stmt,
					   MakeRow(Row),
					   _, IO0, IO1)),
		   Data, !IO),
	     end_transaction(Db, _, !IO),
	     finalize(Stmt, !IO)
	;
	print_line("Failed to compile insert statement", !IO))
    ;
    print_line("Empty data", !IO).

read_query(Db, Query, Headers, Data, !IO) :-
    prepare(Db, Query, MaybeStmt, !IO),
    (MaybeStmt = ok(Stmt) ->
	 with_prepared_stmt(get_header, Db, Stmt, [], Headers, !IO),
	 with_prepared_stmt(get_rows, Db, Stmt, [], Data, !IO),
	 finalize(Stmt, !IO)
    ;
    Headers = error("no headers as query compile failed"),
    Data = error("no data as query compile failed")).


%% Not currently exported
:- func transpose(list(list(T))) = list(list(T)) is det.
transpose(List : list(list(T))) = Y :-
    List = [Head|_] ->
	(some [!Agg] (
	     !:Agg : list(cord(T)) = map(func(_) = init, 1..length(Head)),
	     foldl((pred(Row::in,Agg0::in,Agg1::out) is det :-
			Agg1 = map_corresponding(snoc, Agg0, Row)),
		   List,
		   !Agg),
	     Y = map(to_list,!.Agg)))
    ;
    Y = [[]].

%-----------------------------------------------------------------------------%
% Parsing facilities - not currently exported

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

:- func valid_table_name(string) = bool is det.
valid_table_name(String) = Test :-
    new_src_and_ps(String, Src, PS0),
    parse_table_name(Src, _, PS0, PS1),
    eof(Src, _, PS1, _) -> Test = yes
    ;
    Test = no.

:- func valid_column_name(string) = bool is det.
valid_column_name(String) = Test :-
    new_src_and_ps(String, Src, PS0),
    parse_var_name(Src, _, PS0, PS1),
    eof(Src, _, PS1, _) -> Test = yes
    ;
    Test = no.


%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sts=4 sw=4 et
