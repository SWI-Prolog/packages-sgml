/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (c)  2000-2014, University of Amsterdam
                              VU University Amsterdam
    All rights reserved.

    Redistribution and use in source and binary forms, with or without
    modification, are permitted provided that the following conditions
    are met:

    1. Redistributions of source code must retain the above copyright
       notice, this list of conditions and the following disclaimer.

    2. Redistributions in binary form must reproduce the above copyright
       notice, this list of conditions and the following disclaimer in
       the documentation and/or other materials provided with the
       distribution.

    THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
    "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
    LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS
    FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE
    COPYRIGHT OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT,
    INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING,
    BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
    LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
    CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
    LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN
    ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
    POSSIBILITY OF SUCH DAMAGE.
*/

:- module(sgml_test,
          [ test/1,                     % +File
            testdir/1,                  % +Dir
            pass/1,                     % +File
            show/1,                     % +File
            test/0
          ]).

:- prolog_load_context(directory, CWD),
   working_directory(_, CWD).

:- asserta(user:file_search_path(library, '..')).
:- asserta(user:file_search_path(library, '../../RDF')).
:- asserta(user:file_search_path(foreign, '..')).
:- use_module(library(sgml)).
:- use_module(library(pprint)).

:- dynamic failed/1.

test :-
    testdir(.),
    test_callback.

testdir(Dir) :-
    retractall(failed(_)),
    atom_concat(Dir, '/*', Pattern),
    expand_file_name(Pattern, Files),
    maplist(dotest, Files),
    report_failed.

dotest(File) :-
    file_name_extension(_, Ext, File),
    memberchk(Ext, [sgml, xml, html]),
    !,
    test(File).
dotest(_).

test(File) :-
    debug(sgml(test), 'Test ~w ... ', [File]),
    flush_output,
    load_file(File, Term),
    ground(Term),                   % make sure
    okfile(File, OkFile),
    (   exists_file(OkFile)
    ->  load_prolog_file(OkFile, TermOk, ErrorsOk),
        (   compare_dom(Term, TermOk)
        ->  true
        ;   assert(failed(File)),
            format('WRONG'),
            format('~NOK:~n'),
            pretty_print(TermOk),
            format('~NANSWER:~n'),
            pretty_print(Term)
        ),
        error_terms(Errors),
        (   compare_errors(Errors, ErrorsOk)
        ->  true
        ;   retractall(failed(File)),
            assert(failed(File)),
            format(' [Different errors]~nOK:~n'),
            pretty_print(ErrorsOk),
            format('~NANSWER:~n'),
            pretty_print(Errors)
        )
    ;   show_errors,
        format('Loaded, no validating data~n'),
        pretty_print(Term)
    ).

show(File) :-
    load_file(File, Term),
    pretty_print(Term).

pass(File) :-
    load_file(File, Term),
    okfile(File, OkFile),
    open(OkFile, write, Fd),
    format(Fd, '~q.~n', [Term]),
    (   error_terms(Errors)
    ->  format(Fd, '~q.~n', [Errors])
    ;   true
    ),
    close(Fd).

report_failed :-
    findall(X, failed(X), L),
    length(L, Len),
    (   Len > 0
    ->  format('~N*** ~w tests failed ***~n', [Len]),
        fail
    ;   format('~NAll tests passed~n', [])
    ).

:- dynamic
    error/3.
:- multifile
    user:message_hook/3.

user:message_hook(Term, Kind, Lines) :-
    Term = sgml(_,_,_,_),
    assert(error(Term, Kind, Lines)).

show_errors :-
    (   error(_Term, Kind, Lines),
        atom_concat(Kind, ': ', Prefix),
        print_message_lines(user_error, Prefix, Lines),
        fail
    ;   true
    ).

error_terms(Errors) :-
    findall(Term, error(Term, _, _), Errors).

compare_errors([], []).
compare_errors([sgml(_Parser1, _File1, Line, Msg)|T0],
               [sgml(_Parser2, _File2, Line, Msg)|T]) :-
    compare_errors(T0, T).

load_file(File, Term) :-
    load_pred(Ext, Pred),
    file_name_extension(_, Ext, File),
    !,
    retractall(error(_,_,_)),
    call(Pred, File, Term).
load_file(Base, Term) :-
    load_pred(Ext, Pred),
    file_name_extension(Base, Ext, File),
    exists_file(File),
    !,
    retractall(error(_,_,_)),
    call(Pred, File, Term).


load_pred(sgml, load_sgml_file).
load_pred(xml,  load_xml_file).
load_pred(html, load_html_file).

okfile(File, OkFile) :-
    file_name_extension(Base, _, File),
    file_directory_name(Base, Dir),
    atomic_list_concat([Dir, '/ok/', Base, '.ok'], OkFile).

load_prolog_file(File, Term, Errors) :-
    open(File, read, Fd,
         [ encoding(utf8)
         ]),
    read(Fd, Term),
    (   read(Fd, Errors),
        Errors \== end_of_file
    ->  true
    ;   Errors = []
    ),
    close(Fd).

compare_dom([], []) :- !.
compare_dom([H1|T1], [H2|T2]) :-
    !,
    compare_dom(H1, H2),
    compare_dom(T1, T2).
compare_dom(X, X) :- !.
compare_dom(element(Name, A1, Content1),
            element(Name, A2, Content2)) :-
    compare_attributes(A1, A2),
    compare_dom(Content1, Content2).

compare_attributes(A1, A2) :-
    sort(A1, L1),
    sort(A2, L2),
    L1 == L2.

pretty_print(Term) :-
    print_term(Term, [output(current_output)]).

                 /*******************************
                 *          OTHER TESTS         *
                 *******************************/

:- thread_local content/1.

test_callback :-
    retractall(content(_)),
    File = 'utf8.xml',
    open(File, read, In),
    new_sgml_parser(Parser, []),
    set_sgml_parser(Parser, file(File)),
    set_sgml_parser(Parser, dialect(xml)),
    sgml_parse(Parser,
               [ source(In),
                 call(begin, on_begin),
                 call(end, on_end)
               ]),
    close(In),
    findall(X, content(X), Xs),
    length(Xs, 2),
    maplist(cdata, Xs).

cdata([]).
cdata([Atom]) :- atom(Atom).

on_begin(name, _Attr, Parser) :-
    sgml_parse(Parser,
               [ document(Content),
                 parse(content)
               ]),
    assertz(content(Content)).

on_end(_Tag, _Parser).
