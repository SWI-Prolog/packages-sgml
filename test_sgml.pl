/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (c)  2000-2018, University of Amsterdam
                              VU University Amsterdam
                              CWI, Amsterdam
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
          [ test_sgml/1,                % +File
            testdir/1,                  % +Dir
            pass/1,                     % +File
            show/1,                     % +File
            test_sgml/0
          ]).

:- use_module(library(sgml)).
:- use_module(library(pprint)).
:- use_module(library(apply)).
:- use_module(library(debug)).
:- use_module(library(error)).
:- use_module(library(sandbox), [safe_goal/1]).
:- use_module(library(xpath)).
:- if(exists_source(library(time))).
:- use_module(library(time)).
:- endif.
:- use_module(library(yall)).

:- dynamic failed/1.

test_sgml :-
    test_input('Test', Path),
    testdir(Path),
    test_callback,
    test_max_errors0,
    test_sandbox,
    test_entity_option,
    test_xpath_pi,
    test_dtd_entity_property,
    test_parser_lifetime,
    test_allowed_overflow,
    test_long_system_id,
    test_malformed_dtd,
    test_entity_expansion,
    test_external_entities,
    test_html_dtd_cache,
    test_invalid_chars.

test_input(Name, Path) :-
    source_file(test_sgml, MyFile),
    file_directory_name(MyFile, MyDir),
    atomic_list_concat([MyDir, Name], /, Path).

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
    test_sgml(File).
dotest(_).

test_sgml(File) :-
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

load_file(Spec, Term) :-
    (   load_pred(Ext, Pred),
        file_name_extension(_, Ext, Spec)
    ->  File = Spec
    ;   load_pred(Ext, Pred),
        file_name_extension(Spec, Ext, File),
        exists_file(File)
    ->  true
    ;   existence_error(test_file, Spec)
    ),
    retractall(error(_,_,_)),
    call(Pred, File, Term,
         [ encoding('utf-8')
         ]).

load_pred(sgml, load_sgml).
load_pred(xml,  load_xml).
load_pred(html, load_html).

okfile(File, OkFile) :-
    file_name_extension(Plain, _, File),
    file_base_name(Plain, Base),
    file_directory_name(Plain, Dir),
    atomic_list_concat([Dir, '/ok/', Base, '.ok'], OkFile),
    writeln(File-OkFile).

load_prolog_file(File, Term, Errors) :-
    setup_call_cleanup(
        open(File, read, Fd,
             [ encoding(utf8),
               newline(detect)
             ]),
        ( read(Fd, Term),
          (   read(Fd, Errors),
              Errors \== end_of_file
          ->  true
          ;   Errors = []
          )
        ),
        close(Fd)).

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
    test_input('Test/utf8.xml', File),
    setup_call_cleanup(
        open(File, read, In),
        setup_call_cleanup(
            new_sgml_parser(Parser, []),
            ( set_sgml_parser(Parser, file(File)),
              set_sgml_parser(Parser, dialect(xml)),
              sgml_parse(Parser,
                         [ source(In),
                           call(begin, on_begin),
                           call(end, on_end)
                         ])
            ),
            free_sgml_parser(Parser)),
        close(In)),
    findall(X, content(X), Xs),
    length(Xs, 2),
    maplist(cdata, Xs).

cdata([]).
cdata([Atom]) :- must_be(atom, Atom).

on_begin(name, _Attr, Parser) :-
    sgml_parse(Parser,
               [ document(Content),
                 parse(content)
               ]),
    assertz(content(Content)).

on_end(_Tag, _Parser).

%   test_max_errors0
%
%   With max_errors(0), an error raised while closing open elements
%   at the end of the input leaked a foreign frame, corrupting the
%   local stack (packages-sgml#100).

test_max_errors0 :-
    forall(between(1, 100, _),
           ( catch(load_truncated, E, true),
             subsumes_term(error(syntax_error(_), _), E)
           )),
    garbage_collect.

load_truncated :-
    setup_call_cleanup(
        open_string("<a>\n<b x=\"1\">\n", In),
        load_structure(In, _, [max_errors(0), dialect(xml)]),
        close(In)).

%   test_sandbox
%
%   load_structure/3 and friends are only safe on in-memory input and
%   with safe call/2 option closures.

test_sandbox :-
    safe_goal(load_html(string("<p>x</p>"), _, [])),
    safe_goal(load_structure(string("<p>x</p>"), _, [dialect(xml)])),
    \+ catch(safe_goal(load_structure(string("<a>ls</a>"), _,
                                     [call(cdata, shell)])), _, fail),
    \+ catch(safe_goal(load_xml('/etc/passwd', _, [])), _, fail),
    \+ catch(safe_goal(load_structure(string("<a/>"), _, _)), _, fail).

%   test_entity_option
%
%   The entity(Name, Value) option must not allow injecting declarations.

test_entity_option :-
    load_structure(string("<a>&x;</a>"), [element(a, [], ['50%'])],
                   [dialect(xml), entity(x, '50%')]),
    catch(load_structure(string("<a/>"), _,
                         [dialect(xml), entity('x "y"> <!ENTITY z', v)]),
          error(domain_error(xml_name, _), _),
          true).

%   test_xpath_pi
%
%   Extracting text skips processing instructions.

test_xpath_pi :-
    load_structure(string("<a>x<?y z?>w</a>"), DOM, [dialect(xml)]),
    xpath(DOM, //a(text), Text),
    Text == xw.

%   test_dtd_entity_property
%
%   dtd_property/2 for entities and notations used to read wide strings
%   as narrow ones.

test_dtd_entity_property :-
    new_dtd(test, DTD),
    setup_call_cleanup(
        open_dtd(DTD, [dialect(xml)], Out),
        format(Out, '<!ENTITY s SYSTEM "sys.xml">~n\c
                     <!ENTITY p PUBLIC "-//p//id" "pub.xml">~n\c
                     <!NOTATION n SYSTEM "viewer">~n', []),
        close(Out)),
    dtd_property(DTD, entity(s, system('sys.xml'))),
    dtd_property(DTD, entity(p, public('-//p//id', 'pub.xml'))),
    dtd_property(DTD, notation(n, [system(viewer)])),
    free_dtd(DTD).

%   test_parser_lifetime
%
%   Using a freed parser raises an existence error, freeing it during
%   parsing raises a permission error.

test_parser_lifetime :-
    new_sgml_parser(P, []),
    free_sgml_parser(P),
    catch(free_sgml_parser(P), error(existence_error(sgml_parser, _), _), true),
    new_sgml_parser(P2, []),
    setup_call_cleanup(
        open_string("<a><b/></a>", In),
        catch(sgml_parse(P2, [source(In), call(begin, free_in_begin)]),
              error(permission_error(free, sgml_parser, _), _), true),
        close(In)),
    free_sgml_parser(P2).

free_in_begin(_, _, Parser) :-
    free_sgml_parser(Parser).

%   test_allowed_overflow
%
%   get_sgml_parser(P, allowed(L)) with more than 256 alternatives.

test_allowed_overflow :-
    numlist(1, 600, L),
    maplist([I,E]>>format(atom(E), 'e~d', [I]), L, Es),
    atomic_list_concat(Es, '|', Alt),
    findall(D, ( member(E, Es),
                 format(atom(D), '<!ELEMENT ~w EMPTY>', [E])
               ), Ds),
    atomic_list_concat(Ds, Decls),
    format(string(Doc), '<!DOCTYPE root [<!ELEMENT root (~w)*>~w]><root>',
           [Alt, Decls]),
    new_sgml_parser(P, []),
    set_sgml_parser(P, dialect(xml)),
    setup_call_cleanup(
        open_string(Doc, In),
        sgml_parse(P, [source(In), parse(input), syntax_errors(quiet)]),
        close(In)),
    get_sgml_parser(P, allowed(A)),
    length(A, N),
    N > 0,
    free_sgml_parser(P).

%   test_long_system_id
%
%   A long SYSTEM id relative to the document location used to overflow
%   a buffer.

test_long_system_id :-
    tmp_file_stream(text, File, Out),
    length(Cs, 3000),
    maplist(=(0'a), Cs),
    format(Out, '<!DOCTYPE a [<!ENTITY x SYSTEM "~s">]><a>&x;</a>~n', [Cs]),
    close(Out),
    call_cleanup(catch(load_structure(File, _, [dialect(xml),
                                                syntax_errors(quiet)]),
                       _, true),
                 delete_file(File)).

%   test_malformed_dtd
%
%   Documents that used to crash the parser.  We only verify that
%   parsing terminates.

test_malformed_dtd :-
    forall(malformed(Dialect, Doc),
           parse_quietly(Dialect, Doc)),
    length(L, 600),
    foldl([N,N0,N1]>>(format(atom(N), 'e~d', [N0]), N1 is N0+1), L, 0, _),
    atomic_list_concat(L, '|', Group),
    format(string(Doc2), '<!DOCTYPE a [<!ATTLIST (~w) x CDATA #IMPLIED>]><a/>',
           [Group]),
    parse_quietly(xml, Doc2),
    length(Cs, 9000),
    maplist(=(0'a), Cs),
    format(string(Doc3), '<?xml version=~s?><a/>', [Cs]),
    parse_quietly(xml, Doc3),
    numlist(1, 60, Ns),
    findall(D, ( member(N, Ns), N1 is N+1,
                 format(atom(D), '<!ELEMENT a~d O O (a~d)>', [N, N1])
               ), Ds),
    atomic_list_concat(Ds, Decls),
    format(string(Doc4), '<!DOCTYPE r [<!ELEMENT r - - (a1)>~w]><r><a60>',
           [Decls]),
    parse_quietly(sgml, Doc4).

malformed(xml,  "<a><![IGNORE[<![CDATA[x]]>]]>hello</a>").
malformed(xml,  "<!DOCTYPE a [<!ATTLIST (a|b) x CDATA #IMPLIED>\c
                 <!ATTLIST (a|c) x CDATA #IMPLIED>]><a/>").
malformed(xml,  "<!DOCTYPE a [<!SHORTREF m \"\u0400\" e>]><a/>").
malformed(sgml, "<!DOCTYPE a [<!SHORTREF m \"\" e>]><a/>").
malformed(xml,  "<!DOCTYPE a [<!ENTITY e \"%nope;\">]><a>&e;</a>").
malformed(xml,  "<!DOCTYPE a [<!ENTITY e \"&e;\">]><a>&e;</a>").
malformed(xml,  "<!DOCTYPE a [<!ENTITY e \"&e;\">]><a x=\"&e;\"/>").
malformed(xml,  "<!DOCTYPE a [<!ENTITY % q \"&#37;q;\"> %q;]><a/>").

parse_quietly(Dialect, Doc) :-
    catch(load_structure(string(Doc), _,
                         [ dialect(Dialect),
                           max_errors(-1),
                           syntax_errors(quiet)
                         ]),
          _, true).

%   test_entity_expansion
%
%   Nested entity expansion is limited.

test_entity_expansion :-
    findall(D, ( between(1, 9, I), I0 is I-1,
                 format(atom(R), '&l~d;', [I0]),
                 length(Rs, 10), maplist(=(R), Rs),
                 atomic_list_concat(Rs, V),
                 format(atom(D), '<!ENTITY l~d "~w">', [I, V])
               ), Ds),
    atomic_list_concat(Ds, Decls),
    format(string(Doc), '<!DOCTYPE a [<!ENTITY l0 "lol">~w]><a>&l9;</a>',
           [Decls]),
    time_limited(10, parse_quietly(xml, Doc)).

%   time_limited(+Time, :Goal)
%
%   Run Goal with a time limit if library(time) is available.  It is
%   not in single-threaded builds.

:- if(current_predicate(call_with_time_limit/2)).
time_limited(Time, Goal) :-
    call_with_time_limit(Time, Goal).
:- else.
time_limited(_Time, Goal) :-
    call(Goal).
:- endif.

%   test_external_entities
%
%   Documents read from a string cannot access files through parameter
%   entities, entities in attributes or an external DTD subset unless
%   system_entities(true) is given.

test_external_entities :-
    tmp_file_stream(text, File, Out),
    format(Out, 'SECRET', []),
    close(Out),
    call_cleanup(external_entities(File), delete_file(File)).

external_entities(File) :-
    format(string(D1), '<!DOCTYPE a [<!ENTITY % p SYSTEM "~w">\c
                        <!ENTITY x "%p;">]><a>&x;</a>', [File]),
    format(string(D2), '<!DOCTYPE a [<!ENTITY x SYSTEM "~w">]>\c
                        <a v="&x;"/>', [File]),
    forall(member(D, [D1,D2]),
           ( load_structure(string(D), DOM, [ dialect(xml),
                                              syntax_errors(quiet)
                                            ]),
             \+ sub_term('SECRET', DOM)
           )),
    load_structure(string(D1), [element(a, [], ['SECRET'])],
                   [dialect(xml), system_entities(true)]).

%   test_html_dtd_cache
%
%   The internal subset of a document does not affect later documents
%   parsed by load_html/3.

test_html_dtd_cache :-
    load_html(string("<!DOCTYPE html [<!ENTITY foo \"bar\">]><p>x</p>"),
              _, []),
    load_html(string("<p>&foo;</p>"), DOM, []),
    \+ sub_term(bar, DOM).

%   test_invalid_chars
%
%   Character references outside Unicode and invalid UTF-8 do not abort
%   parsing.

test_invalid_chars :-
    load_structure(string("<a v=\"&#x110000;\">x&#xD800;y</a>"),
                   [element(a, [v=''], [xy])],
                   [dialect(xml), syntax_errors(quiet)]),
    tmp_file_stream(octet, File, Out),
    format(Out, '~s', [[0'<,0'a,0'>,0xc3,0'<,0'/,0'a,0'>,0'<,0'b,0'/,0'>]]),
    close(Out),
    call_cleanup(
        load_structure(File, [element(a, [], []), element(b, [], [])],
                       [dialect(xml), syntax_errors(quiet)]),
        delete_file(File)).
