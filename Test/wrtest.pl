/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@cs.vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (C): 2005-2013, University of Amsterdam
			      VU University Amsterdam

    This program is free software; you can redistribute it and/or
    modify it under the terms of the GNU General Public License
    as published by the Free Software Foundation; either version 2
    of the License, or (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public
    License along with this library; if not, write to the Free Software
    Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301  USA

    As a special exception, if you link this library with other files,
    compiled with a Free Software compiler, to produce an executable, this
    library does not by itself cause the resulting executable to be covered
    by the GNU General Public License. This exception does not however
    invalidate any other reasons why the executable file might be covered by
    the GNU General Public License.
*/

:- prolog_load_context(directory, CWD),
   working_directory(_, CWD).

:- asserta(file_search_path(foreign, '..')).
:- asserta(file_search_path(library, '..')).
:- asserta(file_search_path(library, '../../RDF')).

:- use_module(library(sgml)).
:- use_module(library(sgml_write)).
:- use_module(library(pretty_print)).

:- dynamic failed/2.

test :-					% default test
	fp('.').

test(File) :-
	file_name_extension(_, xml, File), !,
	load_xml_file(File, Term),
	xml_write(user_output, Term, []).
test(File) :-
	file_name_extension(_, sgml, File), !,
	load_sgml_file(File, Term),
	sgml_write(user_output, Term, []).
test(File) :-
	file_name_extension(_, html, File), !,
	load_html_file(File, Term),
	html_write(user_output, Term, []).

test(File, Into, Encoding) :-
	file_name_extension(_, xml, File), !,
	load_xml_file(File, Term),
	open(Into, write, Out, [encoding(Encoding)]),
	xml_write(Out, Term, []),
	close(Out).

fp(Dir) :-
	retractall(failed(_, _)),
	atom_concat(Dir, '/*', Pattern),
	expand_file_name(Pattern, Files),
	(   member(File, Files),
	    file_name_extension(_, Ext, File),
	    ml_file(Ext),
	    file_base_name(File, Base),
	    \+ blocked(Base),
	    debug(sgml(test), '~w ... ', [Base]),
	    (	\+ utf8(Base)
	    ->  debug(sgml(test), ' (ISO Latin-1) ... ', []),
		fixed_point(File, iso_latin_1)
	    ;	true
	    ),
	    debug(sgml(test), ' (UTF-8) ... ', []),
	    fixed_point(File, utf8),
	    debug(sgml(test), ' done~n', []),
	    fail
	;   report_failed
	).

ml_file(xml).
ml_file(sgml).
ml_file(html).

%%	blocked(+File)
%
%	List of test-files that are blocked.  These are either negative
%	tests or tests involving SDATA.

blocked('bat.sgml').
blocked('i.sgml').
blocked('sdata.sgml').
blocked('cent-nul.xml').
blocked('defent.sgml').
blocked('comment.xml').
blocked('badxmlent.xml').
blocked('nosysent.xml').


%%	utf8(+File)
%
%	File requires UTF-8.  These are files that have UTF-8 characters
%	in element or attribute names.

utf8('utf8-ru.xml').


report_failed :-
	findall(X, failed(X, _), L),
	length(L, Len),
	(   Len > 0
        ->  format('~N*** ~w tests failed ***~n', [Len]),
	    fail
        ;   format('~NAll read/write roundtrip tests passed~n', [])
	).


%%	fixed_point(+File, +Encoding)
%
%	Perform write/read round-trip and  validate   the  data  has not
%	changed.

fixed_point(File, Encoding) :-
	file_name_extension(_, xml, File), !,
	fp(File, Encoding, load_xml_file, xml_write).
fixed_point(File, Encoding) :-
	file_name_extension(_, sgml, File), !,
	fp(File, Encoding, load_sgml_file, sgml_write).
fixed_point(File, Encoding) :-
	file_name_extension(_, html, File), !,
	fp(File, Encoding, load_html_file, html_write).

fp(File, Encoding, Load, Write) :-
	(   debugging(sgml(test))
	->  put_char(user_error, r)
	;   true
	),
	call(Load, File, Term),
	tmp_file(xml, TmpFile),
	open(TmpFile, write, TmpOut, [encoding(Encoding)]),
	(   debugging(sgml(test))
	->  put_char(user_error, w)
	;   true
	),
	call(Write, TmpOut, Term, []),
	close(TmpOut),
%	cat(TmpFile, Encoding),
	(   debugging(sgml(test))
	->  put_char(user_error, r)
	;   true
	),
	call(Load, TmpFile, Term2),
	delete_file(TmpFile),
	(   eq(Term, Term2)
	->  true
	;   assert(failed(File, Encoding)),
	    format(user_error, 'First file:~n', []),
	    pretty_print(Term),
	    format(user_error, 'Second file:~n', []),
	    pretty_print(Term2),
	    fail
	).

cat(File, Encoding) :-
	open(File, read, In, [encoding(Encoding)]),
	copy_stream_data(In, current_output),
	close(In).

%	eq(M1, M2)
%
%	Test two terms for equivalence.  The following mismatches are
%	allowed:
%
%		* Order of attributes
%		* Layout in `element-only' content

eq(X, X) :- !.
eq([], []) :- !.
eq([B|T], L) :-				% delete blanks
	blank_atom(B), !,
	eq(T, L).
eq(L, [B|T]) :-
	blank_atom(B), !,
	eq(T, L).
eq([H1|T1], [H2|T2]) :- !,
	eq(H1, H2),
	eq(T1, T2).
eq(element(Name, A1, C1), element(Name, A2, C2)) :-
	att_eq(A1, A2),
	ceq(C1, C2).
eq(A1, A2) :-
	atom(A1),
	atom(A2), !,
	normalise_blanks(A1, B1),
	normalise_blanks(A2, B2),
	(   B1 == B2
	->  true
	;   format(user_error,
		   'ERROR: CDATA differs:~n\c
		   \t~p~n\c
		   \t~p~n',
		   [B1, B2])
	).
eq(X, Y) :-
	format(user_error,
	       'ERROR: Content differs:~n\c
	       \t~p~n\c
	       \t~p~n',
	       [X, Y]).

att_eq(A1, A2) :-			% ordering is unimportant
	sort(A1, S),
	sort(A2, S), !.
att_eq(A1, A2) :-
	format(user_error,
	       'ERROR: Attribute lists differ:~n\c
	       \t~p~n\c
	       \t~p~n',
	       [A1, A2]).

ceq(C1, C2) :-
	element_content(C1, E1),
	element_content(C2, E2), !,
	eq(E1, E2).
ceq(C1, C2) :-
	eq(C1, C2).

element_content([], []).
element_content([element(Name,Atts,C)|T0], [element(Name,Atts,C)|T]) :- !,
	element_content(T0, T).
element_content([Blank|T0], T) :-
	blank_atom(Blank),
	element_content(T0, T).

blank_atom(Atom) :-
	atom(Atom),
	atom_codes(Atom, Codes),
	all_blanks(Codes).

all_blanks([]).
all_blanks([H|T]) :-
	code_type(H, space),
	all_blanks(T).

normalise_blanks(Atom, Normalised) :-
	atom_codes(Atom, Codes),
	eat_blanks(Codes, Codes1),
	normalise_blanks2(Codes1, N),
	atom_codes(Normalised, N).

normalise_blanks2([], []).
normalise_blanks2([H|T0], T) :-
	code_type(H, space), !,
	eat_blanks(T0, T1),
	(   T1 == []
	->  T = []
	;   T = [32|T2],
	    normalise_blanks2(T1, T2)
	).
normalise_blanks2([H|T0], [H|T]) :-
	normalise_blanks2(T0, T).

eat_blanks([H|T0], T) :-
	code_type(H, space), !,
	eat_blanks(T0, T).
eat_blanks(L, L).
