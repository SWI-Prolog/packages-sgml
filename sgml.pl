/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@cs.vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (C): 1985-2012, University of Amsterdam
			      VU University Amsterdam

    This program is free software; you can redistribute it and/or
    modify it under the terms of the GNU General Public License
    as published by the Free Software Foundation; either version 2
    of the License, or (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU Lesser General Public
    License along with this library; if not, write to the Free Software
    Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301  USA

    As a special exception, if you link this library with other files,
    compiled with a Free Software compiler, to produce an executable, this
    library does not by itself cause the resulting executable to be covered
    by the GNU General Public License. This exception does not however
    invalidate any other reasons why the executable file might be covered by
    the GNU General Public License.
*/

:- module(sgml,
	  [ load_sgml_file/2,		% +File, -ListOfContent
	    load_xml_file/2,		% +File, -ListOfContent
	    load_html_file/2,		% +File, -Document

	    load_structure/3,		% +File, -Term, +Options

	    load_dtd/2,			% +DTD, +File
	    load_dtd/3,			% +DTD, +File, +Options
	    dtd/2,			% +Type, -DTD
	    dtd_property/2,		% +DTD, ?Property

	    new_dtd/2,			% +Doctype, -DTD
	    free_dtd/1,			% +DTD
	    open_dtd/3,			% +DTD, +Options, -Stream

	    new_sgml_parser/2,		% -Parser, +Options
	    free_sgml_parser/1,		% +Parser
	    set_sgml_parser/2,		% +Parser, +Options
	    get_sgml_parser/2,		% +Parser, +Options
	    sgml_parse/2,		% +Parser, +Options

	    sgml_register_catalog_file/2, % +File, +StartOrEnd

	    xml_quote_attribute/3,	% +In, -Quoted, +Encoding
	    xml_quote_cdata/3,		% +In, -Quoted, +Encoding
	    xml_quote_attribute/2,	% +In, -Quoted
	    xml_quote_cdata/2,		% +In, -Quoted
	    xml_name/1,			% +In
	    xml_name/2,			% +In, +Encoding

	    xml_basechar/1,		% +Code
	    xml_ideographic/1,		% +Code
	    xml_combining_char/1,	% +Code
	    xml_digit/1,		% +Code
	    xml_extender/1,		% +Code

	    iri_xml_namespace/2,	% +IRI, -Namespace
	    iri_xml_namespace/3,	% +IRI, -Namespace, -LocalName
	    xml_is_dom/1		% +Term
	  ]).
:- use_module(library(lists)).
:- use_module(library(option)).

:- meta_predicate
	load_structure(+, -, :).

:- predicate_options(load_structure/3, 3,
		     [ charpos(integer),
		       defaults(boolean),
		       dialect(oneof([sgml,xml,xmlns])),
		       doctype(atom),
		       dtd(any),
		       encoding(oneof(['iso-8859-1', 'utf-8'])),
		       entity(atom,atom),
		       file(atom),
		       line(integer),
		       number(oneof([token,integer])),
		       qualify_attributes(boolean),
		       shorttag(boolean),
		       space(oneof([sgml,preserve,defailt,remove])),
		       xmlns(atom),
		       xmlns(atom,atom),
		       pass_to(sgml_parse/2, 2)
		     ]).
:- predicate_options(load_dtd/3, 3,
		     [ dialect(oneof([sgml,xml,xmlns])),
		       pass_to(open/4, 4)
		     ]).
:- predicate_options(sgml_parse/2, 2,
		     [ call(oneof([begin,end,cdata,pi,decl,error,xmlns,urlns]),
			    callable),
		       content_length(integer),
		       document(-any),
		       max_errors(integer),
		       parse(oneof([file,element,content,declaration,input])),
		       source(any),
		       syntax_errors(oneof([quiet,print,style])),
		       xml_no_ns(oneof([error,quiet]))
		     ]).
:- predicate_options(new_sgml_parser/2, 2,
		     [ dtd(any)
		     ]).


/** <module> SGML, XML and HTML parser

This library allows you to parse SGML, XML   and HTML data into a Prolog
data  structure.  The  high-level  interface  defined  in  library(sgml)
provides  access  at  the  file-level,  while  the  low-level  interface
(load_structure/3) defined in  the  foreign   module  works  with Prolog
streams. Please examine the source as a  starting point for dealing with
data from other sources  than  files,   such  as  SWI-Prolog  resources,
network-sockets, character strings, etc. The   first example below loads
an HTML file.
*/

:- multifile user:file_search_path/2.
:- dynamic   user:file_search_path/2.

user:file_search_path(dtd, '.').
user:file_search_path(dtd, swi('library/DTD')).

sgml_register_catalog_file(File, Location) :-
	prolog_to_os_filename(File, OsFile),
	'_sgml_register_catalog_file'(OsFile, Location).

:- use_foreign_library(foreign(sgml2pl)).

register_catalog(Base) :-
	absolute_file_name(dtd(Base),
			       [ extensions([soc]),
				 access(read),
				 file_errors(fail)
			       ],
			       SocFile),
	sgml_register_catalog_file(SocFile, end).

:- initialization
	ignore(register_catalog('HTML4')).


		 /*******************************
		 *	   DTD HANDLING		*
		 *******************************/

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Note that concurrent access to DTD objects  is not allowed, and hence we
will allocate and destroy them in each   thread.  Possibibly it would be
nicer to find out why  concurrent  access   to  DTD's  is  flawed. It is
diagnosed to mess with the entity resolution by Fabien Todescato.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

:- thread_local
	current_dtd/2.
:- volatile
	current_dtd/2.
:- thread_local
	registered_cleanup/0.
:- volatile
	registered_cleanup/0.

:- multifile
	dtd_alias/2.

dtd_alias(html, 'HTML4').

%%	dtd(+Type, -DTD) is det.
%
%	DTD is a DTD object created from  the file dtd(Type). Loaded DTD
%	objects are cached. Note that  DTD   objects  may  not be shared
%	between threads. Therefore, dtd/2  maintains   the  pool  of DTD
%	objects  using  a  thread_local  predicate.    DTD  objects  are
%	destroyed if a thread terminates.
%
%	@error existence_error(source_sink, dtd(Type))

dtd(Type, DTD) :-
	current_dtd(Type, DTD), !.
dtd(Type, DTD) :-
	new_dtd(Type, DTD),
	(   dtd_alias(Type, Base)
	->  true
	;   Base = Type
	),
	absolute_file_name(dtd(Base),
			   [ extensions([dtd]),
			     access(read)
			   ], DtdFile),
	load_dtd(DTD, DtdFile),
	register_cleanup,
	asserta(current_dtd(Type, DTD)).

%%	load_dtd(+DTD, +DtdFile, +Options)
%
%	Load DtdFile into a DTD.  Defined options are:
%
%		* dialect(+Dialect)
%		Dialect to use (xml, xmlns, sgml)
%
%		* encoding(+Encoding)
%		Encoding of DTD file
%
%	@param	DTD is a fresh DTD object, normally created using
%		new_dtd/1.

load_dtd(DTD, DtdFile) :-
	load_dtd(DTD, DtdFile, []).
load_dtd(DTD, DtdFile, Options) :-
	split_dtd_options(Options, DTDOptions, FileOptions),
	setup_call_cleanup(
	    open_dtd(DTD, DTDOptions, DtdOut),
	    setup_call_cleanup(
		open(DtdFile, read, DtdIn, FileOptions),
		copy_stream_data(DtdIn, DtdOut),
		close(DtdIn)),
	    close(DtdOut)).

split_dtd_options([], [], []).
split_dtd_options([H|T], [H|TD], S) :-
	dtd_option(H), !,
	split_dtd_options(T, TD, S).
split_dtd_options([H|T], TD, [H|S]) :-
	split_dtd_options(T, TD, S).

dtd_option(dialect(_)).


%%	destroy_dtds
%
%	Destroy  DTDs  cached  by  this  thread   as  they  will  become
%	unreachable anyway.

destroy_dtds :-
	(   current_dtd(_Type, DTD),
	    free_dtd(DTD),
	    fail
	;   true
	).

%%	register_cleanup
%
%	Register cleanup of DTDs created for this thread.

register_cleanup :-
	registered_cleanup, !.
register_cleanup :-
	catch(thread_at_exit(destroy_dtds), _, true),
	assert(registered_cleanup).


		 /*******************************
		 *	    EXAMINE DTD		*
		 *******************************/

prop(doctype(_), _).
prop(elements(_), _).
prop(entities(_), _).
prop(notations(_), _).
prop(entity(E, _), DTD) :-
	(   nonvar(E)
	->  true
	;   '$dtd_property'(DTD, entities(EL)),
	    member(E, EL)
	).
prop(element(E, _, _), DTD) :-
	(   nonvar(E)
	->  true
	;   '$dtd_property'(DTD, elements(EL)),
	    member(E, EL)
	).
prop(attributes(E, _), DTD) :-
	(   nonvar(E)
	->  true
	;   '$dtd_property'(DTD, elements(EL)),
	    member(E, EL)
	).
prop(attribute(E, A, _, _), DTD) :-
	(   nonvar(E)
	->  true
	;   '$dtd_property'(DTD, elements(EL)),
	    member(E, EL)
	),
	(   nonvar(A)
	->  true
	;   '$dtd_property'(DTD, attributes(E, AL)),
	    member(A, AL)
	).
prop(notation(N, _), DTD) :-
	(   nonvar(N)
	->  true
	;   '$dtd_property'(DTD, notations(NL)),
	    member(N, NL)
	).

dtd_property(DTD, Prop) :-
	prop(Prop, DTD),
	'$dtd_property'(DTD, Prop).


		 /*******************************
		 *	       SGML		*
		 *******************************/

parser_option(dialect(_)).
parser_option(shorttag(_)).
parser_option(file(_)).
parser_option(line(_)).
parser_option(space(_)).
parser_option(number(_)).
parser_option(defaults(_)).
parser_option(doctype(_)).
parser_option(qualify_attributes(_)).
parser_option(encoding(_)).

set_parser_options(Parser, Options, RestOptions) :-
	parser_option(Option),
	select_option(Option, Options, RestOptions0), !,
	set_sgml_parser(Parser, Option),
	set_parser_options(Parser, RestOptions0, RestOptions).
set_parser_options(_, Options, Options).


%%	load_structure(+Source, -ListOfContent, :Options) is det.
%
%	Parse   Source   and   return   the   resulting   structure   in
%	ListOfContent.  Source  is  either   a    term   of  the  format
%	stream(StreamHandle) or a  file-name.  Options   is  a  list  of
%	options controlling the conversion process.
%
%	A proper XML document contains only   a  single toplevel element
%	whose name matches the document type.   Nevertheless,  a list is
%	returned for consistency with  the   representation  of  element
%	content.

load_structure(stream(In), Term, M:Options) :- !,
	(   select_option(offset(Offset), Options, Options1)
	->  seek(In, Offset, bof, _)
	;   Options1 = Options
	),
	(   select_option(dtd(DTD), Options1, Options2)
	->  ExplicitDTD = true
	;   ExplicitDTD = false,
	    Options2 = Options1
	),
	new_sgml_parser(Parser,
			[ dtd(DTD)
			]),
	def_entities(Options2, Parser, Options3),
	call_cleanup(parse(Parser, M:Options3, TermRead, In),
		     free_sgml_parser(Parser)),
	(   ExplicitDTD == true
	->  (   DTD = dtd(_, DocType),
	        dtd_property(DTD, doctype(DocType))
	    ->	true
	    ;	true
	    )
	;   free_dtd(DTD)
	),
	Term = TermRead.
load_structure(Stream, Term, Options) :-
	is_stream(Stream), !,
	load_structure(stream(Stream), Term, Options).
load_structure(File, Term, M:Options) :-
	setup_call_cleanup(
	    open(File, read, In, [type(binary)]),
	    load_structure(stream(In), Term, M:[file(File)|Options]),
	    close(In)).

parse(Parser, M:Options, Document, In) :-
	set_parser_options(Parser, Options, Options1),
	parser_meta_options(Options1, M, Options2),
	sgml_parse(Parser,
		   [ document(Document),
		     source(In)
		   | Options2
		   ]).

parser_meta_options([], _, []).
parser_meta_options([call(When, Closure)|T0], M, [call(When, M:Closure)|T]) :- !,
	parser_meta_options(T0, M, T).
parser_meta_options([H|T0], M, [H|T]) :-
	parser_meta_options(T0, M, T).


def_entities([], _, []).
def_entities([H|T], Parser, Opts) :-
	def_entity(H, Parser), !,
	def_entities(T, Parser, Opts).
def_entities([H|T0], Parser, [H|T]) :-
	def_entities(T0, Parser, T).

def_entity(entity(Name, Value), Parser) :-
	get_sgml_parser(Parser, dtd(DTD)),
	xml_quote_attribute(Value, QValue),
	setup_call_cleanup(open_dtd(DTD, [], Stream),
			   format(Stream, '<!ENTITY ~w "~w">~n',
				  [Name, QValue]),
			   close(Stream)).
def_entity(xmlns(URI), Parser) :-
	set_sgml_parser(Parser, xmlns(URI)).
def_entity(xmlns(NS, URI), Parser) :-
	set_sgml_parser(Parser, xmlns(NS, URI)).


		 /*******************************
		 *	     UTILITIES		*
		 *******************************/

load_sgml_file(File, Term) :-
	load_structure(File, Term, [dialect(sgml)]).

load_xml_file(File, Term) :-
	load_structure(File, Term, [dialect(xml)]).

load_html_file(File, Term) :-
	dtd(html, DTD),
	load_structure(File, Term,
		       [ dtd(DTD),
			 dialect(sgml),
			 shorttag(false)
		       ]).


		 /*******************************
		 *	      ENCODING		*
		 *******************************/

%%	xml_quote_attribute(+In, -Quoted) is det.
%%	xml_quote_cdata(+In, -Quoted) is det.
%
%	Backward  compatibility  for  versions  that  allow  to  specify
%	encoding. All characters that cannot fit the encoding are mapped
%	to XML character entities (&#dd;).  Using   ASCII  is the safest
%	value.

xml_quote_attribute(In, Quoted) :-
	xml_quote_attribute(In, Quoted, ascii).

xml_quote_cdata(In, Quoted) :-
	xml_quote_cdata(In, Quoted, ascii).

%%	xml_name(+Atom) is semidet.
%
%	True if Atom is a valid XML name.

xml_name(In) :-
	xml_name(In, ascii).


		 /*******************************
		 *    XML CHARACTER CLASSES	*
		 *******************************/

%%	xml_basechar(+CodeOrChar) is semidet.
%%	xml_ideographic(+CodeOrChar) is semidet.
%%	xml_combining_char(+CodeOrChar) is semidet.
%%	xml_digit(+CodeOrChar) is semidet.
%%	xml_extender(+CodeOrChar) is semidet.
%
%	XML  character  classification   predicates.    Each   of  these
%	predicates accept both a character   (one-character  atom) and a
%	code (integer).
%
%	@see http://www.w3.org/TR/2006/REC-xml-20060816


		 /*******************************
		 *	   TYPE CHECKING	*
		 *******************************/

%%	xml_is_dom(@Term) is semidet.
%
%	True  if  term  statisfies   the    structure   as  returned  by
%	load_structure/3 and friends.

xml_is_dom(0) :- !, fail.		% catch variables
xml_is_dom(List) :-
	is_list(List), !,
	xml_is_content_list(List).
xml_is_dom(Term) :-
	xml_is_element(Term).

xml_is_content_list([]).
xml_is_content_list([H|T]) :-
	xml_is_content(H),
	xml_is_content_list(T).

xml_is_content(0) :- !, fail.
xml_is_content(pi(Pi)) :- !,
	atom(Pi).
xml_is_content(CDATA) :-
	atom(CDATA), !.
xml_is_content(Term) :-
	xml_is_element(Term).

xml_is_element(element(Name, Attributes, Content)) :-
	dom_name(Name),
	dom_attributes(Attributes),
	xml_is_content_list(Content).

dom_name(NS:Local) :-
	atom(NS),
	atom(Local), !.
dom_name(Local) :-
	atom(Local).

dom_attributes(0) :- !, fail.
dom_attributes([]).
dom_attributes([H|T]) :-
	dom_attribute(H),
	dom_attributes(T).

dom_attribute(Name=Value) :-
	dom_name(Name),
	atomic(Value).


		 /*******************************
		 *	      MESSAGES		*
		 *******************************/
:- multifile
	prolog:message/3.

%	Catch messages.  sgml/4 is generated by the SGML2PL binding.

prolog:message(sgml(Parser, File, Line, Message)) -->
	{ get_sgml_parser(Parser, dialect(Dialect))
	},
	[ 'SGML2PL(~w): ~w:~w: ~w'-[Dialect, File, Line, Message] ].


		 /*******************************
		 *	   XREF SUPPORT		*
		 *******************************/

:- multifile
	prolog:called_by/2.

prolog:called_by(sgml_parse(_, Options), Called) :-
	findall(Meta, meta_call_term(_, Meta, Options), Called).

meta_call_term(T, G+N, Options) :-
	T = call(Event, G),
	pmember(T, Options),
	call_params(Event, Term),
	functor(Term, _, N).

pmember(X, List) :-			% member for partial lists
	nonvar(List),
	List = [H|T],
	(   X = H
	;   pmember(X, T)
	).

call_params(begin, begin(tag,attributes,parser)).
call_params(end,   end(tag,parser)).
call_params(cdata, cdata(cdata,parser)).
call_params(pi,	   pi(cdata,parser)).
call_params(decl,  decl(cdata,parser)).
call_params(error, error(severity,message,parser)).
call_params(xmlns, xmlns(namespace,url,parser)).
call_params(urlns, urlns(url,url,parser)).
