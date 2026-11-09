/*  Part of SWI-Prolog

    Author:        Jan Wielemaker and Matt Lilley
    E-mail:        J.Wielemaker@vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (c)  2016, CWI Amsterdam
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

:- module(c14n2,
          [ xml_write_canonical/3       % +Stream, +Term, +Options
          ]).
:- autoload(library(apply),
            [partition/4,maplist/3,foldl/4,include/3]).
:- autoload(library(error),
            [instantiation_error/1,must_be/2,existence_error/2]).
:- autoload(library(lists),[member/2,append/3]).
:- autoload(library(option),[option/3]).
:- autoload(library(pairs),[pairs_values/2]).
:- autoload(library(sgml_write),[xml_write/3]).

/** <module> C14n2 canonical XML documents

C14n2 specifies a _canonical_ XML document.   This library writes such a
document from an XML DOM as returned by   the  XML (or SGML) parser. The
process takes two steps:

  - Normalise the DOM
  - Call xml_write/3 with appropriate flags
*/

%!  xml_write_canonical(+Stream, +DOM, +Options) is det.
%
%   Write an XML DOM using the   canonical conventions as defined by
%   C14n2. Namespace declarations in the   canonical document depend
%   on the original namespace  declarations.   For  this  reason the
%   input document must be parsed   (see load_structure/3) using the
%   dialect `xmlns` and the option keep_prefix(true).  Options:
%
%     - method(+URI)
%       One of `http://www.w3.org/TR/2001/REC-xml-c14n-20010315`
%       (inclusive, default), `http://www.w3.org/2006/12/xml-c14n11`
%       (handled as inclusive) or
%       `http://www.w3.org/2001/10/xml-exc-c14n#` (exclusive).

xml_write_canonical(Stream, DOM, Options) :-
    option(method(Method), Options,
           'http://www.w3.org/TR/2001/REC-xml-c14n-20010315'),
    method_mode(Method, Mode),
    xml_canonical_dom(DOM, CDOM, ctx(Mode, ns{}, ns{})),
    xml_write(Stream, CDOM,
              [ header(false),
                layout(false),
                net(false)
              ]).

method_mode('http://www.w3.org/TR/2001/REC-xml-c14n-20010315', inclusive).
method_mode('http://www.w3.org/2006/12/xml-c14n11', inclusive).
method_mode('http://www.w3.org/2001/10/xml-exc-c14n#', exclusive).

%!  xml_canonical_dom(+DOM, -CDOM, +Context)
%
%   Context is a term ctx(Mode, InScope, Rendered), where InScope is a
%   dict mapping prefixes to namespace URIs that are in scope and
%   Rendered is a dict mapping the prefixes that are declared by the
%   output ancestors.  The default namespace uses the prefix `''`.
%
%   For  _inclusive_  canonicalization,  an    element  declares  each
%   in-scope namespace that is not declared  with the same URI by its
%   output ancestors.  For _exclusive_ canonicalization, it only does so
%   for the namespaces that are _visibly utilized_  by the element name
%   and its attributes.  Namespace declarations are ordered by prefix,
%   the default namespace first.  Attributes are  ordered  by namespace
%   URI and local name, attributes without a namespace first.

xml_canonical_dom(Var, _, _) :-
    var(Var),
    !,
    instantiation_error(Var).
xml_canonical_dom(DOM, CDOM, Ctx) :-
    is_list(DOM),
    !,
    xml_canonical_list(DOM, CDOM, Ctx).
xml_canonical_dom(element(Name, Attrs, Content),
                  element(CName, CAttrs, CContent),
                  ctx(Mode, InScope0, Rendered0)) :-
    !,
    must_be(list, Attrs),
    must_be(list, Content),
    partition(ns_decl, Attrs, Decls, Attrs1),
    foldl(add_decl, Decls, InScope0, InScope),
    element_name(Name, InScope, EPrefix, ELocal, EURI),
    maplist(attribute_parts(InScope), Attrs1, AttrParts),
    utilized(EPrefix-EURI, AttrParts, Utilized),
    (   Mode == inclusive
    ->  dict_pairs(InScope, _, InScopePairs),
        append(Utilized, InScopePairs, Candidates0) % Utilized has priority
    ;   Candidates0 = Utilized
    ),
    sort(1, @<, Candidates0, Candidates),
    include(must_render(Rendered0), Candidates, Render),
    foldl(add_decl_pair, Render, Rendered0, Rendered),
    maplist(ns_attr, Render, NSAttrs),
    sort(1, @<, AttrParts, SortedAttrParts),
    pairs_values(SortedAttrParts, CAttrs1),
    append(NSAttrs, CAttrs1, CAttrs),
    qname(EPrefix, ELocal, CName),
    xml_canonical_list(Content, CContent, ctx(Mode, InScope, Rendered)).
xml_canonical_dom(CDATA, CDATA, _) :-
    atomic(CDATA),
    !.
xml_canonical_dom(Node, Node, _).

xml_canonical_list([], [], _).
xml_canonical_list([H0|T0], [H|T], Ctx) :-
    xml_canonical_dom(H0, H, Ctx),
    xml_canonical_list(T0, T, Ctx).

%   ns_decl(+Attribute) is semidet.
%   add_decl(+Attribute, +InScope0, -InScope) is det.

ns_decl(Attr) :-
    xml_ns(Attr, _, _).

add_decl(Attr, InScope0, InScope) :-
    xml_ns(Attr, Prefix, URI),
    InScope = InScope0.put(Prefix, URI).

add_decl_pair(Prefix-URI, Dict0, Dict) :-
    Dict = Dict0.put(Prefix, URI).

xml_ns(ns('', xmlns):NS=URL, NS, URL) :- !.
xml_ns(ns(xmlns, _):NS=URL, NS, URL) :- !.
xml_ns(xmlns=URL, '', URL) :- !.
xml_ns(xmlns:NS=URL, NS, URL) :- !.
xml_ns(Name=URL, NS, URL) :-
    atom(Name),
    atom_concat('xmlns:', NS, Name).

%   element_name(+Name, +InScope, -Prefix, -Local, -URI) is det.
%   attribute_parts(+InScope, +Attribute, -Key-CAttribute) is det.
%
%   Find the prefix, local name and namespace URI.  Key is URI-Local,
%   used for sorting the attributes.

element_name(Name, InScope, Prefix, Local, URI) :-
    name_parts(Name, element, InScope, Prefix, Local, URI).

attribute_parts(InScope, Name=Value, (URI-Local)-(QName=Value)) :-
    name_parts(Name, attribute, InScope, Prefix, Local, URI),
    qname(Prefix, Local, QName).

name_parts(ns(P, URI0):Local, _, _, Prefix, Local, URI) :-
    !,
    (   (P == xml ; URI0 == xml)
    ->  Prefix = xml,
        xml_namespace(URI)
    ;   Prefix = P,
        URI = URI0
    ).
name_parts(URI:Local, Type, InScope, Prefix, Local, URI) :-
    !,
    (   URI == xml
    ->  Prefix = xml
    ;   dict_pairs(InScope, _, Pairs),
        (   Type == element,
            memberchk(''-URI, Pairs)
        ->  Prefix = ''
        ;   member(Prefix-URI, Pairs),
            Prefix \== ''
        ->  true
        ;   existence_error(namespace_prefix, URI)
        )
    ).
name_parts(Name, Type, InScope, Prefix, Local, URI) :-
    must_be(atom, Name),
    (   sub_atom(Name, B, _, A, :)
    ->  sub_atom(Name, 0, B, _, Prefix),
        sub_atom(Name, _, A, 0, Local),
        (   Prefix == xml
        ->  xml_namespace(URI)
        ;   URI = InScope.get(Prefix)
        ->  true
        ;   existence_error(namespace_prefix, Prefix)
        )
    ;   Prefix = '',
        Local = Name,
        (   Type == element
        ->  URI = InScope.get('', '')
        ;   URI = ''
        )
    ).

xml_namespace('http://www.w3.org/XML/1998/namespace').

qname('', Local, Local) :- !.
qname(Prefix, Local, QName) :-
    atomic_list_concat([Prefix, Local], :, QName).

%   utilized(+ElementPrefix-URI, +AttrParts, -PrefixURIPairs)
%
%   The namespaces that are visibly utilized by an element.

utilized(Element, AttrParts, [Element|AttrNS]) :-
    findall(Prefix-URI,
            ( member((URI-_)-(QName=_), AttrParts),
              sub_atom(QName, B, _, _, :),
              sub_atom(QName, 0, B, _, Prefix)
            ), AttrNS).

%   must_render(+Rendered, +Prefix-URI) is semidet.

must_render(_, xml-_) :-
    !,
    fail.
must_render(Rendered, ''-URI) :-
    !,
    URI \== Rendered.get('', '').
must_render(Rendered, Prefix-URI) :-
    \+ URI == Rendered.get(Prefix).

ns_attr(''-URI, xmlns=URI) :- !.
ns_attr(Prefix-URI, QName=URI) :-
    atom_concat('xmlns:', Prefix, QName).
