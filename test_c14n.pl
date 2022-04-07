/*  Part of SWI-Prolog

    Author:        Matt Lilley
    E-mail:        matt.lilley@securitease.com
    WWW:           http://www.swi-prolog.org
    Copyright (c)  2013-2020, University of Amsterdam
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

:- module(test_c14n,
          [ test_c14n/0
          ]).

:- use_module(library(c14n2)).

test_c14n :-
    run_tests([ c14n ]).

:-begin_tests(c14n).


%!  xml_file(+File, -Absolute)
%
%   Find an absolute path to the xml sample data in the `testdata` directory.

xml_file(File, Abs) :-
    source_file(xml_file(_,_), MyFile),
    file_directory_name(MyFile, MyDir),
    atomic_list_concat([MyDir, File], /, Abs).

%!  c14n_test(+InputFile, +XPath, +TargetFile).
%
%   Canonicalize the document obtained by applying the XPath specification to
%   the document specified by InputFile and confirm it matches exactly the
%   bytes in TargetFile.
%   The XPath specification is a shorthand for logic that a built-in XPath
%   expression cannot directly express

c14n_test(InputFile, XPathSpec, TargetFile):-
        xml_file(InputFile, InputFilename),
        xml_file(TargetFile, TargetFilename),
        setup_call_cleanup(open(InputFilename, read, InputStream,
                                [ encoding(utf8),
                                  newline(detect)
                                ]),
                           load_structure(InputStream, InputDocument,
                                          [ dialect(xmlns),
                                            space(preserve),
                                            keep_prefix(true)
                                          ]),
                           close(InputStream)),
        setup_call_cleanup(open(TargetFilename, read, TargetStream,
                                [ encoding(utf8),
                                  newline(detect)
                                ]),
                           read_string(TargetStream, _, TargetDocument),
                           close(TargetStream)),
        findall(SubDocument,
                extract_subdocument(InputDocument, XPathSpec, SubDocument),
                SubDocuments),
        with_output_to(
            string(GeneratedDocument),
            forall(member(SubDocument, SubDocuments),
                   xml_write_canonical(
                       current_output, SubDocument,
                       [ method('http://www.w3.org/2001/10/xml-exc-c14n#')
                       ]))),
        (   TargetDocument == GeneratedDocument
        ->  true
        ;   format('~NGenerated:~n~q~nTarget from ~p:~n~q~n',
                   [ GeneratedDocument,
                     TargetFile,
                     TargetDocument
                   ]),
            fail
        ).

extract_subdocument(InputDocument, (A ; B), SubDocument):-
        !,
        ( extract_subdocument(InputDocument, A, SubDocument)
        ; extract_subdocument(InputDocument, B, SubDocument)
        ).

extract_subdocument(InputDocument, (A, \+B), SubDocument):-
        !,
        extract_subdocument(InputDocument, A, Intermediate),
        delete_subdocument([Intermediate], B, [SubDocument]).

extract_subdocument(InputDocument, ElementName, SubDocument):-
        xpath(InputDocument, //(_:ElementName), SubDocument).

delete_subdocument(Document, (A ; B), SubDocument):-
        !,
        delete_subdocument(Document, A, S1),
        delete_subdocument(S1, B, SubDocument).

delete_subdocument([], _, []):- !.

delete_subdocument(Document, Element, Document):-
        % Nothing to do - fail quickly
        \+xpath_chk(Document, //(_:Element), _),
        !.

delete_subdocument([element(_:Element, _, _)|Siblings], Element, NewSiblings):-
        !,
        delete_subdocument(Siblings, Element, NewSiblings).

delete_subdocument([element(NS:OtherElement, Attributes, Children)|Siblings], Element, [element(NS:OtherElement, Attributes, NewChildren)|NewSiblings]):-
        !,
        delete_subdocument(Children, Element, NewChildren),
        delete_subdocument(Siblings, Element, NewSiblings).

delete_subdocument([Atom|Siblings], Element, [Atom|NewSiblings]):-
        delete_subdocument(Siblings, Element, NewSiblings).

:- meta_predicate ignore_space_error(0).

ignore_space_error(Goal) :-
    setup_call_cleanup(
        asserta((user:thread_message_hook(sgml(_Parser,_File,_Line,Msg), error, _) :-
                          sub_string(Msg, 0, _, _, "xml:space-mode")), Ref),
        Goal,
        erase(Ref)).


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
These tests are derived from the w3c tests available at
https://www.w3.org/TR/xmldsig2ed-tests/#TestCases-C14n11
The input/target documents are stored in testdata/
The xpath expressions are encoded inline in the tests. This is because the
SWI-Prolog implementation of xpath is not syntax-compatible with the w3c one
The test names include the section of the source document they pertain to
Note that the tests originally are for xml-c14n and not xml-c14n-exc
I have extracted the equivalent *-exc.output documents using xmlstarlet and
the appropriate xpath files
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

test('3.2.1.1 Test case c14n11/xmllang-1'):-
        c14n_test('testdata/xmllang-input.xml', e1, 'testdata/xmllang-1-exc.output').

test('3.2.1.2 Test case c14n11/xmllang-2'):-
        c14n_test('testdata/xmllang-input.xml', e2, 'testdata/xmllang-2-exc.output').

test('3.2.1.3 Test case c14n11/xmllang-3'):-
        c14n_test('testdata/xmllang-input.xml', e11, 'testdata/xmllang-3-exc.output').

test('3.2.1.4 Test case c14n11/xmllang-4'):-
        c14n_test('testdata/xmllang-input.xml', (e11 ; e12), 'testdata/xmllang-4-exc.output').

% These 4 tests all produce an SGML warning that xml:space="true" is invalid
% It is indeed invalid, but that is what is in the official input document
test('3.2.2.1 Test case c14n11/xmlspace-1'):-
        ignore_space_error(c14n_test('testdata/xmlspace-input.xml', e1, 'testdata/xmlspace-1-exc.output')).

test('3.2.2.2 Test case c14n11/xmlspace-2'):-
        ignore_space_error(c14n_test('testdata/xmlspace-input.xml', e2, 'testdata/xmlspace-2-exc.output')).

test('3.2.2.3 Test case c14n11/xmlspace-3'):-
        ignore_space_error(c14n_test('testdata/xmlspace-input.xml', e11, 'testdata/xmlspace-3-exc.output')).

test('3.2.2.4 Test case c14n11/xmlspace-4'):-
        ignore_space_error(c14n_test('testdata/xmlspace-input.xml', (e11 ; e12), 'testdata/xmlspace-4-exc.output')).

test('3.2.3.1 Test case c14n11/xmlid-1'):-
        c14n_test('testdata/xmlid-input.xml', e1, 'testdata/xmlid-1-exc.output').

test('3.2.3.2 Test case c14n11/xmlid-2'):-
        c14n_test('testdata/xmlid-input.xml', (e11 ; e12), 'testdata/xmlid-2-exc.output').

test('3.2.4.1,1 Test case c14n11/xmlbase-prop-1'):-
        c14n_test('testdata/xmlbase-prop-input.xml', (c14n11XmlBaseDoc1, \+e2), 'testdata/xmlbase-prop-1-exc.output').

test('3.2.4.1.2 Test case c14n11/xmlbase-prop-2'):-
        c14n_test('testdata/xmlbase-prop-input.xml', e1, 'testdata/xmlbase-prop-2-exc.output').

test('3.2.4.1.3 Test case c14n11/xmlbase-prop-3'):-
        c14n_test('testdata/xmlbase-prop-input.xml', e11, 'testdata/xmlbase-prop-3-exc.output').

test('3.2.4.1.4 Test case c14n11/xmlbase-prop-4'):-
        c14n_test('testdata/xmlbase-prop-input.xml', e111, 'testdata/xmlbase-prop-4-exc.output').

test('3.2.4.1.5 Test case c14n11/xmlbase-prop-5'):-
        c14n_test('testdata/xmlbase-prop-input.xml', e21, 'testdata/xmlbase-prop-5-exc.output').

test('3.2.4.1.6 Test case c14n11/xmlbase-prop-6'):-
        c14n_test('testdata/xmlbase-prop-input.xml', e3, 'testdata/xmlbase-prop-6-exc.output').

test('3.2.4.1.7 Test case c14n11/xmlbase-prop-7'):-
        c14n_test('testdata/xmlbase-prop-input.xml', (c14n11XmlBaseDoc1, \+(e1 ; e2)), 'testdata/xmlbase-prop-7-exc.output').

test('3.2.4.2.1 Test case c14n11/xmlbase-c14n11spec-102', [blocked('Cannot express [self::ietf:e1 or (parent::ietf:e1 and not(self::text() or self::e2)) or count(id("E3")|ancestor-or-self::node()) = count(ancestor-or-self::node())] using builtin xpath')]):-
        c14n_test('testdata/xmlbase-c14n11spec-input.xml', unknown, 'xmlbase-c14n11spec-102.output').

test('3.2.4.2.2 Test case c14n11/xmlbase-c14n11spec-102', [blocked('Cannot express [self::ietf:e1 or (parent::ietf:e1 and not(self::text() or self::e2)) or count(id("E3")|ancestor-or-self::node()) = count(ancestor-or-self::node())] using builtin xpath')]):-
        c14n_test('testdata/xmlbase-c14n11spec2-input.xml', unknown, 'xmlbase-c14n11spec2-102.output').

test('3.2.4.2.3 Test case c14n11/xmlbase-c14n11spec-102', [blocked('Cannot express [self::a or ancestor-or-self::d] using builtin xpath')]):-
        c14n_test('testdata/xmlbase-c14n11spec3-input.xml', unknown, 'xmlbase-c14n11spec3-102.output').


:-end_tests(c14n).
