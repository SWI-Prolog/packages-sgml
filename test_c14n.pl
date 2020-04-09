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
    source_file(test_c14n, MyFile),
    file_directory_name(MyFile, MyDir),
    atomic_list_concat([MyDir, File], /, Abs).

%!  c14n_test(+InputFile, +XPath, +TargetFile).
%
%   Canonicalize the document obtained by applying the XPath expression to
%   the document specified by InputFile and confirm it matches exactly the
%   bytes in TargetFile.

c14n_test(InputFile, XPath, TargetFile):-
        xml_file(InputFile, InputFilename),
        xml_file(TargetFile, TargetFilename),
        setup_call_cleanup(open(InputFilename, read, InputStream),
                           load_structure(InputStream, InputDocument, [dialect(xmlns), space(preserve), keep_prefix(true)]),
                           close(InputStream)),
        setup_call_cleanup(open(TargetFilename, read, TargetStream),
                           read_string(TargetStream, _, TargetDocument),
                           close(TargetStream)),
        findall(SubDocument,
                xpath(InputDocument, XPath, SubDocument),
                SubDocuments),
        with_output_to(string(GeneratedDocument),
                       forall(member(SubDocument, SubDocuments),
                              xml_write_canonical(current_output, SubDocument, [method('http://www.w3.org/2001/10/xml-exc-c14n#')]))),
        TargetDocument == GeneratedDocument.

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
These tests are derived from the w3c tests available at
https://www.w3.org/TR/xmldsig2ed-tests/#TestCases-C14n11
The input/target documents are stored in testdata/
The xpath expressions are encoded inline in the tests. This is because the
SWI-Prolog implementation of xpath is not syntax-compatible with the w3c one
The test names include the section of the source document they pertain to
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

test('3.2.1.1 Test case c14n11/xmllang-1'):-
        c14n_test('testdata/xmllang-input.xml', //(ns(ietf, 'http://www.ietf.org'):e1), 'xmllang-1.output').

test('3.2.1.2 Test case c14n11/xmllang-2'):-
        c14n_test('testdata/xmllang-input.xml', //(ns(ietf, 'http://www.ietf.org'):e2), 'xmllang-2.output').

test('3.2.1.3 Test case c14n11/xmllang-3'):-
        c14n_test('testdata/xmllang-input.xml', //(ns(ietf, 'http://www.ietf.org'):e11), 'xmllang-3.output').

test('3.2.1.4 Test case c14n11/xmllang-4'):-
        % FIXME: How to encode (//. | //@* | //namespace::*) [ancestor-or-self::ietf:e11 or ancestor-or-self::ietf:e12]
        c14n_test('testdata/xmllang-input.xml', //(ns(ietf, 'http://www.ietf.org'):e1), 'xmllang-4.output').

test('3.2.2.1 Test case c14n11/xmlspace-1'):-
        c14n_test('testdata/xmlspace-input.xml', //(ns(ietf, 'http://www.ietf.org'):e1), 'xmlspace-1.output').

test('3.2.2.2 Test case c14n11/xmlspace-2'):-
        c14n_test('testdata/xmlspace-input.xml', //(ns(ietf, 'http://www.ietf.org'):e2), 'xmlspace-2.output').

test('3.2.2.3 Test case c14n11/xmlspace-3'):-
        c14n_test('testdata/xmlspace-input.xml', //(ns(ietf, 'http://www.ietf.org'):e11), 'xmlspace-3.output').

test('3.2.2.4 Test case c14n11/xmlspace-4'):-
        % FIXME: How to encode (//. | //@* | //namespace::*) [ancestor-or-self::ietf:e11 or ancestor-or-self::ietf:e12]
        c14n_test('testdata/xmlspace-input.xml', //(ns(ietf, 'http://www.ietf.org'):e1), 'xmlspace-4.output').

test('3.2.3.1 Test case c14n11/xmlid-1'):-
        c14n_test('testdata/xmlid-input.xml', //(ns(ietf, 'http://www.ietf.org'):e1), 'xmlid-1.output').

test('3.2.3.2 Test case c14n11/xmlid-2'):-
        % FIXME: How to encode (//. | //@* | //namespace::*) [ancestor-or-self::ietf:e11 or ancestor-or-self::ietf:e12]
        c14n_test('testdata/xmlid-input.xml', //(ns(ietf, 'http://www.ietf.org'):e1), 'xmlid-2.output').

test('3.2.4.1,1 Test case c14n11/xmlbase-prop-1'):-
        % FIXME: How to encode (//. | //@* | //namespace::*) [ancestor-or-self::ietf:c14n11XmlBaseDoc1 and not(ancestor-or-self::ietf:e2)]
        c14n_test('testdata/xmlbase-prop-input.xml', //(ns(ietf, 'http://www.ietf.org'):e1), 'xmlbase-prop-1.output').

test('3.2.4.1.2 Test case c14n11/xmlbase-prop-2'):-
        c14n_test('testdata/xmlbase-prop-input.xml', //(ns(ietf, 'http://www.ietf.org'):e1), 'xmlbase-prop-2.output').

test('3.2.4.1.3 Test case c14n11/xmlbase-prop-3'):-
        c14n_test('testdata/xmlbase-prop-input.xml', //(ns(ietf, 'http://www.ietf.org'):e11), 'xmlbase-prop-3.output').

test('3.2.4.1.4 Test case c14n11/xmlbase-prop-4'):-
        c14n_test('testdata/xmlbase-prop-input.xml', //(ns(ietf, 'http://www.ietf.org'):e111), 'xmlbase-prop-4.output').

test('3.2.4.1.5 Test case c14n11/xmlbase-prop-5'):-
        c14n_test('testdata/xmlbase-prop-input.xml', //(ns(ietf, 'http://www.ietf.org'):e21), 'xmlbase-prop-5.output').

test('3.2.4.1.6 Test case c14n11/xmlbase-prop-6'):-
        c14n_test('testdata/xmlbase-prop-input.xml', //(ns(ietf, 'http://www.ietf.org'):e3), 'xmlbase-prop-6.output').

test('3.2.4.1.7 Test case c14n11/xmlbase-prop-7'):-
        % FIXME: How to encode (//. | //@* | //namespace::*) [ancestor-or-self::ietf:c14n11XmlBaseDoc1 and not(ancestor-or-self::ietf:e1 or ancestor-or-self::ietf:e2)]
        c14n_test('testdata/xmlbase-prop-input.xml', //(ns(ietf, 'http://www.ietf.org'):e3), 'xmlbase-prop-7.output').

test('3.2.4.2.1 Test case c14n11/xmlbase-c14n11spec-102'):-
        % FIXME: (//. | //@* | //namespace::*)[self::ietf:e1 or (parent::ietf:e1 and not(self::text() or self::e2)) or count(id("E3")|ancestor-or-self::node()) = count(ancestor-or-self::node())]
        c14n_test('testdata/xmlbase-c14n11spec-input.xml', //(ns(ietf, 'http://www.ietf.org'):e3), 'xmlbase-c14n11spec-102.output').

test('3.2.4.2.2 Test case c14n11/xmlbase-c14n11spec-102'):-
        % FIXME: (//. | //@* | //namespace::*)[self::ietf:e1 or (parent::ietf:e1 and not(self::text() or self::e2)) or count(id("E3")|ancestor-or-self::node()) = count(ancestor-or-self::node())]
        c14n_test('testdata/xmlbase-c14n11spec2-input.xml', //(ns(ietf, 'http://www.ietf.org'):e3), 'xmlbase-c14n11spec2-102.output').

test('3.2.4.2.3 Test case c14n11/xmlbase-c14n11spec-102'):-
        % FIXME: (//. | //@* | //namespace::*) [self::a or ancestor-or-self::d]
        c14n_test('testdata/xmlbase-c14n11spec3-input.xml', //(ns(ietf, 'http://www.ietf.org'):e3), 'xmlbase-c14n11spec3-102.output').


:-end_tests(c14n).