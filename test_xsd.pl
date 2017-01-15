:- module(test_xsd, [test_xsd/0]).

user:file_search_path(foreign, '.').
user:file_search_path(library, '.').
user:file_search_path(library, '../plunit').

:- use_module(library(sgml)).
:- use_module(library(debug)).
:- use_module(library(plunit)).

test_xsd :-
    run_tests([ xsd_number_string,
                xsd_time_string
              ]).

expand_xsd(XSD:Local, Out) :-
    XSD == xsd,
    !,
    atom_concat('http://www.w3.org/2001/XMLSchema#', Local, Out).
expand_xsd(In, Out) :-
    compound(In),
    !,
    compound_name_arguments(In, Name, Args0),
    maplist(expand_xsd, Args0, Args),
    compound_name_arguments(Out, Name, Args).
expand_xsd(T, T).

term_expansion(In, Out) :-
    expand_xsd(In, Out).

:- begin_tests(xsd_number_string).

test(string_0, N == 0.0) :-
    xsd_number_string(N, "0.0E0").
test(string_0, N == -0.0) :-
    xsd_number_string(N, "-0.0E0").
test(string_nan, N == NaN) :-
    xsd_number_string(N, "NaN"),
    NaN is nan.
test(string_inf, N =:= inf) :-
    xsd_number_string(N, "INF").
test(string_neginf, N =:= -inf) :-
    xsd_number_string(N, "-INF").
test(nan_string, S == "NaN") :-
    NaN is nan,
    xsd_number_string(NaN, S).
test(nan_inf, S == "INF") :-
    Inf is inf,
    xsd_number_string(Inf, S).
test(nan_inf, S == "-INF") :-
    Inf is -inf,
    xsd_number_string(Inf, S).
test(float, F == 110.0) :-
    xsd_number_string(F, "1.1e2").

:- end_tests(xsd_number_string).

:- begin_tests(xsd_time_string).

% Term --> String

test(time_string, S == "2016-01-30") :-
    xsd_time_string(date(2016,01,30), Y, S),
    assertion(Y == xsd:date).
test(time_string, S == "-0400-01-30") :-
    xsd_time_string(date(-400,01,30), Y, S),
    assertion(Y == xsd:date).
test(time_string, S == "2016-01-30T20:29:15") :-
    xsd_time_string(date_time(2016,01,30,20,29,15), Y, S),
    assertion(Y == xsd:dateTime).
test(time_string, S == "2016-01-30T20:29:15.25") :-
    xsd_time_string(date_time(2016,01,30,20,29,15.25), Y, S),
    assertion(Y == xsd:dateTime).
test(time_string, S == "2016-01-30T20:29:15Z") :-
    xsd_time_string(date_time(2016,01,30,20,29,15,0), Y, S),
    assertion(Y == xsd:dateTime).
test(time_string, S == "2016-01-30T20:29:05.456Z") :-
    xsd_time_string(date_time(2016,01,30,20,29,5.456,0), Y, S),
    assertion(Y == xsd:dateTime).
test(time_string, S == "2016-01-30T20:29:15+01:00") :-
    xsd_time_string(date_time(2016,01,30,20,29,15,3600), Y, S),
    assertion(Y == xsd:dateTime).
test(time_string, S == "2016-01-30T20:29:15.56+01:00") :-
    xsd_time_string(date_time(2016,01,30,20,29,15.56,3600), Y, S),
    assertion(Y == xsd:dateTime).
test(time_string, S == "20:29:15") :-
    xsd_time_string(time(20,29,15), Y, S),
    assertion(Y == xsd:time).
test(time_string, S == "20:29:15.75") :-
    xsd_time_string(time(20,29,15.75), Y, S),
    assertion(Y == xsd:time).
test(time_string, S == "2016-01") :-
    xsd_time_string(year_month(2016,1), Y, S),
    assertion(Y == xsd:gYearMonth).
test(time_string, S == "01-30") :-
    xsd_time_string(month_day(1,30), Y, S),
    assertion(Y == xsd:gMonthDay).
test(time_string, S == "2016") :-
    xsd_time_string(2016, xsd:gYear, S).
test(time_string, S == "-0399") :-
    xsd_time_string(-400, xsd:gYear, S).
test(time_string, S == "01") :-
    xsd_time_string(1, xsd:gMonth, S).
test(time_string, S == "30") :-
    xsd_time_string(30, xsd:gDay, S).

% Term <- String

test(string_time, T = date(2016,01,30)) :-
    xsd_time_string(T, Y, "2016-01-30"),
    assertion(Y == xsd:date).
test(string_time, T = date(-2017,01,30)) :-
    xsd_time_string(T, Y, "-2016-01-30"),
    assertion(Y == xsd:date).
test(string_time, T = date_time(2016,01,30,20,29,15)) :-
    xsd_time_string(T, Y, "2016-01-30T20:29:15"),
    assertion(Y == xsd:dateTime).
test(string_time, T = date_time(2016,01,30,20,29,15.25)) :-
    xsd_time_string(T, Y, "2016-01-30T20:29:15.25"),
    assertion(Y == xsd:dateTime).
test(string_time, T = date_time(2016,01,30,24,0,0)) :-
    xsd_time_string(T, Y, "2016-01-30T24:00:00"),
    assertion(Y == xsd:dateTime).
test(string_time, T = date_time(2016,01,30,20,29,15,0)) :-
    xsd_time_string(T, Y, "2016-01-30T20:29:15Z"),
    assertion(Y == xsd:dateTime).
test(string_time, T = date_time(2016,01,30,20,29,15.10,0)) :-
    xsd_time_string(T, Y, "2016-01-30T20:29:15.10Z"),
    assertion(Y == xsd:dateTime).
test(string_time, T = date_time(2016,01,30,20,29,15,3600)) :-
    xsd_time_string(T, Y, "2016-01-30T20:29:15+01:00"),
    assertion(Y == xsd:dateTime).
test(string_time, T = date_time(2016,01,30,20,29,05.6,3600)) :-
    xsd_time_string(T, Y, "2016-01-30T20:29:05.6+01:00"),
    assertion(Y == xsd:dateTime).
test(string_time, T = date_time(2016,01,30,20,29,05.6,50400)) :-
    xsd_time_string(T, Y, "2016-01-30T20:29:05.6+14:00"),
    assertion(Y == xsd:dateTime).
test(string_time, T = time(20,29,15)) :-
    xsd_time_string(T, Y, "20:29:15"),
    assertion(Y == xsd:time).
test(string_time, T = time(20,29,15.75)) :-
    xsd_time_string(T, Y, "20:29:15.75"),
    assertion(Y == xsd:time).
test(string_time, T = year_month(2016,1)) :-
    xsd_time_string(T, Y, "2016-01"),
    assertion(Y == xsd:gYearMonth).
test(string_time, T = month_day(1,30)) :-
    xsd_time_string(T, Y, "01-30"),
    assertion(Y == xsd:gMonthDay).
test(string_time, T = 2016) :-
    xsd_time_string(T, Y, "2016"),
    assertion(Y == xsd:gYear).
test(string_time, T = -2017) :-
    xsd_time_string(T, Y, "-2016"),
    assertion(Y == xsd:gYear).
test(string_time, T = 1) :-
    xsd_time_string(T, xsd:gMonth, "01").
test(string_time, T = 30) :-
    xsd_time_string(T, xsd:gDay, "30").

% Parsing errors

test(string_error, error(syntax_error(xsd_time))) :-
    xsd_time_string(_, _, "").
test(string_error, error(domain_error(day,35))) :-
    xsd_time_string(_, _, "2016-01-35").
test(string_error, error(domain_error(second,65))) :-
    xsd_time_string(_, _, "20:29:65").
test(string_time, error(domain_error(day,35))) :-
    xsd_time_string(_, xsd:gDay, "35").
test(string_time, error(domain_error(month,13))) :-
    xsd_time_string(_, xsd:gMonth, "13").
test(string_time, error(instantiation_error)) :-
    xsd_time_string(_, _, "13").

% Generate errors

test(term_error, error(domain_error(day,35))) :-
    xsd_time_string(date(2016,01,35), _, _).
test(term_error, error(domain_error(day,35))) :-
    xsd_time_string(date_time(2016,01,35,20,29,15), _, _).
test(term_error, error(domain_error(hour,25))) :-
    xsd_time_string(date_time(2016,01,30,25,29,15), _, _).
test(term_error, error(domain_error(hour,25))) :-
    xsd_time_string(date_time(2016,01,30,25,29,15,0), _, _).
test(term_error, error(domain_error(tz_offset,100000))) :-
    xsd_time_string(date_time(2016,01,30,20,29,15,100000), _, _).
test(term_error, error(domain_error(year,10000))) :-
    xsd_time_string(10000, xsd:gYear, _).
test(term_error, error(domain_error(month,20))) :-
    xsd_time_string(20, xsd:gMonth, _).
test(term_error, error(domain_error(day,35))) :-
    xsd_time_string(35, xsd:gDay, _).
test(term_error, error(domain_error(day,-1))) :-
    xsd_time_string(-1, xsd:gDay, _).
test(term_error, error(domain_error(year,0))) :-
    xsd_time_string(0, xsd:gYear, _).

:- end_tests(xsd_time_string).
