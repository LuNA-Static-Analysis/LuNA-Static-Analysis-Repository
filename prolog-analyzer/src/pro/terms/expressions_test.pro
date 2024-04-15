:- begin_tests(expressions).

:- use_module('src/pro/terms/expressions.pro', [
    expression_dict/2
]).

% swipl -g run_tests -t halt .\src\pro\terms\expressions_test.pro

test(expression_dict) :-
    expression_dict(1, D),
    D = _{
        'type': "const",
        'value': 1
    }.
test(expression_dict) :-
    expression_dict(E, _{
        'type': "const",
        'value': 1
    }),
    E = 1.

test(expression_dict) :-
    expression_dict(-3, D),
    D = _{
        'type': "const",
        'value': -3
    }.
test(expression_dict) :-
    expression_dict(E, _{
        'type': "const",
        'value': -3
    }),
    E = -3.

test(expression_dict) :-
    expression_dict("X", D),
    D = _{
        'type': "ref",
        'name': "X"
    }.
test(expression_dict) :-
    expression_dict(E, _{
        'type': "ref",
        'name': "X"
    }),
    E = "X".

test(expression_dict) :-
    expression_dict(luna_ref(["X", 7, "Y"]), D),
    D = _{
        'type': "ref",
        'name': "X",
        'indices': [
            _{'type': "const", 'value': 7},
            _{'type': "ref", 'name': "Y"}
        ]
    }.
test(expression_dict) :-
    expression_dict(E, _{
        'type': "ref",
        'name': "X",
        'indices': [
            _{'type': "const", 'value': 7},
            _{'type': "ref", 'name': "Y"}
        ]
    }),
    E = luna_ref(["X", 7, "Y"]).

:- end_tests(expressions).
