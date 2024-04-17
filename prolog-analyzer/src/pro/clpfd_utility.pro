:- module(clpfd_utility, [build_term/3]).

:- use_module(library(clpfd)).
:- reexport([library(clpfd)]).

%!
build_term(OpStr, ArgStrs, Term) :-
    atomics_to_string(ArgStrs, ", ", ArgsInnerStr),
    atomics_to_string(["[", ArgsInnerStr, "]"], "", ArgsListStr),
    term_string(ArgsList, ArgsListStr),
    atom_string(OpAtom, OpStr),
    get_dict(
        OpAtom,
        _{
            '#=': #=,
            '#\\=': #\=,
            '#<': #<,
            '#=<': #=<,
            '#>': #>,
            '#>=': #>=
        },
        Op
    ),
    Term =.. [Op|ArgsList].