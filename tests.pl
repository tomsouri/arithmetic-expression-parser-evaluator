:- use_module(main).



:- begin_tests(partially_succeed).
    test("integer_parsing") :- test_evaluation(
        [
            "jedna"-1,
            "nula"-0,
            "patnact"-15,
            "deset"-10,
            "sto dvacet pet"-125,
            "tri tisice"-3000,
            "sedesat pet tisic osm set devadesat ctyri"-65894,
            "devět set osmdesát sedm bilionů šest set padesát sedm miliard dvě stě třicet čtyři milionů pět set třicet dva tisíc dvě stě třicet čtyři"-987657234532234
        ]
        ).

    test("eval") :- test_evaluation(
       [
        "jedna plus dva krát tři minus závorka sedm minus čtyři konec závorky"-4,
        "třináct krát tři"-39,
        "třináct krát čtrnáct minus patnáct minus šestnáct minus sedm krát dva"-137,
        "jedna minus dva minus tri minus ctyri minus pet"-(-13),
        "tricet jedna plus tri krat zavorka devět set osmdesát sedm bilionů šest set padesát sedm miliard dvě stě třicet čtyři milionů pět set třicet dva tisíc dvě stě třicet čtyři minus dve ste padesat devet milionu pet konec zavorky deleno peti"-592594185319368.4,
        "patnact deleno tremi"-5,
        "osm set triliard deleno dvema sty padesati biliardami tremi sty osmdesati sedmi biliony devatenacti miliardami"-3195053.813872036
        

       ]
        ).

%    test("integer") :- 
:- end_tests(partially_succeed).

test_evaluation([]).
test_evaluation([Input-Out|Rest]) :- 
            evaluateExpressionString(Input, Output), 
            Output is Out,
            test_evaluation(Rest).

test:-run_tests.
t:-run_tests.


% "třicet dva tisíc dvě stě třicet čtyři minus zavorka dva minus tri konec zavorky"

% "devět set osmdesát sedm bilionů šest set padesát sedm miliard dvě stě třicet čtyři milionů pět set třicet dva tisíc dvě stě třicet čtyři plus dva"