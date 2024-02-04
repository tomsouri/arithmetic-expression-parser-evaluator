:- module(main, [evaluateExpressionString/2]).

:- use_module(process_expression).
:- use_module(eval_expr_list).

% evaluateExpressionString(+Input, -Output) :- Input is a string containing an
%       expression written in words, Output is tha value of the expression.
evaluateExpressionString(Input,Output) :- 
    processExpression(Input, OutList),
    evalExprList(OutList,Output),
    prettyPrint(Input,OutList, Output).           

% prettyPrint(+Input, +List, +Output) :- Input is the input string containing the
%       expression, List is the list of number/operator tokens and Output is the
%       value of the expression.
prettyPrint(Input,List, Output) :-    
                        nl,
                        write("Input string: "),
                        write(Input),
                        nl,
                        write("Expression:   "),
                        atomics_to_string(List," ", Out),
                        write(Out),
                        nl,
                        write("Result:       "),
                        write(Output),
                        nl,
                        nl.