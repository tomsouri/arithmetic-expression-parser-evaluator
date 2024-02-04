:- module(eval_expr_list, [evalExprList/2, expr/3]).

% Evaluate expression in list

% evalExprList(+ExpressionList, -Value) :- ExpressionList is a list
%       containing the tokens of expression, Value is the result of its
%       evaluation.
%       Allowed operators are +, - (binary), *, /.
%       Allowed parentheses are (, ).
%       Allowed values are integers.
%       
%       Example: 
%       evalExprList([100,-,2,-,3,+,2,*,3,*,'(',4,+,5,')'], Val).
%       Val = 149;
%       false.

evalExprList(ExpressionList, Value) :- expr(Value, ExpressionList, []).

% allow left recursion:
:- table expr/3, term/3.

% A Prolog DCG grammar for parsing arithmetic expressions with parentheses and
% left-asociative operators.
% Heavily based on the source:
% https://www3.cs.stonybrook.edu/~warren/xsbbook/node24.html

expr(Val) --> expr(Eval), [+], term(Tval), {Val is Eval+Tval}.
expr(Val) --> expr(Eval), [-], term(Tval), {Val is Eval-Tval}.
expr(Val) --> term(Val).
term(Val) --> term(Tval), [*], primary(Fval), {Val is Tval*Fval}.
term(Val) --> term(Tval), [/], primary(Fval), {Val is Tval/Fval}.
term(Val) --> primary(Val).
primary(Val) --> ['('], expr(Val), [')'].
primary(Int) --> [Int], {integer(Int)}.