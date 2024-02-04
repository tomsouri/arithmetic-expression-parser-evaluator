:- module(process_expression, [processExpression/2]).

:- use_module(rules).
:- use_module(process_integer).

% processExpression(+Unprocessed, -Processed) :- Unprocessed is the string
%       containing the expression written in words, Processed is the list containing
%       its number and operator variants.
processExpression(Unprocessed, Processed) :-    
    split_string(Unprocessed, " ", "", Tokens),
    processExpressionList(Tokens,Processed).

% processExpressionList(+Tokens, -Processed) :- from list of word tokens (Tokens)
%       creates a list of numbers and operators.
processExpressionList([],[]).
processExpressionList(Tokens, [ProcessedToken|ProcessedRest]) :- 
        length(Tokens, N),
        between(1,N,M),
        K is N+1-M,
        length(Unit, K),
        % split the tokens list, but start from longer token counts in the unit
        % (because some parts of an integer unit can be valid itself but we do
        % not want to get an invalid expression):
        % For example, expression 
        % `tricet dva krat padesat sest`, 
        % which is correctly recognized as 32 * 56, could be represented also
        % as 30 2 * 50 6.
         
        append(Unit, Rest, Tokens),
        processUnit(Unit,ProcessedToken),

        % Cut. Stop trying to find a shorter sequence as a valid unit (for the
        % reason see the comment above).
        !,
        processExpressionList(Rest,ProcessedRest),
        !.

% processUnit(+Unit, -Result) :- Unit is a list containing the string tokens of
%       one unit (token representing an operator, such as `plus` etc., token(s)
%       representing parentheses, such as `zavorka` or `konec zavorky` or multiple
%       tokens representing an integer).
%
%       For example, expression
%       `jedna plus dve ste padesat sest`
%       contains 3 tokens.
processUnit(OperatorUnit, Processed)    :- isOperator(OperatorUnit, Processed).
processUnit(ParenthesisUnit, Processed) :- isParenthesis(ParenthesisUnit, Processed).
processUnit(IntegerUnit, Processed)     :- processInteger(IntegerUnit, Processed).
