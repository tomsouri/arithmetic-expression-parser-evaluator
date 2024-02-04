:- module(process_integer, [processInteger/2]).

:- use_module(rules).

% processInteger(+Tokens, -Value) :- Tokens is the list of words tokens
%               representing an integer, Value is its value. The whole
%               processing consists of a decomposition to smaller and smaller
%               pieces:
%               1) The integer unit (the whole list of words representing an
%               integer) is reversed, to allow easier processing of components
%               that usually end with their identifier (where by 'component' we
%               mean a part ending with 'tisice', 'milionu' etc., e.i. 'sto
%               padesat sest tisic').
%               2) The element is extracted from the beginning of the reversed
%               list. By `element` we mean a part containing `hundred` and/or
%               `tens` and/or `digits` and/or `teens`. It has to be extracted
%               separately from the beginning of the list, because after its
%               removal the list contains only whole components with their
%               identifier.
%               3) The rest of the reversed list (which consists of components
%               only - or can be empty of course) is processed by an internal
%               predicate.

processInteger(UnprocessedList, Value) :-
        reverse(UnprocessedList, ReversedUnprocList),
        append(Element, Rest, ReversedUnprocList),
        processElement(Element, ElemValue),
        processIntegerInternal(Rest, ElemValue, Value),
        !.


% processIntegerInternal(+List, +Acc, -Value) :- List a reversed list with removed
%               `element` part from the beginning and therefore consists only
%               of components (beginning with their identifier).
%               Acc is the accumulator where the value of all components that
%               have been already processed is summed.
%               Value is the Value of the whole processed integer.
%               We start by extracting a non-empty part representing a
%               component, processing the component (and getting its value),
%               adding the value to the accumulator and calling self
%               recursively with the rest.

processIntegerInternal([], Value, Value) :- !.
processIntegerInternal(IntegerList, Accumulator, Value) :- 
        list_non_empty(Component),
        append(Component, Rest, IntegerList),
        processComponent(Component, CompValue),
        NewAccumulator is Accumulator + CompValue,
        processIntegerInternal(Rest, NewAccumulator, Value),
        !.


% processComponent(+[First|Rest], -Value) :- First is the component identifier
%               (e.g. 'tisic' or 'milion' etc.), Rest is a List containing the
%               element part of the component, Value is the value of the
%               component.
%               The value of the component identifier is multiplied by the
%               value of the element part (e.i., for 'tri sta padesat set
%               tisic' the value of component ID is 1000 and is multiplied by
%               356 as the value of the element part to result in 356000 as a
%               value of the whole component).
processComponent([First|Rest], Value) :-
        isThousandOrHigherIdentifier([First], IDValue),
        processComponentPart(Rest, PartValue),
        Value is IDValue * PartValue,
        !.


% processComponentPart(+List, -Value) :- List contains the element part of a
%               component, Value represents the element part value.
%               If the list is empty, the Value is 1, because in Czech we can
%               ommit the element part and it means that it is equal to 1 (e.i.
%               'tisic' == 'jeden tisic').
processComponentPart([], 1) :- !.
processComponentPart(Element, Value) :- 
        list_non_empty(Element), 
        processElement(Element,Value),
        !.


% processElement(+List, -Value) :- the List consists of tokens representing an
%               element, Value is its value.
%               Non-existent element (empty) has zero value.
%               We split the element list to a TenPart (possibly empty, e.g.
%               'dvacet pet' or 'patnact' or 'ctyricet' or 'sest') and a
%               HundredPart (possibly empty, e.g. 'sto' or 'tri sta'), getting
%               their values and adding them to obtain the value of the element.
processElement([],0) :- !.
processElement(Element, Value) :-
        append(TenPart, Hundreds, Element),
        processTenPart(TenPart, TenValue),
        processHundreds(Hundreds, HundredValue),
        Value is TenValue + HundredValue,
        !.


% processTenPart(+List, -Value) :- The List can either be a representation of a
%               `teen` value ('sestnact', 'deset', 'ctrnact') or a composition
%               of a `ten` value ('dvacet', 'padesat') and a `digit` value
%               ('jedna', 'sedm').
processTenPart(StringListReversed, Value) :- isTeen(StringListReversed, Value), !.
processTenPart(TenPartReversed, Value) :- 
        append(Digits, Tens, TenPartReversed),
        processDigits(Digits, DigValue),
        processTens(Tens, TenValue),
        Value is DigValue + TenValue,
        !.

% processDigits(+List, -Value) :- List consist of a `digit` identifier, Value is
%               its value. The List can be empty (e.g. in case of 'sto dvacet')
%               with the `digits` value equal to 0, or non-empty with a given
%               value. 
processDigits([],0).
processDigits(StringListReversed, Value) :- isDigit(StringListReversed, Value).

% processTens(+List, -Value) :- the List consists of a `tens` identifier, Value
%               is its value. The List can be empty (e.g. in case of 'sto pet')
%               with the `tens` value equal to 0, or non-empty with a given
%               value.
processTens([],0).
processTens(StringListReversed, Value) :- isTen(StringListReversed, Value).


% processHundreds(+List, -Value) :-
%       List is a reversed list of strings representing a hundred number, Value
%       is its value.
processHundreds([],0).
processHundreds(StringListReversed, Value) :- 
        reverse(StringListReversed, StringList),
        isHundred(StringList, Value).

% list_non_empty(?List) :- True is List is not empty.
list_non_empty([_|_]).
