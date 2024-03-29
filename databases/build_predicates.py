#!/usr/bin/env python3.9

"""
Completely build the `rules.pl` file from the *.txt files in the databases directory.
"""



separator = " "

names = ["digits", "teens", "tens", "hundreds", "identifiers", "operators", "parentheses"]
filenames = [f"{name}.txt" for name in names]

predicates = ["isDigit", "isTeen", "isTen", "isHundred", "isThousandOrHigherIdentifier", "isOperator", "isParenthesis"]

output_filename = "../rules.pl"


LETTERS_DIA = "áčďéěíňóřšťúůýž"

def remove_dia(diacritised):
    """Removes diacritics."""

    LETTERS_NODIA = "acdeeinorstuuyz"

    # A translation table usable with `str.translate` to rewrite characters with dia to the ones without them.
    DIA_TO_NODIA = str.maketrans(LETTERS_DIA + LETTERS_DIA.upper(), LETTERS_NODIA + LETTERS_NODIA.upper())

    return diacritised.translate(DIA_TO_NODIA)

def is_diacritised(list_of_strings):
    for ch in LETTERS_DIA:
        for token in list_of_strings:
            if ch in token:
                return True
    return False

def quotate(input):
    return f"\"{input}\""

def list_to_string(inputs):
    return "[" + ", ".join(quotate(input) for input in inputs) + "]"


def inflect(inputs):
    return [inputs]
"""
from sklonuj_cz import SklonujCzModel
inflectionModel = SklonujCzModel()
def inflect(inputs):
    print(inputs)
    input()
    inflected_inputs = [inflectionModel.inflect(input) for input in inputs]

    print(inflected_inputs)
    input()
    
    joined = [[inf_inp[i] for inf_inp in inflected_inputs] for i in range(len(inflected_inputs[0]))]

    print(joined)
    input()

    selected = [joined[7-1], joined[14-1]]

    print(selected)
    input()

    cleared = [inp_list for inp_list in joined if "?" not in inp_list in selected]
    print(cleared)
    input()
    return [inputs]
"""
def main():
    with open(output_filename, "w") as out:
        out.write(":- module(rules, ["+ ', '.join(f"{predicate}/2" for predicate in predicates) +"]).")
        out.write("\n\n")
        out.write("% All these rules are automatically generated by the python script\n% `build_predicates.py` from the `databases` directory.\n% It is based on the data stored in the *.txt files in the `databases` directory.")
        out.write("\n\n")

        for (file,pred) in zip (filenames, predicates):
            out.write(f"% source file: {file}")
            out.write("\n")
            with open(file, "r") as f:
                for line in f:
                    line = line.rstrip()
                    if line != "":
                        toks = line.split(separator)
                        inputs = toks[1:]

                        inflected_inputs = inflect(inputs)

                        quotated_inputs = [[f"{inflected_input[i]}" for i in range(len(inputs))] for inflected_input in inflected_inputs]
                        string_inputs = [list_to_string(quotated_input) for quotated_input in quotated_inputs]
                        possible_input_strings = string_inputs

                        to_be_appended = []
                        # if needed, add no-diacritised variant
                        for poss_input_string in possible_input_strings:    
                            if is_diacritised(poss_input_string):
                                inputs_nodia = remove_dia(poss_input_string)
                                to_be_appended.append(inputs_nodia)

                        for inputs_nodia in to_be_appended:
                            possible_input_strings.append(inputs_nodia)

                        # print all variants:
                        for poss_input_string in possible_input_strings:
                            out.write(f"{pred}({poss_input_string}, {toks[0]}) :- !.")
                            out.write("\n")



            out.write("\n\n")

if __name__ == "__main__":
    main()