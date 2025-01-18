# Qafny-PY main
# -*- coding: UTF-8 -*-

import argparse  # usage: parsing cli arguments, printing help
import os  # usage: getting relative paths and directoy names
from colored import stylize, fore # console ui styling

from antlr4 import FileStream, CommonTokenStream  # usage: reading in a file and creating a token stream
from ExpLexer import ExpLexer  # usage: lexing the file stream
from ExpParser import ExpParser  # usage: parsing the token stream
from ProgramTransformer import ProgramTransformer # usage: transforming antlr ast into the qafny one
from CollectKind import CollectKind # usage: collecting the kind environment from the qafny AST (see the README for a breakdown of kinds vs. types)
from TypeCollector import TypeCollector # usage: collecting the type environment from the qafny AST (see the README for a breakdown of types vs. kinds)
from TypeChecker import TypeChecker # usage: type checking the parsed file
from ProgramTransfer import ProgramTransfer # usage: transforming the qafny ast into a dafny one
from PrinterVisitor import PrinterVisitor # usage: outputting string text dafny code from a dafny (TargetProgrammer) AST

#######################################
# Qafny Options (a.k.a. Defines)
#######################################

## Functions required for the Qafny Options

# returns a path transformed to be relative to this script file as opposed to the current working directory 
def path_relative_to_self(path: str) -> str:
    return os.path.relpath(path, start=os.path.dirname(__file__))

# returns a path following the format "../../test/Qafny/<name.qfy>"
def example_program(filename: str) -> str:
    return path_relative_to_self(f"../../test/Qafny/{filename}.qfy")

# The suite of test qafny files (Qafny defaults to verifying these)
DEFAULT_FILENAMES = [
    example_program("test1"),
    example_program("test2"),
    example_program("test3"),
    example_program("test4"),
    example_program("test5"),
    example_program("test6"),
    example_program("test7"),
    example_program("BellPair"),
    example_program("GHZ"),
    example_program("Teleportation"),
    example_program("Superdense"),
    example_program("Shors"),
    example_program("DeutschJozsa"),
    example_program("simon"),
    example_program("DiscreteLog"),
    example_program("Grovers"),
    example_program("DiscreteLog"),
    example_program("QPE"),
    example_program("SWAPTest")
]

#######################################
# Helper Functions
#######################################

def show_step_status(filename: str, description: str, is_success: bool):
    """show_step_status shows a status message to the user about the current file"""
    human_readable_filename = os.path.basename(filename)
    blue_hr_filename = stylize(f'"{human_readable_filename}"', fore('blue'))
    print(f"{description} {blue_hr_filename}: ", end='')
    if is_success:
        print(stylize("✓ (pass)", fore('green')))
    else:
        print(stylize("🞫 (fail)", fore('red')))

#######################################
# Main Routine
#######################################

# standard top-level run check
# Qafny usage:
# qafny BellPair.qfy
# running without arguments will cause it to loop through a list of test files
#? Do we want to make the cli similar to dafny?
#? i.e. qafny verify <file> for verification
#?      qafny build <file> for compilation (i.e. to QASM)
if __name__ == "__main__":
    # extremely simple argument parsing (see above usage)
    cli_parser = argparse.ArgumentParser(prog="Qafny", description="A Quantum Program Verifier")
    cli_parser.add_argument('filename', nargs='?', default=DEFAULT_FILENAMES, help="The location of the qafny file to verify.")
    args = cli_parser.parse_args()
    
    # if the user provided a filename, it's not going to be an array
    # so we have to convert it to one
    if not isinstance(args.filename, list):
        args.filename = [args.filename]

    # loop through each file in args.filename
    for filename in args.filename:
        # filename w/o the folders
        human_readable_filename = os.path.basename(filename)
        blue_hr_filename = stylize(f'"{human_readable_filename}"', fore('blue'))
        print(f"Verifying: {blue_hr_filename}")
        # create a file stream
        file_stream = FileStream(filename, encoding="utf-8")
        # scan (lex the file)
        lexer = ExpLexer(file_stream)
        token_stream = CommonTokenStream(lexer)
        # parse
        parser = ExpParser(token_stream)
        # abstract syntax tree
        ast = parser.program() # program is the root node in the tree
        if parser.getNumberOfSyntaxErrors() > 0:
            print(f"Failed to parse: {filename}")
        else:
            # Transform ANTLR AST to Qafny AST
            transformer = ProgramTransformer()
            qafny_ast = transformer.visit(ast)

            # Collect the types + kinds in the AST
            collect_kind = CollectKind()
            collect_kind.visit(qafny_ast)
            # QXBind can have a none type
            print(collect_kind.get_kenv())

            type_collector = TypeCollector(collect_kind.get_kenv())
            type_collector.visit(qafny_ast)

            # print(f"{type(collect_kind.get_kenv())}, {type(type_collector.get_tenv())}")
            # print(type_collector.get_tenv())

            # Type-check (for each method in the ast)
            types_correct = True
            for method in qafny_ast.method():
                modified_tenv = [(x[0], x[1], 0) for x in type_collector.get_tenv(method.ID())]
                print(modified_tenv)
                type_checker = TypeChecker(collect_kind.get_kenv(), modified_tenv, 0)
                types_correct = types_correct and type_checker.visit(qafny_ast)

            show_step_status(filename, "Type-check", types_correct)

            # Convert to Dafny AST
            dafny_transfer = ProgramTransfer(collect_kind.get_kenv(), type_collector.get_env())
            dafny_ast = dafny_transfer.visit(qafny_ast)

            # Print Dafny AST
            target_printer_visitor = PrinterVisitor()
            dafny_code = target_printer_visitor.visit(dafny_ast)

            print(f"Dafny:\n{dafny_code}")

            show_step_status(filename, "Verify", False)
            print("") # newline break

