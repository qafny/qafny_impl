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
from DafnyLibrary import DafnyLibrary # usage: generating template library functions for verification
from CleanupVisitor import CleanupVisitor # usage: perforaming final cleanup operations before verifying such as convertiong x ^ y to powN(x, y)

import subprocess # usage: calling dafny to verify generated code

from error_reporter.CodeReport import CodeReport

#######################################
# Qafny Options (a.k.a. Defines)
#######################################

## Functions required for the Qafny Options

# returns a path transformed to be relative to this script file as opposed to the current working directory 
from pathlib import Path

def path_relative_to_self(path: str) -> str:
    """Return path relative to the current script (__file__)."""
    script_dir = Path(__file__).resolve().parent
    full_path = (script_dir / path).resolve()
    return str(full_path)

def example_program(filename: str) -> str:
    """Return full path to '../../test/Qafny/<filename>.qfy' relative to __main__.py."""
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
    example_program("test8"),
    example_program("test9"),
    example_program("test10"),
    example_program("test11"),
    example_program("test12"),
    example_program("test13"),
    example_program("test14"),
    example_program("BellPair"),
    example_program("GHZ"),
    example_program("Teleportation"),
    example_program("Superdense"),
    example_program("Shors"),
    example_program("DeutschJozsa"),
    example_program("simon"),
    example_program("DiscreteLog"),
    example_program("Grovers"),
    example_program("QPE"),
    example_program("SWAPTest"),
    example_program("AmpAmp"),
    example_program("AmplitudeEstimation"),
    example_program("AppxCounting"),
    example_program("BHSP"),
    example_program("FirstAmpEstimate"),
    example_program("FOQA"),
    example_program("LongDistanceEntangle"),
    example_program("NonBoolean"),
    example_program("QFTModQ"),
    example_program("SimpleAmpEstimate"),
    example_program("Stabilizer"),
    example_program("StateDistinguishing")
]

#######################################
# Helper Functions
#######################################
output_dir = "generated"
os.makedirs(output_dir, exist_ok=True)  # Ensure the directory exists

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
    cli_parser = argparse.ArgumentParser(prog="qafny", description="A Quantum Program Verifier")
    # the file to verify
    cli_parser.add_argument('filename', nargs='?', default=DEFAULT_FILENAMES, help="the location of the qafny file to verify. if not specified, qafny verifies all the files in DEFAULT_FILENAMES from __main__.py")
    # debug flag to print dafny code generated from the file
    cli_parser.add_argument('-d', '--print-dafny', action='store_true', help='print out the dafny code when verifying')
    # argument to write the generated dafny code to a file
    # the default behavior is to just pipe the code into dafny, but this argument will ensure run dafny on the output file
    cli_parser.add_argument('-o', '--output', nargs='?', const='', help='if specified, write the generated dafny code to the file specified by OUTPUT. if not provided, the name is based on the input filename')
    # skip the verification step (if you just want to generate dafny)
    cli_parser.add_argument('-x', '--skip-verify', action='store_true', help='don\'t verify the code in dafny, useful when developing')
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
        ast = parser.program() # program is the root node in the tree as defined in Exp.g4
        if parser.getNumberOfSyntaxErrors() > 0:
            print(f"Failed to parse: {blue_hr_filename}")
        else:
            # DEBUG
            # print out ast in lisp like format
            # print(ast.toStringTree(recog=parser))
            # print out tokens from TerminalNodes
            # token_printer = TokenPrinter()
            # token_printer.visit(ast)
            # /DEBUG

            # Transform ANTLR AST to Qafny AST
            transformer = ProgramTransformer()
            qafny_ast = transformer.visitProgram(ast)
#            print(f"\n qafny_ast:{qafny_ast}")

            # Collect the types + kinds in the AST
            collect_kind = CollectKind()
            collect_kind.visit(qafny_ast)
            
            type_collector = TypeCollector(collect_kind.get_kenv())
            type_collector.visit(qafny_ast)

            # Convert to Dafny AST
#            print(f"\nkinds:{collect_kind.get_kenv()}\ntypes:{type_collector.get_env()}")
            dafny_transfer = ProgramTransfer(collect_kind.get_kenv(), type_collector.get_env())
            dafny_ast = dafny_transfer.visit(qafny_ast)

            # Pass the result through a Cleanup Visitor to perform final cleanup operations
            cleanup = CleanupVisitor()
            dafny_ast = cleanup.visitProgram(dafny_ast)

            # Convert Dafny AST to string
            target_printer_visitor = PrinterVisitor()
            dafny_code = ''

            # add library functions
            dafny_code += DafnyLibrary.buildLibrary(dafny_transfer.libFuns)

            # this is required to print out the generated lambda functions
            for i in dafny_transfer.addFuns:
                ci = cleanup.visit(i)
                dafny_code += target_printer_visitor.visitMethod(ci) + "\n"

            # now, add the actual code
            dafny_code += target_printer_visitor.visit(dafny_ast)

            # debugging purposes, print out the generated dafny output
            if args.print_dafny:
                print("Dafny:")
                print(CodeReport(dafny_code))
            output_filename = None
            if args.output is not None:
                # in the case of a default const (the argument was specified, but no filename provided)
                if args.output == '':
                    output_filename = os.path.join(output_dir, os.path.splitext(human_readable_filename)[0] + '_generated.dfy')
                    # check if this file exists, if so, keep trying random bits on the end till one doesn't exist
                else:
                    output_filename = os.path.join(output_dir, args.output)

            if output_filename is not None:
                blue_filename = stylize(f'"{output_filename}"', fore('blue'))
                print(f'Saving Dafny to: {blue_filename}')
                with open(output_filename, 'w') as dafny_file:
                    dafny_file.write(dafny_code)
                if not args.skip_verify:
                    dafny_result = subprocess.run(["dafny", "verify", output_filename])
            else:
                if not args.skip_verify:
                    dafny_result = subprocess.run(
                        ["dafny", "verify", "--stdin", "--allow-warnings", "--verification-time-limit=60"],
                        input=dafny_code, text=True
                    )

            if not args.skip_verify:
                show_step_status(filename, "Verify", dafny_result.returncode == 0)
            print("") # newline break
