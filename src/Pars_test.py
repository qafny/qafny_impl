from pathlib import Path
import os
from antlr4 import FileStream, CommonTokenStream
from ExpLexer import ExpLexer
from ExpParser import ExpParser

def path_relative_to_self(path: str) -> str:
    script_dir = Path(__file__).resolve().parent
    full_path = (script_dir / path).resolve()
    return str(full_path)

test_files = [
    "test1", "test2", "test3", "test4", "test5", "test6", "test7", "test8", "test9",
    "test11", "test12", "test13", "test14", "BellPair", "GHZ", "Teleportation",
    "Superdense", "Shors", "DeutschJozsa", "simon", "DiscreteLog", "Grovers",
    "QPE", "SWAPTest", "AmpAmp", "AmplitudeEstimation", "AppxCounting", "BHSP",
    "FirstAmpEstimate", "FOQA", "LongDistanceEntangle", "NonBoolean", "QFTModQ",
    "SimpleAmpEstimate", "Stabilizer", "StateDistinguishing"
]

DEFAULT_FILENAMES = [path_relative_to_self(f"../test/Qafny/{f}.qfy") for f in test_files]

for filename in DEFAULT_FILENAMES:
    if not os.path.exists(filename):
        print(f"File not found: {filename}")
        continue
    file_stream = FileStream(filename, encoding="utf-8")
    lexer = ExpLexer(file_stream)
    token_stream = CommonTokenStream(lexer)
    parser = ExpParser(token_stream)
    ast = parser.program()
    errors = parser.getNumberOfSyntaxErrors()
    status = "PASS" if errors == 0 else f"FAIL ({errors} errors)"
    print(f"{os.path.basename(filename)}: {status}")