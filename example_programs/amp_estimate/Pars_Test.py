import sys
from pathlib import Path
import os
from antlr4 import FileStream, CommonTokenStream

script_dir = Path(__file__).resolve().parent
root_dir = script_dir.parent.parent  # Up to qafny_impl
sys.path.insert(0, str(root_dir))

from src.ExpLexer import ExpLexer
from src.ExpParser import ExpParser

amp_estimate_dir = script_dir
qfy_files = [str(f) for f in amp_estimate_dir.glob("*.qfy") if f.is_file()]

for filename in qfy_files:
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