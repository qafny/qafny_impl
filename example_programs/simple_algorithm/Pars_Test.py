import sys
from pathlib import Path
import os
from antlr4 import FileStream, CommonTokenStream

script_dir = Path(__file__).resolve().parent
root_dir = script_dir.parent.parent  # Up to qafny_impl
src_dir = root_dir / "src"
sys.path.insert(0, str(root_dir))
sys.path.insert(0, str(src_dir))  # Add src directory to path

from src.ExpLexer import ExpLexer
from src.ExpParser import ExpParser
from src.ProgramTransformer import ProgramTransformer

simple_algorithm_dir = script_dir
qfy_files = [str(f) for f in simple_algorithm_dir.glob("*.qfy") if f.is_file()]

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
    if errors > 0:
        status = f"FAIL ({errors} errors)"
    else:
        try:
            transformer = ProgramTransformer()
            qafny_ast = transformer.visitProgram(ast)
            status = "PASS (Qafny AST)"
        except Exception as e:
            status = f"FAIL (Transformer: {e})"
    print(f"{os.path.basename(filename)}: {status}")