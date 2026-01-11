import sys
from pathlib import Path
import os
import traceback
from antlr4 import FileStream, CommonTokenStream

script_dir = Path(__file__).resolve().parent
root_dir = script_dir.parent.parent
src_dir = root_dir / "src"
#sys.path.insert(0, str(root_dir))
sys.path.insert(0, str(src_dir))

from src.ExpLexer import ExpLexer
from src.ExpParser import ExpParser
from src.ProgramTransformer import ProgramTransformer
from src.ProgramVisitor import *
from src.AbstractProgramVisitor import AbstractProgramVisitor
from src.CollectKind import CollectKind
from src.TypeCollector import TypeCollector
from src.TypeChecker import TypeChecker

hsg_dir = script_dir
qfy_files = [str(f) for f in hsg_dir.glob("*.qfy") if f.is_file()]

for filename in qfy_files:
    print(f"Testing {os.path.basename(filename)}:")
    file_stream = FileStream(filename, encoding="utf-8")
    lexer = ExpLexer(file_stream)
    token_stream = CommonTokenStream(lexer)
    parser = ExpParser(token_stream)
    ast = parser.program()
    errors = parser.getNumberOfSyntaxErrors()
    if errors > 0:
        print(f"  Parsing failed ({errors} errors) — skipping further steps")
        continue

    try:
        transformer = ProgramTransformer()
        qafny_ast = transformer.visitProgram(ast)
        print(qafny_ast)
        print("  ProgramTransformer succeeded")
    except Exception as e:
        print(f"  ProgramTransformer failed: {e}")
        traceback.print_exc()
        continue

    try:
        collect_kind = CollectKind()
        collect_kind.visit(qafny_ast)
        print("  CollectKind succeeded")
    except Exception as e:
        print(f"  CollectKind failed: {e}")
        traceback.print_exc()
        continue

    try:
        type_collector = TypeCollector(collect_kind.get_kenv())
        type_collector.visit(qafny_ast)
        print("  TypeCollector succeeded")
    except Exception as e:
        tb = traceback.extract_tb(sys.exc_info()[2])
        last = tb[-1]
        print(f"  TypeCollector FAILED: {type(e).__name__}: {e}")
        print(f"    Location: {last.filename}:{last.lineno} (line: '{last.line}')")
        traceback.print_exc()
        continue

    try:
        type_checker = TypeChecker(type_collector.get_env())
        type_checker.visit(qafny_ast)
        print("  TypeChecker succeeded (no errors)")
    except Exception as e:
        tb = traceback.extract_tb(sys.exc_info()[2])
        last = tb[-1]
        print(f"  TypeChecker FAILED: {type(e).__name__}: {e}")
        print(f"    Location: {last.filename}:{last.lineno} (line: '{last.line}')")
        traceback.print_exc()