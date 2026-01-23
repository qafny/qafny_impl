import sys
from pathlib import Path
import os
import traceback
from antlr4 import FileStream, CommonTokenStream

script_dir = Path(__file__).resolve().parent
root_dir = script_dir.parent.parent
sys.path.insert(0, str(root_dir / "src"))

from ExpLexer import ExpLexer
from ExpParser import ExpParser
from ProgramTransformer import ProgramTransformer
from CollectKind import CollectKind
from TypeCollector import TypeCollector
from TypeChecker import TypeChecker

hsg_dir = script_dir
qfy_files = [str(f) for f in hsg_dir.glob("*.qfy") if f.is_file()]

for filename in qfy_files:
    print(f"\n=== Testing {os.path.basename(filename)} ===")
    file_stream = FileStream(filename, encoding="utf-8")
    lines = file_stream.strdata.splitlines()
    lexer = ExpLexer(file_stream)
    token_stream = CommonTokenStream(lexer)
    parser = ExpParser(token_stream)
    ast = parser.program()
    errors = parser.getNumberOfSyntaxErrors()
    if errors > 0:
        print(f"  Parsing failed ({errors} errors) — skipping")
        continue

    try:
        transformer = ProgramTransformer()
        qafny_ast = transformer.visitProgram(ast)
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
        src_line_no = last.lineno - 1
        src_line = lines[src_line_no].strip() if 0 <= src_line_no < len(lines) else "<unknown>"
        print(f"  TypeCollector FAILED around line {src_line_no + 1}: {type(e).__name__}: {e}")
        print(f"    Source: {src_line}")
        traceback.print_exc()
        continue

    try:
        type_checker = TypeChecker(type_collector.get_env(), {}, {}, 0)
        type_checker.visit(qafny_ast._exps[0])
        print("  TypeChecker succeeded")
    except Exception as e:
        tb = traceback.extract_tb(sys.exc_info()[2])
        last = tb[-1]
        src_line_no = last.lineno - 1
        src_line = lines[src_line_no].strip() if 0 <= src_line_no < len(lines) else "<unknown>"
        print(f"  TypeChecker FAILED around line {src_line_no + 1}: {type(e).__name__}: {e}")
        print(f"    Source: {src_line}")
        traceback.print_exc()