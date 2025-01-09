from ProgramTransfer import *
from PrinterVisitor import *
from Programmer import *
from CollectKind import *
from TypeCollector import *
from ProgramTransformer import *
import os
from antlr4 import InputStream, CommonTokenStream
from ExpParser import ExpParser
from ExpLexer import ExpLexer




test_file_path = f"{os.path.dirname(os.path.realpath(__file__))}/test1.qfy"
with open(test_file_path, 'r', encoding='utf-8') as f:
    str = f.read()
i_stream = InputStream(str)
lexer = ExpLexer(i_stream)
t_stream = CommonTokenStream(lexer)
parser = ExpParser(t_stream)
tree = parser.program()

transformer = ProgramTransformer()

x = transformer.visitProgram(tree)
k = CollectKind()
if k.visitProgram(x):
    ty = TypeCollector(k.env)
    if ty.visitProgram(x):
        pt = ProgramTransfer(k.env, ty.env)
        res = pt.visitProgram(x)
        pv = PrinterVisitor()
        print(pv.visitProgram(res))