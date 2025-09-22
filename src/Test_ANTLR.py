from antlr4 import *
from ExpLexer import ExpLexer
from ExpParser import ExpParser
lexer = ExpLexer(FileStream('D:/Qafny_Project/qafny_impl/test/Qafny/test1.qfy', encoding='utf-8'))
parser = ExpParser(CommonTokenStream(lexer)); tree = parser.program(); print(tree.toStringTree(recog=parser))