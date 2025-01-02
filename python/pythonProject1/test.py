import os
import time
#import pytest
import random
import json
import os
from antlr4 import InputStream, CommonTokenStream
from ExpParser import ExpParser
from ExpLexer import ExpLexer
from ProgramTransformer import *


def run_test():
    test_file_path = f"{os.path.dirname(os.path.realpath(__file__))}/test1.qfy"
    with open(test_file_path, 'r', encoding='utf-8') as f:
        str = f.read()
    i_stream = InputStream(str)
    lexer = ExpLexer(i_stream)
    t_stream = CommonTokenStream(lexer)
    parser = ExpParser(t_stream)
    tree = parser.program()
    #transform = ProgramTransformer() need to define a program AST form then define a transformer visitor pattern to transform
    #newTree = transform.visitRoot(tree)
    transformer = ProgramTransformer()
    print(tree.toStringTree(recog=parser))


    #x = parser.program().method()[0].conds().spec()[1]
    #print(x.toStringTree(recog=parser))
    x = transformer.visitProgram(tree)
    convert_hadamard(x)


def convert_hadamard(visitor: QXProgram):
    final_output = ""
    with open(f"{os.path.dirname(os.path.realpath(__file__))}/hadnor.dfy", 'r', encoding='utf-8') as f:
        hadnor = f.read()

    final_output += hadnor + "\n"

    method = visitor.method(0)
    method_name = method.ID().getText()
    method_bindings = method.bindings()

    inputparam1ID = method_bindings[0].ID()
    inputparam2ID = method_bindings[1].ID()
    binding1 = method_bindings[0].ID() + ":" + method_bindings[0].type().type()
    binding2 = method_bindings[1].ID() + ":" + 'seq<bv1>'

    conditions = []
    conditions.append('requires |' + inputparam2ID + '| == ' + inputparam1ID)
    conditions.append('requires forall k :: 0<=k<'+inputparam1ID+' ==> '+inputparam2ID+'[k] == 0')

    body = 'var y:= hadNorHad('+ inputparam2ID + ');'
    body+= 'var i:=0;\nwhile(i<n){\nassert (y[i] == omega(0,2));\ni:=i+1;\n}\n'


    conds = '\n'.join(conditions) + '\n'
    final_method = 'method ' + method_name +  ' (' + binding1 + ', ' + binding2 + ')\n' + conds + '{\n' + body + "}"

    final_output+=final_method


    print(final_output)




run_test()
