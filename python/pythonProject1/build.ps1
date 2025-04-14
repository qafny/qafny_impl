# build.ps1
# runs antlr4 to generate the required python files needed for qafny
# todo: consider an actual build system
# todo: check that the antlr version matches the installed python package version
# todo: maybe version range allowance

# run antlr
antlr4 -Dlanguage=Python3 -visitor Exp.g4
# antlr4 -Dlanguage=Python3 -o dafny -visitor dafny/Dafny.g4