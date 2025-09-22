import test_qafny_grammar as tqg
import test_program_transformer as tpt
import test_kind_collector as tkc
# test locus collector
# test type collector

import rich

rich.print('[magenta]Running ANTLR Grammar Tests:')
tqg.run()
rich.print('[magenta]Running ProgramTransformer Tests:')
tpt.run()
rich.print('[magenta]Running KindCollector Tests:')
tkc.run()
