# Qafny-py

## Requirements:

-Python 3.13.1 or later.
-antlr4-python3-runtime 4.9.2 (python package).
-rich 13.9.4 (python package).
-Dafny 4.10 or later (added to PATH).

## Usage:

In the folder, run: ```python .```
This will default to verifying a suite of test qafny files, but it can be changed by appending a filename to the end of  ```python .``` command to verify any specific file.

### Options:

`--print-dafny` Print out the generated dafny code with verifying.
`--output [FILENAME]` Write the generated dafny code to a file called FILENAME.
`--skip-verify` Just generate the dafny code, don't try to verify it.