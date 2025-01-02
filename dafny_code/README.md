# The generated Dafny proofs for the prototype Qafny compiler

The Qafny compiler is based on the version of 2023-9-30.

## Setup

The code will be run in the Dafny 3.03. To set up Dafny 3.03, users can utilize our Docker image file, or they could download vscode, find Dafny in the extensions, install Dafny, and then downgrade the version to 3.03.

## Directory Contents

The generated Dafny code includes:

* GHZ.dfy - The Dafny proof for GHZ and Controlled GHZ.
* dj.dfy - The Dafny proof for DeutschJozsa.
* grovers.dfy - The Dafny proof for Grover's algorithm.
* qwalk.dfy - The Dafny proof for simple quantum walk.
* shors.dfy - The Dafny proof for Shor's algorithm.
