Constraint solver for type definitions for C like languages
=====================================

Install
-----

1 - First, download Haskell [stack](https://github.com/commercialhaskell/stack/blob/master/doc/GUIDE.md).

2 - Run

    stack setup

to install the correct version for GHC Haskell compiler (that will be
the latest one).

3 - Run

    stack build

to build the tool. Since it will install all necessary libraries, this
may take some time.

The executable is inside .stack_work directory in typedefsolver folder.


Executing
--------

The best way to execute (without having to install it) is using
stack. Just run:

    stack exec typedefsolver-exe -- -i INFILENAME -o OUTFILENAME

where INFILENAME is a file containing the constraints and OUTFILENAME
is the file that will hold the results of inference.


