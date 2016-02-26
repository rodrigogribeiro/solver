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


Running the test suite
------------------

Tests are under folder ** test ** and the main function to run all
tests are in **Spec.hs** file. Test cases are all in /test/cases
folder and all are composed by two files

    - A .ctr file that contains the constraint 
    - A .out file that contains the expected answer

To add a new example just edit the **tests** function that contains
all test cases in the suite.

To run all tests, just use the command

     stack test

that stack tool will run all tests and output its results.

