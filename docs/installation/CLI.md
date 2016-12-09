Command Line Interface Setup
============================

## Installing

With `cabal` already installed, setting the project involves simply cloning and
running the setup command:

```
$ git clone $REPO_URL $REPO_DIR
$ cd $REPO_DIR
$ cabal install
```

This will place an executable `greater-prompt` on your `PATH`.

## Checking the Installation

```
$ greater-prompt --help
```

you should see a message along the lines of

```
greater-prompt
usage : greater-prompt [infile] [outfile] [-s] [-l] [-h] [--version]

optional arguments:
 infile                        Source to input file. By default
                               this is stdin.
 outfile                       Destination to output file. By
                               default script.tex
 -s, --silent                  Turn off the verbosity
 -l, --latex-off               Turns off automatic call to
                               latexmk
 -h, --help                    show this help message and exit
 --version                     print the program version and exit
```
