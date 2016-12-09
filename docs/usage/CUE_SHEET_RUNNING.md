Cue Sheet Running
=================

## CLI

To run a cue sheet, you will use the command line interface `greater-prompt`:

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

## Example

Paste the following into a file named `Hamlet.txt`:

```
Characters:
  @BARNARDO
  @POLONIUS
  @FRANCISCO

Departments:
  #LX
  #SD
  #BS

Act 1:
  Scene 1:
    (line) @BARNARDO:"Who's there?"
      #LX {1} STBY "t"
      #SD {1} GO "comment 2"
      #BS {1} WARN

    (line) @FRANCISCO:"Not a mouse stirring."
      #LX {1} GO

Act 2:
  Scene 1:
    (line) @POLONIUS:"Give him this money"
      #LX {2} STBY "this is a comment on this cue"
      #SD {2} GO "comment 2"
      #BS {2} WARN

--- END ---
```

Then run:

```
$ greater-prompt --latex-off Hamlet.txt HamletScriptAnnotated.tex
```

This will output a `.tex` file annotated with your cues.

Play around with the language by adding more cues to the example file. Make typos
in names and departments and have some fun getting errors. :smiley:
