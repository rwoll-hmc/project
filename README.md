\> Prompt
=========

[![Build Status](https://travis-ci.org/rwoll-hmc/project.svg?branch=master)](https://travis-ci.org/rwoll-hmc/project)

## About

\> Prompt (*greater prompt*) is a domain specific language (DSL) for easily
expressing and controlling cue placements in a theatrical script or [prompt
book][promptbook].

This project has been completed as [coursework for Domain Specific Languages][hmc-class]
at [Harvey Mudd College][hmc] taught by [Professor Ben Wiedermann][prof].

## Language Details

### Quick Sample

The below photo illustrates the DSL on the right, and the output of the program
on the left:

![example](/exampleRendering.png)

### Outline of a > Prompt Cue Sheet

Each cue sheet should begin with a __character declaration block__ like below:

```
Characters:
  @SARAH
  @CECIL
```

The word `Characters:` should be flush with the left side of the file and each
character should be listed below __indented with 2 spaces__. The characters name
must start with the `@` symbol and only contain uppercase alphabetical characters
(`[A-Z]`)
or underscores (`_`).

> __NOTE__: You do not need to declare every character that appears in the script you
> are annotating; you simply need to declare any character you refer to within
> your cues.

After the characters block, __a newline must appear__ before the __departments
declaration block__:

```
Departments:
  #LX
  #SD
  #BS
```

The word `Departments:` should be flush with the left side of the file and each
department should be listed below __indented with 2 spaces__. The departments name
must start with the `#` symbol and only contain uppercase alphabetical characters
(`[A-Z]`).

:construction: More docs coming soon...

```
Characters:
  @SARAH
  @CECIL

Departments:
  #LX
  #SD
  #BS

Act 1:
  Scene 1:
    (visual) @ATHEN:EXIT
      #LX {100000} STBY
      #SD {2071231283712318} STBY

    (visual) @ATHEN:ENTR
      #LX {2222} WARN
```

## Setup

### Installing Haskell Platform

To setup the project, we will assume you have the [Haskell Platform][hp] installed
which includes [`cabal`][cabal], a build tool for Haskell. If you're on a macOS
with [Homebrew](http://brew.sh/) installed, you can install the Haskell Platform
with `cabal` via

```
$ brew cask install haskell-platform
```

### Compiling and Downloading Dependencies

With `cabal` already installed, setting the project involves simply cloning and
running the setup command.

```
$ git clone $REPO_URL $REPO_DIR
$ cd $REPO_DIR
$ cabal install
```

This will place an executable `greater-prompt` in `$REPO_DIR/dist/build/greater-prompt/`
which is the CLI to process the DSL and generate output.

## Documentation

The documentation for using the DSL and the command-line program can be found here
in the wiki. If you're looking to hack on the backend, you can generate Haddock
html documentation by running

```
$ cabal haddock --executable
```

This will generate documentation in `$REPO_DIR/dist/doc/html/greater-prompt/greater-prompt/index.html`.

## Running Tests

```
$ cabal test --log=/dev/stdout
```

## Running

```
$ cat examples/example.txt | $REPO_DIR/dist/build/greater-prompt/greater-prompt
```

[hp]: https://www.haskell.org/downloads
[cabal]: https://www.haskell.org/cabal/
[promptbook]: https://en.wikipedia.org/wiki/Prompt_book
[hmc-class]: https://hmc-cs111-spring2016.github.io/
[hmc]: https://www.cs.hmc.edu/
[prof]: https://www.cs.hmc.edu/~benw/
