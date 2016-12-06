\> Prompt
=========

[![Build Status](https://travis-ci.org/rwoll-hmc/project.svg?branch=master)](https://travis-ci.org/rwoll-hmc/project)

## Samples

### Sample DSL Language

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
### Sample Rendering

![example](/exampleRendering.png)

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
