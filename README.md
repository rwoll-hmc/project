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

```
$ git clone $REPO_URL $REPO_DIR
$ cd $REPO_DIR
$ cabal install
```

## Running Tests

```
$ cabal test
```

## Running

```
$ cat examples/example.txt | $REPO_DIR/dist/build/greater-prompt/greater-prompt 
```
