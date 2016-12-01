\> Prompt
=========

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
$ cabal install parsec HaTeX ansi-terminal
$ make
```

## Running Tests

### Download Test Dependencies

```
$ cabal install tasty tasty-hunit
```

### Run Tests

```
$ make test        # run all tests
$ make test-<NAME> # run a specific test suite
```

## Running

```
$ cat examples/example.txt | ./gp
```
