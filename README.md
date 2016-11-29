\> Prompt
=========

## Setup

```
$ git clone $REPO_URL $REPO_DIR
$ cd $REPO_DIR
$ cabal install parsec
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
