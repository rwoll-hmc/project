Installing Dependencies
=======================

## Haskell Platform and Cabal

The main dependency of > Prompt is the programming language [Haskell](https://www.haskell.org/)
and a common Haskell build tool called [Cabal](https://www.haskell.org/cabal/).
Both can be acquired with a standard installation of the Haskell build platform.

### macOS: Homebrew

If you have [Homebrew](http://brew.sh/) installed on your Mac, simply install
the Haskell platform by typing:

```
$ brew cask install haskell-platform
```

### Alternative Haskell and Cabal Installs

If Homebrew isn't an option, I suggest following the guides on the Haskell
website: [Haskell Downloads](https://www.haskell.org/downloads).

## (optional) `latexmk`

If you would like to visualize your cue placements in a PDF copy of the script,
you will need to install `latexmk` and have it on your `PATH`.

> :eyes: Since installing LaTeX and its dependencies are often an involved
> process, the CLI for the program has a `--latex-off` flag. You won't be able
> to visualize the output, but you will get a `.tex` file that you can render
> at a later time. Regardless, the program will let you know if you've written
> a compiling program.

### macOS: Homebrew

If you have Homebrew installed on your Mac, simply install the `latexmk` as
bundled with `mactex` by typing:

```
$ brew cask install mactex
```

### Alternative `latexmk` Installs

Check to see if an installation exists with your preferred package manager or
online.
