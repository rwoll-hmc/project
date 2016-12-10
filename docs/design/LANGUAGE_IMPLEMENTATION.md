Language Implementation
=======================

## Host Language

The current implementation of \> Prompt is in Haskell, an awesome functional
programming language feature a very nice typing system&mdash;a key factor in my
decision to use it. I wanted to pick a language that, through type-checking, could
help me refactor my work at a later point if I decided to work on the project
again. I also chose this language because I had only used it for academic assignments
so far and had the desire to test my ability to design a more substantial
program in it as well as get familiar with its build tools like cabal and its
documentation (as well as other external packages).

Since my language is an external DSL, I knew parsing would come in to play and
having worked with parser combinators before and more specifically Parsec, which
is the helpful library behind my parser, I thought it would be a good idea to\
use Haskell.

While there are many interesting and well implemented internal DSLs in Haskell
and other host languages, I really wanted to make greater prompt customized to my
original syntax design; therefore, creating an external DSL (as opposed to an internal one)
came naturally. I did not want to make the user type extra characters or worry
about getting foreign stack traces from another language. Although Haskell is great,
the error messages it gives can be frightening&mdash;especially to someone who
has never programmed that may be interested in using my DSL.

## Running a Program

At a high level, when a program runs the parser inputs the data into a list
based data structure that corresponds exactly to the user input. Next, this data
is traversed and converted into another more efficient intermediate implementation
that guarantees uniqueness in the data. If duplicated data is found, errors are
displayed to the user and this process ceases. Once in this secondary form, the data
is merged with a similar data structure (internally they are the same) that captures
the script. Together, the script and the cue sheet become the prompt script.
Finally, if all cues can be merged and resolved uniquely, then the output is
put through a renderer which outputs raw text into a LaTeX file that can then
be rendered into a PDF.

If you look through [`src/Main.hs`](https://github.com/rwoll-hmc/project/blob/master/src/Main.hs),
each line roughly corresponds to a step in this process.

### Parsing

For my DSL, I have used parser combinators to handle the parsing. Unfortunately,
using this technique (as opposed to generators), the parsing code is less readable
as a consequence of me not breaking up the parsers into small enough units and
the grammar being sufficiently complex.

### Handling Errors

All errors are handled monadically. Essentially, with each function, I have decided
either to quit early when an error is encountered or if doing an operation over
multiple scenes, I opted to collect the errors into a list before cutting out
of the program. Monadic programming is common in functional paradigms.

### `CueScript` Data Structure

The `CueScript` data structure is a list-based tree representing the acts, their
scenes, the scenes cue groups, and the cue groups cues. Traversing this data structure
involves many folds and, while complex (and often inefficient) to work with, has
1-to-1 encoding of the user input since it both preserves duplication and order.

This 1-to-1ness allowed me to give more accurate user feedback on a program before
transforming the representation into a more easily traversable and workable
map-based implementation.

### `PromptScript` Data Structure

The `PromptScript` data structure is analogous to the `CueScript` data structure,
except it uses maps internally instead of lists. Therefore, numbering information
is moved up a level as keys in the map. This presented a challenge, although
`mapAccumWithKey` came in handy.

The `PromptScript` data structure has three purposes:
  1. Encode original, unannotated script: [src/SampleScript.hs](https://github.com/rwoll-hmc/project/blob/master/src/SampleScript.hs)
  2. Encode the cue sheet after initial compilation.
  3. Encode the final annotated script.

### Cue Resolution Logic

Writing the logic that uniquely places a cue in the script was the most challenging
since it involved traversing and merging two tree data structures and searching
through them. Deciding what qualifies a cue as uniquely placed was also an involved
thought process: should cues be uniquely placed relative to one another, globally within
a scene, or the entire script. Currently, most designers give enough information
so it is unique at the page level, but without any notion of pages (by design, for better or worse),
I decided to make things unique on the scene level.
