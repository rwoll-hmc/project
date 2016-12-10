Language Design Details
=======================

__\> Prompt__ (in its current form) exists as a command line program where a user
is able to use the __external domain specific language (DSL)__ to specify cue
annotations in a digitized script<sup>1</sup>. The cues themselves are [composed
in a plain text file](../usage/CUE_SHEET_COMPOSITION.md) with contents looking
like this below:

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

An example cue sheet for Hamlet!
```

which, when run through the program with rendering on, outputs:

![](https://raw.githubusercontent.com/rwoll-hmc/project/master/hamletExample.png)

into a PDF as well as a `.tex` file.

## Data Interpretation

The DSL is meant to abstract away all the logic behind places cues and allow
users to specify those placements with the minimum amount to information required
to uniquely place the cue in a scene. __At the highest level, > Prompt is about
describing cue placements within a script.__.

In order to ensure that cues are being placed exactly where the user intended
them, the process of the program placing the cue involves searching through the
text for all possible matches within the target scene. [If no matches are found,
the user will get a friendly error message along with a suggested fix.](../errors/ERRORS.md#nomatcherror)
[If multiple matches are found, the user will also get an error message, along with
information regarding all the possible matches within the scene as well as suggestions
to disambiguate them.](../errors/ERRORS.md#ambiguouserror)

Although the command line program will not automatically fix these problems, I've
worked to add as helpful error messages as possible to help the user fix the problem
or diagnose an alternate problem.

In order to help users avoid making typos, all variables (department names like `#LX`
or character names like `@HAMLET`), must be declared at the start of the file
before being used. If a user fails to do this, or does this too much (i.e. they
declare a character or department twice) they will get a separate, but informative
error for each.

> :eyes: For more details on how a program may go wrong, please look through the
> [errors page](../errors/ERRORS.md). There, you will find a list of all the errors,
> example "stack traces", possible causes, as well as possible fixes.

## Syntax Design

In addition to the helpful error messages, __> Prompt__ has been designed with
ease of writing __and__ reading in mind. Code should be beautiful and human readable&mdash;not
just machine readable.

With this in mind, I've design an __extremely whitespace sensitive language__.
\> Prompt is sensitive down the space character between keywords. While this can
make it slightly harder to write (since the parser's error messages are not always
the most helpful), it ensures that all raw cue sheet code written and distributed
shares the same formatting. This takes away some flexibility from the user in favor
of aesthetic unity. However, if you look at the above raw text, I think you will
agree it is very easy to know what the raw text means&mdash;__no coding background required.__

When choosing the syntax, I turned a bit towards social media standards: the introduction
of the `@` to prefix any character names originates from the common use of it on
social media to designate a physical entity/being. Likewise, the use of `#` stems
from the use of hashtags which designate categories in social media and departments
in the > Prompt DSL. By choosing common symbols, and very straightforward syntax,
I hope to create a writeable and readable language for people with all levels of
previous coding experience.

## CLI

The command line interface is admittedly not the most ideal way to interact with
the DSL; however, as a proof of concept it serves the same purpose that a fancy
GUI would serve. The output of the command line interface is noisy by default
giving the user constant feedback. If there are errors, the program will pretty
print the messages along with helpful hints; if things go well, the output will
let the user know accordingly.

At the end of it all, a user can visualize their cue placements on a PDF copy of
the script in addition to looking at the raw source cue sheet. In a future iteration,
I would imagine adding view options like tables broken down by department as well
as alternate viewing modes like a web app.

## Control Flow

Essentially the DSL breaks down to groups of cues that correspond to places in
the script. You could imagine at a high level, that a cue sheet and a script
are both maps of line numbers to cues/words. The > Prompt DSL simply abstracts
away the line numbers and lets you specify placements on the content of the map
versus the numeric keys. However, in places where that is not enough, one can
be as specific as the key by describing the line number in the script.

For more details, see the [composing a cue sheet page](../usage/CUE_SHEET_COMPOSITION.md).

## Footnotes

1. For this project, I have digitized [*Hamlet*](http://www.folgerdigitaltexts.org/html/Ham.html);
   however in a future iteration of the project, I imagine an implementation in
   which an externally supplied source script may be used. For the time being,
   *Hamlet* is encoded in the binary CLI; the annotations, however, are external.
