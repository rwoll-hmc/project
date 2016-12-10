Evaluation
==========

\> Prompt is distinctly an external DSL focused on cue annotations. It is limited
by design to really only allow a user to specify cue placements and not much else.
It could be extended as an annotation language for other types of text and margin
notes, but beyond that it is not intended to be used nor would it be useful.
I have purposefully left out any GPL control structures, arithmetic, etc. to
simplify the language.

Cue annotations at the line level work really well and the compiler does a very
good job of resolving cues uniquely or telling a user to provide more information
to help it do a better job. However, visual placements (like those that appear
with entrances and exits) are not as clean since many of them require adding
indices to disambiguate.

Another downside of the current implementation is that you can only add cues at
the line/visual level but not at the word/character level. In a future iteration,
I imagine a syntax that allows the user to specify exactly where the highlight mark should
begin:

```
(line) @HAMLET:"Hello, world! It's |going"
```

In this idealized syntax, the `|` character would be used to add another level
of granularity to the user's ability to annotate the script.

Due to some initial oversights in data structures and logic, I do not keep track
of line numbers of the users program after it has been parsed, this has made some
error messages less helpful than they could be.

In a future version of the project, I imagine cleaning up the format of the error
messages as well as including line/column information from the source file. In
addition, I would imagine the entire process be converted to a GUI point and
click process packaged into a web application. I think it would be much easier
to place cues by looking at a rendered version of a script and highlighting a piece
of text then adding cues with a drop down menu. At the very least it would be nice
to have a watch feature of the CLI in which updating the source cue sheet would
trigger a re-render of the PDF.

With all these goals in mind, I think I would like to re-implement the project in
Elm which is very similar to Haskell but more focused on the web app domain.
