Example Programs
================

In addition to the example in the [examples folder](https://github.com/rwoll-hmc/project/tree/master/examples)
and the one described in the [Cue Sheet Composition Section](../usage/CUE_SHEET_COMPOSITION.md),
we will walk through annotating *Hamlet*:


## Defining Characters

We will begin by adding the characters that we will be referencing throughout
our cues. Since we will only be adding cues to the first few pages, we just
need to declare *Barnardo*, *Francisco*, and *Marcellus*:

```
Characters:
  @BARNARDO
  @MARCELLUS
  @FRANCISCO

```

## Defining Departments

Next we will define the departments we will be referencing. For our production,
we have a lighting board operator (`#LX_BOARD`), a sound board operator (`#SD_BOARD`),
as well as a spotlight operator (`#SP_OPRTR`). We can refer to them however, but
their names should be concise:

```
Characters:
  @BARNARDO
  @MARCELLUS
  @FRANCISCO

Departments:
  #LX_BOARD
  #SD_BOARD
  #SP_OPRTR

```

## Adding Cues

Next, we will add a group of cues that should be called when Barnardo enters at the
top of the scene in Act 1, Scene 1. We want lights and sound to take their first
cues and the spotlight operator to wait:

```
Characters:
  @BARNARDO
  @MARCELLUS
  @FRANCISCO

Departments:
  #LX_BOARD
  #SD_BOARD
  #SP_OPRTR

Act 1:
  Scene 1:
    (visual) @BARNARDO:ENTR
      #LX_BOARD {1} GO
      #SD_BOARD {1} GO
      #SP_OPRTR {1} STBY "@BARNARDO ENTR SL"

--- END ---
```

## Running

If we save the above to a new file, `example.txt` and run

```
$ greater-prompt example.txt
```

we get a `.tex` output and a `script.pdf`:

![](https://raw.githubusercontent.com/rwoll-hmc/project/master/hamletExample1.png)

Congrats! We have successfully annotated a script!

Now, when we need the stage manager to say `GO` for the spot
when Barnardo says "Who's there?":

```
Characters:
  @BARNARDO
  @MARCELLUS
  @FRANCISCO

Departments:
  #LX_BOARD
  #SD_BOARD
  #SP_OPRTR

Act 1:
  Scene 1:
    (visual) @BARNARDO:ENTR
      #LX_BOARD {1} GO
      #SD_BOARD {1} GO
      #SP_OPRTR {1} STBY "@BARNARDO ENTR SL"

    (line) @BARNARDO:"Who's there"
      #SP_OPRTR {1} GO

--- END ---
```

Running the program again, we get:

![](https://raw.githubusercontent.com/rwoll-hmc/project/master/hamletExample2.png)

:eyes: Notice how we ommitted the `?` from the line in our specification of the
cue placement? We can ommit as much of line as we want as long as it it unique
within the scene. If not, we will get an error message with all the matches, that
you can then specify further by adding more text. For example, if the only two matches
were "Who's there?" and "Who's there in the yard", we could simply add the "?" to
disambiguate. If however, someone says a line repeatedly or makes an entrance or
exit multiple times, throughout a scene, we can specify an index in parentheses like so:

```
(line) @BARNARDO:"Who's there" (2)
  #SP_OPRTR {1} GO
```

where the index is the line number in the scene. (In our case, we count each
entrance as a line!)

> :warning: It is preferred to add more of a line than to use indices since they
> are harder for a human to resolve!

## What next?

[To learn more about the syntax, head to the composition page or look through the
other examples!](../usage/CUE_SHEET_COMPOSITION.md) Scroll all the way down to
see the different type of commands as well as the appropriate levels of indentation.
