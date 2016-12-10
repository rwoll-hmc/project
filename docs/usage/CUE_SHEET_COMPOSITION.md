Cue Sheet Composition
=====================

## Intro

In order to use > Prompt, you simply compose cue sheets in a text file. The
\> Prompt language and syntax is meant to make it easy to specify where cues
should be placed in a script without the need to overspecify.

## Whitespace Sensitivity

\> Prompt is an extremely whitespace sensitive language by design. In order to
ensure best formatting practices by users and to ensure that raw cue sheet text
files are easy to read by a human, you must have newlines in exact locations and
use proper indentation as outlined below. ***The only place that whitespace is
ignored is at the end of the line.***

Yes, this does make it slightly harder to write at first, but the rules are
straightforward as you'll see below.

> :eyes: In an ideal version of this project, we would make available a formatting
> tool that would take a flexibly spaced language and autoformat it.

## Anatomy of a Cue Sheet

* [__Character(s) Declaration Block__](#character(s)-declaration-block):  
  Declare any characters that appear in the script that you will reference throughout
  your cues. (e.g. `@HAMLET`, `@POLONIUS`, etc.)
* [__Department(s) Declaration Block__](#department(s)-declaration-block):  
  Declare any departments you may reference in your cues like lights, sound, etc.
  (e.g. `#LX`, `#SD`, `#BACKSTAGE`, etc.)
* [__Act Declaration Block(s)__](#act-declaration-block(s)):  
  Add declarations for any acts in the script that have scenes with cues.
  * [__Scene Declaration Block(s)__](#scene-declaration-block(s)):  
    Add declarations for any scenes in the script that have cue points in them.
    * [__Cue Group Declaration(s)__](#cue-group-declaration(s)):  
      Specify where in the scene a group of cues should be called: either at a
      line or an entrance/exit.
      * [__Cue Declaration(s)__](#cue-declaration(s)):  
      Specify the actual cues to be called on a department level.
* [__End Block and Notes__](#end-block)

### Character(s) Declaration Block

The character declaration block should be the first block to appear in your file.
The word `Characters:` should appear on the first line flush with left side
of the page. On a new line each, the name of a character should appear __indented
with 2 spaces__, beginning with a `@`, and only containing upper case letters `A-Z`
as well as underscores (`_`).

```
Characters:
  @HAMLET
  @POLONIUS
```

> :eyes: You do not need to declare all the characters in the script; however,
> you must declare any character you reference in a cue placement. This is done
> to avoid typos.

__After the block of characters, you must have one blank newline.__

### Department(s) Declaration Block

The department declaration block should be the second block to appear in your file.
The word `Departments:` should appear flush with left side
of the page. On a new line each, the name of a department should appear __indented
with 2 spaces__, beginning with a `#`, and only containing upper case letters `A-Z`
as well as underscores (`_`).

```
Departments:
  #LX
  #SD
  #BACKSTAGE
  #SPOT_I
  #SPOT_II
```

> :warning: Although `#SPOT_I` and `#SPOT_II` may be considered part of the
> lighting department (`#LX`), it is good practice to declare them as separate
> departments, if they will be receiving cues different from one another they
> should be declared separately.

__After the block of characters, you must have one blank newline.__

### Act Declaration Block(s)

Next, you should declare any acts that will have cues. For each `Act n:` where
`n` is some integer should appear flush left in the file:

```
Act 1:
Act 30:
```

> :warning: You must not declare an act unless it will have scenes with cues.

### Scene Declaration Block(s)

Next, for each scene that will have cues within an act, you should add a line
indented with 2 spaces containing `Scene n:` where `n` is some integer:

```
Act 1:
  Scene 1:
  Scene 2:
Act 30:
  Scene 47:
```

> :warning: You must not declare a scene unless it will have cues.

### Cue Group Declaration(s)

A cue group is a natural grouping of individual cues that implies that any child
cues should be called together at some point in the script. In the current version
of the language implementation a cue group can be either a line or a visual
entrance/exit of a character:

#### Line Cue Group

Indented 2 spaces from the `Scene n:` block taking the form of:

```
(line) @CHAR_NAME:"part of the line"
```

where the quoted text should be enough text to uniquely identify the placement
within the scene. In other words, if `@HAMLET` says "Hello!" 5 times throughout
the scene you can disambiguate the placement by using more words from that line.

In the case where adding more words is not sufficient to disambiguate the placement,
you can add an index in parenthesis after the line (see below) which is the lines
index number within a scene:

```
(line) @CHAR_NAME:"part of the line" (47)
```

> :warning: Index style disambiguations should be used only when it is
> absolutely necessary! Prefer to add more words.

#### Visual Line Group

Indented 2 spaces from the `Scene n:` block taking the form of:

```
(visual) @CHAR_NAME:ENTR
```

for an entrance. If the cue should be placed at an exit, substitute `ENTR` for
`EXIT`. Like line groups, visual entrance placements can be disambiguated with
indices:

```
(visual) @CHAR_NAME:ENTR (47)
```

Index disambiguations should only be used when necessary. (The compiler will tell
to use one if needed).


### Cue Declaration(s)

Under each [cue group](#cue-group-declaration(s)) you should place the actual
cues that should be called to each department. Each should appear on a newline.
Following the last cue, you should insert a blank newline before adding the next
group.

The format is as follows:

```
#DEPARTMENT {n} GO "Optional comment....."
```

where `DEPARTMENT` can be replaced by a department you have declared, `n` is some
integer identifying the cue number (this is up to you); `GO` can be substituted
with `STBY` (standby), or `WARN` if you'd like to specify an alternate type of cue.
Optionally, you can also include a comment enclosed in `"`s. For example, a warning
cue might look like this:

```
#LX {47} WARN "@HAMLET about to enter stage left"
```

If you do not have a comment, ommit the quotes:


```
#LX {47} WARN
```

You may have as many cues (1 per department) assigned with a cue group:

```
(visual) @HAMLET:ENTR
  #LX {10} STBY
  #SD {99} GO "wait a half a beat"
  #BS {97} WARN
```

### End Block

The file must and with `--- END ---` flush with the left side of the page. Anything
after the `--- END ---` marker will be ignored, so feel free to leave comments
or notes to yourself after this line!

## Quick Example

Here is all of the above together:

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

    Scene 2:
      (line) @POLONIUS:"Give him this money"
        #LX {3} STBY "this is a comment on this cue"
        #SD {3} GO "comment 2"
        #BS {3} WARN


--- END ---
```
