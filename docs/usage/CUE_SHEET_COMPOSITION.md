Cue Sheet Composition
=====================

## Intro

In order to use > Prompt, you simply compose cue sheets in a text file. The
\> Prompt language and syntax is meant to make it easy to specify where cues
should be placed in a script without the need to overspecify.

## Whitespace Sensitivity

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

### Cue Declaration(s)

### End Block
