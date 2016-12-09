Errors
======

## Parsing Errors

__Example Trace__:  

```
Parsing Error:
  "examples/Hamlet.cuesheet" (line 5, column 1):
  unexpected "D"
  expecting "  " or lf new-line
```

__Possible Causes__:  

This likely means you forgot a newline somewhere or forgot to give the block
the right amount of indentation.

__Possible Fixes__:  

Make sure there is a newline after every unit block. Look at the example to see
where newlines and indentions appear.

## `NoMatchError`

__Example Trace__:  

```
Processing Error:
  [NoMatchError] The following marker could not be placed:

    (line) @BARNARDO:"I never say this..."

  Perhaps you spelled something incorrectly or have used the wrong index.
```

__Possible Causes__:  

The cue group you specified did not have any matching line in the script. Perhaps
the character you specified never says those words or makes the entrance/exit.
A more likely alternative, is that you've mistyped a word in the quoted line.

__Possible Fixes__:  

* Double check the character says/does what you think they do in the script.
* Check that all words are spelled correctly and are exactly one line.

## `AmbiguousError`

__Example Trace__:  

```
Processing Error:
  [AmbiguousError] The following marker has too many possible options:

    PromptMarker {pMarker = (line) @BARNARDO:"h", pCues = [#LX {1} STBY "t",#SD {1} GO "comment 2",#BS {1} WARN]}

  Here are all the possible matches:

    PromptMarker {pMarker = (line) @BARNARDO:"Who's there?", pCues = []}
    .
    .
    .
    PromptMarker {pMarker = (line) @BARNARDO:"It was about to speak when the cock crew.", pCues = []}


  Please disambiguate the placement by providing an index or specifying more of a line:
        (visual) @CECIL:ENTR (99)
     OR
        (line) @CECIL:"more words here"
```

__Possible Causes__:  

A cue group has many matches within a scene. > Prompt will give this error if
there is more than one possible placement within a scene of the specified cue.
It will enumerate ALL possible placements so you may pick one.

__Possible Fixes__:  

Add more context to disambiguate the placement by either providing more words
from the line or adding a parenthesised index after the cue group.

## `DuplicateCharacterDeclaration`

__Example Trace__:  

```
Compiling Error:
  [DuplicateCharacterDeclaration] The following character is defined twice:

    @BARNARDO

  Delete the extra occurences in your 'Characters' declaration block.
```

__Possible Causes__:  

You declared a character more the once:

```
Characters:
  @HAMLET
  @POLONIUS
  @HAMLET
```

__Possible Fixes__:  

Delete any duplicate occurences.

## `DuplicateDepartmentDeclaration`

__Example Trace__:  

```
Compiling Error:
  [DuplicateDepartmentDeclaration] The following character is defined twice:

    #LX

  Delete the extra occurences in your 'Departments' declaration block.
```

__Possible Causes__:  

Similar to above, except with departments:

```
Departments:
  #LX
  #SD
  #LX
```

__Possible Fixes__:  

Delete any duplicate occurences.

## `UndeclaredCharacterError`

__Example Trace__:  

```
Compiling Error:
  [UndeclaredCharacterError] The following character was referenced in a cue, but not defined:

    @HAMLET

  Perhaps it is a typo? If not, please declare the character in the 'Characters' block.
```

__Possible Causes__:  

You tried to refer to a character that you forgot to declare or you made a typo
when specifying their name.

__Possible Fixes__:  

Add the character to the declaration list:

```
Characters:
  @POLONIUS
  @HAMLET
```

or check for a typo!

## `UndeclaredDepartmentError`

__Example Trace__:  

```
Compiling Error:
  [UndeclaredDepartmentError] The following department was referenced in a cue, but not defined:

    #TYPO

  Perhaps it is a typo? If not, please declare the department in the 'Departments' block.
```

__Possible Causes__:

Analogous to above.

__Possible Fixes__:

Analogous to above.  

## `UnkownTargetCharacterError`

__Example Trace__:  

```
Compiling Error:
  [UnkownTargetCharacterError] The following character was referenced in a the cuesheet,
                               but does not appear in the target script:

    @NONEXISTENT

  Perhaps it is a typo? If so, please change the character declaration and all references.
```

__Possible Causes__:  

You correctly declared a character and used it in the cue sheet, but the character
doesn't exist in the destination script.

__Possible Fixes__:  

Check the spelling and look at the character list of the destination script. Use
the names as declared in the destination script.

## `OutOfOrderOrDuplicateActError`

__Example Trace__:  

```
Compiling Error:
  [OutOfOrderOrDuplicateActError] The following act was declared out of order or twice:

    Act 1

  Please check the numbering.
```

__Possible Causes__:  

The `Act 1` declaration came after `Act 2` or another later act. Or you declared
`Act 1` twice.

__Possible Fixes__:  

If you have duplicated occurrences, simply merge the two together. Otherwise,
reorder so that acts appear in numerical ascending order.

## `OutOfOrderOrDuplicateSceneError`

__Example Trace__:  

```
Compiling Error:
  [OutOfOrderOrDuplicateSceneError] The following scene was declared out of order or twice:

    Scene 1

  Please check the numbering.
```

__Possible Causes__:  

Analogous to above.

__Possible Fixes__:  

Analagous to above.  
