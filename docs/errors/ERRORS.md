Errors
======

## Parsing Errors

__Example Trace__:  

__Possible Causes__:  

__Possible Fixes__:  

## `NoMatchError`

__Example Trace__:  

```
Processing Error:
  [NoMatchError] The following marker could not be placed:

    (line) @BARNARDO:"I never say this..."

  Perhaps you spelled something incorrectly or have used the wrong index.
```

__Possible Causes__:  

__Possible Fixes__:  

## `AmbiguousError`

__Example Trace__:  

```
Processing Error:
  [AmbiguousError] The following marker has too many possible options:

    PromptMarker {pMarker = (line) @BARNARDO:"h", pCues = [#LX {1} STBY "t",#SD {1} GO "comment 2",#BS {1} WARN]}

  Here are all the possible matches:

    PromptMarker {pMarker = (line) @BARNARDO:"Who's there?", pCues = []}
    PromptMarker {pMarker = (line) @BARNARDO:"Long live the King!", pCues = []}
    PromptMarker {pMarker = (line) @BARNARDO:"'Tis now struck twelve. Get thee to bed, Francisco.", pCues = []}
    PromptMarker {pMarker = (line) @BARNARDO:"Have you had quiet guard?", pCues = []}
    PromptMarker {pMarker = (line) @BARNARDO:"Well, good night.", pCues = []}
    PromptMarker {pMarker = (line) @BARNARDO:"The rivals of my watch, bid them make haste.", pCues = []}
    PromptMarker {pMarker = (line) @BARNARDO:"Say, what, is Horatio there?", pCues = []}
    PromptMarker {pMarker = (line) @BARNARDO:"I have seen nothing.", pCues = []}
    PromptMarker {pMarker = (line) @BARNARDO:"Sit down awhile,", pCues = []}
    PromptMarker {pMarker = (line) @BARNARDO:"That are so fortified against our story,", pCues = []}
    PromptMarker {pMarker = (line) @BARNARDO:"What we have two nights seen.", pCues = []}
    PromptMarker {pMarker = (line) @BARNARDO:"Last night of all,", pCues = []}
    PromptMarker {pMarker = (line) @BARNARDO:"When yond same star that's westward from the pole", pCues = []}
    PromptMarker {pMarker = (line) @BARNARDO:"Had made his course t' illume that part of heaven", pCues = []}
    PromptMarker {pMarker = (line) @BARNARDO:"Where now it burns, Marcellus and myself,", pCues = []}
    PromptMarker {pMarker = (line) @BARNARDO:"The bell then beating one\8212", pCues = []}
    PromptMarker {pMarker = (line) @BARNARDO:"In the same figure like the King that's dead.", pCues = []}
    PromptMarker {pMarker = (line) @BARNARDO:"Looks he not like the King? Mark it, Horatio.", pCues = []}
    PromptMarker {pMarker = (line) @BARNARDO:"Is not this something more than fantasy?", pCues = []}
    PromptMarker {pMarker = (line) @BARNARDO:"What think you on 't?", pCues = []}
    PromptMarker {pMarker = (line) @BARNARDO:"I think it be no other but e'en so.", pCues = []}
    PromptMarker {pMarker = (line) @BARNARDO:"Well may it sort that this portentous figure", pCues = []}
    PromptMarker {pMarker = (line) @BARNARDO:"Comes arm\232d through our watch so like the king", pCues = []}
    PromptMarker {pMarker = (line) @BARNARDO:"That was and is the question of these wars.", pCues = []}
    PromptMarker {pMarker = (line) @BARNARDO:"'Tis here.", pCues = []}
    PromptMarker {pMarker = (line) @BARNARDO:"It was about to speak when the cock crew.", pCues = []}


  Please disambiguate the placement by providing an index or specifying more of a line:
        (visual) @CECIL:ENTR (99)
     OR
        (line) @CECIL:"more words here"
```

__Possible Causes__:  

__Possible Fixes__:  

## `DuplicateCharacterDeclaration`

__Example Trace__:  

```
Compiling Error:
  [DuplicateCharacterDeclaration] The following character is defined twice:

    @BARNARDO

  Delete the extra occurences in your 'Characters' declaration block.
```

__Possible Causes__:  

__Possible Fixes__:  

## `DuplicateDepartmentDeclaration`

__Example Trace__:  

```
Compiling Error:
  [DuplicateDepartmentDeclaration] The following character is defined twice:

    #LX

  Delete the extra occurences in your 'Departments' declaration block.
```

__Possible Causes__:  

__Possible Fixes__:  

## `UndeclaredCharacterError`

__Example Trace__:  

```
Compiling Error:
  [UndeclaredCharacterError] The following character was referenced in a cue, but not defined:

    @HAMLET

  Perhaps it is a typo? If not, please declare the character in the 'Characters' block.
```

__Possible Causes__:  

__Possible Fixes__:  

## `UndeclaredDepartmentError`

__Example Trace__:  

```
Compiling Error:
  [UndeclaredDepartmentError] The following department was referenced in a cue, but not defined:

    #TYPO

  Perhaps it is a typo? If not, please declare the department in the 'Departments' block.
```

__Possible Causes__:  

__Possible Fixes__:  

## `UnkownTargetCharacterError`

__Example Trace__:  

__Possible Causes__:  

__Possible Fixes__:  

## `OutOfOrderOrDuplicateActError`

__Example Trace__:  

__Possible Causes__:  

__Possible Fixes__:  

## `OutOfOrderOrDuplicateSceneError`

__Example Trace__:  

__Possible Causes__:  

__Possible Fixes__:  
