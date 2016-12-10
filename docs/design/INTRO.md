Introduction
============


## Motivation

The __> Prompt__ Domain Specific Language was created to simplify the otherwise
arduous and error prone process of adding cue annotations to scripts&mdash;an
essential task every stage manager completes for live theater productions.

Traditionally, this task involves aggregating multiple loosely structure cue
sheets from several designers and, with a ruler and pencil, adding annotations
in the margins in a script indicating when certain cues are to be called during
a performance. The below image shows the result of this process when done
traditionally:

![](http://www.theatrecrafts.com/pages/wp-content/uploads/2016/02/promptbook.jpg)


In the above example, you can see the stage manger user standard PDF software to
add the annotations digitally. Currently, this is as state of the art as it gets.
Well, at least until __> Prompt__ was created! :smiley:.

Now, we can take a document that looks like this:

```
Characters:
  @BARNARDO
  @POLONIUS
  @FRANCISCO
  @NONEXISTENT

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
```

and generate the annotated script in an instant:

![](https://raw.githubusercontent.com/rwoll-hmc/project/master/hamletExample.png).

## Benefits

### Best Practices
Because the cue sheet is in a standard syntax, it enforces designers to follow
best practices and ensures that they provide enough information to uniquely
identify where the cue should be placed in the script&mdash;[if not, the command
line interface will give them friendly warning with tips on how to fix it!](../errors/ERRORS.md)

### Save Time

This will save stage managers a lot of time and save designers time from
needing to unnecessarily have conversations with stage managers on where their
ambiguous cue placements go.

### No More Excel Sheets

No need to use excel sheets with differing formats from designer to designer.
__> Prompt__ adds standards to one of the most common tasks.

### Easy to Learn

 As you can see in the above example, the syntax of the language is straightforward
 and should be easy learn. It's whitespace sensitive by design, so it will keep
 your documents formatted nicely.

### Color Coded PDFs

Stager manager's frequently enjoy color coding their prompt books. With __> Prompt__
the default creates color coded output letting you easily read the generated
PDF.

### Fast

Need to move a bunch of cues or renumber them? Now there is no need to get out
a big eraser or manually move a bunch of comments in a PDF. Simply adjust what
you need in the cue sheet file and instantly render the updated copy. All the cues
are localize in one place and the raw file with its whitespace sensitivity and
clean syntax make it easy to read by both the computer and humans!
