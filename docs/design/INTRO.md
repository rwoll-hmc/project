Introduction
============

## Motivation

The __> Prompt__ Domain Specific Language was created to simplify the otherwise
arduous and error prone process of adding cue annotations to scripts&mdash;an
essential task every stage manager completes for __live theater productions__.

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

## Personal Motivation

After stage managing several large productions with many individual cue sheets
being provided by different lighting designers, sound designers, projection
designers, and scenic designers, I began brainstorming an alternate way to
capture this information. Paired with a digitized script, __> Prompt__
serves to unify this process and make it more efficient, trackable, and enjoyable.

## What's a Prompt Book?

For live theater productions, stage managers produce and use a *prompt book*
that includes actor blocking, lighting, sound, and scenic cues.

Wikipedia nicely describes the essence of a prompt book:

> The prompt book, also called transcript, the bible or sometimes simply
> "the book," is the copy of a production script that contains the information
> necessary to create a theatrical production from the ground up. It is a
> compilation of all blocking, business, light, speech and sound cues, lists of
> properties, drawings of the set, contact information for the cast and crew,
> and any other relevant information that might be necessary to help the
> production run smoothly and nicely.

While there are many important components of the prompt book, this project
aims to help stage manager's digitize their cue notes making them searchable,
summarizable, and editable for not only the stage manager, but the rest of the
production team&mdash;including the designers who may alter cues many times
before deciding on a final position for the cue. In the below picture, on
the right side, you can see how these cues are traditionally recorded: in pencil
on paper in the margin of the script:

![Prompt Book Example](https://lifeinthethe8tre.files.wordpress.com/2015/07/img_4634.jpg)

During a production, *each* of the designers will create cue sheets that
look something like:

| Cue # | Page # | Cue Name/Description | Location                 |
|-------|--------|----------------------|--------------------------|
| 0.5   | 0      | House to Half        | after House closes       |
| 0.75  | 0      | Blackout             | 5 second follow          |
| …     | …      | …                    | …                        |
| 247.0 | 86     | Blackout             | Audrey: "…all was well." |      

For a standard production there will likely exist such a cue sheet independently
for scenic transitions, lights, and sound. The stage manager merges all the cues
into the prompt book's script and works with the designers to move their exact
position around as the designers watch the production rehearse.

Not only is this inconvenient, but it can create a disconnect between what the
designer thinks the cue is and what finally ends up in the prompt book; as they
adjust cues, if they are not careful to update their respective cue sheets, the
individual cues may be out of sync.

## Benefits

By creating a digital and semantic way to describe cues, individual "cue sheets"
can be automatically merged into a digital script and allow for edits to be
propagated to/from the prompt book layout and the individual cue sheets. With
a fully developed system, cues could be edited in realtime by multiple people
and be searchable. (Support coming!) There would be no need to flip through the script to see where
that cue at line "…all was well" occurs or what it says. During a rehearsal, if
a designer says take it back to cue 47, the stage manager would instantly be able
to find the page saving time and page flipping. In an ideal world, a nice GUI
would allow the stage manager and production team to easily interact with the
script and generate individualized cue sheets. For now, a command line interface
has been created for your convenience!

Although there is no standardized cue format, *most* designers will have a
procedural way of describing the common details of a cue (number, page, description,
and location), so it seemed appropriate to formalize this into a DSL that could
efficiently capture this information in a standardized way and be more useable
and extensible for goals like cue sheet generation, distribution, syncing and
prompt book layouting as well as searching.


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
