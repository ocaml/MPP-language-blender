# MDD is a language blender.

This is a draft for its specifications and documentation.

-------------------------------------------------- 

MPP is a meta pre processor. It works by helping you combine different
programming  languages and tools  in order  to pre-process  the source
code of your  documents or your programmes. In  other words, thanks to
MPP,  any programming  language  may easily  become  a pre  processing
language.

[ document.extension.mpp ] --mpp--> [ document.extension ]

A more classical tool is CPP, the C pre processor.
[ document.c ] (meta version) --cpp--> [ document.c ] (compiled version)
[ document.h ] (meta version) --cpp--> [ document.h ] (compiled version)

MPP is more  generic than (most) existing tools. It  is intended to be
used  for any  (not  completely-insane) programming  language and  any
description language.

MPP is itself a language, which is minimal and designed to be the glue
to combining different languages.

As MPP is a pre processor, you should think of the "additional" languages
as pre processor languages. 

-------------------------------------------------- 

## Delimiters

- A special block is declared using a special token, which can be
customized.  They Are closed using a different special token, which
can be customized as well. In the following examples, we will use
respectively [[ and ]].

- Endline comments are declared with a special token, which can be
customized. In the following examples, we will use %%.

- Comment blocks are between two special tokens, which can
be customized. In the following examples, we will use /* and */.


Blocks are considered named if the  opening token is not followed by a
space character. Named blocks have to use the name to close the block.
(If you don't want to use the name to close blocks, don't give names.)
An unnamed special block:
 [[ -echo Hello World ]]
A special block named Foo:
 [[Foo -echo Hello Foo Foo]] 

An unnamed comment block:
 /* plop */

A named comment block:
 /*bar plop bar*/

We use named blocks to allow nested blocks.


## Templating

Templating is a feature which is freely given by the fundamental 
design of MPP. 
If the tpl function is implemented, it may be used like this:
{{ -tpl project-name }}
and this would have the effect of including the data associated with
project-name.



**************************************************

### Several passes
- inlined code with shared environment => whole document transformation (non code is transformed into "print").
- special block calling a predefined function => everything is given as a single character string.
- special block calling a shell command => the block is given to the stdin of the command.

### Order of passes
- inlined code first
- in case of several inlined code with different environments, first one to appear is executed first (this means, possibly make an empty block at the beginning)
- the rest is interpreted as any other code


# Misc

Shell command

Functions
