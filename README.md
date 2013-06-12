# MPP: a meta preprocessor that blends programming languages



MPP is a meta preprocessor. It works by helping you combine different
programming  languages and tools  in order  to pre-process  the source
code of your  documents or your programmes. In  other words, thanks to
MPP,  any programming  language  may easily  become  a preprocessing
language.

<pre>
[ document.extension.mpp ] --mpp--> [ document.extension ]
</pre>

A more classical tool is CPP, the C preprocessor.
<pre>
[ document.c ] (meta version) --cpp--> [ document.c ] (compiled version)
[ document.h ] (meta version) --cpp--> [ document.h ] (compiled version)
</pre>

MPP is more  generic than (most) existing tools. It  is intended to be
used  for any  (not  completely-insane) programming  language and  any
description language.

MPP also provides a language, which is minimal and designed to be the glue
to combining different languages. As part of its library, some builtins are
provided. They include directives such as input, ifdef, ifndef, else, elseifdef,
set, get, unset, unsetall, cmd.

MPP is implemented in OCaml. The goal is to have a preprocessor that is 
easy to extend, easy to customize, and very powerful.
It is powerful because it allows to use any programming language to programme
your document, therefore you can actually turn your document into any programme,
including very bad jokes.
It is still in active development because a lot of features are still to be implemented but
it's already usable.

# Log

* from 7 June 2013: until further notice, please consider that large refactoring is taking place.
* 6 June 2013: new features are being pushed, so it's going to be fragile for a day or two. If something is broken, take a version of 4 June.
* 6 June 2013: some bug fixes.
* 5 June 2013: option -see replaced by -iee. Now all default behaviour will be to stop at any error, and options may be provided to ask MPP to ignore specific errors.
* 4 June 2013: options -ignoreerrors and -soee replaced by -ine and -see.
* 4 June 2013: new builtin: builtins (it is useful when trying MPP on stdin).
* 4 June 2013: documentation of builtin commands are now activated: "mpp -builtins" will output the list of builtins with a small description.
* 4 June 2013: new features: options -cc and -cl for builtin frag.
* 4 June 2013: bug fix: options -tl and -tc for builtin frag.
* 26 May 2013: new builtin: ignore.
* 20 May 2013: new builtin: tryget (it's a get that won't fail).
* 20 May 2013: new builtin: frag (for fragment), very useful to include parts of a file.
* 19 May 2013: new builtins: ocamlcerror, ocamlhtml and ocamlhtmlcss.
* 19 May 2013: some bug fixes and minor enhancements.
* 16 May 2013: new command-line builtin: -set x=s
* 16 May 2013: (( and )) replace [[ and ]] as default opening/closing tokens.
* 16 May 2013: bug fixes for ifdef and ifndef builtins.
* 16 May 2013: variables should never contain a space (otherwise, it might have unexpected behaviour). In particular, for command get, spaces that follow the variable's name are ignored.
* 15 May 2013: new builtins (suggested by X. Clerc): ifdef, ifndef, else, elseifdef, error.
* 15 May 2013: minor fixes.
* 14 May 2013: new target in Makefile, for future templating-related features.
* 14 May 2013: updating mpp_charstream.ml (a new function).
* 13 May 2013: clean up on entire repository.
* 13 May 2013: software architecture refactoring.
* 13 May 2013: new features. Special commands don't start with a dash anymore: a dash is now for shell commands.
* 9-12 May 2013: improvements.
* 8 May 2013: MPP is born (it compiles and started preprocessing stuff).
* late April 2013: This project is still in development.
* late April 2013: The implementation is on-going work.
* late April 2013: The specifications are not frozen.

# Licence

MPP is under the CeCILL-B licence. You may find its whole text here:
http://www.cecill.info/licences/Licence_CeCILL-B_V1-en.html 

CeCILL-B is a BSD-like licence. So, basically, I believe it means you can do almost whatever you want with the source code of MPP, however you should refer to the contents of http://www.cecill.info/licences/Licence_CeCILL-B_V1-en.html for reliable information.

Anyhow, if you find MPP useful or have some comments, please let me know by dropping me an email :-)

# Contact

philippe.wang@cl.cam.ac.uk


