# MPP-language-blender

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


# Log

* 15 May 2013: new builtins (suggested by X. Clerc): ifdef, ifndef, else, elseifdef, error.
* 15 May 2013: minor fixes.
* 14 May 2013: new target in Makefile, for future templating-related features.
* 14 May 2013: updating mpp_charstream.ml (a new function).
* 13 May 2013: clean up on entire repository.
* 13 May 2013: software architecture refactoring.
* 13 May 2013: new features. Special commands don't start with a dash anymore: a dash is now for shell commands.
* 9-12 May 2013: improvements.
* 8 May 2013: MPP is born (it compiles and started preprocessing stuff)
* late April 2013: This project is still in development.
* late April 2013: The implementation is on-going work.
* late April 2013: The specifications are not frozen.

# Licence

MPP is under the CeCILL-B licence. You may find its whole text here:
http://www.cecill.info/licences/Licence_CeCILL-B_V1-en.html 

CeCILL-B is a BSD-like licence. So, basically, I believe it means you can do almost whatever you want with the source code of MPP.

However, if you find it useful or have some comments, please let me know by dropping me an email philippe.wang@cl.cam.ac.uk :-)
