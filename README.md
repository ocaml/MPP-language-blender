MPP-language-blender
====================

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
