# MPP: a meta pre processor that blends programming languages



MPP is a meta pre processor. It works by helping you combine different
programming  languages and tools  in order  to pre-process  the source
code of your  documents or your programmes. In  other words, thanks to
MPP,  any programming  language  may easily  become  a pre  processing
language.

<pre>
[ document.extension.mpp ] --mpp--> [ document.extension ]
</pre>

A more classical tool is CPP, the C pre processor.
<pre>
[ document.c ] (meta version) --cpp--> [ document.c ] (compiled version)
[ document.h ] (meta version) --cpp--> [ document.h ] (compiled version)
</pre>

MPP is more  generic than (most) existing tools. It  is intended to be
used  for any  (not  completely-insane) programming  language and  any
description language.

MPP also provides a language, which is minimal and designed to be the glue
to combining different languages. As part of its library, some built-ins are
provided. They include directives such as input, ifdef, ifndef, else, elseifdef,
set, get, unset, unsetall, cmd.


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

CeCILL-B is a BSD-like licence. So, basically, I believe it means you can do almost whatever you want with the source code of MPP, however you should refer to the contents of http://www.cecill.info/licences/Licence_CeCILL-B_V1-en.html for reliable information.

Anyhow, if you find MPP useful or have some comments, please let me know by dropping me an email :-)

# Contact

philippe.wang@cl.cam.ac.uk


