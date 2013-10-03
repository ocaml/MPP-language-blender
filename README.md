# MPP: a meta preprocessor that blends programming languages


MPP is a meta preprocessor.  It works by helping you combine different
programming  languages and tools  in order  to pre-process  the source
code of  your documents  or your programs.  In other words,  thanks to
MPP,  any  programming  language can  easily  become  a  preprocessing
language.

MPP is more  generic than (most) existing tools. It  is intended to be
used to  preprocess any ascii-based  files (strict supersets  of ascii
such as utf-8 are fine), using either the light language built in MPP,
and/or any  programming language of  your choice (currently,  OCaml is
the only  one supported, but supporting other  languages only consists
in writing a short description, so many languages will be supported in
the future).

MPP provides a built-in language,  which is minimal and designed to be
the glue  to combining  different languages. As  part of  its library,
some  builtins are provided.  They include  directives such  as input,
ifdef, ifndef, else, elseifdef, set, get, unset, unsetall, cmd.

MPP is implemented  in OCaml. The goal is to  have a preprocessor that
is easy to  extend, easy to customize and that  has a great potential.


# Licence

MPP is under the ISC licence.
http://www.isc.org/downloads/software-support-policy/isc-license/


# Contact

[philippe.wang@cl.cam.ac.uk](mailto:philippe.wang@cl.cam.ac.uk?subject=mpp)


