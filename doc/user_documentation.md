# MPP: Meta Pre Processor (user documentation)
-------------------------------------------------------------------

MPP is a generic and parametric meta pre processor for text files.

MPP is a preprocessor, which means that it's a tool that can be used to process your text files in order to enhance them before you actually pass them to the final tool.

MPP is generic because it applies to "all" programming languages and document description languages.
By "all", we don't actually mean all, because some languages won't be well supported. For instance, there has been absolutely no efforts to make MPP work with "toy languages" such as Brainfuck or Whitespace. MPP might work with them but it's rather unlikely or useless.

MPP is parametric because you can change the tokens that are used to declare special blocks. The tokens may be changed in 3 ways: 
1. modify them in the source code.
2. specify them in the command line (use the --help option to see how it works).
3. modify them in special blocks. Indeed, you may choose to change the tokens on the fly.

MPP is meta because it is designed to allow you to build a simple preprocessor for the language of your choice.



<code>
open token: user-defined (here: [[)
 if the token is followed by a space, it is ignored; 
 ethis allows to easily use "let" as a token!

close token: user-defined (here: ]])

end-line comments token: user-defined (here: %%)

open comments token: user-defined (here: /*)

close comments token: user-defined (here: */)

variables: declaration, assignment, deletion

file inclusion: verbatim or interpreted

no escaping: it *has* to be *unnecessary*
</code>
