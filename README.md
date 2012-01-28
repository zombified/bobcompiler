This "compiler" was a part of a class project spanning a semester in 2007.

It was designed to compile a language that the instructor made up, and for which I no longer have documentation. The best documentation is the test.txt file, which should contain most, if not all, of the features of the compiler.

It is horribly coded, and contains very few comments. It was also the first time where I used Python for anything larger than a image cropping script.

The intent is that you pass a program into the _translator.py_ script, which then generates a _prog.asm_ script -- 32-bit x86 assembler. You are then intended to use nasm to compile the assembly into an object file, and then you use gcc to link in the C standard library and produce an executable.
