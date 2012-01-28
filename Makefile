all:
	python translator.py test.txt    
	nasm -f elf prog.asm
	gcc -o prog prog.o -lc

clean:
	rm -f prog.* prog *.pyc
