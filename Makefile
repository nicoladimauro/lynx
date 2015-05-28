cgrasp.o: cgrasp.c
	gcc -c -shared -fPIC cgrasp.c
	ld -shared -o cgrasp.so cgrasp.o

