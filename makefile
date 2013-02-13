# generic rule for compiling C files

CFLAGS=-g -O -Wall

%.o: %.c
	gcc -c $(CFLAGS) -o $@ $<

%: %.o
	gcc $(CFLAGS) -o $@ $<

all: out

out: out.o

clean: cleaner
	rm -f out.*	

cleaner: 
	rm -f *.o
	rm -f out.exe