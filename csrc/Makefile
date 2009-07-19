#######################################################
#          CSHADigest class By Adrien Pinet           #
#          Use this like you want it's free           #
#######################################################

CC = g++
CFLAGS = -Wall -ansi -pedantic

OBJS= sha1.o CSHADigest.o

sha1 :		$(OBJS)
			$(CC) $(CFLAGS) $^ -o $@
.o : .cpp
	$(CC) $(CFLAGS) - c $<

sha1.o : sha1.cpp
CSHADigest.o : CSHADigest.cpp CSHADigest.h

clean :
	rm -f *.o sha1 SHADigest *~