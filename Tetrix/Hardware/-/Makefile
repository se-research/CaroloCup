 ##/********************************************************
 ## * Author:    Zlatan Habul
 ## * Created:   27.01.2014
 ## * Makefile
 ## *
 ## ********************************************************/

all: carControl

CC       ?= gcc


LBITS := $(shell getconf LONG_BIT)
ifeq ($(LBITS),64)
   CCFLAGS ?= -Wall -g -lpthread
   MAKENIF ?= g++ -fPIC -shared -o pcbnif.so pcbnif.cpp carControl.c  -I /usr/lib64/erlang/usr/include/ $(INCLUDES) $(LIBS)
else
   CCFLAGS   ?= -Wall -g
   MAKENIF ?= g++ -fPIC -shared -o pcbnif.so pcbnif.cpp carControl.c
endif


COBJS   = carControl.o
OBJS      = $(COBJS) $(CPPOBJS)
LIBS      = `pkg-config --libs`
INCLUDES ?= -I. `pkg-config --cflags `


carControl: $(OBJS)
	$(CC) $(CCFLAGS) $(LDFLAGS) $^ -o carControl -lpthread
	$(MAKENIF)

clean:
	rm -f $(OBJS) carControl pcbnif.so

.PHONY: clean

