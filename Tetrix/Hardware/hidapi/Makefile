 ##/********************************************************
 ## * Author:    Zlatan Habul
 ## * Created:   28.10.2013
 ## * Makefile
 ## *
 ## ********************************************************/

all: carControl

CC       ?= gcc
CXX      ?= g++

LBITS := $(shell getconf LONG_BIT)
ifeq ($(LBITS),64)
   CXXFLAGS ?= -Wall -g -lpthread
   MAKENIF ?= gcc -fPIC -shared -lstdc++ -o hidnif.so hidnif.cpp carControl.o hid.o -I /usr/lib64/erlang/usr/include/ $(INCLUDES) $(LIBS)
else
   CXXFLAGS   ?= -Wall -g
   MAKENIF ?= g++ -o hidnif.so -fPIC -shared hidnif.cpp -lstdc++ carControl.o hid.o $(LIBS)
endif


COBJS     = hid.o
CPPOBJS   = carControl.o
OBJS      = $(COBJS) $(CPPOBJS)
LIBS      = `pkg-config libusb-1.0 libudev --libs`
INCLUDES ?= -I. `pkg-config libusb-1.0 --cflags `


carControl: $(OBJS)
	$(CXX) $(CXXFLAGS) $(LDFLAGS) $^ $(LIBS) -o carControl
	$(MAKENIF)


$(COBJS): %.o: %.c
	$(CC) $(CXXFLAGS) -c $(INCLUDES) $< -o $@ -fPIC 

$(CPPOBJS): %.o: %.cpp
	$(CXX) $(CXXFLAGS) -c $(INCLUDES) $< -o $@ -fPIC 

clean:
	rm -f $(OBJS) carControl hidnif.so

.PHONY: clean

nif:
	g++ -o hidnif.so -fPIC -shared hidnif.cpp -lstdc++ carControl.o hid.o -I /usr/lib64/erlang/usr/include/ $(LIBS)
