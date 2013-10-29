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
else
   CXXFLAGS   ?= -Wall -g
endif


COBJS     = hid.o
CPPOBJS   = carControl.o
OBJS      = $(COBJS) $(CPPOBJS)
LIBS      = `pkg-config libusb-1.0 libudev --libs`
INCLUDES ?= -I. `pkg-config libusb-1.0 --cflags`


carControl: $(OBJS)
	$(CXX) $(CXXFLAGS) $(LDFLAGS) $^ $(LIBS) -o carControl
	g++ -o hidnif.so -fpic -shared hidnif.cpp -lstdc++ carControl.o hid.o $(LIBS)

$(COBJS): %.o: %.c
	$(CC) $(CXXFLAGS) -c $(INCLUDES) $< -o $@

$(CPPOBJS): %.o: %.cpp
	$(CXX) $(CXXFLAGS) -c $(INCLUDES) $< -o $@

clean:
	rm -f $(OBJS) carControl hidnif.so

.PHONY: clean

nif:
	g++ -o hidnif.so -fpic -shared hidnif.cpp -lstdc++ carControl.o hid.o $(LIBS)
