 ##/********************************************************
 ## * Author:    Zlatan Habul
 ## * Created:   12.10.2013
 ## * Makefile
 ## *
 ## ********************************************************/

all: hidtest

CC       ?= gcc
CFLAGS   ?= -Wall -g

CXX      ?= g++
CXXFLAGS ?= -Wall -g -lpthread


COBJS     = hid.o
CPPOBJS   = hidtest.o
OBJS      = $(COBJS) $(CPPOBJS)
LIBS      = `pkg-config libusb-1.0 libudev --libs`
INCLUDES ?= -I. `pkg-config libusb-1.0 --cflags`


hidtest: $(OBJS)
	$(CXX) $(CXXFLAGS) $(LDFLAGS) $^ $(LIBS) -o hidtest

$(COBJS): %.o: %.c
	$(CC) $(CFLAGS) -c $(INCLUDES) $< -o $@

$(CPPOBJS): %.o: %.cpp
	$(CXX) $(CXXFLAGS) -c $(INCLUDES) $< -o $@

clean:
	rm -f $(OBJS) hidtest

.PHONY: clean
