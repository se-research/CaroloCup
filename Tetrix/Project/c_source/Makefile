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
   MAKENIF ?= gcc -fPIC -shared -lstdc++ -o ../ebin/hidnif.so hidnif.cpp ../ebin/carControl.o ../ebin/hid.o -I /usr/lib64/erlang/usr/include/ $(INCLUDES) $(LIBS)
else
   CXXFLAGS   ?= -Wall -g
   MAKENIF ?= g++ -o ../ebin/hidnif.so -fPIC -shared hidnif.cpp -lstdc++ ../ebin/carControl.o ../ebin/hid.o $(LIBS)
endif


COBJS     = hid.o
CPPOBJS   = carControl.o
OBJS      = $(COBJS) $(CPPOBJS)
LIBS      = `pkg-config libusb-1.0 libudev --libs`
INCLUDES ?= -I. `pkg-config libusb-1.0 --cflags `


carControl: $(OBJS)
	$(MAKENIF)
	g++ -fPIC -shared -o ../ebin/imgproc_nif.so imgproc_nif.cpp camera_functions.cpp -I../c_include/ -lopencv_core -lopencv_imgproc -lopencv_highgui -lueye_api



$(COBJS): %.o: %.c
	$(CC) $(CXXFLAGS) -c $(INCLUDES) $< -o ../ebin/hid.o -fPIC 

$(CPPOBJS): %.o: %.cpp
	$(CXX) $(CXXFLAGS) -c $(INCLUDES) $< -o ../ebin/carControl.o -fPIC 

clean:
	rm -f ../ebin/*.o ../ebin/*.so ../ebin/*.beam

.PHONY: clean

nif:
	$(MAKENIF)

img_proc: 
	g++ -fPIC -shared -o ../ebin/imgproc_nif.so imgproc_nif.cpp camera_functions.cpp -I../c_include/ -lopencv_core -lopencv_imgproc -lopencv_highgui -lueye_api
