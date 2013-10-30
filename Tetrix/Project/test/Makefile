CXX = g++
INCLUDES= -I../c_include/
CXXFLAGS = -g $(INCLUDES)
SRCM= ../c_src/camera_functions.cpp
OBJM = $(SRCM:.cpp=.o)
LINKFLAGS= -lcppunit -lopencv_core -lopencv_highgui -lopencv_imgproc -lueye_api 

TestCameraFunctions: TestCameraFunctions.cpp $(OBJM)
	$(CXX) $(CXXFLAGS) -o $@ TestCameraFunctions.cpp $(OBJM) $(LINKFLAGS) $(LINKFLAGSLOG4) $(LIBLOG)  

clean:
	rm TestCameraFunctions

# Default compile

.cpp.o:
	$(CXX) $(CXXFLAGS) -c $< -o $@


