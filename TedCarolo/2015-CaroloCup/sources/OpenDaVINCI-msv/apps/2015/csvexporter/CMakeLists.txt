#
# Mini-Smart-Vehicles.
#
# This software is open source. Please see COPYING and AUTHORS for further information.
#

PROJECT (2015-Carolo-CsvExporter)

# Include directories from core.
INCLUDE_DIRECTORIES (${libopendavinci_SOURCE_DIR}/include)
INCLUDE_DIRECTORIES (include)
INCLUDE_DIRECTORIES (../msv-data/include)

# Recipe for building "csvexporter".
FILE(GLOB_RECURSE csvexporter-sources "${CMAKE_CURRENT_SOURCE_DIR}/src/*.cpp")
ADD_LIBRARY (csvexporterlib STATIC ${csvexporter-sources})
ADD_EXECUTABLE (csvexporter "${CMAKE_CURRENT_SOURCE_DIR}/apps/MainModule.cpp")
TARGET_LINK_LIBRARIES (csvexporter csvexporterlib ${MSVDATA_LIBS} ${OPENDAVINCI_LIBS} ${LIBS} ) 

# Recipe for installing "csvexporter".
INSTALL(TARGETS csvexporter RUNTIME DESTINATION bin/2013/DIT-168/project-template)