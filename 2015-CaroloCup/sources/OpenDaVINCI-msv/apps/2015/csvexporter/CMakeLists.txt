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
FILE(GLOB_RECURSE csvexporter-sources "${CMAKE_CURRENT_SOURCE_DIR}/src/CsvExporter.cpp")
ADD_LIBRARY (csvexporterlib STATIC ${csvexporter-sources})
ADD_EXECUTABLE (csvexporter "${CMAKE_CURRENT_SOURCE_DIR}/apps/MainModule.cpp")
TARGET_LINK_LIBRARIES (csvexporter csvexporterlib ${MSVDATA_LIBS} ${OPENDAVINCI_LIBS} ${LIBS} ) 

# Recipe for building "csvexportersensor".
FILE(GLOB_RECURSE csvexportersensors-sources "${CMAKE_CURRENT_SOURCE_DIR}/src/CsvExporterSensors.cpp")
ADD_LIBRARY (csvexportersensorslib STATIC ${csvexportersensors-sources})
ADD_EXECUTABLE (csvexportersensors "${CMAKE_CURRENT_SOURCE_DIR}/apps/SensorModule.cpp")
TARGET_LINK_LIBRARIES (csvexportersensors csvexportersensorslib ${MSVDATA_LIBS} ${OPENDAVINCI_LIBS} ${LIBS} )

# Recipe for installing "csvexporter and csvexportersensor".
INSTALL(TARGETS csvexporter RUNTIME DESTINATION bin/2013/DIT-168/project-template)
INSTALL(TARGETS csvexportersensors RUNTIME DESTINATION bin/2013/DIT-168/project-template)