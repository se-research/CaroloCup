#
# qstlink2 - tool for transferring binary images to STM32F4 Discovery board.
#
# This software is open source. Please see COPYING and AUTHORS for further information.
#

PROJECT (qstlink2)

# Qt4 libraries.
FIND_PACKAGE(Qt4 REQUIRED)

# libUSB library.
FIND_PACKAGE(Usb REQUIRED)

# Include current build directory to include generated header files.
INCLUDE_DIRECTORIES(${CMAKE_CURRENT_BINARY_DIR})
INCLUDE_DIRECTORIES (include)

# Libraries to link against.
SET (LIBS ${LIBS} pthread usb QtXml QtGui QtCore)

SET (CMAKE_C_FLAGS "-pipe -ansi -Wall -W -D_REENTRANT -DQT_WEBKIT -DQT_NO_DEBUG -DQT_XML_LIB -DQT_GUI_LIB -DQT_CORE_LIB -DQT_SHARED")
SET (CMAKE_CXX_FLAGS "-pipe -ansi -Wall -W -D_REENTRANT -DQT_WEBKIT -DQT_NO_DEBUG -DQT_XML_LIB -DQT_GUI_LIB -DQT_CORE_LIB -DQT_SHARED")

# Recipe for building "qstlink2".
SET(qstlink2_SOURCES src/main.cpp src/mainwindow.cpp src/stlinkv2.cpp src/LibUsb.cpp src/devices.cpp src/dialog.cpp src/transferthread.cpp)
SET(qstlink2_HEADERS include/LibUsb.h include/libusbwrapper.h)
SET(qstlink2_HEADERS_FOR_MOC include/devices.h include/dialog.h include/mainwindow.h include/transferthread.h include/stlinkv2.h)
SET(qstlink2_RESOURCES res/ressources.qrc)
SET(qstlink2_FORMS forms/mainwindow.ui forms/dialog.ui)

# Generate the meta object files.
QT4_WRAP_CPP(qstlink2_HEADERS_MOC ${qstlink2_HEADERS_FOR_MOC})
QT4_WRAP_UI(qstlink2_FORMS_HEADERS ${qstlink2_FORMS})
QT4_ADD_RESOURCES(qstlink2_RESOURCES_RCC ${qstlink2_RESOURCES})
SET(QT_USE_QTXML TRUE)

# Include the Qt4 libraries definitions.
INCLUDE(${QT_USE_FILE})
ADD_DEFINITIONS(${QT_DEFINITIONS})

ADD_EXECUTABLE(qstlink2 ${qstlink2_SOURCES} ${qstlink2_HEADERS} ${qstlink2_HEADERS_MOC} ${qstlink2_FORMS_HEADERS} ${qstlink2_RESOURCES_RCC})
TARGET_LINK_LIBRARIES(qstlink2 ${LIBS})

# Recipe for installing "qstlink2".
INSTALL(TARGETS qstlink2 RUNTIME DESTINATION bin) 
INSTALL_FILES(/bin FILES res/devices.xml)

# Recipe for installing udev configuration file.
ADD_CUSTOM_TARGET(install-udev
    COMMAND cp ${CMAKE_CURRENT_SOURCE_DIR}/udev/49-stlinkv2.rules /etc/udev/rules.d
)


