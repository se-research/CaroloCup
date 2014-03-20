# -------------------------------------------------
# Project created by QtCreator 2012-06-30T21:00:25
# -------------------------------------------------
QT += core \
    gui
QT += network
QT += phonon
QT += opengl
TARGET = UdpRC
TEMPLATE = app
SOURCES += main.cpp \
    mainwindow.cpp \
    settingswidget.cpp \
    joystick.cpp \
    packetinterface.cpp \
    mapwidget.cpp \
    locpoint.cpp \
    carinfo.cpp \
    carcalculations.cpp
HEADERS += mainwindow.h \
    settingswidget.h \
    joystick.h \
    packetinterface.h \
    mapwidget.h \
    locpoint.h \
    carinfo.h \
    carcalculations.h
FORMS += mainwindow.ui \
    settingswidget.ui
CONFIG += link_pkgconfig
PKGCONFIG += opencv
