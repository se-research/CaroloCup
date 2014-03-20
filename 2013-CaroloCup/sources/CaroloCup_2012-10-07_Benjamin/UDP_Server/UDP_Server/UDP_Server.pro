#-------------------------------------------------
#
# Project created by QtCreator 2012-06-30T18:23:59
#
#-------------------------------------------------

QT       += core
QT       -= gui
QT       += network

TARGET = UDP_Server
CONFIG   += console
CONFIG   -= app_bundle

TEMPLATE = app


SOURCES += main.cpp \
    serialport.cpp \
    udpserver.cpp

HEADERS += \
    serialport.h \
    udpserver.h
