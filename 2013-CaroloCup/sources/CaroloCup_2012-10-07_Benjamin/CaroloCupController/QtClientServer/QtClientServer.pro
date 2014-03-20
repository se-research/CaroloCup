# -------------------------------------------------
# Project created by QtCreator 2012-11-29T19:09:28
# -------------------------------------------------
QT += network
QT -= gui
TARGET = QtClientServer
CONFIG += console
CONFIG -= app_bundle
TEMPLATE = app
SOURCES += main.cpp \
    packetinterface.cpp \
    udpserver.cpp \
    locpoint.cpp
HEADERS += packetinterface.h \
    udpserver.h \
    locpoint.h
