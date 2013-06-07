from SCons.Script import *
import os.path

# HESPERIA Configuration File for Version Numbering
#
# Versionsnummer des Builds. Diese wird per Hand festgelegt.
# Die Versionsnummer wird als Postfix der Form MAJOR.MINOR.REVISION
# fuer erstellte Bibliotheken und das Include-Verzeichnis im Install
# -Verzeichnis verwendet.
VERSION = { 'MAJOR' : 0, 'MINOR' : 1, 'REVISION' : 3 }

#DO NOT CHANGE ANY LINES BELOW!
def getVersion():
    return VERSION

def getVersionMajor():
    return VERSION['MAJOR']

def getVersionMinor():
    return VERSION['MINOR']

def getVersionRevision():
    return VERSION['REVISION']

def getVersionString():
    return str(VERSION['MAJOR']) + '.' + str(VERSION['MINOR']) + '.' + str(VERSION['REVISION'])

def update(env):
    pass

def getSVNRevision(directory):
    return None

def getSVNModifiedState(directory):
    return False

def generateVersionHeader(target, template, revisionSVN, modifiedSVN):#, revisionData, modifiedData):
    version_info = { "SUBVERSIONREVISION" : "",
                     "MAJOR" : getVersionMajor(),
                     "MINOR" : getVersionMinor(),
                     "REVISION" : getVersionRevision() }
                     #"SUBVERSIONDATAREVISION" : revisionData + modifiedData }
    
    version_h = file(str(target), "w")
    version_template = file(str(template), "r")
    version_h.write(version_template.read() % version_info)
    version_h.close()
    version_template.close()

