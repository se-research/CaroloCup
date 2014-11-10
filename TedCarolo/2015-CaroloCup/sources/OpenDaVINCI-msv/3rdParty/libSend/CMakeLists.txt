#
# OpenDaVINCI.
#
# This software is open source. Please see COPYING and AUTHORS for further information.
#

PROJECT (libSend)

INCLUDE_DIRECTORIES (${libSend_SOURCE_DIR}/include)
INCLUDE_DIRECTORIES (include)

SET (libSend-sources
	src/libsend.pb.c	
	src/pb_decode.c
	src/pb_encode.c
)

ADD_LIBRARY (libSend STATIC ${libSend-sources})
