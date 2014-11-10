#
# OpenDaVINCI.
#
# This software is open source. Please see COPYING and AUTHORS for further information.
#

PROJECT (libReceive)

INCLUDE_DIRECTORIES (${libReceive_SOURCE_DIR}/include)
INCLUDE_DIRECTORIES (include)

SET (libReceive-sources
	src/libreceive.pb.c	
	src/pb_decode.c
	src/pb_encode.c
)

ADD_LIBRARY (libReceive STATIC ${libReceive-sources})
