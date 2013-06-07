/*
 * OpenDaVINCI.
 *
 * This software is open source. Please see COPYING and AUTHORS for further information.
 */

#ifndef OPENDAVINCI_CORE_PLATFORM_H_
#define OPENDAVINCI_CORE_PLATFORM_H_

#include "core/native.h"

/**************************************************************************************/

#ifndef WIN32
	/*
	 * POSIX types.
	 */
	#include <stdint.h>

	/*
	 * POSIX network.
	 */
	#include <netdb.h>
	#include <arpa/inet.h>
	#include <netinet/in.h>
	#include <sys/socket.h>
	#include <sys/select.h>
	#include <sys/types.h>

	/*
	 * POSIX IPC.
	 */
	#include <pthread.h>
	#include <semaphore.h>
	#include <sys/ipc.h>
	#include <sys/shm.h>
	#include <sys/stat.h>

	/*
	 * Other POSIX stuff.
	 */
	#include <fcntl.h>
	#include <unistd.h>
	#include <sys/time.h>

	/* Use regular unlink. */
	#define UNLINK unlink
	
#elif WIN32

	/* Prevent error C4003. */
	#define NOMINMAX

	/* Unlink causes an error under Windows. */
	#define UNLINK _unlink
	
	#include <Winsock2.h>
	#include "platform/win/stdint.h"

#endif

/**************************************************************************************/

/*
 * C++ wrapping headers for C headers.
 */
#include <cassert>
#include <cerrno>
#include <cmath>
#include <csignal>
#include <cstdio>
#include <cstdlib>
#include <cstring>
#include <ctime>

/*
 * C++ headers.
 */
#include <algorithm>
#include <deque>
#include <fstream>
#include <functional>
#include <iomanip>
#include <iostream>
#include <limits>
#include <map>
#include <memory>
#include <queue>
#include <sstream>
#include <string>
#include <vector>

/*
 * Basic header for OpenDaVINCI macros.
 */
#include "core/macros.h"
#include "core/StringToolbox.h"

#endif // OPENDAVINCI_CORE_PLATFORM_H_
