/*
 * OpenDaVINCI.
 *
 * This software is open source. Please see COPYING and AUTHORS for further information.
 */

#ifndef OPENDAVINCI_CORE_NATIVE_H_
#define OPENDAVINCI_CORE_NATIVE_H_

/**
 * The following defines are needed for building a dynamic library using
 * the Microsoft C++ compiler or the GNU compiler collection.
 */
#ifdef _WIN32
    // Disable warning "__declspec(nothrow)..."
    #pragma warning( disable : 4290 )
    // Disable warning "Possible loss of precision"
    #pragma warning( disable : 4244 )
    // Disable warning "DLL interface..."
    #pragma warning( disable : 4251 )
    // Disable warning "this pointer during initialization..."
    #pragma warning( disable : 4355 )
    // Disable warning "'dynamic_cast' for polymorphic type..."
    #pragma warning( disable : 4541 )
    // Disable warning "Not all control paths..."
    #pragma warning( disable : 4715 )
    // Disable warning "C++ exceptio specification ignored..."
    #pragma warning( disable : 4290 )
    // Disable warning "Missing dllimport for std::exception..."
    #pragma warning( disable : 4275 )
    // Disable warning "new behavior: elements of array will be default initialized..."
    #pragma warning( disable : 4351 )

    // Link to ws2_32.lib to get symbol "htonl and ntohl".
    #pragma comment(lib, "ws2_32.lib")

    // Define Windows 7.
    #ifndef _WIN32_WINNT
        #define _WIN32_WINNT 0x0601
    #endif
    #define WIN32_LEAN_AND_MEAN

    #ifdef OPENDAVINCI_SHARED
        #ifdef OPENDAVINCI_EXPORTS
            #define OPENDAVINCI_API __declspec(dllexport)
        #else
            #define OPENDAVINCI_API __declspec(dllimport)
        #endif
    #else
        // In the case of static linking:
        #define OPENDAVINCI_API
    #endif
#else // Not _WIN32 (i.e. LINUX or something else)
    #define OPENDAVINCI_API
#endif // _WIN32

#endif // OPENDAVINCI_CORE_NATIVE_H_
