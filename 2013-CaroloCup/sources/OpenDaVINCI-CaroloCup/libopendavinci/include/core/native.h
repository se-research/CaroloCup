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
// Disable warning "DLL interface..."
#pragma warning( disable : 4251 )
// Disable warning "this pointer during initialization..."
#pragma warning( disable : 4355 )
// Disable warning "'dynamic_cast' f�r polymorphen Typ..."
#pragma warning( disable : 4541 )
// Disable warning "Nicht alle Steuerelementpfade..."
#pragma warning( disable : 4715 )
// Disable warning "C++-Ausnahmespezifikation ignoriert..."
#pragma warning( disable : 4290 )
// Disable warning "Missing dllimport for std::exception..."
#pragma warning( disable : 4275 )


// Define Windows XP.
#ifndef _WIN32_WINNT
#define _WIN32_WINNT 0x0501
#endif
#define WIN32_LEAN_AND_MEAN

#ifdef OPENDAVINCI_EXPORTS
#define OPENDAVINCI_API __declspec(dllexport)
#else
#define OPENDAVINCI_API __declspec(dllimport)
#endif
#else // Not _WIN32 (i.e. LINUX or something else)
#define OPENDAVINCI_API
#endif // _WIN32

#endif // OPENDAVINCI_CORE_NATIVE_H_
