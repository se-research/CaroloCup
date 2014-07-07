/*
 * OpenDaVINCI.
 *
 * This software is open source. Please see COPYING and AUTHORS for further information.
 */

#ifndef OPENDAVINCI_CORE_MACROS_H_
#define OPENDAVINCI_CORE_MACROS_H_

/* This macro eases the declaration of exceptions. */
#define OPENDAVINCI_CORE_DECLARE_EXCEPTION(ExceptionName) class OPENDAVINCI_API ExceptionName : public Exceptions { public: ExceptionName(const string &exceptionMessage, const string &fileName, const uint32_t &lineNumber) : Exceptions(exceptionMessage, fileName, lineNumber) {}; const string getExceptionName() const { return "" #ExceptionName ""; } };

/* This macro eases the usage of exceptions. */
#define OPENDAVINCI_CORE_THROW_EXCEPTION(ExceptionClass, ExceptionMessage) do { throw core::exceptions::ExceptionClass(ExceptionMessage, __FILE__, __LINE__); } while (false)

/* This macro eases the usage of freeing a pointer. */
#define OPENDAVINCI_CORE_FREE_POINTER(ptr) do { if (ptr != NULL) { free(ptr); }; ptr = NULL; } while (false)

/* This macro eases the usage of deleting a pointer. */
#define OPENDAVINCI_CORE_DELETE_POINTER(ptr) do { if (ptr != NULL) { delete(ptr); }; ptr = NULL; } while (false)

/* This macro eases the usage of deleting an array. */
#define OPENDAVINCI_CORE_DELETE_ARRAY(ptr) do { if (ptr != NULL) { delete [] (ptr); }; ptr = NULL; } while (false)

#endif /*OPENDAVINCI_CORE_MACROS_H_*/
