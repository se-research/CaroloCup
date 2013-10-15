/*
 * OpenDaVINCI.
 *
 * This software is open source. Please see COPYING and AUTHORS for further information.
 */

#include "core/exceptions/Exceptions.h"

namespace core {
    namespace exceptions {

        using namespace std;

        Exceptions::Exceptions(const string &exceptionMessage, const string &fileName, const uint32_t &lineNumber) :
                m_message(exceptionMessage),
                m_fileName(fileName),
                m_lineNumber(lineNumber),
                m_whatMessage(NULL) {
            if ("" == exceptionMessage) {
                m_message = "unknown exception message";
            }

            // Try to get the error's reason.
            if (errno != 0) {
#ifdef WIN32
                char buffer[1024];
                strerror_s(buffer, 1024, errno);
                string errorMessage(buffer);
#else
                string errorMessage(strerror(errno));
#endif
                m_message += ": " + errorMessage;
            }
        }

        Exceptions::~Exceptions() throw () {
            OPENDAVINCI_CORE_FREE_POINTER(m_whatMessage);
        }

        Exceptions::Exceptions(const Exceptions& obj) :
                exception(obj),
                m_message(obj.getMessage()),
                m_fileName(obj.getFileName()),
                m_lineNumber(obj.getLineNumber()),
                m_whatMessage(NULL) {}

        Exceptions& Exceptions::operator=(const Exceptions& obj) {
            exception::operator=(obj);
            OPENDAVINCI_CORE_FREE_POINTER(m_whatMessage);

            m_message = obj.getMessage();
            m_fileName = obj.getFileName();
            m_lineNumber = obj.getLineNumber();

            return *this;
        }

        const char* Exceptions::what() const throw () {
            if (m_whatMessage == NULL) {
                // strdup creates a pointer to NEW memory area.
#ifdef WIN32
                m_whatMessage = ::_strdup(toString().c_str());
#else
                m_whatMessage = ::strdup(toString().c_str());
#endif
            }
            return m_whatMessage;
        }

        const string Exceptions::toString() const {
            ostringstream out;
            out << getExceptionName() << ": " << getMessage() << " at " << getFileName() << ": " << getLineNumber();
            return out.str();
        }

        const string Exceptions::getMessage() const {
            return m_message;
        }

        const string Exceptions::getFileName() const {
            return m_fileName;
        }

        uint32_t Exceptions::getLineNumber() const {
            return m_lineNumber;
        }

    }
} // core::exceptions
