/*
 * Copyright (c) Christian Berger and Bernhard Rumpe, Rheinisch-Westfaelische Technische Hochschule Aachen, Germany.
 *
 * The Hesperia Framework.
 */

#ifndef HESPERIA_CORE_EXCEPTIONS_EXCEPTIONS_H_
#define HESPERIA_CORE_EXCEPTIONS_EXCEPTIONS_H_

#ifdef _WIN32
// Disable warning "Missing dllimport for std::exception..."
#pragma warning( disable : 4275 )
#endif

#include <cerrno>
#include <iostream>
#include <string>

// native.h must be included as first header file for definition of _WIN32_WINNT.
#include "core/native.h"

#include "core/macros.h"

namespace core {
    namespace exceptions {

        using namespace std;

        /**
         * This is the base class for all exceptions.
         * New exceptions are defined as follows:
         *
         * @code
         * HESPERIA_CORE_DECLARE_EXCEPTION(IAMTHEREASONException);
         * @endcode
         */
        class HESPERIA_API Exceptions: public exception {

            private:
                /**
                 * Forbidden default constructor.
                 */
                Exceptions():
                        m_message("undefined exception"),
                        m_fileName("undefined file"),
                        m_lineNumber(0),
                        m_whatMessage(NULL) {};

            public:
                /**
                 * Constructor for the exception class.
                 *
                 * The HESPERIA_CORE_THROW_EXCEPTION macro uses this signature for adding file name
                 * and line number. Every exception derived from this class has to use
                 * the same signature:
                 *
                 * @code
                 * class MyException : public exceptions::Exceptions {
                 *     public:
                 *         MyException(string exceptionMessage, string fileName, const uint32_t lineNumber)
                 *         : Exceptions(exceptionMessage, fileName, lineNumber) {};
                 * };
                 * @endcode
                 *
                 * @param exceptionMessage Message for this exception.
                 * @param fileName File name of the file where this exception occured.
                 * @param lineNumber Line number.
                 */
                Exceptions(const string &exceptionMessage, const string &fileName, const uint32_t &lineNumber);

                /**
                 * Copy constructor.
                 *
                 * @param obj Another exception.
                 */
                Exceptions(const Exceptions &obj);

                virtual ~Exceptions() throw ();

                /**
                 * Assignment operator.
                 *
                 * @param obj Another exception.
                 * @return Reference to this instance.
                 */
                Exceptions& operator=(const Exceptions &obj);

                /**
                 * This method re-implements the same interface as the standard C++ exception
                 * for overriding the that method.
                 *
                 * @return The exception's message.
                 */
                const char* what() const throw ();

                /**
                 * This method returns a string representation.
                 *
                 * @return String representation.
                 */
                const string toString() const;

                /**
                 * This method returns the name of the Exception.
                 *
                 * @return name of the exception.
                 */
                virtual const string getExceptionName() const = 0;

                /**
                 * This method returns this exception's message.
                 *
                 * @return This exception's message.
                 */
                const string getMessage() const;

                /**
                 * This method returns the file name.
                 *
                 * @return file name.
                 */
                const string getFileName() const;

                /**
                 * This method returns the line number.
                 *
                 * @return line number.
                 */
                uint32_t getLineNumber() const;

            private:
                string m_message;
                string m_fileName;
                uint32_t m_lineNumber;
                mutable char *m_whatMessage; // This attribute is mutable for allocation memory in the what()-message.
        };

        /* Exception declarations. */
        HESPERIA_CORE_DECLARE_EXCEPTION(ArrayIndexOutOfBoundsException);
        HESPERIA_CORE_DECLARE_EXCEPTION(CommandLineParserException);
        HESPERIA_CORE_DECLARE_EXCEPTION(ConferenceException);
        HESPERIA_CORE_DECLARE_EXCEPTION(ConfigurationFileNotFoundException);
        HESPERIA_CORE_DECLARE_EXCEPTION(ConnectException);
        HESPERIA_CORE_DECLARE_EXCEPTION(ConnectionException);
        HESPERIA_CORE_DECLARE_EXCEPTION(ConnectionAcceptorException);
        HESPERIA_CORE_DECLARE_EXCEPTION(InvalidArgumentException);
        HESPERIA_CORE_DECLARE_EXCEPTION(IOException);
        HESPERIA_CORE_DECLARE_EXCEPTION(MasterPlugInMultiInstanceException);
        HESPERIA_CORE_DECLARE_EXCEPTION(ModulesNotRespondingException);
        HESPERIA_CORE_DECLARE_EXCEPTION(NoDatabaseAvailableException);
        HESPERIA_CORE_DECLARE_EXCEPTION(RealtimeExecutionException);
        HESPERIA_CORE_DECLARE_EXCEPTION(RealtimeSetupException);
        HESPERIA_CORE_DECLARE_EXCEPTION(SingletonException);
        HESPERIA_CORE_DECLARE_EXCEPTION(SocketException);
        HESPERIA_CORE_DECLARE_EXCEPTION(TransformationException);
        HESPERIA_CORE_DECLARE_EXCEPTION(ThreadControlException);
        HESPERIA_CORE_DECLARE_EXCEPTION(ThreadException);
        HESPERIA_CORE_DECLARE_EXCEPTION(ToManyTestSenderStarted);
        HESPERIA_CORE_DECLARE_EXCEPTION(UnimplementedException);
        HESPERIA_CORE_DECLARE_EXCEPTION(ValueForKeyNotFoundException);
        HESPERIA_CORE_DECLARE_EXCEPTION(DMCPServerAlreadyStartedException);
        HESPERIA_CORE_DECLARE_EXCEPTION(DMCPServerNotFoundException);
        HESPERIA_CORE_DECLARE_EXCEPTION(DMCPServerConnectionException);
        HESPERIA_CORE_DECLARE_EXCEPTION(DMCPClientConfigurationException);
        HESPERIA_CORE_DECLARE_EXCEPTION(SCNXScenarioFileNotFoundException);
        HESPERIA_CORE_DECLARE_EXCEPTION(SCNScenarioVisitorException);
        HESPERIA_CORE_DECLARE_EXCEPTION(SITSituationVisitorException);

    }
} // core::exceptions

#endif /*HESPERIA_CORE_EXCEPTIONS_EXCEPTIONS_H_*/
