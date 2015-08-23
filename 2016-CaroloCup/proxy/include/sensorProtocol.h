/*
 * sensorProtocol.h
 *
 *  Created on: Nov 18, 2014
 *      Author: ashfaq
 */

#ifndef SENSORPROTOCOL_H_
#define SENSORPROTOCOL_H_

#include "core/platform.h"

#include <sstream>

#include "core/wrapper/Mutex.h"
#include "core/io/protocol/AbstractProtocol.h"
#include "core/io/StringObserver.h"
#include "core/io/StringSender.h"

namespace msv {
    using namespace core::wrapper;
	using namespace core::io;
    using namespace std;

    class sensorProtocol : public StringObserver,public StringListener {
    	private:
            /**
             * "Forbidden" copy constructor. Goal: The compiler should warn
             * already at compile time for unwanted bugs caused by any misuse
             * of the copy constructor.
             */
			sensorProtocol(const sensorProtocol &);

            /**
             * "Forbidden" assignment operator. Goal: The compiler should warn
             * already at compile time for unwanted bugs caused by any misuse
             * of the assignment operator.
             */
			sensorProtocol& operator=(const sensorProtocol &);

        public:
            /**
             * Constructor.
             */
    		sensorProtocol();

            virtual ~sensorProtocol();

            /**
             * This method sets the StringListener that will receive
             * incoming data.
             *
             * @param listener StringListener that will receive incoming data.
             */
            void setStringListener(StringListener* listener);

            virtual void nextString(const string &s);

        private:
            bool hasCompleteData();

            /**
             * This method is used to pass received data thread-safe
             * to the registered StringListener.
             */
            void invokeStringListener(const string& data);

            auto_ptr<Mutex> m_stringListenerMutex;
            StringListener *m_stringListener;

            stringstream m_partialData;
    };
}



#endif /* SENSORPROTOCOL_H_ */
