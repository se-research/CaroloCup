/*
 * Copyright (c) Christian Berger and Bernhard Rumpe, Rheinisch-Westfaelische Technische Hochschule Aachen, Germany.
 *
 * The Hesperia Framework.
 */

#ifndef HESPERIA_CORE_WRAPPER_STRINGPIPELINE_H_
#define HESPERIA_CORE_WRAPPER_STRINGPIPELINE_H_

#include <queue>
#include <string>

#include "core/wrapper/Condition.h"
#include "core/wrapper/Mutex.h"
#include "core/wrapper/Runnable.h"
#include "core/wrapper/StringObserver.h"
#include "core/wrapper/StringListener.h"
#include "core/wrapper/Thread.h"

namespace core {
    namespace wrapper {

        using namespace std;

        /**
         * This class distributes strings using an asynchronous pipeline.
         */
        class StringPipeline : public Runnable, public StringObserver, public StringListener {
            private:
                /**
                 * "Forbidden" copy constructor. Goal: The compiler should warn
                 * already at compile time for unwanted bugs caused by any misuse
                 * of the copy constructor.
                 */
                StringPipeline(const StringPipeline &);

                /**
                 * "Forbidden" assignment operator. Goal: The compiler should warn
                 * already at compile time for unwanted bugs caused by any misuse
                 * of the assignment operator.
                 */
                StringPipeline& operator=(const StringPipeline &);

            public:
                StringPipeline();

                virtual ~StringPipeline();

                virtual void setStringListener(StringListener *sl);

                virtual void nextString(const string &s);

                /**
                 * This method starts the string pipeline.
                 */
                void start();

                /**
                 * This method stops the string pipeline.
                 */
                void stop();

            private:
                Condition *m_queueCondition;
                Mutex *m_queueMutex;
                queue<string> m_queue;

                Mutex *m_stringListenerMutex;
                StringListener *m_stringListener;

                Thread *m_thread;

                Mutex *m_threadStateMutex;
                bool m_threadState;

                /**
                 * This method changes the thread's state.
                 *
                 * @param state True iff the StringPipeline's thread is running.
                 */
                void setRunning(const bool &state);

                virtual void run();

                virtual bool isRunning();
        };

    }
} // core::wrapper

#endif /*HESPERIA_CORE_WRAPPER_STRINGPIPELINE_H_*/
