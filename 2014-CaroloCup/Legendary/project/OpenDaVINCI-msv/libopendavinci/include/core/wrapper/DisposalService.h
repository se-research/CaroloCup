/*
 * OpenDaVINCI.
 *
 * This software is open source. Please see COPYING and AUTHORS for further information.
 */

#ifndef OPENDAVINCI_CORE_WRAPPER_DISPOSALSERVICE_H_
#define OPENDAVINCI_CORE_WRAPPER_DISPOSALSERVICE_H_

// core/platform.h must be included to setup platform-dependent header files and configurations.
#include "core/platform.h"

#include "core/wrapper/Condition.h"
#include "core/wrapper/Disposable.h"
#include "core/wrapper/Mutex.h"
#include "core/wrapper/Runnable.h"
#include "core/wrapper/Thread.h"

namespace core {
    namespace wrapper {

        using namespace std;

        /**
         * Disposal service for cleaning up.
         */
        class OPENDAVINCI_API DisposalService {
            private:
                /**
                 * "Forbidden" copy constructor. Goal: The compiler should warn
                 * already at compile time for unwanted bugs caused by any misuse
                 * of the copy constructor.
                 */
                DisposalService(const DisposalService &);

                /**
                 * "Forbidden" assignment operator. Goal: The compiler should warn
                 * already at compile time for unwanted bugs caused by any misuse
                 * of the assignment operator.
                 */
                DisposalService& operator=(const DisposalService &);

            protected:
                DisposalService();

            public:
                virtual ~DisposalService();

                /**
                 * Singleton getter.
                 *
                 * @return Instance of the concrete factory.
                 */
                static DisposalService& getInstance();

                /**
                 * This method cleans up finally all existing
                 * instances and should be called inside atexit().
                 */
                void cleanUpFinally();

                /**
                 * This method adds a pointer to get deleted next time
                 * the regular cleaner is running.
                 *
                 * @param d Disposable to delete.
                 */
                void addDisposableForRegularRemoval(Disposable **d);

                /**
                 * This method adds a pointer to get deleted when
                 * closing the program.
                 *
                 * @param d Disposable to delete.
                 */
                void addDisposableForFinalRemoval(Disposable **d);

                /**
                 * This method invokes an immediate cleanup pass.
                 */
                void cleanUpImmediately();

            private:
                static Mutex *m_singletonMutex;
                static DisposalService *m_singleton;

                Condition *m_queueCondition;
                Mutex *m_finalRemovalMutex;
                Mutex *m_queueMutex;
                deque<Disposable**> m_queueForRegularRemoval;
                deque<Disposable**> m_queueForFinalRemoval;

                Thread *m_thread;

                /**
                 * This class is responsible for cleaning up the queue.
                 */
                class QueueCleaner : public Runnable {
                    private:
                        /**
                         * "Forbidden" copy constructor. Goal: The compiler should warn
                         * already at compile time for unwanted bugs caused by any misuse
                         * of the copy constructor.
                         */
                        QueueCleaner(const QueueCleaner &);

                        /**
                         * "Forbidden" assignment operator. Goal: The compiler should warn
                         * already at compile time for unwanted bugs caused by any misuse
                         * of the assignment operator.
                         */
                        QueueCleaner& operator=(const QueueCleaner &);

                    public:
                        /**
                         * Constructor.
                         *
                         * @param condition Condition for getting informed about new entries in the queue.
                         * @param finalRemovalMutex Mutex for locking the final removal.
                         * @param mutex Mutex for locking the queue.
                         * @param queue Queue with entries.
                         */
                        QueueCleaner(Condition &condition, Mutex &finalRemovalMutex, Mutex &mutex, deque<Disposable**> &queue);

                        virtual ~QueueCleaner();

                        /**
                         * This method controls the state of this thread.
                         *
                         * @param b True iff this thread is running.
                         */
                        void setRunning(const bool &b);

                    private:
                        Condition &m_queueCondition;
                        Mutex &m_finalRemovalMutex;
                        Mutex &m_queueMutex;
                        deque<Disposable**> &m_queue;

                        Mutex *m_threadStateMutex;
                        bool m_threadState;

                        virtual void run();

                        virtual bool isRunning();
                } *m_queueCleaner;
        };

    }
} // core::wrapper

#endif /*OPENDAVINCI_CORE_WRAPPER_DISPOSALSERVICE_H_*/
