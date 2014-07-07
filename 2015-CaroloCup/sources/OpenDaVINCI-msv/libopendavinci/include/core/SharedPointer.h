/*
 * OpenDaVINCI.
 *
 * This software is open source. Please see COPYING and AUTHORS for further information.
 */

#ifndef OPENDAVINCI_CORE_SHAREDPOINTER_H_
#define OPENDAVINCI_CORE_SHAREDPOINTER_H_

// core/platform.h must be included to setup platform-dependent header files and configurations.
#include "core/platform.h"

namespace core {

    /**
     * This class can be used to share pointers to dynamically allocated memory
     * that should be released automagically. Contrary to auto_ptr from STL,
     * it can be used in STL containers. It can be used as following:
     *
     * SharedPointer<TestData> p1(new TestData());
     * p1->val = 10;
     *
     * vector<SharedPointer<TestData> > listOfPtrs;
     * SharedPointer<TestData> p2(new TestData());
     * listOfPtrs.push_back(p2);
     * ...
     */
    template<class T>
    class SharedPointer {
        public:
            typedef T elementType;

            /**
             * Constructor.
             *
             * @param pointer Pointer to the type T.
             */
            explicit SharedPointer(T *pointer = NULL) :
                    m_countablePointer(NULL) {
                if (pointer != NULL) {
                    m_countablePointer = new CountablePointer(pointer);
                }
            }

            ~SharedPointer() {
                release();
            }

            /**
             * This method returns true if this shared pointer
             * contains a valid (i.e. != NULL) pointer.
             *
             * @return true if (*this).operator->() != NULL.
             */
            bool isValid() const {
                return ( (m_countablePointer != NULL) && (m_countablePointer->m_pointer != NULL) );
            }

            /**
             * Copy constructor. If this instance gets copied the counter
             * will be incremented.
             *
             * @param obj Reference to an instance of this class.
             */
            SharedPointer(const SharedPointer &obj) throw() :
                    m_countablePointer(NULL) {
                acquire(obj.m_countablePointer);
            }

            /**
             * Assignment operator. If this instance gets assigned,
             * its old referring instance is released and the new
             * one given by obj is acquired.
             *
             * @param obj Object of this class.
             * @return Reference to itself.
             */
            SharedPointer& operator=(const SharedPointer &obj) {
                if (this != &obj) {
                    // Release reference to old pointer.
                    release();

                    // Acquire a counted reference to the new one.
                    acquire(obj.m_countablePointer);
                }
                return (*this);
            }

            /**
             * This method passes access to the actual pointer.
             *
             * @return Access to the value pointed by the contained pointer.
             */
            T& operator*() const throw() {
                return (*(m_countablePointer->m_pointer));
            }

            /**
             * This method passes access to the actual pointer.
             *
             * @return Access to the contained pointer.
             */
            T* operator->() const throw() {
                return (m_countablePointer->m_pointer);
            }

            /**
             * This method decrements the counter to the given pointer.
             * If no more references exist, the contained pointer gets freed.
             */
            void release() {
                if ( (m_countablePointer != NULL) && (m_countablePointer->m_counter > 0) ) {
                    m_countablePointer->m_counter--;
                    if (m_countablePointer->m_counter == 0) {
                        OPENDAVINCI_CORE_DELETE_POINTER(m_countablePointer->m_pointer);
                        OPENDAVINCI_CORE_DELETE_POINTER(m_countablePointer);
                    }
                    // Reset own reference.
                    m_countablePointer = NULL;
                }
            }

        private:
            /**
             * This class is a simple data structure for keeping
             * and counting references to a given pointer.
             */
            class CountablePointer {
                public:
                    /**
                     * Constructor. The references counter gets initialized by 1.
                     *
                     * @param pointer Pointer.
                     */
                    CountablePointer(T *pointer = NULL) :
                            m_pointer(pointer),
                            m_counter(1) {}

                public:
                    T* m_pointer;
                    uint32_t m_counter;
            };

            // Pointer to the data structure previously defined.
            CountablePointer *m_countablePointer;

            /**
             * This method acquires a given pointer.
             *
             * @param countablePointer CountablePointer describing the references to the contained pointer.
             */
            void acquire(CountablePointer *countablePointer) throw() {
                m_countablePointer = countablePointer;
                if (m_countablePointer != NULL) {
                    m_countablePointer->m_counter++;
                }
            }
    };
}

#endif /*OPENDAVINCI_CORE_SHAREDPOINTER_H_*/
