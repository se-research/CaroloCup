/*
 * OpenDaVINCI.
 *
 * This software is open source. Please see COPYING and AUTHORS for further information.
 */

#include "core/wrapper/POSIX/POSIXSharedMemory.h"

namespace core {
    namespace wrapper {
        namespace POSIX {

            using namespace std;

            POSIXSharedMemory::POSIXSharedMemory(const string &name, const uint32_t &size) :
                    m_name(name),
                    m_releaseSharedMemory(true),
                    m_shmID(0),
                    m_mutexSharedMemory(NULL),
                    m_sharedMemory(NULL),
                    m_size(size) {

                if (m_name.size() > 0) {
                    m_mutexSharedMemory = sem_open(m_name.c_str(), O_CREAT, S_IRUSR | S_IWUSR, 1);
                    if (m_mutexSharedMemory == SEM_FAILED) {
                        clog << "Semaphore could not be created." << endl;
                        sem_unlink(m_name.c_str());
                        m_mutexSharedMemory = NULL;
                    } else {
                        // Create the shared memory segment with this name.
                        const uint32_t hash = getCRC32(name);
                        m_shmID = shmget(hash, size + sizeof(uint32_t), IPC_CREAT | S_IRUSR | S_IWUSR);
                        if (m_shmID < 0) {
                            clog << "Shared memory could not be requested." << endl;
                            sem_unlink(m_name.c_str());
                            m_mutexSharedMemory = NULL;
                        } else {
                            // Attach to virtual memory and store its size to the beginning of the shared memory.
                            m_sharedMemory = shmat(m_shmID, NULL, 0);
                            *(uint32_t *) m_sharedMemory = m_size;
                        }
                    }
                }
            }

            POSIXSharedMemory::POSIXSharedMemory(const string &name) :
                    m_name(name),
                    m_releaseSharedMemory(false),
                    m_shmID(0),
                    m_mutexSharedMemory(NULL),
                    m_sharedMemory(NULL),
                    m_size(0) {

                if (m_name.size() > 0) {
                    m_mutexSharedMemory = sem_open(m_name.c_str(), 0, S_IRUSR | S_IWUSR, 0);
                    if (m_mutexSharedMemory == SEM_FAILED) {
                        clog << "Shared memory could not be created." << endl;
                        sem_close(m_mutexSharedMemory);
                        m_mutexSharedMemory = NULL;
                    } else {
                        // Create the shared memory segment with this key.
                        const uint32_t hash = getCRC32(name);
                        m_shmID = shmget(hash, sizeof(uint32_t), S_IRUSR | S_IWUSR);
                        if (m_shmID < 0) {
                            clog << "Intermediate shared memory could not be requested." << endl;
                            sem_close(m_mutexSharedMemory);
                            m_mutexSharedMemory = NULL;
                        } else {
                            // Attach to virtual memory and try to read its entire size.
                            m_sharedMemory = shmat(m_shmID, NULL, 0);
                            m_size = *(uint32_t *) m_sharedMemory;

                            // Detach and try to attach to entire size.
                            shmdt(m_sharedMemory);

                            m_shmID = shmget(hash, m_size + sizeof(uint32_t), S_IRUSR | S_IWUSR);
                            if (m_shmID < 0) {
                                clog << "Final shared memory could not be requested." << endl;
                                sem_close(m_mutexSharedMemory);
                                m_mutexSharedMemory = NULL;
                            } else {
                                // Attach this segment to virtual memory.
                                m_sharedMemory = shmat(m_shmID, NULL, 0);
                            }
                        }
                    }
                }
            }

            POSIXSharedMemory::~POSIXSharedMemory() {
                if (m_releaseSharedMemory) {
                    if (m_mutexSharedMemory != NULL) {
                        sem_close(m_mutexSharedMemory);
                    }
                    // Remove semaphore.
                    sem_unlink(m_name.c_str());

                    // Detach shared memory.
                    shmdt(m_sharedMemory);

                    // Remove shared memory if released by other processes.
                    shmctl(m_shmID, IPC_RMID, 0);
                } else {
                    shmdt(m_sharedMemory);
                }
            }

            bool POSIXSharedMemory::isValid() const {
                return ((m_mutexSharedMemory != NULL) && (m_sharedMemory != NULL));
            }

            const string POSIXSharedMemory::getName() const {
                return m_name;
            }

            void POSIXSharedMemory::lock() {
                sem_wait(m_mutexSharedMemory);
            }

            void POSIXSharedMemory::unlock() {
                sem_post(m_mutexSharedMemory);
            }

            void* POSIXSharedMemory::getSharedMemory() const {
                // Adjust the address of the shared memory's beginning.
                return static_cast<void*>(static_cast<char*>(m_sharedMemory) + sizeof(uint32_t));
            }

            uint32_t POSIXSharedMemory::getSize() const {
                return m_size;
            }

            uint32_t POSIXSharedMemory::getCRC32(const string &s) const {
                // The CRC32 polynomial.
                const uint32_t CRC32POLYNOMIAL = 0x04C11DB7;

                uint32_t retVal = 0;
                for (uint32_t i = 0; i < s.size(); i++) {
                    retVal = retVal ^ (s.at(i) ^ CRC32POLYNOMIAL);
                }

                return retVal;
            }

        }
    }
} // core::wrapper::POSIX
