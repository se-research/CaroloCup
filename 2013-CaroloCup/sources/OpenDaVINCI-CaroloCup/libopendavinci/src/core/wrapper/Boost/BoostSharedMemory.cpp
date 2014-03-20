/*
 * OpenDaVINCI.
 *
 * This software is open source. Please see COPYING and AUTHORS for further information.
 */

#include "core/wrapper/Boost/BoostSharedMemory.h"

namespace core {
    namespace wrapper {
        namespace Boost {

            using namespace std;

            BoostSharedMemory::BoostSharedMemory(const string &name, const uint32_t &size) :
                    m_name(name),
                    m_size(0),
                    m_valid(false),
                    m_removeSharedMemory(true),
                    m_sharedMemory(),
                    m_mappedRegionOfSharedMemory(),
                    m_sharedMemoryMutex(NULL) {

                if (name.size() > 0) {
                    try {
                        // Remove any previously existing shared memory.
                        boost::interprocess::shared_memory_object::remove(name.c_str());

                        // Create a shared memory object.
                        m_sharedMemory = SharedPointer<boost::interprocess::shared_memory_object>(new boost::interprocess::shared_memory_object(boost::interprocess::create_only, name.c_str(), boost::interprocess::read_write));

                        // Set size of shared memory.
                        if (m_sharedMemory.isValid()) {
                            m_sharedMemory->truncate(size + sizeof(boost::interprocess::interprocess_semaphore));

                            // Map the entire shared memory in this process.
                            m_mappedRegionOfSharedMemory = SharedPointer<boost::interprocess::mapped_region>(new boost::interprocess::mapped_region(*m_sharedMemory, boost::interprocess::read_write));

                            // Check validity.
                            m_valid = ( (m_mappedRegionOfSharedMemory.isValid()) && (m_mappedRegionOfSharedMemory->get_address() != NULL) && (m_mappedRegionOfSharedMemory->get_size() > 0) );

                            if (m_valid) {
                                m_size = m_mappedRegionOfSharedMemory->get_size() - sizeof(boost::interprocess::interprocess_semaphore);

                                // Create a new mutex at the beginning of the shared memory segment.
                                m_sharedMemoryMutex = new(m_mappedRegionOfSharedMemory->get_address()) boost::interprocess::interprocess_semaphore(1);
                            }
                        }
                    } catch (boost::interprocess::interprocess_exception &ie) {
                        boost::interprocess::shared_memory_object::remove(name.c_str());
                        clog << "Error while creating shared memory: " << ie.what() << endl;
                        m_valid = false;
                    }
                }
            }

            BoostSharedMemory::BoostSharedMemory(const string &name) :
                    m_name(name),
                    m_size(0),
                    m_valid(false),
                    m_removeSharedMemory(false),
                    m_sharedMemory(),
                    m_mappedRegionOfSharedMemory(),
                    m_sharedMemoryMutex(NULL) {

                if (name.size() > 0) {
                    try {
                        // Open a previously created shared memory.
                        m_sharedMemory = SharedPointer<boost::interprocess::shared_memory_object>(new boost::interprocess::shared_memory_object(boost::interprocess::open_only, name.c_str(), boost::interprocess::read_write));

                        if (m_sharedMemory.isValid()) {
                            // Map the entire shared memory in this process.
                            m_mappedRegionOfSharedMemory = SharedPointer<boost::interprocess::mapped_region>(new boost::interprocess::mapped_region(*m_sharedMemory, boost::interprocess::read_write));

                            // Check validity.
                            m_valid = ( (m_mappedRegionOfSharedMemory.isValid()) && (m_mappedRegionOfSharedMemory->get_address() != NULL) && (m_mappedRegionOfSharedMemory->get_size() > 0) );

                            if (m_valid) {
                                m_size = m_mappedRegionOfSharedMemory->get_size() - sizeof(boost::interprocess::interprocess_semaphore);

                                m_sharedMemoryMutex = static_cast<boost::interprocess::interprocess_semaphore*>(m_mappedRegionOfSharedMemory->get_address());
                            }
                        }
                    } catch (boost::interprocess::interprocess_exception &ie) {
                        clog << "Error while creating shared memory: " << ie.what() << endl;
                        m_valid = false;
                    }
                }
            }

            BoostSharedMemory::~BoostSharedMemory() {
                if (m_valid && m_removeSharedMemory) {
                    boost::interprocess::shared_memory_object::remove(m_name.c_str());
                }
            }

            bool BoostSharedMemory::isValid() const {
                return m_valid;
            }

            const string BoostSharedMemory::getName() const {
                return m_name;
            }

            void BoostSharedMemory::lock() {
                m_sharedMemoryMutex->wait();
            }

            void BoostSharedMemory::unlock() {
                m_sharedMemoryMutex->post();
            }

            void* BoostSharedMemory::getSharedMemory() const {
                // Adjust pointer and skip first bytes.
                char *ptr = static_cast<char*>(m_mappedRegionOfSharedMemory->get_address()) + sizeof(boost::interprocess::interprocess_semaphore);
                return (m_valid ? (static_cast<void*>(ptr)) : NULL);
            }

            uint32_t BoostSharedMemory::getSize() const {
                return m_size;
            }

        }
    }
} // core::wrapper::Boost
