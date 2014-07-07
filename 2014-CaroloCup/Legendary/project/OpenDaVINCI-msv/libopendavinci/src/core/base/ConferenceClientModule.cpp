/*
 * OpenDaVINCI.
 *
 * This software is open source. Please see COPYING and AUTHORS for further information.
 */

#include "core/base/Lock.h"
#include "core/io/ContainerConferenceFactory.h"
#include "core/wrapper/KeyValueDatabaseFactory.h"

#include "core/base/ConferenceClientModule.h"

namespace core {
    namespace base {

        using namespace std;

        using namespace core;
        using namespace core::base;
        using namespace core::data;
        using namespace core::io;
        using namespace core::exceptions;

        ConferenceClientModule::ConferenceClientModule(const int32_t &argc, char **argv, const string &name) throw (InvalidArgumentException, NoDatabaseAvailableException) :
                ClientModule(argc, argv, name),
                m_conference(NULL),
                m_dataStoresMutex(),
                m_listOfDataStores(),
                m_mapOfListOfDataStores(),
                m_keyValueDataStore() {
            // Create an in-memory database.
            m_keyValueDataStore = SharedPointer<KeyValueDataStore>(new KeyValueDataStore(wrapper::KeyValueDatabaseFactory::createKeyValueDatabase("")));

            // Create conference.
            m_conference = ContainerConferenceFactory::getInstance().getContainerConference(getMultiCastGroup());
            if (m_conference == NULL) {
                OPENDAVINCI_CORE_THROW_EXCEPTION(InvalidArgumentException, "ContainerConference invalid!");
            }

            // Register ourselves as ContainerListener.
            m_conference->setContainerListener(this);
        }

        ConferenceClientModule::~ConferenceClientModule() {
            m_conference->setContainerListener(NULL);

            OPENDAVINCI_CORE_DELETE_POINTER(m_conference);

            // Database will be cleaned up by SharedPointer.
            {
                Lock l(m_dataStoresMutex);
                m_listOfDataStores.clear();

                map<Container::DATATYPE, vector<AbstractDataStore*> >::iterator it = m_mapOfListOfDataStores.begin();
                while (it != m_mapOfListOfDataStores.end()) {
                    vector<AbstractDataStore*> listOfDataStores = it->second;
                    listOfDataStores.clear();
                    it++;
                }
                m_mapOfListOfDataStores.clear();
            }
        }

        void ConferenceClientModule::nextContainer(Container &c) {
            // Distribute data to datastores.
            {
                Lock l(m_dataStoresMutex);

                vector<AbstractDataStore*>::iterator it = m_listOfDataStores.begin();
                while (it != m_listOfDataStores.end()) {
                    AbstractDataStore *ads = (*it++);
                    if (ads != NULL) {
                        ads->add(c); // Currently waiting threads are awaken automagically.
                    }
                }

                vector<AbstractDataStore*> listOfDataStores = m_mapOfListOfDataStores[c.getDataType()];
                vector<AbstractDataStore*>::iterator jt = listOfDataStores.begin();
                while (jt != listOfDataStores.end()) {
                    AbstractDataStore *ads = (*jt++);
                    if (ads != NULL) {
                        ads->add(c); // Currently waiting threads are awaken automagically.
                    }
                }
            }

            // Store data using a plain map.
            m_keyValueDataStore->put(c.getDataType(), c);
        }

        ContainerConference& ConferenceClientModule::getConference() {
            return *m_conference;
        }

        void ConferenceClientModule::addDataStoreFor(AbstractDataStore &dataStore) {
            Lock l(m_dataStoresMutex);

            m_listOfDataStores.push_back(&dataStore);
        }

        void ConferenceClientModule::addDataStoreFor(const Container::DATATYPE &datatype, AbstractDataStore &dataStore) {
            Lock l(m_dataStoresMutex);

            vector<AbstractDataStore*> listOfDataStores = m_mapOfListOfDataStores[datatype];
            listOfDataStores.push_back(&dataStore);
            m_mapOfListOfDataStores[datatype] = listOfDataStores;
        }

        KeyValueDataStore& ConferenceClientModule::getKeyValueDataStore() {
            return *m_keyValueDataStore;
        }

    }
} // core::base
