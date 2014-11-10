/*
 * OpenDaVINCI.
 *
 * This software is open source. Please see COPYING and AUTHORS for further information.
 */

#ifndef OPENDAVINCI_CORE_WRAPPER_KEYVALUEDATABASELIBRARYPRODUCTS_H_
#define OPENDAVINCI_CORE_WRAPPER_KEYVALUEDATABASELIBRARYPRODUCTS_H_

namespace core {
    namespace wrapper {

        /**
         * This enumeration describes the avaiable
         * products of the KeyValueDatabaseFactory
         *
         * @See KeyValueDatabaseFactory,
         *      KeyValueDatabaseFactoryWorker
         */
        enum KeyValueDatabaseLibraryProducts
        {
                KeyValueDatabaseBerkeleyDB,
                KeyValueDatabaseSimpleDB
        };

    }
} // core::wrapper

#endif /*OPENDAVINCI_CORE_WRAPPER_KEYVALUEDATABASELIBRARYPRODUCTS_H_*/
