/*
 * OpenDaVINCI.
 *
 * This software is open source. Please see COPYING and AUTHORS for further information.
 */

#ifndef OPENDAVINCI_CORE_DATA_SERIALIZABLEDATA_H_
#define OPENDAVINCI_CORE_DATA_SERIALIZABLEDATA_H_

// core/platform.h must be included to setup platform-dependent header files and configurations.
#include "core/platform.h"
#include "core/base/Serializable.h"

namespace core {
    namespace data {

        using namespace std;

        /**
         * This class is the superclass for all data.
         */
        class OPENDAVINCI_API SerializableData : public core::base::Serializable {
            public:
                SerializableData();

                virtual ~SerializableData();

                /**
                 * This method returns a human readable format
                 * of the contained data.
                 *
                 * @return Human readable representation.
                 */
                virtual const string toString() const = 0;
        };

    }
} // core::data

#endif /*OPENDAVINCI_CORE_DATA_SERIALIZABLEDATA_H_*/
