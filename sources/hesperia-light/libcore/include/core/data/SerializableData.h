/*
 * Copyright (c) Christian Berger and Bernhard Rumpe, Rheinisch-Westfaelische Technische Hochschule Aachen, Germany.
 *
 * The Hesperia Framework.
 */

#ifndef HESPERIA_CORE_DATA_SERIALIZABLEDATA_H_
#define HESPERIA_CORE_DATA_SERIALIZABLEDATA_H_

// native.h must be included as first header file for definition of _WIN32_WINNT.
#include "core/native.h"

#include "core/macros.h"
#include "core/version.h"
#include "core/base/Serializable.h"

namespace core {
    namespace data {

        using namespace std;

        /**
         * This class is the superclass for all data.
         */
        class HESPERIA_API SerializableData : public core::base::Serializable {
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

            public:
                /**
                 * This method returns the version of data as computed by
                 * compile time.
                 *
                 * @return Compile time version of data.
                 */
                inline string getDataVersion() const {
                    return HESPERIA_CORE_DATA_VERSION;
                };

                /**
                 * This method sets the version of the deserialized data.
                 *
                 * @param deserializedVersion Version of deserialized data.
                 */
                inline void setDeserializedVersion(const string &deserializedVersion) {
                    m_deserializedVersion = deserializedVersion;
                };

                /**
                 * This method returns the version of the deserialized data.
                 *
                 * @return Version of the deserialized data.
                 */
                inline string getDeserializedVersion() const {
                    return m_deserializedVersion;
                };

            private:
                string m_deserializedVersion;
        };

    }
} // core::data

#endif /*HESPERIA_CORE_DATA_SERIALIZABLEDATA_H_*/
