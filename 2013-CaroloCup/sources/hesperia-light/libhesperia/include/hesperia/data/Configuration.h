/*
 * Copyright (c) Christian Berger and Bernhard Rumpe, Rheinisch-Westfaelische Technische Hochschule Aachen, Germany.
 *
 * The Hesperia Framework.
 */

#ifndef HESPERIA_CORE_DATA_CONFIGURATION_H_
#define HESPERIA_CORE_DATA_CONFIGURATION_H_

// native.h must be included as first header file for definition of _WIN32_WINNT.
#include "core/native.h"
#include "core/base/KeyValueConfiguration.h"
#include "core/data/SerializableData.h"

namespace hesperia {
    namespace data {

        using namespace std;

        /**
         * This class contains a specific configuration.
         */
        class HESPERIA_API Configuration : public core::data::SerializableData {
            public:
                Configuration();

                /**
                 * Constructor.
                 *
                 * @param keyValueConfiguration Key/value-configuration for constructing this configuration object.
                 */
                Configuration(const core::base::KeyValueConfiguration &keyValueConfiguration);

                virtual ~Configuration();

                /**
                 * Copy constructor.
                 *
                 * @param obj Reference to an object of this class.
                 */
                Configuration(const Configuration &obj);

                /**
                 * Assignment operator.
                 *
                 * @param obj Reference to an object of this class.
                 * @return Reference to this instance.
                 */
                Configuration& operator=(const Configuration &obj);

                /**
                 * This method returns the key/value-configuration.
                 *
                 * @return Key/value-configuration.
                 */
                const core::base::KeyValueConfiguration getKeyValueConfiguration() const;

                virtual ostream& operator<<(ostream &out) const;
                virtual istream& operator>>(istream &in);

                virtual const string toString() const;

            private:
                core::base::KeyValueConfiguration m_keyValueConfiguration;
        };

    }
} // hesperia::data

#endif /*HESPERIA_CORE_DATA_CONFIGURATION_H_*/
