/*
 * Copyright (c) Christian Berger and Bernhard Rumpe, Rheinisch-Westfaelische Technische Hochschule Aachen, Germany.
 *
 * The Hesperia Framework.
 */

#ifndef HESPERIA_CORE_BASE_KEYVALUECONFIGURATION_H_
#define HESPERIA_CORE_BASE_KEYVALUECONFIGURATION_H_

#include <cerrno>
#include <iostream>
#include <map>
#include <string>
#include <sstream>
#include <vector>

// native.h must be included as first header file for definition of _WIN32_WINNT.
#include "core/native.h"

#include "core/exceptions/Exceptions.h"
#include "core/wrapper/StringComparator.h"

namespace core {
    namespace base {

        using namespace std;

        /**
         * This class is the key/value-based configuration.
         * It reads configuration data using istream:
         *
         * @code
         * KeyValueConfiguration kvc;
         * istream i = ...;
         * i >> kvc;
         *
         * @endcode
         *
         * Format for the configuration data:
         * # Comment
         * key=value
         * #key=value Disabled key.
         * anotherKey=anotherValue # Commented key-value-pair.
         */
        class HESPERIA_API KeyValueConfiguration {
            public:
                KeyValueConfiguration();

                /**
                 * Copy constructor.
                 *
                 * @param obj Reference to an object of this class.
                 */
                KeyValueConfiguration(const KeyValueConfiguration &obj);

                virtual ~KeyValueConfiguration();

                /**
                 * Assignment operator.
                 *
                 * @param obj Reference to an object of this class.
                 * @return Reference to this instance.
                 */
                KeyValueConfiguration& operator=(const KeyValueConfiguration &obj);

                /**
                 * This method is used to write configuration data using iostreams.
                 * It is necessary to allow human readable data on disks.
                 *
                 * @param out Stream to which the configuration data will be written.
                 * @return Stream.
                 */
                ostream& operator<<(ostream &out) const;

                /**
                 * This method is used to read configuration data using iostreams.
                 * It is necessary to allow human readable data on disks.
                 *
                 * @param in Stream from which the configuration data will be read.
                 * @return Stream.
                 */
                istream& operator>>(istream &in);

                /**
                 * This method returns a configuration value
                 *
                 * @code
                 * KeyValueConfiguration kvc;
                 * ...
                 * T t = kvc.getValue<T>();
                 * @endcode
                 *
                 * @param key Key for retrieving the value.
                 * @return Value.
                 * @throws ValueForKeyNotFoundException is the value for the given key does not exist.
                 */
                template<class T>
                inline T getValue(string key) const throw (exceptions::ValueForKeyNotFoundException) {
                    string stringValue(getValueFor(key));
                    if (stringValue == "") {
                        stringstream s;
                        s << "Value for key '" << key << "' not found.";
                        errno = 0;
                        HESPERIA_CORE_THROW_EXCEPTION(ValueForKeyNotFoundException, s.str());
                    }
                    stringstream s(stringValue);
                    T value;
                    s >> value;
                    return value;
                };

                /**
                 * This method returns a subset of this key/value-configuration, i.e.
                 * all key/value-pairs starting with "section".
                 *
                 * @param section Build subset key/value-configuration for this section.
                 * @return (Empty) subset key/value-configuration.
                 */
                KeyValueConfiguration getSubsetForSection(const string &section) const;

                /**
                 * This method returns a subset of this key/value-configuration, i.e.
                 * all key/value-pairs starting with "section" and removes the leading
                 * "section" from the resulting subset.
                 *
                 * @param section Build subset key/value-configuration for this section without "section".
                 * @return (Empty) subset key/value-configuration.
                 */
                KeyValueConfiguration getSubsetForSectionRemoveLeadingSectionName(const string &section) const;

                /**
                 * This method returns a vector of keys.
                 *
                 * @return Vector of contained keys.
                 */
                const vector<string> getListOfKeys() const;

            private:
                map<string, string, wrapper::StringComparator> m_keyValueConfiguration;

                /**
                 * This method returns the string for given key or ""
                 * if the key does not exist.
                 *
                 * @param key Key for which the value is queried for.
                 * @return value for the key or "".
                 */
                string getValueFor(string key) const;
        };

    }
} // core::base

namespace std {
    // The following methods are declarations for convenient usage.
    HESPERIA_API ostream& operator<<(ostream& out, const core::base::KeyValueConfiguration &configuration);
    HESPERIA_API istream& operator>>(istream& in, core::base::KeyValueConfiguration &configuration);
}

#endif /*HESPERIA_CORE_BASE_KEYVALUECONFIGURATION_H_*/
