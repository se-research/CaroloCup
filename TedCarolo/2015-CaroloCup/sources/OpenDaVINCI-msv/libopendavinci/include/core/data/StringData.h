/*
 * OpenDaVINCI.
 *
 * This software is open source. Please see COPYING and AUTHORS for further information.
 */

#ifndef OPENDAVINCI_CORE_DATA_STRINGDATA_H_
#define OPENDAVINCI_CORE_DATA_STRINGDATA_H_

#include <string>

// core/platform.h must be included to setup platform-dependent header files and configurations.
#include "core/platform.h"

#include "core/data/SerializableData.h"

namespace core {
    namespace data {

        using namespace std;

        /**
         * This class can be used for exchanging string data.
         */
        class OPENDAVINCI_API StringData : public SerializableData {
            public:
                StringData();

                virtual ~StringData();

                /**
                 * Copy constructor.
                 *
                 * @param obj Reference to an object of this class.
                 */
                StringData(const StringData &obj);

                /**
                 * Assignment operator.
                 *
                 * @param obj Reference to an object of this class.
                 * @return Reference to this instance.
                 */
                StringData& operator=(const StringData &obj);

                void setData(const std::string &data);

                const string getData() const;

                virtual ostream& operator<<(ostream &out) const;
                virtual istream& operator>>(istream &in);

                virtual const string toString() const;

            private:
                std::string m_data;
        };

    }
} // core::data

#endif /*OPENDAVINCI_CORE_DATA_STRINGDATA_H_*/
