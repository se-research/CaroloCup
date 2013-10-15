/*
 * Copyright (c) Christian Berger and Bernhard Rumpe, Rheinisch-Westfaelische Technische Hochschule Aachen, Germany.
 *
 * The Hesperia Framework.
 */

#ifndef HESPERIA_DATA_SENSOR_NMEA_GPRMC_H_
#define HESPERIA_DATA_SENSOR_NMEA_GPRMC_H_

#include <string>

// native.h must be included as first header file for definition of _WIN32_WINNT.
#include "core/native.h"

#include "core/data/SerializableData.h"
#include "core/data/TimeStamp.h"
#include "hesperia/data/environment/WGS84Coordinate.h"

namespace hesperia {
    namespace data {
        namespace sensor {
            namespace nmea {

                using namespace std;

                /**
                 * This class encapsulated a GPRMC (GPS recommended) message.
                 */
                class OPENDAVINCI_API GPRMC : public core::data::SerializableData {
                    private:
                        enum {
                            BASE_YEAR = 2000
                        };
                    public:
                        GPRMC();

                        /**
                         * Copy constructor.
                         *
                         * @param obj Reference to an object of this class.
                         */
                        GPRMC(const GPRMC &obj);

                        virtual ~GPRMC();

                        /**
                         * Assignment operator.
                         *
                         * @param obj Reference to an object of this class.
                         * @return Reference to this instance.
                         */
                        GPRMC& operator=(const GPRMC &obj);

                        /**
                         * This method returns the raw GPRMC message.
                         *
                         * @return GPRMC message.
                         */
                        const string getMessage() const;

                        /**
                         * This method sets the GPRMC message.
                         *
                         * @param msg Message to be set.
                         */
                        void setMessage(const string &msg);

                        /**
                         * This method returns the time stamp for this
                         * GPRMC message.
                         *
                         * @return timestamp.
                         */
                        const core::data::TimeStamp getTimeStamp() const;

                        /**
                         * This method sets the time stamp for this
                         * GPRMC message.
                         *
                         * @param timeStamp Timestamp.
                         */
                        void setTimeStamp(const core::data::TimeStamp &timeStamp);

                        /**
                         * This methods returns the WGS84 coordinate.
                         *
                         * @return WGS84Coordinate.
                         */
                        const hesperia::data::environment::WGS84Coordinate getCoordinate() const;

                        /**
                         * This methods sets the WGS84 coordinate.
                         *
                         * @param coordinate WGS84Coordinate.
                         */
                        void setCoordinate(const hesperia::data::environment::WGS84Coordinate &coordinate);

                        virtual ostream& operator<<(ostream &out) const;
                        virtual istream& operator>>(istream &in);

                        virtual const string toString() const;

                    private:
                        string m_message;
                        core::data::TimeStamp m_timeStamp;
                        hesperia::data::environment::WGS84Coordinate m_coordinate;

                        /**
                         * This method decodes a given GPRMC string.
                         */
                        void decode();

                        /**
                         * This method encodes a GPRMC string based on the data of this instance.
                         */
                        void encode();
                };

            }
        }
    }
} // hesperia::data::sensor::nmea

#endif /*HESPERIA_DATA_SENSOR_NMEA_GPRMC_H_*/
