/*
 * Copyright (c) Christian Berger and Bernhard Rumpe, Rheinisch-Westfaelische Technische Hochschule Aachen, Germany.
 *
 * The Hesperia Framework.
 */

#ifndef HESPERIA_DATA_SENSOR_LMS291MESSAGE_H_
#define HESPERIA_DATA_SENSOR_LMS291MESSAGE_H_

#include <string>

// native.h must be included as first header file for definition of _WIN32_WINNT.
#include "core/native.h"

#include "core/data/SerializableData.h"

namespace hesperia {
    namespace data {
        namespace sensor {

            using namespace std;

            /**
             * This class encapsulated an LMS291 message.
             */
            class OPENDAVINCI_API LMS291Message : public core::data::SerializableData {
                public:
                    LMS291Message();

                    /**
                     * Copy constructor.
                     *
                     * @param obj Reference to an object of this class.
                     */
                    LMS291Message(const LMS291Message &obj);

                    virtual ~LMS291Message();

                    /**
                     * Assignment operator.
                     *
                     * @param obj Reference to an object of this class.
                     * @return Reference to this instance.
                     */
                    LMS291Message& operator=(const LMS291Message &obj);

                    /**
                     * This method returns the ID of the LMS291.
                     *
                     * @return LMS291's ID.
                     */
                    const string getID() const;

                    /**
                     * This method sets the LMS291 ID.
                     *
                     * @param id ID to be set.
                     */
                    void setID(const string &id);

                    /**
                     * This method returns the raw LMS291 message.
                     *
                     * @return LMS291 message.
                     */
                    const string getMessage() const;

                    /**
                     * This method sets the LMS291 message.
                     *
                     * @param msg Message to be set.
                     */
                    void setMessage(const string &msg);

                    virtual ostream& operator<<(ostream &out) const;
                    virtual istream& operator>>(istream &in);

                    virtual const string toString() const;

                private:
                    string m_id;
                    string m_message;
            };

        }
    }
} // hesperia::data::sensor

#endif /*HESPERIA_DATA_SENSOR_LMS291MESSAGE_H_*/
