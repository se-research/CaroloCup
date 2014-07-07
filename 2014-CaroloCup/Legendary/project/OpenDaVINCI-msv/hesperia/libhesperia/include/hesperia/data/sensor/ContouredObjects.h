/*
 * Copyright (c) Christian Berger and Bernhard Rumpe, Rheinisch-Westfaelische Technische Hochschule Aachen, Germany.
 *
 * The Hesperia Framework.
 */

#ifndef HESPERIA_CORE_DATA_SENSOR_CONTOUREDOBJECTS_H_
#define HESPERIA_CORE_DATA_SENSOR_CONTOUREDOBJECTS_H_

#include <vector>

// native.h must be included as first header file for definition of _WIN32_WINNT.
#include "core/native.h"
#include "core/data/SerializableData.h"

#include "hesperia/data/sensor/ContouredObject.h"

namespace hesperia {
    namespace data {
        namespace sensor {

            using namespace std;

            /**
             * This class contains all data from one specific sensor.
             */
            class OPENDAVINCI_API ContouredObjects : public core::data::SerializableData {
                public:
                    enum COLOR {
                        RED,
                        GREEN,
                        BLUE,
                        YELLOW
                    };

                public:
                    ContouredObjects();

                    virtual ~ContouredObjects();

                    /**
                     * Copy constructor.
                     *
                     * @param obj Reference to an object of this class.
                     */
                    ContouredObjects(const ContouredObjects &obj);

                    /**
                     * Assignment operator.
                     *
                     * @param obj Reference to an object of this class.
                     * @return Reference to this instance.
                     */
                    ContouredObjects& operator=(const ContouredObjects &obj);

                    /**
                     * This method returns all contoured objects.
                     *
                     * @return Set of contoured objects.
                     */
                    const vector<ContouredObject> getContouredObjects() const;

                    /**
                     * This method adds a contoured object.
                     *
                     * @param contouredObject New contoured object to be added.
                     */
                    void add(const ContouredObject &contouredObject);

                    /**
                     * This method returns the color.
                     *
                     * @return Color.
                     */
                    enum COLOR getColor() const;

                    /**
                     * This method sets the color.
                     *
                     * @param color The new color.
                     */
                    void setColor(const enum COLOR &color);

                    /**
                     * This method resets all current contoured objects.
                     */
                    void reset();

                    virtual ostream& operator<<(ostream &out) const;
                    virtual istream& operator>>(istream &in);

                    virtual const string toString() const;

                private:
                    vector<ContouredObject> m_contouredObjects;
                    enum COLOR m_color;
            };

        }
    }
} // hesperia::data::sensor

#endif /*HESPERIA_CORE_DATA_SENSOR_CONTOUREDOBJECTS_H_*/
