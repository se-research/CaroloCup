/*
 * Copyright (c) Christian Berger and Bernhard Rumpe, Rheinisch-Westfaelische Technische Hochschule Aachen, Germany.
 *
 * The Hesperia Framework.
 */

#ifndef HESPERIA_DATA_CAMERA_IMAGEGRABBERID_H_
#define HESPERIA_DATA_CAMERA_IMAGEGRABBERID_H_

#include <string>

// native.h must be included as first header file for definition of _WIN32_WINNT.
#include "core/native.h"

#include "core/data/SerializableData.h"

namespace hesperia {
    namespace data {
        namespace camera {

            using namespace std;

            /**
             * This class implements a CAN message.
             */
            class OPENDAVINCI_API ImageGrabberID : public core::data::SerializableData {
                public:
                    /**
                     * Constructor.
                     *
                     * @param name Name.
                     */
                    ImageGrabberID(const string &name);

                    /**
                     * Copy constructor.
                     *
                     * @param obj Reference to an object of this class.
                     */
                    ImageGrabberID(const ImageGrabberID &obj);

                    virtual ~ImageGrabberID();

                    /**
                     * Assignment operator.
                     *
                     * @param obj Reference to an object of this class.
                     * @return Reference to this instance.
                     */
                    ImageGrabberID& operator=(const ImageGrabberID &obj);

                    /**
                     * This method returns the name.
                     *
                     * @return Name.
                     */
                    const string getName() const;

                    virtual ostream& operator<<(ostream &out) const;
                    virtual istream& operator>>(istream &in);

                    virtual const string toString() const;

                private:
                    string m_name;
            };
        }
    }
} // hesperia::data::camera

#endif /*HESPERIA_DATA_CAMERA_IMAGEGRABBERID_H_*/
