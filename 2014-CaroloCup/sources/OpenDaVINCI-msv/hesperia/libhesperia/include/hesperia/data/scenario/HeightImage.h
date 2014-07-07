/*
 * Copyright (c) Christian Berger and Bernhard Rumpe, Rheinisch-Westfaelische Technische Hochschule Aachen, Germany.
 *
 * The Hesperia Framework.
 */

#ifndef HESPERIA_CORE_DATA_SCENARIO_HEIGHTIMAGE_H_
#define HESPERIA_CORE_DATA_SCENARIO_HEIGHTIMAGE_H_

#include <string>

// native.h must be included as first header file for definition of _WIN32_WINNT.
#include "core/native.h"

#include "hesperia/data/scenario/Image.h"

namespace hesperia {
    namespace data {
        namespace scenario {

            using namespace std;

            /**
             * This class represents a height image.
             */
            class OPENDAVINCI_API HeightImage : public Image {
                public:
                    HeightImage();

                    /**
                     * Copy constructor.
                     *
                     * @param obj Reference to an object of this class.
                     */
                    HeightImage(const HeightImage &obj);

                    /**
                     * Assignment operator.
                     *
                     * @param obj Reference to an object of this class.
                     * @return Reference to this instance.
                     */
                    HeightImage& operator=(const HeightImage &obj);

                    virtual ~HeightImage();

                    virtual void accept(ScenarioVisitor &visitor);

                    /**
                     * This method returns the color representing the
                     * ground's height.
                     *
                     * @return Color representing the ground's height.
                     */
                    double getGroundHeight() const;

                    /**
                     * This method sets the color representing the
                     * ground's height.
                     *
                     * @return gh Color representing the ground's height.
                     */
                    void setGroundHeight(const double &gh);

                    /**
                     * This method returns the minimum height
                     * represented by color 0.
                     *
                     * @return Minimum height.
                     */
                    double getMinimumHeight() const;

                    /**
                     * This method sets the minimum height
                     * represented by color 0.
                     *
                     * @param minH Minimum height represented by color 0.
                     */
                    void setMinimumHeight(const double &minH);

                    /**
                     * This method returns the maximum height
                     * represented by color MAXCOLOR (depends
                     * on the image's data).
                     *
                     * @return Minimum height.
                     */
                    double getMaximumHeight() const;

                    /**
                     * This method sets the maximum height
                     * represented by color MAXCOLOR (depends
                     * on the image's data).
                     *
                     * @param maxH Maximum height represented by color MAXCOLOR.
                     */
                    void setMaximumHeight(const double &maxH);

                    virtual ostream& operator<<(ostream &out) const;
                    virtual istream& operator>>(istream &in);

                    virtual const string toString() const;

                private:
                    double m_groundHeight;
                    double m_minimumHeight;
                    double m_maximumHeight;
            };

        }
    }
} // core::data::scenario

#endif /*HESPERIA_CORE_DATA_SCENARIO_HEIGHTIMAGE_H_*/
