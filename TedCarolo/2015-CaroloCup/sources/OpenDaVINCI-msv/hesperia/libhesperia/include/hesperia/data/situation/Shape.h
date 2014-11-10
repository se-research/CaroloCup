/*
 * Copyright (c) Christian Berger and Bernhard Rumpe, Rheinisch-Westfaelische Technische Hochschule Aachen, Germany.
 *
 * The Hesperia Framework.
 */

#ifndef HESPERIA_CORE_DATA_SITUATION_SHAPE_H_
#define HESPERIA_CORE_DATA_SITUATION_SHAPE_H_

#include <string>

// native.h must be included as first header file for definition of _WIN32_WINNT.
#include "core/native.h"
#include "core/data/SerializableData.h"
#include "hesperia/data/situation/SituationNode.h"
#include "hesperia/data/situation/SituationVisitor.h"
#include "hesperia/data/situation/Vertex3.h"

namespace hesperia {
    namespace data {
        namespace situation {

            using namespace std;

            /**
             * This class represents a shape (i.e. a polygon, rectangle or complex model).
             */
            class OPENDAVINCI_API Shape : public core::data::SerializableData, public SituationNode {
                public:
                    enum SHAPETYPE {
                        UNDEFINED,
                        COMPLEXMODEL,
                        POLYGON,
                        RECTANGLE
                    };

                protected:
                    Shape();

                    /**
                     * Copy constructor.
                     *
                     * @param obj Reference to an object of this class.
                     */
                    Shape(const Shape &obj);

                    /**
                     * Assignment operator.
                     *
                     * @param obj Reference to an object of this class.
                     * @return Reference to this instance.
                     */
                    Shape& operator=(const Shape &obj);

                public:
                    virtual ~Shape();

                    virtual void accept(SituationVisitor &visitor) = 0;

                    /**
                     * This method returns the shape's name.
                     *
                     * @return Shape's name.
                     */
                    const string getName() const;

                    /**
                     * This method sets the shape's name.
                     *
                     * @param name Shape's name.
                     */
                    void setName(const string &name);

                    /**
                     * This method returns the type of the shape.
                     *
                     * @return Type of shape.
                     */
                    enum SHAPETYPE getType() const;

                    /**
                     * This method sets the shape's type.
                     *
                     * @param type Type of the shape.
                     */
                    void setType(const enum Shape::SHAPETYPE &type);

                    /**
                     * This method returns the front point.
                     *
                     * @return Front point.
                     */
                    const Vertex3 getFront() const;

                    /**
                     * This method sets the front point.
                     *
                     * @param f Front point.
                     */
                    void setFront(const Vertex3 f);

                    virtual ostream& operator<<(ostream &out) const;
                    virtual istream& operator>>(istream &in);

                    virtual const string toString() const;

                private:
                    string m_name;
                    enum SHAPETYPE m_type;
                    Vertex3 m_front;
            };

        }
    }
} // core::data::situation

#endif /*HESPERIA_CORE_DATA_SITUATION_SHAPE_H_*/
