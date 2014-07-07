/*
 * Copyright (c) Christian Berger and Bernhard Rumpe, Rheinisch-Westfaelische Technische Hochschule Aachen, Germany.
 *
 * The Hesperia Framework.
 */

#ifndef HESPERIA_CORE_DATA_SITUATION_SITUATION_H_
#define HESPERIA_CORE_DATA_SITUATION_SITUATION_H_

#include <string>
#include <vector>

// native.h must be included as first header file for definition of _WIN32_WINNT.
#include "core/native.h"
#include "core/data/SerializableData.h"

#include "hesperia/data/situation/Header.h"
#include "hesperia/data/situation/Object.h"
#include "hesperia/data/situation/SituationNode.h"
#include "hesperia/data/situation/SituationVisitor.h"

namespace hesperia {
    namespace data {
        namespace situation {

            using namespace std;

            /**
             * This class represents a situation.
             */
            class OPENDAVINCI_API Situation : public core::data::SerializableData, public SituationNode {
                public:
                    Situation();

                    virtual ~Situation();

                    /**
                     * Copy constructor.
                     *
                     * @param obj Reference to an object of this class.
                     */
                    Situation(const Situation &obj);

                    /**
                     * Assignment operator.
                     *
                     * @param obj Reference to an object of this class.
                     * @return Reference to this instance.
                     */
                    Situation& operator=(const Situation &obj);

                    virtual void accept(situation::SituationVisitor &visitor);

                    /**
                     * This method returns the situation's header.
                     *
                     * @return Situation's header.
                     */
                    const Header& getHeader() const;

                    /**
                     * This method sets the situation's header.
                     *
                     * @param h Situation's header.
                     */
                    void setHeader(const Header &h);

                    /**
                     * This method returns the list of objects.
                     *
                     * @return List of objects.
                     */
                    const vector<Object>& getListOfObjects() const;

                    /**
                     * This method adds a object.
                     *
                     * @param o Object to be added.
                     */
                    void addObject(const Object &o);

                    virtual ostream& operator<<(ostream &out) const;
                    virtual istream& operator>>(istream &in);

                    virtual const string toString() const;

                private:
                    Header m_header;
                    vector<Object> m_listOfObjects;

                    /**
                     * This method creates a deep copy.
                     *
                     * @param obj Object of another instance of this class.
                     */
                    void deepCopy(const Situation &obj);
            };

        }
    }
} // hesperia::data::situation

#endif /*HESPERIA_CORE_DATA_SITUATION_SITUATION_H_*/
