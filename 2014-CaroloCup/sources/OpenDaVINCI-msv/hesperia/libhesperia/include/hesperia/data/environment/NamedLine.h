/*
 * Copyright (c) Christian Berger.
 *
 * The Hesperia Framework.
 */

#ifndef HESPERIA_DATA_ENVIRONMENT_NAMEDLINE_H_
#define HESPERIA_DATA_ENVIRONMENT_NAMEDLINE_H_

#include "hesperia/data/environment/Line.h"

namespace hesperia {
    namespace data {
        namespace environment {

            using namespace std;

            /**
             * This class can be used for line operations.
             */
            class OPENDAVINCI_API NamedLine : public Line {
                public:
                    NamedLine();

                    /**
                     * Constructor.
                     *
                     * @param A
                     * @param B
                     */
                    NamedLine(const string &name, const Point3 &A, const Point3 &B);

                    /**
                     * Copy constructor.
                     *
                     * @param obj Reference to an object of this class.
                     */
                    NamedLine(const NamedLine &obj);

                    virtual ~NamedLine();

                    /**
                     * Assignment operator.
                     *
                     * @param obj Reference to an object of this class.
                     * @return Reference to this instance.
                     */
                    NamedLine& operator=(const NamedLine &obj);

                    /**
                     * This method sets the name.
                     *
                     * @param n Name.
                     */
                    void setName(const string &n);

                    /**
                     * This method returns the name.
                     *
                     * @return name.
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
} // hesperia::data::environment

#endif /*HESPERIA_DATA_ENVIRONMENT_NAMEDLINE_H_*/
