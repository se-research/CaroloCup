/*
 * Copyright (c) Christian Berger and Bernhard Rumpe, Rheinisch-Westfaelische Technische Hochschule Aachen, Germany.
 *
 * The Hesperia Framework.
 */

#ifndef HESPERIA_CORE_THREED_NODEDESCRIPTOR_H_
#define HESPERIA_CORE_THREED_NODEDESCRIPTOR_H_

#include <string>

// native.h must be included as first header file for definition of _WIN32_WINNT.
#include "core/native.h"

namespace hesperia {
    namespace threeD {

        using namespace std;

        /**
         * This class provides a description for the current node.
         */
        class OPENDAVINCI_API NodeDescriptor {
            public:
                NodeDescriptor();

                /**
                 * Constructor.
                 *
                 * @param name Name of this node descriptor.
                 */
                NodeDescriptor(const string &name);

                /**
                 * Copy constructor.
                 *
                 * @param obj Reference to an object of this class.
                 */
                NodeDescriptor(const NodeDescriptor &obj);

                virtual ~NodeDescriptor();

                /**
                 * Assignment operator.
                 *
                 * @param obj Reference to an object of this class.
                 * @return Reference to this instance.
                 */
                NodeDescriptor& operator=(const NodeDescriptor &obj);

                /**
                 * This method returns the name of this node.
                 *
                 * @return Name of this node.
                 */
                const string getName() const;

            private:
                string m_name;
        };

    }
} // hesperia::threeD

#endif /*HESPERIA_CORE_THREED_NODEDESCRIPTOR_H_*/
