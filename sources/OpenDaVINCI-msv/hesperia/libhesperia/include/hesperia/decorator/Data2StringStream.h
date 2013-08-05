/*
 * Copyright (c) Christian Berger and Bernhard Rumpe, Rheinisch-Westfaelische Technische Hochschule Aachen, Germany.
 *
 * The Hesperia Framework.
 */

#ifndef HESPERIA_CORE_DECORATOR_DATA2STRINGSTREAM_H_
#define HESPERIA_CORE_DECORATOR_DATA2STRINGSTREAM_H_

#include <sstream>

// native.h must be included as first header file for definition of _WIN32_WINNT.
#include "core/native.h"
#include "core/data/Container.h"

#include "hesperia/data/environment/EgoState.h"
#include "hesperia/data/planning/Route.h"

namespace hesperia {
    namespace decorator {

        using namespace std;

        /**
         * This class is renders data to a stringstream using toString of serializable data.
         */
        class OPENDAVINCI_API Data2StringStream {
            private:
                /**
                 * "Forbidden" copy constructor. Goal: The compiler should warn
                 * already at compile time for unwanted bugs caused by any misuse
                 * of the copy constructor.
                 */
                Data2StringStream(const Data2StringStream &/*obj*/);

                /**
                 * "Forbidden" assignment operator. Goal: The compiler should warn
                 * already at compile time for unwanted bugs caused by any misuseScenarioRenderer
                 * of the assignment operator.
                 */
                Data2StringStream& operator=(const Data2StringStream &/*obj*/);

            public:
                /**
                 * Constructor.
                 *
                 * @param sstr StringStream to render data into.
                 */
                Data2StringStream(stringstream &sstr);

                virtual ~Data2StringStream();

                /**
                 * This method tries to draw an arbitrary container.
                 *
                 * @param c Container to draw.
                 */
                void toStringStream(core::data::Container &c);

                /**
                 * This method draws an EgoState.
                 *
                 * @param es EgoState to be drawn.
                 */
                void toStringStream(const hesperia::data::environment::EgoState &es);

                /**
                 * This method draws the planned Route.
                 *
                 * @param r Planned route to be drawn.
                 */
                void toStringStream(const hesperia::data::planning::Route &r);

            private:
                stringstream &m_sstr;
        };

    }
} // hesperia::decorator

#endif /*HESPERIA_CORE_DECORATOR_DATA2STRINGSTREAM_H_*/
