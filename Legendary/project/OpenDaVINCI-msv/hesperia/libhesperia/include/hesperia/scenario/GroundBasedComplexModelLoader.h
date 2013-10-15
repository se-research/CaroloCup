/*
 * Copyright (c) Christian Berger and Bernhard Rumpe, Rheinisch-Westfaelische Technische Hochschule Aachen, Germany.
 *
 * The Hesperia Framework.
 */

#ifndef HESPERIA_SCENARIO_GROUNDBASEDCOMPLEXMODELLOADER_H_
#define HESPERIA_SCENARIO_GROUNDBASEDCOMPLEXMODELLOADER_H_

// native.h must be included as first header file for definition of _WIN32_WINNT.
#include "core/native.h"

#include "hesperia/scenario/SCNXArchive.h"
#include "hesperia/threeD/TransformGroup.h"

namespace hesperia {
    namespace scenario {

        using namespace std;

        /**
         * This class traverses the list of ground based complex
         * models and returns a renderable tranformation group.
         */
        class OPENDAVINCI_API GroundBasedComplexModelLoader {
            private:
                /**
                 * "Forbidden" copy constructor. Goal: The compiler should warn
                 * already at compile time for unwanted bugs caused by any misuse
                 * of the copy constructor.
                 */
                GroundBasedComplexModelLoader(const GroundBasedComplexModelLoader &);

                /**
                 * "Forbidden" assignment operator. Goal: The compiler should warn
                 * already at compile time for unwanted bugs caused by any misuse
                 * of the assignment operator.
                 */
                GroundBasedComplexModelLoader& operator=(const GroundBasedComplexModelLoader &);

            public:
                GroundBasedComplexModelLoader();

                virtual ~GroundBasedComplexModelLoader();

                threeD::TransformGroup* getGroundBasedComplexModels(const scenario::SCNXArchive &scnxArchive) const;
        };

    }
} // hesperia::scenario

#endif /*HESPERIA_SCENARIO_GROUNDBASEDCOMPLEXMODELLOADER_H_*/
