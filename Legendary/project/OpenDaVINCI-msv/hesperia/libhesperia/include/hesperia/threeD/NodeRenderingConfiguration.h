/*
 * Copyright (c) Christian Berger and Bernhard Rumpe, Rheinisch-Westfaelische Technische Hochschule Aachen, Germany.
 *
 * The Hesperia Framework.
 */

#ifndef HESPERIA_CORE_THREED_NODERENDERINGCONFIGURATION_H_
#define HESPERIA_CORE_THREED_NODERENDERINGCONFIGURATION_H_

#include <stdint.h>

// native.h must be included as first header file for definition of _WIN32_WINNT.
#include "core/native.h"

namespace hesperia {
    namespace threeD {

        using namespace std;

        /**
         * This class configures several options for the rendering method.
         */
        class OPENDAVINCI_API NodeRenderingConfiguration {
            public:
                // Rendering parameters (must be power of two numbers since it's set using |'s).
                enum RENDERING_PARAMETERS {
                    ENABLED = 1,
                    //NEXT_PARAMETER = 2,
                    //FURTHER_PARAMETERS = 4, ...
                };

            public:
                NodeRenderingConfiguration();

                /**
                 * Copy constructor.
                 *
                 * @param obj Reference to an object of this class.
                 */
                NodeRenderingConfiguration(const NodeRenderingConfiguration &obj);

                virtual ~NodeRenderingConfiguration();

                /**
                 * Assignment operator.
                 *
                 * @param obj Reference to an object of this class.
                 * @return Reference to this instance.
                 */
                NodeRenderingConfiguration& operator=(const NodeRenderingConfiguration &obj);

                /**
                 * This method returns true if the selected parameter is set.
                 *
                 * @param p Parameter to check.
                 * @return True if the selected parameter is s.
                 */
                bool hasParameter(const enum RENDERING_PARAMETERS &p) const;

                /**
                 * This method sets a rendering parameter.
                 *
                 * @param p Parameter to set.
                 * @param enabled True if p is set.
                 */
                void setParameter(const enum RENDERING_PARAMETERS &p, const bool &enabled);

            private:
                uint32_t m_parameters;
        };

    }
} // hesperia::threeD

#endif /*HESPERIA_CORE_THREED_NODERENDERINGCONFIGURATION_H_*/
