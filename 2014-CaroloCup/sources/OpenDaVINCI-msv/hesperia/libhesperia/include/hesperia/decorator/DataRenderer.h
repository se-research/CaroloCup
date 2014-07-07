/*
 * Copyright (c) Christian Berger and Bernhard Rumpe, Rheinisch-Westfaelische Technische Hochschule Aachen, Germany.
 *
 * The Hesperia Framework.
 */

#ifndef HESPERIA_CORE_DECORATOR_DATARENDERER_H_
#define HESPERIA_CORE_DECORATOR_DATARENDERER_H_

#include <map>
#include <string>
#include <vector>

// native.h must be included as first header file for definition of _WIN32_WINNT.
#include "core/native.h"
#include "core/base/Mutex.h"
#include "core/data/Container.h"

#include "hesperia/data/environment/EgoState.h"
#include "hesperia/data/environment/Obstacle.h"
#include "hesperia/data/environment/OtherVehicleState.h"
#include "hesperia/data/planning/Route.h"
#include "hesperia/data/sensor/ContouredObjects.h"
#include "hesperia/data/situation/ComplexModel.h"
#include "hesperia/decorator/Renderer.h"
#include "hesperia/decorator/models/OBJXArchive.h"
#include "hesperia/decorator/models/TriangleSet.h"
#include "hesperia/scenario/SCNXArchive.h"

namespace hesperia {
    namespace decorator {

        using namespace std;

        /**
         * This class is a device independent data painter.
         */
        class OPENDAVINCI_API DataRenderer {
            private:
                /**
                 * "Forbidden" copy constructor. Goal: The compiler should warn
                 * already at compile time for unwanted bugs caused by any misuse
                 * of the copy constructor.
                 */
                DataRenderer(const DataRenderer &/*obj*/);

                /**
                 * "Forbidden" assignment operator. Goal: The compiler should warn
                 * already at compile time for unwanted bugs caused by any misuseScenarioRenderer
                 * of the assignment operator.
                 */
                DataRenderer& operator=(const DataRenderer &/*obj*/);

            public:
                DataRenderer();

                virtual ~DataRenderer();

                /**
                 * This method tries to draw an arbitrary container.
                 *
                 * @param c Container to draw.
                 */
                void draw(core::data::Container &c);

                /**
                 * This method draws an EgoState.
                 *
                 * @param es EgoState to be drawn.
                 */
                void draw(const hesperia::data::environment::EgoState &es);

                /**
                 * This method draws a ContouredObjects.
                 *
                 * @param cos ContouredObjectes to be drawn.
                 */
                void draw(const hesperia::data::sensor::ContouredObjects &cos);

                /**
                 * This method draws an Obstacle.
                 *
                 * @param o Obstacle to be drawn.
                 */
                void draw(const hesperia::data::environment::Obstacle &o);

                /**
                 * This method draws the planned Route.
                 *
                 * @param r Planned route to be drawn.
                 */
                void draw(const hesperia::data::planning::Route &r);

                /**
                 * This method draws another object.
                 *
                 * @param o Other object.
                 */
                void draw(const hesperia::data::environment::OtherVehicleState &o);

                /**
                 * This method sets the renderer to be used for rendering.
                 *
                 * @param renderer Renderer to be used.
                 */
                void setRenderer(Renderer *renderer);

                /**
                 * This method sets a model for the decorating the EgoState.
                 *
                 * @param fn Filename to load *.objx model.
                 */
                void setEgoStateModel(const string &fn);

                /**
                 * This method can be used to set the associated SCNXArchive
                 * to load more data like images or models.
                 *
                 * @param scnxArchive SCNXArchive to be used. If set to NULL no archive will be used. The given point won't get freed.
                 */
                void setSCNXArchive(hesperia::scenario::SCNXArchive *scnxArchive);

            private:
                core::base::Mutex m_dataRendererMutex;
                Renderer *m_renderer;

                hesperia::scenario::SCNXArchive *m_scnxArchive;

                vector<hesperia::decorator::models::TriangleSet> m_egoStateModel;
                vector<hesperia::decorator::models::OBJXArchive*> m_listOfLoadedOBJXArchives;

                map<uint32_t, vector<hesperia::decorator::models::TriangleSet> > m_mapOfModels;

                /**
                 * Load complex model from SCNX archive.
                 *
                 * @param id Identifier.
                 * @param cm ComplexModel data structure.
                 */
                void loadComplexModel(const uint32_t &id, hesperia::data::situation::ComplexModel &cm);
        };

    }
} // hesperia::decorator

#endif /*HESPERIA_CORE_DECORATOR_DATARENDERER_H_*/
