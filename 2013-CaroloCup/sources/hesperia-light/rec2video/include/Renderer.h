/*
 * Copyright (c) Christian Berger and Bernhard Rumpe, Rheinisch-Westfaelische Technische Hochschule Aachen, Germany.
 *
 * The Hesperia Framework.
 */

#ifndef RENDERER_H_
#define RENDERER_H_

#include <map>

#include <opencv/cxcore.h>

#include "core/base/Condition.h"
#include "core/base/KeyValueConfiguration.h"
#include "core/base/Service.h"
#include "core/data/Container.h"
#include "hesperia/data/Constants.h"
#include "hesperia/data/environment/Point3.h"
#include "hesperia/data/scenario/Scenario.h"
#include "hesperia/decorator/DataRenderer.h"
#include "hesperia/decorator/ScenarioRenderer.h"
#include "hesperia/decorator/threeD/Renderer3D.h"
#include "hesperia/scenario/SCNXArchive.h"
#include "hesperia/threeD/GLUTWindowBase.h"

namespace rec2video {

    using namespace std;

    class Renderer : public core::base::Service,
                     public hesperia::threeD::GLUTWindowBase<Renderer> {
        private:
            /**
             * "Forbidden" copy constructor. Goal: The compiler should warn
             * already at compile time for unwanted bugs caused by any misuse
             * of the copy constructor.
             *
             * @param obj Reference to an object of this class.
             */
            Renderer(const Renderer &/*obj*/);

            /**
             * "Forbidden" assignment operator. Goal: The compiler should warn
             * already at compile time for unwanted bugs caused by any misuse
             * of the assignment operator.
             *
             * @param obj Reference to an object of this class.
             * @return Reference to this instance.
             */
            Renderer& operator=(const Renderer &/*obj*/);

        public:
            /**
             * Constructor.
             *
             * @param kvc KeyValueConfiguration.
             * @param condition Condition to control rendering.
             * @param doRendering Boolean variable for condition.
             */
            Renderer(const core::base::KeyValueConfiguration &kvc, core::base::Condition &condition, bool &doRendering);

            virtual ~Renderer();

            virtual void render();

            virtual void resize(int32_t w, int32_t h);

            virtual void processKey(unsigned char key, int32_t x, int32_t y);

            virtual void processMouseMotion(int32_t x, int32_t y);

            virtual void processMouseEvent(int32_t button, int32_t state, int32_t x, int32_t y);

            virtual void doIdle();

            virtual void beforeStop();

            virtual void run();

            /**
             * This method processes incoming data for modifying the world.
             *
             * @param c Container to process.
             */
            void process(core::data::Container &c);

        private:
            core::base::KeyValueConfiguration m_kvc;
            core::base::Condition &m_condition;
            bool &m_doRendering;

            IplImage *m_image;
            IplImage *m_flippedImage;
            uint32_t m_imageNumber;

            hesperia::data::environment::Point3 m_positionCamera;
            hesperia::data::environment::Point3 m_lookAtPointCamera;

            hesperia::decorator::threeD::Renderer3D m_renderer3D;

            map<core::data::Container::DATATYPE, core::data::Container> m_drawMap;

            hesperia::decorator::DataRenderer m_dataRendererWithCarModel;

            hesperia::scenario::SCNXArchive *m_scnxArchive;
            hesperia::data::scenario::Scenario *m_scenario;
            hesperia::decorator::ScenarioRenderer m_scenarioRenderer;
    };

} // rec2video

#endif /*RENDERER_H_*/
