/*
 * Copyright (c) Christian Berger and Bernhard Rumpe, Rheinisch-Westfaelische Technische Hochschule Aachen, Germany.
 *
 * The Hesperia Framework.
 */

#ifndef EGOCONTROLLER_H_
#define EGOCONTROLLER_H_

#include <string>

#include "core/data/TimeStamp.h"
#include "core/base/ConferenceClientModule.h"
#include "core/data/environment/Point3.h"

#include "ForceControlBehaviour.h"
#include "ForceControlBehaviourBicycleModel.h"
#include "ForceControlBehaviourSimplifiedBicycleModel.h"

namespace egocontroller {

    using namespace std;

    /**
     * This class is the camera server providing new camera images.
     */
    class EgoController : public core::base::ConferenceClientModule {
        private:
            /**
             * "Forbidden" copy constructor. Goal: The compiler should warn
             * already at compile time for unwanted bugs caused by any misuse
             * of the copy constructor.
             *
             * @param obj Reference to an object of this class.
             */
            EgoController(const EgoController &/*obj*/);

            /**
             * "Forbidden" assignment operator. Goal: The compiler should warn
             * already at compile time for unwanted bugs caused by any misuse
             * of the assignment operator.
             *
             * @param obj Reference to an object of this class.
             * @return Reference to this instance.
             */
            EgoController& operator=(const EgoController &/*obj*/);

        public:
            /**
             * Constructor.
             *
             * @param argc Number of command line arguments.
             * @param argv Command line arguments.
             */
            EgoController(const int &argc, char **argv);

            virtual ~EgoController();

            core::base::ModuleState::MODULE_EXITCODE body();

        private:
            string m_device;

            ForceControlBehaviour* createForceControlBehaviour();

            ForceControlBehaviourBicycleModel* createForceControlBehaviourBicycleModel();

            ForceControlBehaviourSimplifiedBicycleModel* createForceControlBehaviourSimplifiedBicycleModel();

            virtual void setUp();

            virtual void tearDown();
    };

} // egocontroller

#endif /*EGOCONTROLLER_H_*/
