/*
 * Copyright (c) Christian Berger and Bernhard Rumpe, Rheinisch-Westfaelische Technische Hochschule Aachen, Germany.
 *
 * The Hesperia Framework.
 */

#include <termios.h>
#include <unistd.h>
#include <sys/select.h>
#include <sys/time.h>

#include <cstdio>
#include <algorithm>
#include <iostream>
#include <cmath>

#include "core/SharedPointer.h"
#include "core/base/Thread.h"
#include "core/base/KeyValueConfiguration.h"
#include "core/data/Container.h"
#include "core/data/environment/Point3.h"
#include "core/data/Constants.h"
#include "hesperia/data/environment/EgoState.h"

#include "EgoController.h"
#include "ControlBehaviour.h"
#include "ForceControlBehaviour.h"
#include "SimpleControlBehaviour.h"
#include "Controller.h"
#include "KeyBoardController.h"
#include "JoystickController.h"

namespace egocontroller {

    using namespace std;
    using namespace core::base;
    using namespace core::data;
    using namespace core::data::environment;
    using namespace hesperia::data::environment;

    EgoController::EgoController(const int &argc, char **argv) :
            ConferenceClientModule(argc, argv, "EgoController"),
            m_device()
    {}

    EgoController::~EgoController() {}

    void EgoController::setUp() {}

    void EgoController::tearDown() {}

    ForceControlBehaviour* EgoController::createForceControlBehaviour() {
        KeyValueConfiguration kvc = getKeyValueConfiguration();

        return new ForceControlBehaviour(
                kvc.getValue<double>("egocontroller.minimumTurningRadius"),
                kvc.getValue<double>("egocontroller.vehicleMass"),
                kvc.getValue<double>("egocontroller.adherenceCoefficient"),
                kvc.getValue<double>("egocontroller.idleForce"),
                kvc.getValue<double>("egocontroller.Ksteering"),
                kvc.getValue<double>("egocontroller.maximumSteeringRate"),
                kvc.getValue<double>("egocontroller.Kthrottle"),
                kvc.getValue<double>("egocontroller.tauBrake"),
                kvc.getValue<double>("egocontroller.KstaticBrake"),
                kvc.getValue<double>("egocontroller.KdynamicBrake") );
    }

    ForceControlBehaviourBicycleModel* EgoController::createForceControlBehaviourBicycleModel() {
        KeyValueConfiguration kvc = getKeyValueConfiguration();

        return new ForceControlBehaviourBicycleModel(
                kvc.getValue<double>("egocontroller.minimumTurningRadius"),
                kvc.getValue<double>("egocontroller.vehicleMass"),
                kvc.getValue<double>("egocontroller.adherenceCoefficient"),
                kvc.getValue<double>("egocontroller.idleForce"),
                kvc.getValue<double>("egocontroller.Ksteering"),
                kvc.getValue<double>("egocontroller.maximumSteeringRate"),
                kvc.getValue<double>("egocontroller.Kthrottle"),
                kvc.getValue<double>("egocontroller.tauBrake"),
                kvc.getValue<double>("egocontroller.KstaticBrake"),
                kvc.getValue<double>("egocontroller.KdynamicBrake"),
                kvc.getValue<double>("egocontroller.distanceCenterOfMassToFrontAxle"),
                kvc.getValue<double>("egocontroller.distanceCenterOfMassToRearAxle"),
                kvc.getValue<double>("egocontroller.momentOfInertia"),
                kvc.getValue<double>("egocontroller.skewStiffnessFront"),
                kvc.getValue<double>("egocontroller.skewStiffnessRear"));
    }

    ForceControlBehaviourSimplifiedBicycleModel* EgoController::createForceControlBehaviourSimplifiedBicycleModel() {
        KeyValueConfiguration kvc = getKeyValueConfiguration();

        return new ForceControlBehaviourSimplifiedBicycleModel(
                kvc.getValue<double>("egocontroller.minimumTurningRadius"),
                kvc.getValue<double>("egocontroller.vehicleMass"),
                kvc.getValue<double>("egocontroller.adherenceCoefficient"),
                kvc.getValue<double>("egocontroller.idleForce"),
                kvc.getValue<double>("egocontroller.Ksteering"),
                kvc.getValue<double>("egocontroller.maximumSteeringRate"),
                kvc.getValue<double>("egocontroller.Kthrottle"),
                kvc.getValue<double>("egocontroller.tauBrake"),
                kvc.getValue<double>("egocontroller.KstaticBrake"),
                kvc.getValue<double>("egocontroller.KdynamicBrake"),
                kvc.getValue<double>("egocontroller.wheelbase") );
    }

    ModuleState::MODULE_EXITCODE EgoController::body() {
        KeyValueConfiguration kvc = getKeyValueConfiguration();

        m_device = kvc.getValue<string>("egocontroller.device");
        transform(m_device.begin(), m_device.end(), m_device.begin(), ptr_fun(::tolower));

        string behaviorType = kvc.getValue<string>("egocontroller.behavior");
        transform(behaviorType.begin(), behaviorType.end(), behaviorType.begin(), ptr_fun(::tolower));

        ControlBehaviour* behaviour = NULL;
        if (behaviorType == "force") {
            cerr << "Using force control." << endl;
            behaviour = createForceControlBehaviour();
        } else if (behaviorType == "forcebicycle") {
            cerr << "Using bicycle control." << endl;
            behaviour = createForceControlBehaviourBicycleModel();
        } else if (behaviorType == "forcesimplifiedbicycle") {
            cerr << "Using simplified bicycle control." << endl;
            behaviour = createForceControlBehaviourSimplifiedBicycleModel();
        } else if (behaviorType == "simple") {
            cerr << "Using simple control." << endl;

            stringstream vehicleTranslation;
            vehicleTranslation << "egocontroller.simpleStart";
            Point3 translation(getKeyValueConfiguration().getValue<string>(vehicleTranslation.str()));

            stringstream vehicleRotZ;
            vehicleRotZ << "egocontroller.simpleRotZ";
            const double rotZ = getKeyValueConfiguration().getValue<double>(vehicleRotZ.str());

            behaviour = new SimpleControlBehaviour(translation, rotZ);
        }

        if (behaviour == NULL) {
            cerr << "Cannot create control behavior " << behaviorType << endl;
            return ModuleState::SERIOUS_ERROR;
        }

        Controller* controller = NULL;
        if (m_device == "keyboard") {
            controller = new KeyboardController(*behaviour, 'w', 's','a','d','b');
        }
        else {
            // Try joystick.
            controller = new JoystickController(*behaviour, m_device);
        }

        if (controller == NULL) {
            cerr << "Cannot create controller for " << m_device << endl;
            return ModuleState::SERIOUS_ERROR;
        }


        while (getModuleState() == ModuleState::RUNNING) {
            controller->doWork();

            Container container(Container::EGOSTATE, controller->getEgoState());
            getConference().send(container);
        }

        return ModuleState::OKAY;
    }
} // egocontroller
