//
// Created by Mickaël on 2015-11-25.
//

#ifndef AUTOMOTIVE_CAROLOCUP_DRIVERGENERIC_H
#define AUTOMOTIVE_CAROLOCUP_DRIVERGENERIC_H

#include "core/base/module/TimeTriggeredConferenceClientModule.h"
#include "core/base/KeyValueConfiguration.h"
#include "GeneratedHeaders_AutomotiveData.h"
#include "GeneratedHeaders_CoreData.h"

namespace msv {

    using namespace std;
    using namespace core::base;
    using namespace core::exceptions;
    using namespace automotive;
    using namespace automotive::miniature;

/**
 * This class is virtual and represents the skeleton of a driver.
 * To create a new driver, derive this class and define the virtual method.
 */
    class DriverGeneric : public core::base::module::TimeTriggeredConferenceClientModule {

    private:

        bool initialized;

        DriverGeneric(const DriverGeneric &/*obj*/);

        DriverGeneric &operator=(const DriverGeneric &/*obj*/);

        virtual void setUp() = 0;

        virtual void tearDown() = 0;

    public:
        float desiredSpeed; // in m/s
        float desiredSteering; // in radians
        bool brakeLights;
        bool flashingLightsLeft;
        bool flashingLightsRight;
        bool debug;

        DriverGeneric(const int32_t &argc, char **argv);

        virtual ~DriverGeneric();

        /**
         *  This method contains the main logic of the GenericDriver.
         *  It takes care of the initialization and routine. This is the interface between opendavinci module's system
         *  and the Driver implementation.
         */
        coredata::dmcp::ModuleExitCodeMessage::ModuleExitCode body();

        /**
         *  This is the main routine of the driver. This method is called at each iteration by the DriverManager.
         *  It needs to be defined in child classes.
         */
        virtual void Routine() = 0;

        /**
         *  Used for value initialization, this method is called once by the DriverManager before running the routine.
         *  It needs to be defined in child classes.
         */
        virtual void Initialize() = 0;

        /**
         *  @return A VehicleControl with current desiredSpeed, desiredSteering and light information.
         */
        VehicleControl GetControlData();

        /** This method allow the access to a vehicle data corresponding to a stopped vehicle.
         *
         *  @return A VehicleControl with speed and steering at 0 and all lights off.
         */
        static VehicleControl GetStopControlData();

    };

} // msv

#endif //AUTOMOTIVE_CAROLOCUP_DRIVERGENERIC_H
