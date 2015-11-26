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
 * This class is a skeleton to send driving commands to Hesperia-light's vehicle driving dynamics simulation.
 */
    class DriverGeneric : public core::base::module::TimeTriggeredConferenceClientModule {

    private:

        bool initialized;

        DriverGeneric(const DriverGeneric &/*obj*/);

        DriverGeneric &operator=(const DriverGeneric &/*obj*/);

        virtual void setUp() = 0;

        virtual void tearDown() = 0;


    protected:
        float desiredSpeed; // in m/s
        float desiredSteering; // in radians
        bool brakeLights;
        bool flashingLightsLeft;
        bool flashingLightsRight;
        bool debug;

    public:

        DriverGeneric(const int32_t &argc, char **argv);

        virtual ~DriverGeneric();

        coredata::dmcp::ModuleExitCodeMessage::ModuleExitCode body();

        virtual void Routine() = 0;

        virtual void Initialize() = 0;

        VehicleControl GetControlData();

    };

} // msv

#endif //AUTOMOTIVE_CAROLOCUP_DRIVERGENERIC_H
