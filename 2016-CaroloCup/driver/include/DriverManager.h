//
// Created by Mickaël on 2015-11-25.
//

#ifndef AUTOMOTIVE_CAROLOCUP_DRIVERMANAGER_H
#define AUTOMOTIVE_CAROLOCUP_DRIVERMANAGER_H


#include "opendavinci/odcore/base/module/TimeTriggeredConferenceClientModule.h"
#include "automotivedata/GeneratedHeaders_AutomotiveData.h"
#include "opendavinci/GeneratedHeaders_OpenDaVINCI.h"
#include "opendavinci/odcore/io/conference/ContainerConference.h"
#include "opendavinci/odcore/data/Container.h"
#include <DriverGeneric.h>


namespace msv {

    using namespace std;
    using namespace odcore::base;
    using namespace odcore::data;
    using namespace automotive;
    using namespace automotive::miniature;

    /**
     * Correspond to the different car states associated to buttons sequences.
     */
    enum DRIVER_STATE {
        Lane_Following = 0,
        Parking = 1,
        Overtaking = 2,
        Neutral = 99
    };


    class DriverManager : public odcore::base::module::TimeTriggeredConferenceClientModule {

    private:
        int32_t argc;
        char **argv;
        DRIVER_STATE driver_state;
        DriverGeneric *driverGeneric;
        bool debug;

        DriverManager(const DriverManager &/*obj*/);

        DriverManager &operator=(const DriverManager &/*obj*/);

        /**
         * This method will be call automatically _before_ running body().
         */
        virtual void setUp() { };

        /**
         * This method will be call automatically _after_ return from body().
         */
        virtual void tearDown() { };


        /**
         * Stops the car by sending the VehicleControl data using the DriverGeneric stop values.
         */
        void stopCar();

    public:
        DriverManager(const int32_t &argci, char **argvi);

        virtual ~DriverManager();

        odcore::data::dmcp::ModuleExitCodeMessage::ModuleExitCode body();
    };

} // msv

#endif //AUTOMOTIVE_CAROLOCUP_DRIVERMANAGER_H
