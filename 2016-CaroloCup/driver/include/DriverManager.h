//
// Created by Mickaël on 2015-11-25.
//

#ifndef AUTOMOTIVE_CAROLOCUP_DRIVERMANAGER_H
#define AUTOMOTIVE_CAROLOCUP_DRIVERMANAGER_H


#include "core/base/module/TimeTriggeredConferenceClientModule.h"
#include "GeneratedHeaders_AutomotiveData.h"
#include "GeneratedHeaders_CoreData.h"
#include "core/io/conference/ContainerConference.h"
#include "core/data/Container.h"
#include <DriverGeneric.h>


namespace msv {

    using namespace std;
    using namespace core::base;
    using namespace core::data;
    using namespace automotive;
    using namespace automotive::miniature;

    enum DRIVER_STATE {
        Lane_Following = 0,
        Parking = 1,
        Overtaking = 2,
        None = 99
    };


    class DriverManager : public core::base::module::TimeTriggeredConferenceClientModule {

    private:
        int32_t argc;
        char **argv;
        DRIVER_STATE state;
        DriverGeneric *driver_ptr;
        DriverGeneric *driverHelper_ptr;
        bool debug;

        DriverManager(const DriverManager &/*obj*/);

        DriverManager &operator=(const DriverManager &/*obj*/);

        // This method will be call automatically _before_ running body().
        virtual void setUp() { };

        // This method will be call automatically _after_ return from body().
        virtual void tearDown() { };

    public:
        DriverManager(const int32_t &argci, char **argvi);

        virtual ~DriverManager();

        coredata::dmcp::ModuleExitCodeMessage::ModuleExitCode body();
    };

} // msv

#endif //AUTOMOTIVE_CAROLOCUP_DRIVERMANAGER_H
