/*
* Mini-Smart-Vehicles.
*
* This software is open source. Please see COPYING and AUTHORS for further information.
*/

#include <stdio.h>
#include <math.h>
#include <sstream>

#include "core/io/ContainerConference.h"
#include "core/data/Container.h"
#include "core/data/Constants.h"
#include "core/data/control/VehicleControl.h"
#include "core/data/environment/VehicleData.h"
#include "core/base/LIFOQueue.h"

// Data structures from msv-data library:
#include "SensorBoardData.h"

#include "MappingNew.h"

namespace carolocup
{

using namespace std;
using namespace core::base;
using namespace core::data;
using namespace core::data::control;
using namespace core::data::environment;

// Constructor
MappingNew::MappingNew(const int32_t &argc, char **argv) :
    ConferenceClientModule(argc, argv, "MappingNew") ,
    m_hasReceivedLaneDetectionData(false)
{
   
}

// Destructor
MappingNew::~MappingNew() {}

void MappingNew::setUp()
{
    // This method will be call automatically _before_ running body().
   
}

void MappingNew::tearDown()
{
    // This method will be call automatically _after_ return from body().
}



// This method will do the main data processing job.
ModuleState::MODULE_EXITCODE MappingNew::body()
{
    core::base::LIFOQueue lifo;
    
    while (getModuleState() == ModuleState::RUNNING)
    {
        m_hasReceivedLaneDetectionData = false;


     cout<<"THIS IS THE MAPPING COMPONENT \n"<<endl;
  
        while (!lifo.isEmpty())
        {
            // Read the recently received container.
            Container con = lifo.pop();

            // Check the container type if it is one of our expected types.
            if (con.getDataType() == Container::USER_DATA_1)
            {
                // We have found our expected container.
                m_hasReceivedLaneDetectionData = true;
                break;
            }
        }
        lifo.clear();

    }
    return ModuleState::OKAY;
}

} // msv
