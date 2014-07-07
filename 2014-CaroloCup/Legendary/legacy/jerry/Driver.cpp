#include<stdio.h>
#include<math.h>

#include "core/io/ContainerConference.h"
#include "core/data/Container.h"
#include "core/data/Constants.h"
#include "core/data/control/VehicleControl.h"
#include "core/data/environment/VehicleData.h"

#include<ctime>

// Data structures from msv-data library:
#include "SteeringData.h"
#include "SensorBoardData.h"
#include "UserButtonData.h"

#include "Driver.h"

namespace msv
{

  using namespace std ;
  using namespace core :: base ;
  using namespace core :: data ;
  using namespace core :: data :: control ;
  using namespace core :: data :: environment ;

  Driver :: Driver(const int32_t&argc,char**argv):
    ConferenceClientModule(argc,argv,"Driver")
  {
    for (int i=0; i<10; i++) prevDistance[i] = 0;
  }

  Driver ::~Driver()
  {
  }

  void Driver :: setUp()
  {
    // This method will be call automatically _before_ running body().
  }

  void Driver :: tearDown()
  {
    // This method will be call automatically _after_ return from body().
  }

  bool check_distance_balk(double ValA, double ValB) ///////detect whether the obstacle exist.
  {
    return fabs(ValA - (-1)) < 0.001 or ValA > ValB or fabs(ValA - (-2)) < 0.001;
  }

  bool check_distance_desc(double Distance[])
  {
    double tmpDistance=0;
    for (int i = 2; i >= 0; i--) {
      if (fabs(Distance[i]) < 0.0001) return false;
      if (fabs(Distance[i] - (-1)) < 0.001) return false;
      if (Distance[i] - tmpDistance > 0.01) tmpDistance = Distance[i];
      else return false;
    }
    return true;
  }

  // This method will do the main data processing job.
  ModuleState :: MODULE_EXITCODE Driver :: body()
  {
    int overStep=0 ;

    while(getModuleState()==ModuleState :: RUNNING)
    {
      // In the following, you find example for the various data sources that are available:

      // 1. Get most recent vehicle data:
      Container containerVehicleData=getKeyValueDataStore().get(Container :: VEHICLEDATA);
      VehicleData vd=containerVehicleData.getData<VehicleData>();
      //cerr<<"Most recent vehicle data: '"<<vd.toString()<<"'"<<endl ;

      // 2. Get most recent sensor board data:
      Container containerSensorBoardData=getKeyValueDataStore().get(Container :: USER_DATA_0);
      SensorBoardData sbd=containerSensorBoardData.getData<SensorBoardData>();
      //cerr<<"Most recent sensor board data: '"<<sbd.toString()<<"'"<<endl ;

      // 3. Get most recent user button data:
      Container containerUserButtonData=getKeyValueDataStore().get(Container :: USER_BUTTON);
      UserButtonData ubd=containerUserButtonData.getData<UserButtonData>();
      //cerr<<"Most recent user button data: '"<<ubd.toString()<<"'"<<endl ;

      // 4. Get most recent steering data as fill from lanedetector for example:
      Container containerSteeringData=getKeyValueDataStore().get(Container :: USER_DATA_1);
      SteeringData sd=containerSteeringData.getData<SteeringData>();
      //cerr<<"Most recent steering data: '"<<sd.toString()<<"'"<<endl ;

      Container containerSpeedData=getKeyValueDataStore().get(Container :: USER_DATA_2);
      SteeringData speedData=containerSpeedData.getData<SteeringData>();

      // Design your control algorithm here depending on the input data from above.
      double tmpAngle ;
      double frontDistance ;
      double frontRightInfraredDistance ;
      double frontRightUrltraSonicDistance ;
      double rearRightUltraSonicDistance ;
      double tmpSpeed=0 ;
      bool BoolBrakeLights ;
      bool BoolLeftFlashingLights ;
      bool BoolRightFlashingLights ;

      int leftOffesetAngle=-8 ;
      int rightOffesetAngle=11;
      ////////offset angles we need when switching the lane.
      double lowSpeed=1 ;
      ////////speed of switching lanes.
      double highSpeed=2 ;
      ////////speed of lane followingh.

      frontDistance=sbd.getDistance(3); ////////Get value of front Ultra Sonic sensor.
      frontRightInfraredDistance=sbd.getDistance(0);///////Get value of front right infra sensor.
      frontRightUrltraSonicDistance=sbd.getDistance(4);//////Get value of front right Ultra Sonic sensor.
      rearRightUltraSonicDistance=sbd.getDistance(5);//////Get value of rear right Ultra Sonic sensor.


      /////// There are two situations we need to define here.
      /////// Situation A : Ultra Sonic must be used to detect long distance.
      /////// Situation B : Inra Sensor must be used to detect short distance.
      /////// Long distnace detection used to phase 2nd -> phase 3rd and phase 3rd -> phase 0.
      /////// Short distance detection used to phase 1st -> phase 2nd.




      /////// Phase 0.
      /////// lane following before detecting the obstacle.
      if(overStep==0)
      {
        /////// If detected the obstacle, turn on the left loght, in low speed, overstep to next phase.
        if ((fabs(sd.getExampleData()) > 1 && (not check_distance_balk(frontDistance, 5))) ||
            (fabs(sd.getExampleData()) <= 1 && (not check_distance_balk(frontDistance, 10.5))))
        {
          cerr<<"+++++++++++++++++++++++"<<frontDistance<<endl;
          BoolBrakeLights=true ;
          BoolLeftFlashingLights=true ;
          BoolRightFlashingLights=false ;
          tmpSpeed=lowSpeed ;
          tmpAngle=leftOffesetAngle ;
          overStep=1 ;
          for (int i = 10 - 1; i >= 0; i--) prevDistance[i] = 0;
        }
        else
        {
          /////// If detected onthing, stay in phase 0.
          BoolBrakeLights=false ;
          BoolLeftFlashingLights=false ;
          BoolRightFlashingLights=false ;
          if (fabs(speedData.getExampleData() - (-1)) < 0.1) tmpSpeed=highSpeed ;
          else tmpSpeed = speedData.getExampleData();
          cerr<<__LINE__<<"***************"<<speedData.getExampleData()<<"*********"<<tmpSpeed<<endl;
          tmpAngle=sd.getExampleData() ;
        }
      }
      /////// Phase 0 -> Phase 1st
      /////// switch to left lane.
      else if(overStep==1)
      {
        ///////Here is the condition whether the front right infra sensor detected the obstacle, if frontRightInfraredDiatance= 1.2, then overstep to next phase.
        if (check_distance_balk(frontRightInfraredDistance, 1.2) && check_distance_desc(prevDistance))
        {
          BoolBrakeLights=false ;
          BoolLeftFlashingLights=false ;
          BoolRightFlashingLights=false ;
          tmpSpeed=lowSpeed ;
          tmpAngle=-leftOffesetAngle ;
          overStep=2 ;
          cerr<<"+++++++++++++++++++++++++++++++++++++++"<<endl;
          for (int i = 0; i < 10; i++)cerr<<prevDistance[i]<<",";
          cerr<<endl;
          for (int i = 10 - 1; i >= 0; i--) prevDistance[i] = 0;
        }
        else
        {
          ////////Still in phase 0 -> phase 1, switching to left lane, in low speed.
          BoolBrakeLights=false ;
          BoolLeftFlashingLights=true ;
          BoolRightFlashingLights=false ;
          tmpSpeed=lowSpeed ;
          tmpAngle=leftOffesetAngle ;
          for (int i = 10 - 1; i > 0; i--) prevDistance[i] = prevDistance[i - 1];
          prevDistance[0] = frontRightInfraredDistance;
        }
      }
      else if(overStep==2)
      {
        ////////Phase 2nd -> Phase 3rd
        /////////When frontRightUltraSonic=3.0, which means the car is far away from obstacle, can switch to right lane, in low speed.

        if (check_distance_balk(frontRightUrltraSonicDistance, 3.0) && check_distance_desc(prevDistance))
        {
          BoolBrakeLights=true ;
          BoolLeftFlashingLights=false ;
          BoolRightFlashingLights=true ;
          tmpSpeed=lowSpeed ;
          tmpAngle=rightOffesetAngle ;
          overStep=3 ;
          cerr<<"+++++++++++++++++++++++++++++++++++++++"<<endl;
          for (int i = 0; i < 10; i++)cerr<<prevDistance[i]<<",";
          cerr<<endl;
          for (int i = 10 - 1; i >= 0; i--) prevDistance[i] = 0;
        }
        else
        {
          ///////Otherwise , keep driving on the left lane.
          BoolBrakeLights=false ;
          BoolLeftFlashingLights=false ;
          BoolRightFlashingLights=false ;
          tmpSpeed=highSpeed ;
          tmpAngle=sd.getExampleData() ;
          for (int i = 10 - 1; i > 0; i--) prevDistance[i] = prevDistance[i - 1];
          prevDistance[0] = frontRightUrltraSonicDistance;
        }
      }
      else
      {
        ///////Phase 3rd -> Phase 0.
        ///////When rearRightUltraSonicDistance=5.2, which means the car has passed the obstacle completly, overtaking is over, back to lane following, in high speed.

        if (check_distance_balk(rearRightUltraSonicDistance, 5.2) && check_distance_desc(prevDistance))
        {
          BoolBrakeLights=false ;
          BoolLeftFlashingLights=false ;
          BoolRightFlashingLights=false ;
          tmpSpeed=highSpeed ;
          tmpAngle= -rightOffesetAngle;
          overStep=0 ;
          cerr<<"+++++++++++++++++++++++++++++++++++++++"<<endl;
          for (int i = 0; i < 10; i++)cerr<<prevDistance[i]<<",";
          cerr<<endl;
        }
        else
        {
          ///////Otherwise still in phase 2nd -> 3rd
          ////// Switching to right lane.
          BoolBrakeLights=false ;
          BoolLeftFlashingLights=false ;
          BoolRightFlashingLights=true ;
          tmpSpeed=lowSpeed ;
          tmpAngle=rightOffesetAngle ;
          for (int i = 10 - 1; i > 0; i--) prevDistance[i] = prevDistance[i - 1];
          prevDistance[0] = rearRightUltraSonicDistance;
        }
      }

      cerr<<__LINE__<<"..step..."<<overStep<<"angle:"<<tmpAngle<<"speed.."<<tmpSpeed<<endl;

      // Create vehicle control data.
      VehicleControl vc ;
      // With setSteeringWheelAngle, you can steer in the range of -26 (left) .. 0 (straight) .. +25 (right)
      double desiredSteeringWheelAngle=tmpAngle ;
      // 4 degree but SteeringWheelAngle expects the angle in radians!
      vc.setSteeringWheelAngle(desiredSteeringWheelAngle*Constants :: DEG2RAD);
      // With setSpeed you can set a desired speed for the vehicle in the range of -2.0 (backwards) .. 0 (stop) .. +2.0 (forwards)
      vc.setSpeed(tmpSpeed);

      // You can also turn on or off various lights:
      vc.setBrakeLights(BoolBrakeLights);
      vc.setLeftFlashingLights(BoolLeftFlashingLights);
      vc.setRightFlashingLights(BoolRightFlashingLights);

      // Create container for finally sending the data.
      Container c(Container :: VEHICLECONTROL,vc);
      // Send container.
      getConference().send(c);
    }

    return ModuleState :: OKAY ;
  }
}
// msv

