/*
 * Mini-Smart-Vehicles.
 *
 * This software is open source. Please see COPYING and AUTHORS for further information.
 */
#include <iostream>
#include "opendavinci/odcore/data/Container.h"
#include "opendavinci/odcore/io/conference/ContainerConference.h"
#include "opendavinci/odcore/io/URL.h"
#include "opendavinci/odcore/io/StreamFactory.h"

#include "automotivedata/GeneratedHeaders_AutomotiveData.h"

#include "CsvExporterSensor.h"

namespace msv
{
  using namespace std;
  using namespace odcore::base;
  using namespace odcore::data;
  using namespace odcore::io;
  using namespace automotive::miniature;

  CsvExporterSensor::CsvExporterSensor (const int32_t &argc, char **argv) :
      TimeTriggeredConferenceClientModule (argc, argv, "csvexportersensor"), frame_count (0), m_debug (
	  false), m_fifo (), m_out (NULL)
  {

    stringstream recordingURL;
    recordingURL << "file://" << "sensor_csv_"
	<< TimeStamp ().getYYYYMMDD_HHMMSS () << ".csv";
    URL _url (recordingURL.str ());
    m_out = StreamFactory::getInstance ().getOutputStream (_url);

  }

  CsvExporterSensor::~CsvExporterSensor ()
  {
    cout << "Clearing queue... ";
    if (m_out.get())
      {
	m_out->flush ();
      }
    cout << "done." << endl;
  }

  void
  CsvExporterSensor::setUp ()
  {

  }

  odcore::data::dmcp::ModuleExitCodeMessage::ModuleExitCode
  CsvExporterSensor::body ()
  {

    addDataStoreFor (m_fifo);
    SensorBoardData sensorData;

    cout << "waiting for data....." << endl;
    while (getModuleStateAndWaitForRemainingTimeInTimeslice () == odcore::data::dmcp::ModuleStateMessage::RUNNING)
      {
	while (!m_fifo.isEmpty ())
	  {
	    Container c = m_fifo.leave ();
	    if (c.getDataType () == SensorBoardData::ID())
	      {
		TimeStamp timeNow;
		sensorData = c.getData<SensorBoardData> ();

        int IRdis_SL=-2;
        if(sensorData.containsKey_MapOfDistances(0))
			IRdis_SL = sensorData.getValueForKey_MapOfDistances(0);
	    
        int IRdis_RL=-2;
        if(sensorData.containsKey_MapOfDistances(1))
			IRdis_RL = sensorData.getValueForKey_MapOfDistances(1);
	    
        int IRdis_RR=-2;
        if(sensorData.containsKey_MapOfDistances(2))
			IRdis_RR = sensorData.getValueForKey_MapOfDistances(2);
	    
        int IRdis_SR=-2;
        if(sensorData.containsKey_MapOfDistances(3))
			IRdis_SR = sensorData.getValueForKey_MapOfDistances(3);
	    
        int USFront=-2;
        if(sensorData.containsKey_MapOfDistances(4))
			USFront = sensorData.getValueForKey_MapOfDistances(4);
	    
        int USRear=-2;
        if(sensorData.containsKey_MapOfDistances(5))
			USRear = sensorData.getValueForKey_MapOfDistances(5);
	    
		//int IRdis_SL = sensorData.getDistance (0); // Side Left IR
		//int IRdis_RL = sensorData.getDistance (1); // Rear Left IR
		//int IRdis_RR = sensorData.getDistance (2); // Rear Right IR
		//int IRdis_SR = sensorData.getDistance (3); // Side Right IR
		//int USFront = sensorData.getDistance (4); // Front UltraSonic
		//int USRear = sensorData.getDistance (5); // Rear UltraSonic

		//WheelEncoder
        int Distance=-2;
        if(sensorData.containsKey_MapOfDistances(6))
			Distance = sensorData.getValueForKey_MapOfDistances(6);
	    
		//int Distance = sensorData.getDistance (6); // WheeelEncoder Data (mm)

		if (m_out.get())
		  {
		    cout << " writing sensor data : " << "\n";

		    /*------------------------------------DATA FORMAT(per line)----------------------------------------------------------------------
		     * -------------------------------------------------------------------------------------------------------------------
		     * Time Stamp ; TODO shuffle up the format as you like :)
		     */
		    //Time Stamp
		    (*m_out) << timeNow.getYYYYMMDD_HHMMSSms () << ";";

		    (*m_out) << IRdis_SL << ";";

		    (*m_out) << IRdis_RL << ";";

		    (*m_out) << IRdis_RR << ";";

		    (*m_out) << IRdis_SR << ";";

		    (*m_out) << USFront << ";";

		    (*m_out) << USRear << ";";

		    (*m_out) << Distance << ";";

		    //go to new line
		    (*m_out) << "\n";
		  }

	      }
	  }

      }

    if (m_out.get())
      {
	m_out->flush ();
      }

    return odcore::data::dmcp::ModuleExitCodeMessage::OKAY;
  }

  void
  CsvExporterSensor::tearDown ()
  {

  }
}
