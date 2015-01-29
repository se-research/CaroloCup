/*
 * Mini-Smart-Vehicles.
 *
 * This software is open source. Please see COPYING and AUTHORS for further information.
 */
#include <iostream>
#include "core/data/Container.h"
#include "core/io/ContainerConference.h"
#include "core/io/URL.h"
#include "core/io/StreamFactory.h"
#include "GeneratedHeaders_msv.h"
#include "CsvExporterSensor.h"

namespace msv
{
  using namespace core::base;
  using namespace core::data;
  using std::cout;
  using namespace core::io;

  CsvExporterSensor::CsvExporterSensor (const int32_t &argc, char **argv) :
      ConferenceClientModule (argc, argv, "csvexportersensor"), frame_count (0), m_debug (
	  false), m_fifo (), m_out (NULL)
  {

    stringstream recordingURL;
    recordingURL << "file://" << "sensor_csv_"
	<< TimeStamp ().getYYYYMMDD_HHMMSS () << ".csv";
    URL _url (recordingURL.str ());
    m_out = &(StreamFactory::getInstance ().getOutputStream (_url));

  }

  CsvExporterSensor::~CsvExporterSensor ()
  {
    cout << "Clearing queue... ";
    if (m_out != NULL)
      {
	m_out->flush ();
      }
    cout << "done." << endl;
  }

  void
  CsvExporterSensor::setUp ()
  {

  }

  ModuleState::MODULE_EXITCODE
  CsvExporterSensor::body ()
  {

    addDataStoreFor (m_fifo);
    SensorBoardData sensorData;

    cout << "waiting for data....." << endl;
    while (getModuleState () == ModuleState::RUNNING)
      {
	while (!m_fifo.isEmpty ())
	  {
	    Container c = m_fifo.leave ();
	    if (c.getDataType () == Container::USER_DATA_0)
	      {
		TimeStamp timeNow;
		sensorData = c.getData<SensorBoardData> ();

		int IRdis_SL = sensorData.getDistance (0); // Side Left IR
		int IRdis_RL = sensorData.getDistance (1); // Rear Left IR
		int IRdis_RR = sensorData.getDistance (2); // Rear Right IR
		int IRdis_SR = sensorData.getDistance (3); // Side Right IR
		int USFront = sensorData.getDistance (4); // Front UltraSonic
		int USRear = sensorData.getDistance (5); // Rear UltraSonic

		//WheelEncoder
		int Distance = sensorData.getDistance (6); // WheeelEncoder Data (mm)

		if (m_out != NULL)
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

    if (m_out != NULL)
      {
	m_out->flush ();
      }

    return ModuleState::OKAY;
  }

  void
  CsvExporterSensor::tearDown ()
  {

  }
}
