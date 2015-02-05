/*
 * Mini-Smart-Vehicles.
 *
 * This software is open source. Please see COPYING and AUTHORS for further information.
 */

#include <ctype.h>
#include <cstring>
#include <cmath>
#include <string>

#include "core/base/Thread.h"
#include "core/base/KeyValueConfiguration.h"
#include "core/data/Container.h"
#include "core/data/TimeStamp.h"
#include "core/data/control/VehicleControl.h"
#include "core/base/LIFOQueue.h"
#include "core/wrapper/SerialPortFactory.h"
#include "core/wrapper/SerialPort.h"
#include "core/base/Lock.h"
#include "core/base/Mutex.h"


#include "Proxy.h"
#include "sensorProtocol.h"

#include "OpenCVCamera.h"

#ifdef HAVE_UEYE
#include "uEyeCamera.h"
#endif


namespace msv {

using namespace std;
using namespace core::base;
using namespace core::data;
using namespace core::data::control;
using namespace tools::recorder;

Proxy::Proxy(const int32_t &argc, char **argv) :
		ConferenceClientModule(argc, argv, "proxy"),
		m_recorder(NULL),
		m_camera(NULL),
		previousValues(),
		currentValues(),
		m_sensorBoardMutex(),
		m_sensorBoardData(),
		m_debug(false),
		m_useRealSpeed(false),
		logger(),
		timestamp()
		{

}

Proxy::~Proxy() {
}

void Proxy::setUp() {
	// This method will be call automatically _before_ running body().
	if (getFrequency() < 20) {
		cerr << endl << endl
				<< "Proxy: WARNING! Running proxy with a LOW frequency (consequence: data updates are too seldom and will influence your algorithms in a negative manner!) --> suggestions: --freq=20 or higher! Current frequency: "
				<< getFrequency() << " Hz." << endl << endl << endl;
	}
	stringstream loggerName;
	loggerName<<"proxy_"<<TimeStamp().getYYYYMMDD_HHMMSS()<<".log";
	logger.open(loggerName.str().c_str(),ios::out | ios::app);

	// Get configuration data.
	KeyValueConfiguration kv = getKeyValueConfiguration();
	m_debug=kv.getValue<uint32_t>("proxy.debug")==1;
	// Create built-in recorder.
	const bool useRecorder = kv.getValue<uint32_t>("proxy.useRecorder") == 1;
	 m_useRealSpeed = kv.getValue<uint32_t>("proxy.Actuator.UseRealSpeed") == 1;
	if (useRecorder) {
		// URL for storing containers.
		stringstream recordingURL;
		recordingURL << kv.getValue<string>("proxy.recorder.output");
		recordingURL << "proxy_" << TimeStamp().getYYYYMMDD_HHMMSS() << ".rec";
		// Size of memory segments.
		const uint32_t MEMORY_SEGMENT_SIZE =
				getKeyValueConfiguration().getValue<uint32_t>(
						"global.buffer.memorySegmentSize");
		// Number of memory segments.
		const uint32_t NUMBER_OF_SEGMENTS = getKeyValueConfiguration().getValue<
				uint32_t>("global.buffer.numberOfMemorySegments");

		m_recorder = new Recorder(recordingURL.str(), MEMORY_SEGMENT_SIZE,
				NUMBER_OF_SEGMENTS);
	}

	// Create the camera grabber.
	const string NAME = getKeyValueConfiguration().getValue<string>(
			"proxy.camera.name");
	string TYPE = getKeyValueConfiguration().getValue<string>(
			"proxy.camera.type");
	std::transform(TYPE.begin(), TYPE.end(), TYPE.begin(), ::tolower);
	const uint32_t ID = getKeyValueConfiguration().getValue<uint32_t>(
			"proxy.camera.id");
	const uint32_t WIDTH = getKeyValueConfiguration().getValue<uint32_t>(
			"proxy.camera.width");
	const uint32_t HEIGHT = getKeyValueConfiguration().getValue<uint32_t>(
			"proxy.camera.height");
	const uint32_t BPP = getKeyValueConfiguration().getValue<uint32_t>(
			"proxy.camera.bpp");

	if (TYPE.compare("opencv") == 0) {
		m_camera = new OpenCVCamera(NAME, ID, WIDTH, HEIGHT, BPP);
	}
	if (TYPE.compare("ueye") == 0) {
#ifdef HAVE_UEYE
		m_camera = new uEyeCamera(NAME, ID, WIDTH, HEIGHT, BPP);
#endif
	}

	if (m_camera == NULL) {
		cerr << "No valid camera type defined." << endl;
		log("No valid camera type defined.");
	}


}

void Proxy::nextString(const string &s)
{
	log("Recieved Sensor Data ");
	cout<<"data,"<<s<<endl;
	int posIstr=s.find(':');
	int posUstr=s.find(':',posIstr+1);
	int posLstr=s.find(':',posUstr+1);
	int posI = s.find('i');
	string iStr=s.substr(posI,posIstr);
	string uStr=s.substr(posIstr+1,posUstr-posIstr);
	string lStr=s.substr(posUstr+1,posLstr-posUstr);
	string wStr=s.substr(posLstr+1);
    if (iStr[0] == 'i') {

		char firstInfra[2];
		firstInfra[0] = iStr[1];
		firstInfra[1] = iStr[2];

		char secondInfra[2];
		secondInfra[0] = iStr[4];
		secondInfra[1] = iStr[5];

		char thirdInfra[2];
		thirdInfra[0] = iStr[7];
		thirdInfra[1] = iStr[8];

		char fourthInfra[2];
		fourthInfra[0] = iStr[10];
		fourthInfra[1] = iStr[11];
		char three = iStr[3];     //the ',' symbol
		char six = iStr[6];      //the ',' symbol
		char nine = iStr[9];		//the ',' symbol
		int firstInfraDist;
		int secondInfraDist;
		int thirdInfraDist;
		int fourthInfraDist;

		if (three == ',' && six == ',' && nine == ',' ) {
			firstInfraDist = converter(firstInfra, 2);
			secondInfraDist = converter(secondInfra, 2);
			thirdInfraDist = converter(thirdInfra, 2);
			fourthInfraDist = converter(fourthInfra, 2);
			{
				Lock l(m_sensorBoardMutex);
				m_sensorBoardData.update(1, secondInfraDist);
				m_sensorBoardData.update(0, firstInfraDist);
				m_sensorBoardData.update(2, thirdInfraDist);
				m_sensorBoardData.update(3, fourthInfraDist);
			}
		}
		if (m_debug) {
			cout << "proxy:" << s << endl;
//			cout << "uStr:" << uStr << endl;
//			cout << "iStr:" << iStr << endl;
//			cout << "lStr:" << lStr << endl;
//			cout << "wStr:" << wStr <<endl;
			cout << "Found First: " << m_sensorBoardData.getDistance(0) << endl;
			cout << "Found Second: " << m_sensorBoardData.getDistance(1)
					<< endl;
			cout << "Found Third: " << m_sensorBoardData.getDistance(2) << endl;
			cout << "Found Fourth: " << m_sensorBoardData.getDistance(3)
					<< endl;

		}

	}

    if(uStr[0]=='u')
    {
		char firstUltra[3];
		firstUltra[0] = uStr[1];
		firstUltra[1] = uStr[2];
		firstUltra[2] = uStr[3];

		char secondUltra[3];
		secondUltra[0] = uStr[5];
		secondUltra[1] = uStr[6];
		secondUltra[2] = uStr[7];

		char three = uStr[4];        //The ',' symbol

		int firstUltraDist;
		int secondUltraDist;

		if (three == ',') {
			firstUltraDist = converter(firstUltra, 3);
			secondUltraDist = converter(secondUltra, 3);

			{
				Lock l(m_sensorBoardMutex);
				m_sensorBoardData.update(4, firstUltraDist);
				m_sensorBoardData.update(5, secondUltraDist);
			}
		}
		 if(m_debug)
		    {
		    	cout << "Found First Ultra: " << m_sensorBoardData.getDistance(4) << endl;
		    	cout << "Found Second Ultra: " << m_sensorBoardData.getDistance(5) << endl;
		    }
    }

    if(wStr[0]=='w')
    {
			char wheelEncoder[4];
			wheelEncoder[0] = wStr[1];
			wheelEncoder[1] = wStr[2];
			wheelEncoder[2] = wStr[3];
			wheelEncoder[3] = wStr[4];
			int distanceTraveled;
			distanceTraveled=converter(wheelEncoder,4);

			{
				Lock l(m_sensorBoardMutex);
				m_sensorBoardData.update(6, distanceTraveled);
			}
			if(m_debug)
			     {
			  	    	cout << "Found Encoder: " << m_sensorBoardData.getDistance(6) << endl;
			      }
    }
    if(lStr[0]=='l')
      {
	char ambientLight[3];
	ambientLight[0]=lStr[1];
	ambientLight[1]=lStr[2];
	ambientLight[2]=lStr[3];
	int lux=converter(ambientLight,3);
	{
		Lock l(m_sensorBoardMutex);
		m_sensorBoardData.update(7, lux);
	}
	if(m_debug)
	{
	   	cout << "Found Light: " << m_sensorBoardData.getDistance(7) << endl;
	}
      }

    log("Converted sensor values "+m_sensorBoardData.toString());

}

int Proxy::converter(char* arrayInput, int lenght){
     	int num = 0;

	for(int i = 0;i < lenght; i++){
		if(arrayInput[i] < '0' || arrayInput[i] > '9') {
			arrayInput[i] = '0';
		}
		arrayInput[i] = arrayInput[i] - '0';
		num = (arrayInput[i]*pow(10, lenght-i-1)+num);

	}

    return num;

}//End of Converter function
void Proxy::tearDown() {
	// This method will be call automatically _after_ return from body().
	log("TearDown.");
	OPENDAVINCI_CORE_DELETE_POINTER(m_recorder);
	OPENDAVINCI_CORE_DELETE_POINTER(m_camera);
	logger.close();
}

void Proxy::distribute(Container c) {
	// Store data to recorder.
	c.setReceivedTimeStamp(TimeStamp());
	if (m_recorder != NULL) {
		// Time stamp data before storing.
		m_recorder->store(c);
	}

	// Share data.
	getConference().send(c);
}

void Proxy::log(const string &s)
{
	TimeStamp time=TimeStamp()-timestamp;
	logger<<time.getSeconds()<<"."<<time.getFractionalMicroseconds() <<":"<<s<<endl;
}

// This method will do the main data processing job.
ModuleState::MODULE_EXITCODE Proxy::body() {




	//initialize sensors
	for (uint32_t i = 0;i< getKeyValueConfiguration().getValue<uint32_t>("proxy.numberOfSensors"); i++)
	{
		stringstream sensorID;
		sensorID << "proxy.sensor" << i << ".id";
		uint16_t id(
				getKeyValueConfiguration().getValue<uint32_t>(sensorID.str()));

		// Initialize m_sensorBoardData data structure for this sensor.
		m_sensorBoardData.update(id, -1);

	}
	//End sensor initialization

	//Setup serial port for sensors
	 core::wrapper::SerialPort *serialPort = core::wrapper::SerialPortFactory::createSerialPort(getKeyValueConfiguration().getValue<string>("proxy.Sensor.SerialPort"), getKeyValueConfiguration().getValue<uint32_t>("proxy.Sensor.SerialSpeed"));
	sensorProtocol sp;
	sp.setStringListener(this);
	serialPort->setPartialStringReceiver(&sp);
	serialPort->start();

	//setupSerial port for Actuators
	ArduinoMegaProtocol m_protocol(getKeyValueConfiguration().getValue<string>("proxy.Actuator.SerialPort"),10);

	uint32_t captureCounter = 0;
	LIFOQueue lifo;
	addDataStoreFor(lifo);
	VehicleControl vc;
	bool dataFound = false;

	while (getModuleState() == ModuleState::RUNNING) {
		// Capture frame.
		if (m_camera != NULL) {
			core::data::image::SharedImage si = m_camera->capture();

			Container c(Container::SHARED_IMAGE, si);
			distribute(c);
			captureCounter++;
		}

		// Get sensor data from IR/US, and distribute
		{
			Lock l(m_sensorBoardMutex);
			Container sensorData(Container::USER_DATA_0, m_sensorBoardData);
			distribute(sensorData);
		}

		//Get driver data and send it to the arduino
		while (!lifo.isEmpty() && !dataFound) {
			Container con = lifo.pop();
			if (con.getDataType() == Container::VEHICLECONTROL) {
				vc = con.getData<VehicleControl>();
				dataFound = true;
				cout<<"found data"<<endl;
				log("Found data:"+vc.toString() +" at timestamp "+(con.getReceivedTimeStamp()-timestamp).toString());
			}
		}
		lifo.clear();

		currentValues.speed = vc.getSpeed();
		currentValues.steeringAngle = vc.getSteeringWheelAngle();
		currentValues.leftFlash = vc.getLeftFlashingLights();
		currentValues.rightFlash = vc.getRightFlashingLights();
		currentValues.brakeLight=vc.getBrakeLights();

		if (m_debug) {
			cout << "speed" << currentValues.speed << endl;
			cout << "steeringAngle" << currentValues.steeringAngle << endl;
			cout << "leftFlash" << currentValues.leftFlash << endl;
			cout << "rightFlash" << currentValues.rightFlash << endl;
			cout << "brakeLight" << currentValues.brakeLight << endl;
		}

		if ((previousValues.speed > currentValues.speed) || (previousValues.speed < currentValues.speed) ) {
			stringstream logs;
			if(m_useRealSpeed){
				bool reverse= currentValues.speed<0? true:false;
				m_protocol.setWheelFrequency(uint8_t(abs(currentValues.speed)*10),reverse);
				logs<<"Set Wheel Frequency to "<<currentValues.speed<<endl;
				log(logs.str());
			}
			else{
				m_protocol.setSpeed(currentValues.speed);
				logger<<"set speed to "<<currentValues.speed<<endl;
				log(logs.str());
			}
			previousValues.speed=(int)currentValues.speed;
		}
		if ((previousValues.steeringAngle < currentValues.steeringAngle) || (previousValues.steeringAngle > currentValues.steeringAngle)) {
			stringstream logs;
			m_protocol.setSteeringAngle((int)currentValues.steeringAngle);
			previousValues.steeringAngle=(int)currentValues.steeringAngle;

			logs<<"Set Steering angle to "<<currentValues.steeringAngle<<endl;
			log(logs.str());
		}

		if((previousValues.leftFlash != currentValues.leftFlash) || (previousValues.rightFlash != currentValues.rightFlash))
		{
			m_protocol.setIndicatorsStop();
			if(currentValues.rightFlash)
			{
				m_protocol.setIndicatorsRight();
			}
			if(currentValues.leftFlash)
			{
				m_protocol.setIndicatorsLeft();
			}
			previousValues.leftFlash = currentValues.leftFlash;
			previousValues.rightFlash = currentValues.rightFlash;
		}
		dataFound=false;
	}

	cout << "Proxy: Captured " << captureCounter << " frames." << endl;
	//stop the car when proxy is stopped
	if(m_useRealSpeed) {
	  m_protocol.setWheelFrequency(0, false);
	} else {
	  m_protocol.setSpeed(0); 
	}
	m_protocol.setSteeringAngle(0);
	sp.setStringListener(NULL);
	serialPort->stop();
	core::base::Thread::usleep(1000*1000);
	OPENDAVINCI_CORE_DELETE_POINTER(serialPort);
	log("Proxy killed");
	return ModuleState::OKAY;
}
} // msv

