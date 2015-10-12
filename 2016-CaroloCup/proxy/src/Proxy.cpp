/**
 * proxy - Sample application to encapsulate HW/SW interfacing with embedded systems.
 * Copyright (C) 2012 - 2015 Christian Berger
 *
 * This program is free software; you can redistribute it and/or
 * modify it under the terms of the GNU General Public License
 * as published by the Free Software Foundation; either version 2
 * of the License, or (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301, USA.
 */

#include "core/base/KeyValueConfiguration.h"
#include "core/data/Container.h"
#include "core/data/TimeStamp.h"

#include <core/SharedPointer.h>
#include <core/wrapper/SerialPort.h>
#include <core/wrapper/SerialPortFactory.h>

#include <core/base/ProtoSerializerVisitor.h>
#include <core/base/ProtoDeserializerVisitor.h>
#include "GeneratedHeaders_AutomotiveData.h"

#include "OpenCVCamera.h"

#ifdef HAVE_UEYE
    #include "uEyeCamera.h"
#endif

#include "Proxy.h"

	using namespace std;
        using namespace core::base;
        using namespace core::data;
        using namespace tools::recorder;
	
	  const string SERIAL_PORT = "/dev/ttyACM0";
	  const uint32_t BAUD_RATE = 9600;
	 
	  stringstream netstring;
	  
namespace automotive {
    namespace miniature {

        
        Proxy::Proxy(const int32_t &argc, char **argv) :
	        TimeTriggeredConferenceClientModule(argc, argv, "proxy"),
	    serial(core::wrapper::SerialPortFactory::createSerialPort(SERIAL_PORT, BAUD_RATE)),
            m_recorder(NULL),
            m_camera(NULL)
        {}

        Proxy::~Proxy() {}

	string decodedNetstring(string netstring){
	  //if the Netstring is less than 3 characters, it's either an invalid one or contains an empty string
	  if (netstring.length() < 3) return "";
	  int semicolonIndex = netstring.find(':');
	  // if there's no semicolon, then it's an invalid Netstring
	  if (semicolonIndex < 0) return "";
	  //parse until the semicolon. Those should be the control digits
	  string parsedDigits = netstring.substr(0, semicolonIndex);
	  int controlDigits;
	  stringstream ss(parsedDigits);
	  ss>>controlDigits;
	  //if the control digit is smaller than 1, then it's either not a digit or the Netstring is invalid
	  if (controlDigits < 1) return "";
	  //parse after the semicolon until the end of the string
	  string command = netstring.substr(semicolonIndex+1);
	  // if it's an empty string, return "error"
	  if (command.empty()) return "";
	  //if last character is a comma, remove it
	  if (command.substr(command.length() -1) == ",") command.erase(command.length()-1);
	  //if string's length isn't equal with the control digits, it's an invalid Netstring
	  if ((int)command.length() != controlDigits) return "";
	return command;
	}
	
	Container decodePayload(string payload){
	  cout << "payload length:" << payload.length() << endl;
	  stringstream proto(payload);
	  ProtoDeserializerVisitor protoDeserializerVisitor;
          protoDeserializerVisitor.deserializeDataFromNoHeader(proto);
	    
	  automotive::carolocup::Sensors sd;
	  sd.accept(protoDeserializerVisitor);
	  
	  cout << sd.toString()<< endl;
	      SensorBoardData m_sensorBoardData;
		m_sensorBoardData.putTo_MapOfDistances(0, sd.getUsFront());
		m_sensorBoardData.putTo_MapOfDistances(1, sd.getIrFrontRight());
		m_sensorBoardData.putTo_MapOfDistances(2, sd.getIrRearRight());
		m_sensorBoardData.putTo_MapOfDistances(3, sd.getIrBackRight());
		m_sensorBoardData.putTo_MapOfDistances(4, sd.getUsRear());
		m_sensorBoardData.putTo_MapOfDistances(5, sd.getIrBackLeft());
		Container sensorData(Container::USER_DATA_0, m_sensorBoardData);
		return sensorData;
	}
	
	void Proxy::nextString(const string &s) {
	 for(int i= 0; i< (int)s.length();i++){
	    netstring << s[i];
	    if(s[i] == ','){
	      distribute(decodePayload(decodedNetstring(netstring.str())));
	      netstring.str(std::string());
	    }
	  }
	}
       

        void Proxy::setUp() {
	        // This method will be call automatically _before_ running body().
            if (getFrequency() < 20) {
                cerr << endl << endl << "Proxy: WARNING! Running proxy with a LOW frequency (consequence: data updates are too seldom and will influence your algorithms in a negative manner!) --> suggestions: --freq=20 or higher! Current frequency: " << getFrequency() << " Hz." << endl << endl << endl;
            }

            // Get configuration data.
            KeyValueConfiguration kv = getKeyValueConfiguration();

            // Create built-in recorder.
            const bool useRecorder = kv.getValue<uint32_t>("proxy.useRecorder") == 1;
            if (useRecorder) {
                // URL for storing containers.
                stringstream recordingURL;
                recordingURL << "file://" << "proxy_" << TimeStamp().getYYYYMMDD_HHMMSS() << ".rec";
                // Size of memory segments.
                const uint32_t MEMORY_SEGMENT_SIZE = getKeyValueConfiguration().getValue<uint32_t>("global.buffer.memorySegmentSize");
                // Number of memory segments.
                const uint32_t NUMBER_OF_SEGMENTS = getKeyValueConfiguration().getValue<uint32_t>("global.buffer.numberOfMemorySegments");
                // Run recorder in asynchronous mode to allow real-time recording in background.
                const bool THREADING = true;
                // Dump shared images and shared data?
                const bool DUMP_SHARED_DATA = getKeyValueConfiguration().getValue<uint32_t>("proxy.recorder.dumpshareddata") == 1;

                m_recorder = new Recorder(recordingURL.str(), MEMORY_SEGMENT_SIZE, NUMBER_OF_SEGMENTS, THREADING, DUMP_SHARED_DATA);
            }

            // Create the camera grabber.
            const string NAME = getKeyValueConfiguration().getValue<string>("proxy.camera.name");
            string TYPE = getKeyValueConfiguration().getValue<string>("proxy.camera.type");
            std::transform(TYPE.begin(), TYPE.end(), TYPE.begin(), ::tolower);
            const uint32_t ID = getKeyValueConfiguration().getValue<uint32_t>("proxy.camera.id");
            const uint32_t WIDTH = getKeyValueConfiguration().getValue<uint32_t>("proxy.camera.width");
            const uint32_t HEIGHT = getKeyValueConfiguration().getValue<uint32_t>("proxy.camera.height");
            const uint32_t BPP = getKeyValueConfiguration().getValue<uint32_t>("proxy.camera.bpp");

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
            }
            
	      
	      // This instance will handle any bytes that are received
	      // from our serial port.
	      serial->setStringListener(this);
	      
	      // Start receiving bytes.
	      serial->start();
        }

        void Proxy::tearDown() {
	        // This method will be call automatically _after_ return from body().
            OPENDAVINCI_CORE_DELETE_POINTER(m_recorder);
            OPENDAVINCI_CORE_DELETE_POINTER(m_camera);
	    // Stop receiving bytes and unregister our handler.
	    /*
	    automotive::carolocup::Control cc;
	    cc.setAcceleration(0);
	    cc.setSteering(0);
	    cc.setLights(0);
	    ProtoSerializerVisitor protoSerializerVisitor;
	    cc.accept(protoSerializerVisitor);
	    stringstream proto;
	    protoSerializerVisitor.getSerializedDataNoHeader(proto);
	    stringstream netstring;
	    netstring << proto.str().length() << ':' << proto.str() << ',';
	    serial->send(netstring.str());
	    */
	    serial->stop();
	    serial->setStringListener(NULL);
        }

        void Proxy::distribute(Container c) {
            // Store data to recorder.
            if (m_recorder != NULL) {
                // Time stamp data before storing.
                c.setReceivedTimeStamp(TimeStamp());
                m_recorder->store(c);
            }

            // Share data.
            getConference().send(c);
        }

        // This method will do the main data processing job.
        coredata::dmcp::ModuleExitCodeMessage::ModuleExitCode Proxy::body() {
	    KeyValueDataStore &kvs = getKeyValueDataStore();
            uint32_t captureCounter = 0;
            while (getModuleStateAndWaitForRemainingTimeInTimeslice() == coredata::dmcp::ModuleStateMessage::RUNNING) {
                // Capture frame.
                if (m_camera != NULL) {
                    coredata::image::SharedImage si = m_camera->capture();

                    Container c(Container::SHARED_IMAGE, si);
                    distribute(c);
                    captureCounter++;
                }
                
            Container c = kvs.get(Container::VEHICLECONTROL);
	    VehicleControl vc = c.getData<VehicleControl>();
	    
	    automotive::carolocup::Control cc;
	    
	    cc.setAcceleration(vc.getSpeed());
	    //vc.getSteeringWheelAngle()* (1.0 / (3.141592654 / 180.0))
	    cc.setSteering(vc.getSteeringWheelAngle()* (1.0 / (3.141592654 / 180.0)));
	    cc.setLights(6);
	    ProtoSerializerVisitor protoSerializerVisitor;
	    cc.accept(protoSerializerVisitor);
	    stringstream proto;
	    protoSerializerVisitor.getSerializedDataNoHeader(proto);
	    stringstream netstring;
	    netstring << proto.str().length() << ':' << proto.str() << ',';
	    serial->send(netstring.str());
	    
                // Get sensor data from IR/US. */
            }
	    
	    
	    
            cout << "Proxy: Captured " << captureCounter << " frames." << endl;

            return coredata::dmcp::ModuleExitCodeMessage::OKAY;
        }

    }
} // automotive::miniature

