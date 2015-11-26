/**
 * Simple application realized with OpenDaVINCI to playback
 * recordings captured from our CaroloCup miniature cars.
 *
 * Copyright (C) 2015 Hugo Andrade, Christian Berger, Federico Giaimo.
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

#include <string>
#include <sstream>

#include <opencv/cv.h>
#include <opencv/highgui.h>

#include <core/SharedPointer.h>
#include <core/base/Lock.h>
#include <core/data/Container.h>
#include <core/io/URL.h>
#include <core/wrapper/SharedMemory.h>
#include <core/wrapper/SharedMemoryFactory.h>
#include <tools/player/Player.h>

#include <GeneratedHeaders_CoreData.h>

#include "LineDetector.h"

using namespace std;

// We add some of OpenDaVINCI's namespaces for the sake of readability.
using namespace core;
using namespace core::base;
using namespace core::data;
using namespace coredata::image;
using namespace core::io;
using namespace core::wrapper;
using namespace tools::player;
using namespace cv;

class CSVRow
{
public:
    string const& operator[](uint32_t index) const
    {
        return m_data[index];
    }
    uint32_t size() const
    {
        return m_data.size();
    }
    bool readNextRow(istream& in)
    {
        string line;
        if(!getline(in,line)) return false;

        stringstream lineStream(line);
        string cell;

        m_data.clear();
        while(getline(lineStream,cell,','))
        {
            m_data.push_back(cell);
        }
        return true;
    }
private:
    vector<string>    m_data;
};

void setLabel(Mat& img, const string label, const Point& anchor)
{
    int fontface = FONT_HERSHEY_COMPLEX;
    double scale = 0.4;
    int thickness = 1;
    int baseline = 0;

    Size text = getTextSize(label, fontface, scale, thickness, &baseline);
    rectangle(img, anchor + Point(0, baseline), anchor + Point(text.width, -text.height), CV_RGB(0,0,0), CV_FILLED);
    putText(img, label, anchor, fontface, scale, CV_RGB(255, 255, 255), thickness, CV_AA);
}

void errorMessage(string name)
{
    cerr << "Wrong command line parameters supplied." << endl;
    cerr << "Usage :\t" << name << " -h" << endl
    << "\t" << name << " [-l] myRecording.rec [myRecording.csv]" << endl
    << "Options :" << endl << " -l \t\t\t Enables logging to standard output" << endl;
}

void helpMessage(string name)
{
    cerr << "Usage :\t" << name << " -h" << endl
    << "\t" << name << " [-l] myRecording.rec [myRecording.csv]" << endl
    << "Options :" << endl << " -h \t\t\t Show this help" << endl
    << " -l \t\t\t Enables logging to standard output" << endl;
}

int32_t main(int32_t argc, char **argv)
{
    LineDetector m_line = LineDetector();


    uint32_t retVal = 0;
    int recIndex=1;
    bool log=false;

    if((argc != 2 && argc != 3 && argc != 4) || (argc==4 && string(argv[1]).compare("-l")!=0))
    {
        errorMessage(string(argv[0]));
        retVal = 1;
    }
    else if(argc==2 && string(argv[1]).compare("-h")==0)
    {
        helpMessage(string(argv[0]));
        retVal = 0;
    }
    else
    {
        // if -l option is set
        if(argc==4 || (argc==3 && string(argv[1]).compare("-l")==0))
        {
            ++recIndex;
            log=true;
        }

        // Use command line parameter as file for playback;
        string recordingFile(argv[recIndex]);
        stringstream recordingFileUrl;
        recordingFileUrl << "file://" << recordingFile;

        // Location of the recording file.
        URL url(recordingFileUrl.str());

        // Do we want to rewind the stream on EOF?
        const bool AUTO_REWIND = false;

        // Size of the memory buffer that should fit at least the size of one frame.
        const uint32_t MEMORY_SEGMENT_SIZE = 1024 * 768;

        // Number of memory segments (one is enough as we are running sychronously).
        const uint32_t NUMBER_OF_SEGMENTS = 1;

        // Run player in synchronous mode without data caching in background.
        const bool THREADING = false;

        // Construct the player.
        Player player(url, AUTO_REWIND, MEMORY_SEGMENT_SIZE, NUMBER_OF_SEGMENTS, THREADING);

        // The next container from the recording.
        Container nextContainer;

        // Using OpenCV's IplImage data structure to simply playback the data.
        IplImage *image = NULL;

        // Create the OpenCV playback window.
        cvNamedWindow("CaroloCup-CameraPlayback", CV_WINDOW_AUTOSIZE);

        // This flag indicates whether we have attached already to the shared
        // memory containing the sequence of captured images.
        bool hasAttachedToSharedImageMemory = false;

        // Using this variable, we will access the captured images while
        // also having convenient automated system resource management.
        SharedPointer<SharedMemory> sharedImageMemory;

        ifstream file(argv[recIndex+1]);
        CSVRow row;
        // read out the header row
        row.readNextRow(file);
        uint32_t frameNumber=1, csvFN;
        int32_t VPx,VPy,BLx,BLy,BRx,BRy,TLx,TLy,TRx,TRy;
        stringstream frameMessage;
        stringstream VPMessage;
        frameMessage.str(string());
        VPMessage.str(string());
        bool fbf=false;

        // Main data processing loop.
        while (player.hasMoreData()) {
            // Read next entry from recording.
            nextContainer = player.getNextContainerToBeSent();

            // Data type SHARED_IMAGE contains a SharedImage data structure that
            // provides meta-information about the captured image.
            if (nextContainer.getDataType() == Container::SHARED_IMAGE) {
                // Read the data structure to retrieve information about the image.
                SharedImage si = nextContainer.getData<SharedImage>();

                // Check if we have already attached to the shared memory.
                if (!hasAttachedToSharedImageMemory) {
                    sharedImageMemory = SharedMemoryFactory::attachToSharedMemory(si.getName());

                    // Toggle the flag as we have now attached to the shared memory.
                    hasAttachedToSharedImageMemory = true;
                }

                // Check if we could successfully attach to the shared memory.
                if (sharedImageMemory->isValid()) {
                    // Using a scoped lock to get exclusive access.
                    {
                        Lock l(sharedImageMemory);
                        if (image == NULL) {
                            // Create the IplImage header data and access the shared memory for the actual image data.
                            image = cvCreateImageHeader(cvSize(si.getWidth(), si.getHeight()), IPL_DEPTH_8U, si.getBytesPerPixel());

                            // Let the IplImage point to the shared memory containing the captured image.
                            image->imageData = static_cast<char*>(sharedImageMemory->getSharedMemory());
                        }
                    }

                    // Show the image using OpenCV.
                    // Process Image in here.



                    // if csv parameter is set
                    if(argc==4 || (argc==3 && string(argv[1]).compare("-l")!=0))
                    {
                        if(! row.readNextRow(file)) break;
                        while(row[0].compare("")==0)
                            if(! row.readNextRow(file)) break;

                        sscanf(row[0].c_str(), "%d", &csvFN);

                        if(frameNumber==csvFN)
                        {
                            Mat img = cvarrToMat(image);


                            frameMessage.str(string());
                            VPMessage.str(string());
                            sscanf(row[9].c_str(), "%d", &VPx);
                            sscanf(row[10].c_str(), "%d", &VPy);

                            frameMessage<<"Frame "<<frameNumber;
                            VPMessage<<"Vanishing Point ("<<VPx<<","<<VPy<<")";

                            setLabel(img, frameMessage.str(), cvPoint(30,45));
                            setLabel(img, VPMessage.str(), cvPoint(30,60));

                            if(log)
                                cout << frameNumber << ", " << VPx << ", " << VPy <<endl;

                            // print support points and lines
                            sscanf(row[1].c_str(), "%d", &BLx);
                            sscanf(row[2].c_str(), "%d", &BLy);BLy+=60;
                            sscanf(row[3].c_str(), "%d", &TLx);
                            sscanf(row[4].c_str(), "%d", &TLy);TLy+=60;
                            sscanf(row[5].c_str(), "%d", &TRx);
                            sscanf(row[6].c_str(), "%d", &TRy);TRy+=60;
                            sscanf(row[7].c_str(), "%d", &BRx);
                            sscanf(row[8].c_str(), "%d", &BRy);BRy+=60;

                            circle(img, Point(BLx,BLy), 5, CV_RGB(255, 255, 255), CV_FILLED);
                            circle(img, Point(TLx,TLy), 5, CV_RGB(255, 255, 255), CV_FILLED);
                            circle(img, Point(TRx,TRy), 5, CV_RGB(255, 255, 255), CV_FILLED);
                            circle(img, Point(BRx,BRy), 5, CV_RGB(255, 255, 255), CV_FILLED);

                            double slope1 = static_cast<double>(TLy-BLy)/static_cast<double>(TLx-BLx);
                            double slope2 = static_cast<double>(TRy-BRy)/static_cast<double>(TRx-BRx);
                            Point p1(0,0), q1(img.cols,img.rows);
                            Point p2(0,0), q2(img.cols,img.rows);
                            p1.y = -(BLx-p1.x) * slope1 + BLy;
                            q1.y = -(TLx-q1.x) * slope1 + TLy;
                            p2.y = -(BRx-p2.x) * slope2 + BRy;
                            q2.y = -(TRx-q2.x) * slope2 + TRy;

                            line(img,p1,q1,CV_RGB(255, 255, 255),1,CV_AA);
                            line(img,p2,q2,CV_RGB(255, 255, 255),1,CV_AA);

                            imshow("CaroloCup-CameraPlayback", img);
                        }
                    }
                    else
                        cvShowImage("CaroloCup-CameraPlayback", image);

                    // Let the image render before proceeding to the next image.
                    char c = cvWaitKey(10);
                    // Check if the user wants to stop the replay by pressing ESC or pause it by pressing SPACE (needed also to go frame-by-frame).
                    if (static_cast<uint8_t>(c) == 27) break;
                    else if (static_cast<uint8_t>(c) == 32 || fbf) {
                        do
                        {
                            c = cvWaitKey();
                        }while(c!='n' && static_cast<uint8_t>(c) != 32 && static_cast<uint8_t>(c) != 27);

                        if (static_cast<uint8_t>(c) == 27) break; // ESC
                        else if (static_cast<uint8_t>(c) == 32) fbf=false; // SPACE -> continue
                        else if (c=='n') fbf=true; // pressed 'n' -> next frame
                    }

                    ++frameNumber;
                }
            }
        }

        // maybe print EOF message && wait for user input?

        // Release IplImage data structure.
        cvReleaseImage(&image);

        // Close playback window.
        cvDestroyWindow("CaroloCup-CameraPlayback");

        // The shared memory will be automatically released.
    }

    // Return error code.
    return retVal;
}
