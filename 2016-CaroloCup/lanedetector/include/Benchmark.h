#ifndef LANEDETECTOR_BENCHMARK_H
#define LANEDETECTOR_BENCHMARK_H
#include <fstream>
#include <memory>
#include <opencv/cv.h>
#include <dirent.h>
#include <opendavinci/odcore/io/URL.h>
#include "opendavinci/odtools/player/Player.h"
#include <opendavinci/odcore/data/Container.h>
#include <opendavinci/GeneratedHeaders_OpenDaVINCI.h>
#include <opendavinci/odcore/wrapper/SharedMemoryFactory.h>
#include <opendavinci/odcore/base/Lock.h>
#include <opencv2/highgui/highgui_c.h>
#include "automotivedata/GeneratedHeaders_AutomotiveData.h"
#include "opendavinci/odcore/base/KeyValueConfiguration.h"
#include "opendavinci/odcore/base/module/ClientModule.h"
#include <LineDetector.h>
#include <LaneDetector.h>
#include <thread>

using namespace std;
using namespace odcore;
using namespace odcore::io;
using namespace odtools::player;
using namespace odcore::data;
using namespace odcore::data::image;
using namespace odcore::wrapper;
using namespace odcore::base;
using namespace cv;
using namespace msv;
using namespace automotive::miniature;

const bool AUTO_REWIND = false;
const uint32_t MEMORY_SEGMENT_SIZE = 1024 * 768;
const uint32_t NUMBER_OF_SEGMENTS = 1;
const bool THREADING = false;
Container nextContainer;
IplImage *_image = NULL;
bool hasAttachedToSharedImageMemory = false;
shared_ptr<SharedMemory> sharedImageMemory;
Mat frame;
int imageHeight, imageWidth;
Config cfg;
LaneDetectorDataToDriver dataToDriver;
int previousThresh=48;
std::ofstream csvexport;
string homePath = getenv("HOME");
string projectPath = homePath + "/CaroloCup/2016-CaroloCup/";
string vpGrapherPath = projectPath + "lanedetector/VPGrapher/";
string groundTruthPath = homePath + "/Ground_Truth/";
string tempFolderPath = homePath + "/VPGRAPHERTEMP/";
string benchConfigPath = projectPath + "lanedetector/src/benchmark.conf";
struct scenario {
    string name;
    float fps;
};
vector<scenario> scenarios;
void setThresholdFromConfigFile();
void getScenarioNames(const string &root);
void setupConfig();
int getDynamicThresh(int lux);
void printVPToCSVFile(string path);
void setupCSVFile(string path);
void printScenariosDataToJsonFile(string outputFolder);
void runGraph();
void openBrowser(string path);
void processScenarios(string outputFolder);
void setupTempFolder();
void removeTempFolder();
void compileBranch();
int thresholdConf = 70;
#endif //LANEDETECTOR_BENCHMARK_H
