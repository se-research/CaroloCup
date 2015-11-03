#ifndef LANEDETECTOR_BENCHMARK_H
#define LANEDETECTOR_BENCHMARK_H
#include <opencv/cv.h>
#include <dirent.h>
#include <core/io/URL.h>
#include "tools/player/Player.h"
#include <core/data/Container.h>
#include <GeneratedHeaders_CoreData.h>
#include <core/wrapper/SharedMemoryFactory.h>
#include <core/base/Lock.h>
#include <opencv2/highgui/highgui_c.h>
#include "GeneratedHeaders_AutomotiveData.h"
#include "core/base/KeyValueConfiguration.h"
#include "core/base/module/ClientModule.h"

using namespace std;
using namespace core;
using namespace core::io;
using namespace tools::player;
using namespace core::data;
using namespace coredata::image;
using namespace core::wrapper;
using namespace core::base;
using namespace cv;
using namespace msv;
using namespace automotive::miniature;


string root;
vector<string> scenarioNames;
const bool AUTO_REWIND = false;
const uint32_t MEMORY_SEGMENT_SIZE = 1024 * 768;
const uint32_t NUMBER_OF_SEGMENTS = 1;
const bool THREADING = false;
Container nextContainer;
IplImage *image = NULL;
bool hasAttachedToSharedImageMemory = false;
SharedPointer<SharedMemory> sharedImageMemory;
Mat frame;
int imageHeight, imageWidth;
Config cfg;
LaneDetectorDataToDriver dataToDriver;
int previousThresh=48;

#define CROSS_DUP(fd) _dup(fd)
#define CROSS_DUP2(fd, newfd) _dup2(fd, newfd)

void getScenarioNames(const string &root);
void setupConfig();
int getDynamicThresh(int lux);
#endif //LANEDETECTOR_BENCHMARK_H
