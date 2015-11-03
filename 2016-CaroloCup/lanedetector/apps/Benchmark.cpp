#include <LineDetector.h>
#include <LaneDetector.h>
#include "Benchmark.h"

void setupConfig();

int main() {
    setupConfig();

    root = getenv("HOME");
    root.append("/Ground_Truth/");

    getScenarioNames(root);
    string scenario = scenarioNames[0];
    string scenarioPathString = "file://" + root + scenario + "/" + scenario + ".rec";
    URL scenarioPath(scenarioPathString.c_str());

    Player player(scenarioPath, AUTO_REWIND, MEMORY_SEGMENT_SIZE, NUMBER_OF_SEGMENTS, THREADING);

    int frames = 0;

    while (player.hasMoreData()) {
        nextContainer = player.getNextContainerToBeSent();

        if (nextContainer.getDataType() == Container::SHARED_IMAGE) {
            SharedImage si = nextContainer.getData<SharedImage>();

            if (! hasAttachedToSharedImageMemory) {
                sharedImageMemory = SharedMemoryFactory::attachToSharedMemory(si.getName());

                hasAttachedToSharedImageMemory = true;
            }

            if (sharedImageMemory->isValid()) {

                if (image == NULL) {
                    imageHeight = si.getHeight();
                    imageWidth = si.getWidth();
                    image = cvCreateImageHeader(cvSize(imageWidth, imageHeight), IPL_DEPTH_8U,
                                                si.getBytesPerPixel());
                }

                image->imageData = (char *) sharedImageMemory->getSharedMemory();

                // Copy IplImage to Mat
                Mat frame(image, true);

                cvReleaseImage(&image);

                // Crop top and bottom
                frame = frame(cv::Rect(1, 2 * imageHeight / 16 - 1, imageWidth - 1, 10 * imageHeight / 16 - 1));

                // Set lighting conditions
                previousThresh = cfg.th1;
                int lux=-2;
                SensorBoardData sdb;
                if(sdb.containsKey_MapOfDistances(7)) lux=sdb.getValueForKey_MapOfDistances(7);
                cfg.th1 = getDynamicThresh(lux);

                // Disable cout
                cout.setstate(std::ios_base::failbit);

                // Run line detector
                LineDetector road(frame, cfg, false, 1);
                dataToDriver = *(road.getDriverData());

                // Re-enable cout
                cout.clear();

                ++frames;

                // Get vanishing point
                Point vp = dataToDriver.rightGoalLines0.p1;
                cout << "Frame: " << frames << " VPx: " << vp.x << ", VPy: " << vp.y << endl;
            }
        }
    }

    return 0;
}

void setupConfig() {
    cfg.th1 = 70;
    cfg.th2 = 230;
    cfg.hlTh = THRESH_BINARY;
    cfg.XTimesYMin = 2;
    cfg.XTimesYMax = 20;
    cfg.maxY = 235;
    cfg.maxArea = 4;
}

void getScenarioNames(const string &root) {
    DIR *dir = opendir(root.c_str());

    struct dirent *entry = readdir(dir);

    while (entry != NULL) {
        string scenarioName = entry->d_name;

        if (entry->d_type == DT_DIR
            && scenarioName != "."
            && scenarioName != "..") {
            scenarioNames.push_back(scenarioName);
        }

        entry = readdir(dir);
    }

    closedir(dir);
}

// TODO: make this function globally accessible in lanedetector to remove redundancy
int getDynamicThresh(int lux)
{

    int baseThresh=48;
    int minIntervalValue[]={11,15,17,20,23,26,29,32},maxIntervalValue[]={16,18,21,24,27,31,35,40};
    int foundIndex[3],thresh[]={baseThresh+2,baseThresh+7,baseThresh+12,baseThresh+17,baseThresh+22,baseThresh+27,baseThresh+32};
    if(lux<minIntervalValue[0])
    {
        return baseThresh;
    }
    if(lux>maxIntervalValue[6]){
        return baseThresh+42;
    }
    int cnt=0;
    for(int i=0;i<7;i++)
    {
        if(lux>=minIntervalValue[i] && lux<=maxIntervalValue[i])
        {
            foundIndex[cnt++]=i;
        }
    }
    for(int j=0;j<cnt;j++)
    {
        if(previousThresh==thresh[foundIndex[j]])
        {
            return thresh[foundIndex[j]];
        }
    }
    return thresh[foundIndex[0]];
}