#include <opencv2/highgui/highgui_c.h>
#include "Benchmark.h"


int main() {
    root = getenv("HOME");
    root.append("/Ground_Truth/");

    getScenarioNames(root);
    string scenario = scenarioNames[0];
    string scenarioPathString = "file://" + root + scenario + "/" + scenario + ".rec";
    URL scenarioPath(scenarioPathString.c_str());

    Player player(scenarioPath, AUTO_REWIND, MEMORY_SEGMENT_SIZE, NUMBER_OF_SEGMENTS, THREADING);

    while (player.hasMoreData()) {
        nextContainer = player.getNextContainerToBeSent();

        if (nextContainer.getDataType() == Container::SHARED_IMAGE) {
            SharedImage si = nextContainer.getData<SharedImage>();

            if (! hasAttachedToSharedImageMemory) {
                sharedImageMemory = SharedMemoryFactory::attachToSharedMemory(si.getName());

                hasAttachedToSharedImageMemory = true;
            }


            if (sharedImageMemory->isValid()) {
                image = cvCreateImageHeader(cvSize(si.getWidth(), si.getHeight()), IPL_DEPTH_8U, si.getBytesPerPixel());
                image->imageData = (char *) sharedImageMemory->getSharedMemory();
            }
        }
    }

    return 0;
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
