#include "Benchmark.h"

int main() {
    setupConfig();

    setupTempFolder();

    getScenarioNames(groundTruthPath);

    // save custom branch name
    string branch = getOutputFromCommand("cd " + projectPath + " && git rev-parse --abbrev-ref --short HEAD");

    // checkout to master branch
    system(string("cd " + projectPath + " && git checkout master").c_str());

    processScenarios("primary");

    // checkout to custom branch
    system(string("cd " + projectPath + " && git checkout " + branch).c_str());

    processScenarios("secondary");

    deconstructTempFolder();

    runGraph();

    return 0;
}

void deconstructTempFolder() {
    system(string("rm -R " + vpGrapherPath + "data/calculated/*").c_str());
    system(string("mv " + tempFolderPath + "* " + vpGrapherPath + "data/calculated/").c_str());
    system(string("rmdir " + tempFolderPath).c_str());
}

void setupTempFolder() {
    system(string("mkdir " + tempFolderPath).c_str());
    system(string("mkdir " + tempFolderPath + "/primary").c_str());
    system(string("mkdir " + tempFolderPath + "/secondary").c_str());
}

string getOutputFromCommand(string command) {
    char buffer[50];
    auto *fp = popen(command.c_str(), "r");
    fgets(buffer, 50, fp);
    string result(buffer);
    result.erase(std::remove(result.begin(), result.end(), '\n'), result.end()); // remove new line character
    pclose(fp);

    return result;
}

void processScenarios(string outputFolder) {
    for (int i = 0; i < scenarios.size(); i++) {
        auto totalTime = 0;
        auto frames = 0;

        string scenarioName = scenarios[i].name;
        string scenarioPathString = "file://" + groundTruthPath + scenarioName + "/" + scenarioName + ".rec";
        URL scenarioPath(scenarioPathString.c_str());

        cout << "Saving " << scenarioName << " vanishing points... " << "[" << (i + 1) << "/" << scenarios.size() << "]" << endl;

        Player player(scenarioPath, AUTO_REWIND, MEMORY_SEGMENT_SIZE, NUMBER_OF_SEGMENTS, THREADING);

        // Set CSV file path
        string CSVPath = tempFolderPath + outputFolder + "/" + scenarioName + ".csv";
        setupCSVFile(CSVPath);

        while (player.hasMoreData()) {
            nextContainer = player.getNextContainerToBeSent();

            if (nextContainer.getDataType() == Container::SHARED_IMAGE) {
                SharedImage si = nextContainer.getData<SharedImage>();

                if (!hasAttachedToSharedImageMemory) {
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
                    int lux = -2;
                    SensorBoardData sdb;
                    if (sdb.containsKey_MapOfDistances(7)) lux = sdb.getValueForKey_MapOfDistances(7);
                    cfg.th1 = 70;

                    // Disable cout
                    cout.setstate(std::ios_base::failbit);

                    // Run line detector
                    auto startTimer = chrono::high_resolution_clock::now();

                    LineDetector road(frame, cfg, false, 1);
                    dataToDriver = *(road.getDriverData());

                    auto endTimer = chrono::high_resolution_clock::now();
                    auto timeSpent = chrono::duration_cast<chrono::microseconds>(endTimer - startTimer).count() / 1000;
                    totalTime += timeSpent;
                    frames++;

                    // Re-enable cout
                    cout.clear();

                    // Print vanishing point to CSV file
                    printVPToCSVFile(CSVPath);
                }
            }
        }

        hasAttachedToSharedImageMemory = false;

        scenarios[i].fps = (float) totalTime / (float) frames * 60;
    }

    printScenariosDataToJsonFile(outputFolder);
}

void printScenariosDataToJsonFile(string outputFolder) {
    std::ofstream json;
    string path = tempFolderPath + outputFolder + "/scenarios.json";

    json.open(path.c_str(), ios_base::trunc);
    json << "[";
    for (int i = 0; i < scenarios.size(); i++) {
        json << "{";
        json << "\"name\": " << "\"" << scenarios[i].name << "\", ";
        json << "\"fps\": " << scenarios[i].fps;
        json << "}";

        if (i !=(scenarios.size() - 1)) json << ",";
    }
    json << "]";
}

void printVPToCSVFile(string path) {
    Point vp = dataToDriver.rightGoalLines0.p1;
    csvexport.open(path.c_str(), ios_base::app);
    csvexport << vp.x << "," << vp.y << "\n";
    csvexport.close();
}

void setupCSVFile(string path) {
    // append column names
    csvexport.open(path.c_str(), ios_base::trunc);
    csvexport << "VP_x,VP_y\n";
    csvexport.close();
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
            scenario scenario;
            scenario.name = scenarioName;
            scenarios.push_back(scenario);
        }

        entry = readdir(dir);
    }

    closedir(dir);
}

// TODO: make this function globally accessible in lanedetector to remove redundancy
int getDynamicThresh(int lux) {
    int baseThresh = 48;
    int minIntervalValue[] = {11, 15, 17, 20, 23, 26, 29, 32}, maxIntervalValue[] = {16, 18, 21, 24, 27, 31, 35, 40};
    int foundIndex[3], thresh[] = {baseThresh + 2, baseThresh + 7, baseThresh + 12, baseThresh + 17, baseThresh + 22,
                                   baseThresh + 27, baseThresh + 32};

    if (lux < minIntervalValue[0]) {
        return baseThresh;
    }

    if (lux > maxIntervalValue[6]) {
        return baseThresh + 42;
    }

    int cnt = 0;
    for (int i = 0; i < 7; i++) {
        if (lux >= minIntervalValue[i] && lux <= maxIntervalValue[i]) {
            foundIndex[cnt++] = i;
        }
    }

    for (int j = 0; j < cnt; j++) {
        if (previousThresh == thresh[foundIndex[j]]) {
            return thresh[foundIndex[j]];
        }
    }

    return thresh[foundIndex[0]];
}

void runGraph() {
    chdir(vpGrapherPath.c_str());
    cout << "Opening browser..." << endl;
    thread browser(openBrowser, vpGrapherPath);
    /**
     * Need to run an HTTP server to ensure cross-browser compatibility.
     * While Firefox is more lenient when it comes to AJAX calls, i.e.
     * you can make AJAX calls without running a server as it treats
     * your local directory as same domain, other browsers do not.
     * Regardless, we cannot assume Firefox will keep the same attitude
     * indefinitely.
     */
    cout << "Starting web server..." << endl;
    system("php -S localhost:8000");
}

void openBrowser(string path) {
    sleep(1); // make sure the php server spawns before the browser is opened
    chdir(path.c_str());
    system("firefox localhost:8000");
}