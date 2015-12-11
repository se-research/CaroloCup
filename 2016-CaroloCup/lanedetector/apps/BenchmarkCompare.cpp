#include "BenchmarkCompare.h"

int main(int argc, char** argv) {
    string branch;

    if (argc != 2) {
        cout << "No branch name supplied, using current branch!" << endl;
        branch = getBranchName();
    } else {
        branch = string(argv[1]);
    }

    setupTempFolder();

    checkoutBranch();
    compileBranch();
    runBenchmark("primary");

    checkoutBranch(branch);
    compileBranch();
    runBenchmark("secondary");

    removeTempFolder();

    runGraph();

    return 0;
}

void runBenchmark(string folderName) {
    system(string("cd " + projectPath + "build/lanedetector/ && ./Benchmark " + folderName).c_str());
}

void checkoutBranch() {
    system(string("cd " + projectPath + " && git checkout master").c_str());
}

void checkoutBranch(string branch) {
    system(string("cd " + projectPath + " && git checkout " + branch).c_str());
}

void compileBranch() {
    system(string("rm -R " + projectPath + "build/*").c_str());
    system(string("cd " + projectPath + "build && cmake .. && make -j8").c_str());
}

void removeTempFolder() {
    system(string("rm -R " + vpGrapherPath + "data/calculated/*").c_str());
    system(string("mv " + tempFolderPath + "* " + vpGrapherPath + "data/calculated/").c_str());
    system(string("rmdir " + tempFolderPath).c_str());
}

void setupTempFolder() {
    system(string("mkdir " + tempFolderPath).c_str());
    system(string("mkdir " + tempFolderPath + "/primary").c_str());
    system(string("mkdir " + tempFolderPath + "/secondary").c_str());
}

string getBranchName() {
    string command = "cd " + projectPath + " && git rev-parse --abbrev-ref --short HEAD";

    char buffer[50];
    auto *fp = popen(command.c_str(), "r");
    fgets(buffer, 50, fp);
    string result(buffer);
    result = result.substr(0, result.size() - 1); // remove new line character
    pclose(fp);

    return result;
}

void runGraph() {
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
    system(string("cd " + vpGrapherPath + " &&  php -S localhost:8000").c_str());
}

void openBrowser(string path) {
    sleep(1); // make sure the php server spawns before the browser is opened
    chdir(path.c_str());
    system(string("cd " + vpGrapherPath + " && firefox localhost:8000").c_str());
}