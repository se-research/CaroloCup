#include "Benchmark.h"

int main() {
    root = getenv("HOME");
    root.append("/Ground_Truth");

    getScenarioNames(root);

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
