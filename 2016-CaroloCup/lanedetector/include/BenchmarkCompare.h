#ifndef LANEDETECTOR_BENCHMARKCOMPARE_H
#define LANEDETECTOR_BENCHMARKCOMPARE_H
#include <iostream>
#include <unistd.h>
#include <thread>

using namespace std;

string homePath = getenv("HOME");
string projectPath = homePath + "/CaroloCup/2016-CaroloCup/";
string vpGrapherPath = projectPath + "lanedetector/VPGrapher/";
string groundTruthPath = homePath + "/Ground_Truth/";
string tempFolderPath = homePath + "/VPGRAPHERTEMP/";
string benchConfigPath = projectPath + "lanedetector/src/benchmark.conf";

string getBranchName();
void compileBranch();
void checkoutBranch();
void checkoutBranch(string branch);
void removeTempFolder();
void setupTempFolder();
void runGraph();
void openBrowser(string path);
void runBenchmark(string folderName);

#endif //LANEDETECTOR_BENCHMARKCOMPARE_H
