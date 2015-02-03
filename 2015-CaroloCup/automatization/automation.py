#! /usr/bin/python

import sys
import getopt
import subprocess
import os
import time
import csv
import math

home = os.path.expanduser("~")
BUILD_DIR = home +"/CaroloCup/2015-CaroloCup/sources/OpenDaVINCI-msv/build"
RUN_DIR = "/opt/msv/bin/2013/DIT-168/project-template"
laneCsvFile = ""
recording_file = ""
gt_file = ""
size_per_segment = 0.18 
threshold = 80


def build_and_install():
    os.chdir(BUILD_DIR)
   # subprocess.call("make install -j8", shell = True)
    print '### Done bulding and installing ###'
    print '### Starting lanedetector2csvextractor ###'
    lane2csv()


def lane2csv():
    os.chdir(RUN_DIR)
    global laneCsvFile
    laneCsvFile = os.path.splitext(recording_file)[0] + '_' + time.strftime("%d_%m_") + \
     time.strftime("%H:%M") + '.csv'
    frame_count = int(os.path.getsize(recording_file + '.mem') / 1000000 / size_per_segment)
    timeout = ((frame_count / 10) + (10 - (frame_count / 10) % 10))
    laneString = "./lanedetector2csvextractor " + recording_file + " " + laneCsvFile +" " + str(timeout)
    subprocess.call(laneString,shell=True)
    print '### Done lanedetector2csvextractor ###'
    print '### Analyzing csv files ###'
    cvs_handling()


def cvs_handling():
    #Main dir will be changed in the future
    os.chdir(RUN_DIR)
    groundTruthFile = open(gt_file)
    laneFile = open(laneCsvFile)
    calculationCsvFile = 'result_' + laneCsvFile
    outputfile = open(calculationCsvFile, 'wb')
    mywriter = csv.writer(outputfile, delimiter = ';')
    try:
        reader1 = csv.reader(groundTruthFile, delimiter = ';')
        reader2 = csv.reader(laneFile, delimiter = ';')
        #row1 = groundtruth row, row2 = lanedection csv file row, but it selects same rows 
        row1 = reader1.next()
        row2 = reader2.next()
        while row1 and row2:
            try:
                leftStart = pts_dist([int(row1[1]),int(row1[2])],[int(row2[1]),int(row2[2])])
                leftEnd = pts_dist([int(row1[3]),int(row1[4])],[int(row2[3]),int(row2[4])])
                rightStart = pts_dist([int(row1[5]),int(row1[6])],[int(row2[5]),int(row2[6])])
                rightEnd = pts_dist([int(row1[7]),int(row1[8])],[int(row2[7]),int(row2[8])])
                dashStart = pts_dist([int(row1[9]),int(row1[10])],[int(row2[9]),int(row2[10])])
                dashEnd = pts_dist([int(row1[11]),int(row1[12])],[int(row2[11]),int(row2[12])])
                if (leftStart > threshold or leftEnd > threshold or rightStart > threshold \
                    or rightEnd > threshold or dashStart > threshold or dashEnd > threshold):
                    writeData = [row1[0],leftStart,leftEnd,rightStart,rightEnd,dashStart,dashEnd,'NA']
                else:
                    writeData = [row1[0],leftStart,leftEnd,rightStart,rightEnd,dashStart,dashEnd,'A']
                mywriter.writerow(writeData)
                row1 = reader1.next()
                row2 = reader2.next()
            except StopIteration:
                row1.append(1)
                row2.append(1)
                break
    finally:
        groundTruthFile.close()
        laneFile.close()
        outputfile.close()
        print 'Csv calculations done'


# Calculates distance between two points and returns its length
def pts_dist(a,b):
    return int(math.sqrt(((b[1]-a[1])**2) + ((b[0]-a[0])**2)))


def main(argv):
    try:
        opts,args = getopt.getopt(argv, "hr:c:", ["recording=","csvfile="])
    except getopt.GetoptError:
        print 'Bad usage, for information how to use type -h'
        sys.exit(2)
    for opt, arg in opts:
        if opt == '-h' :
            print 'usage: automation.py -r <recording_file> -c <ground_truth_file>'
            sys.exit()
        elif opt in ("-r", "--recording"):
            global recording_file
            recording_file = arg
        elif opt in ("-c", "--csvfile"):
            global gt_file
            gt_file = arg
    if recording_file.lower().endswith('.rec') and gt_file.lower().endswith('.csv'):
        print 'Recording file: ', recording_file
        print 'Ground truth file: ', gt_file
        # print '### Bulding and installing ###'
        build_and_install()
    else:
        print 'Missing\wrong arguments, please try again. For help use -h'


if __name__ == "__main__":
    main(sys.argv[1:])
