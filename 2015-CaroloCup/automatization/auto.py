# Automation script to build and install current OpendaVINCI version,
# generate csv file of lanedetection and compare to ground truth csv file.
# 
# How to:
# Adjust BUILD_DIR, RUN_DIR according to your system, recFile has to be changed based on filename of your recording
# 
#TODO 
# Add split function to cut out not accepted frames
 
import csv
import sys
import math
import os
import subprocess
import time

#Change acording to the csv files folder
#Build dir where build directory of the OpenDaVINCI is
BUILD_DIR = "/home/cadogan/Git/CaroloCup/2015-CaroloCup/sources/OpenDaVINCI-msv/build"
#Path to the lanedetector2csvextractor 
RUN_DIR = "/opt/msv/bin/2013/DIT-168/project-template"


#Definition of filenames has to be fully dynamic to generate/select correct recordings
laneCsvFile = "output.csv" # TODO add filename generator with "output + date".csv
calculationCsvFile = "result.csv" # TODO add filename generator with "result + date".csv
groundTruthCsv = "groundTruth.csv"
#Rec files has to be in RUN_DIR
recFile = "testing.rec"
#How to make timeout dynamic ? maybe based on the size of the file ?
timeout = 130
# Threshold of acceptance
threshold = 80

#Calculates distance between two points and returns its lenght
def pts_dist(a,b):
	return (math.sqrt(((b[1]-a[1])**2) + ((b[0]-a[0])**2)))


#Extracting points from the csv files,
#calculating different between ground truth file and newly generated one
def cvs_handling():
	#Main dir will be changed in the future
	os.chdir(BUILD_DIR)
	print "i am in cvs"
	groundTruthFile = open(groundTruthCsv)
	laneFile = open(laneCsvFile)
	f3 = open(calculationCsvFile, 'wb')
	mywriter = csv.writer(f3,delimiter=',')
	try:
		reader1 = csv.reader(groundTruthFile)
		reader2 = csv.reader(laneFile)
		#row1 = groundtruth row, row2 = lanedection csv file row, but it selects same rows 
		row1 = reader1.next()
		row2 = reader2.next()

		while (row1 and row2):
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
		f1.close()
		f2.close()
		f3.close()
		print 'Csv calculations done'




#Builds and install OpenDavinci
def build_and_install():
	#cwd = os.getcwd()
	os.chdir(BUILD_DIR)
	error = subprocess.call("make install -j 8",shell=True)
	lane2csv()
			

		
#Need to check if .rec file is there.
def lane2csv():
	os.chdir(RUN_DIR)
	laneString = "./lanedetector2csvextractor " + recFile + " " +laneCsvFile +" " + str(timeout)
	#lanedetector2csvextractor doesnt return any error code even it can not find file to open
	subprocess.call(laneString,shell=True)
	cvs_handling()


def main():
	print 'automatisation is running, to exit press CTRL+c'
	build_and_install()
	print '\n Exiting!!!'


if __name__ == '__main__':main()
