#################################################
# Prototype of automatization					#
#												#
#uses  0% cpu and 7.8 Mb ram					#
#################################################
import csv
import sys
import math
import os
import subprocess
import time
import git
#Change acording to the csv files folder
#Build dir where build directory of the OpenDaVINCI is
BUILD_DIR = "/home/cadogan/Git/CaroloCup/2015-CaroloCup/sources/OpenDaVINCI-msv/build"
#Path to the lanedetector2csvextractor 
RUN_DIR = "/opt/msv/bin/2013/DIT-168/project-template"
#Path to github repo
REPO_DIR = "/home/cadogan/Git/git_monitor"

#Definition of filenames has to be fully dynamic to generate/select correct recordings
laneCsvFile = "testoutput.csv" #This needs to have auto generate name, so each csv file will be diferent
calculationCsvFile = "distance.csv"
groundTruthCsv = "groundTruth.csv"
#Rec files has to be in RUN_DIR
recFile = "testing.rec"
#How to make timeout dynamic ? maybe based on the size of the file ?
timeout = 130
gitRep =git.cmd.Git(REPO_DIR)

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
				writeData = [row1[0],leftStart,leftEnd,rightStart,rightEnd,dashStart,dashEnd]
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
		sendEmail("Report")



#Builds and install OpenDavinci
def build_and_install():
	cwd = os.getcwd()
	try:
		os.chdir(BUILD_DIR)
		error = subprocess.call("make install -j 8",shell=True)
		if error != 0:
			sendEmail("Error while building new changes")
		else:
			print 'Building done'
			lane2csv()
			

		


#Need to check if .rec file is there.
def lane2csv():
	os.chdir(RUN_DIR)
	laneString = "./lanedetector2csvextractor " + recFile + " " +laneCsvFile +" " + str(timeout)
	#lanedetector2csvextractor doesnt return any error code even it can not find file to open
	error = subprocess.call(laneString,shell=True)
	if error != 0:
		#if error occur sending email to the mailing list
		sendEmail("Error while running lanedetector2csvextractor")
	else:
		print 'Lanedetection2 csvextractor done'
		cvs_handling()



def sendEmail(message):
	#TODO write received message to the mailing list
	#script email adress : carolocup2015@gmail.com
	#password for email adress: cclinaro
	#Send to : your own email
	#if message = Report, we need send report to the mailing list
	return 1

#Need find way to enter user name and password,
#or store password , so never need to enter it
def checking_git():
	os.chdir(REPO_DIR)	
	gitRep.checkout()
	subprocess.call("git fetch", shell=True)
	msgMerge = gitRep.merge()
	if msgMerge == 'Already up-to-date.':
		return 0
	else: 
		return 1

	

	#		if git checkout master &&
	#	    git fetch origin master &&
	#	    [ `git rev-list HEAD...origin/master --count` != 0 ] &&
	#	    git merge origin/master
	#	then
	#	    echo 'Updated!'
	#	else
	#	    echo 'Not updated.'
	#	fi
	######Need to transform this to python language#####
	



def main():
	print 'automatisation is running, to exit press CTRL+c'
	try:
		while True:
			msg = checking_git()
			if msg == 1:
				build_and_install()
			else:
				print "going to sleep for 10 seconds"
				time.sleep(10)
	except KeyboardInterrupt:
		print '\n Exiting!!!'


if __name__ == '__main__':main()