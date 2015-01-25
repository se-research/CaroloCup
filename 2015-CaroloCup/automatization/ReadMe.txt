README for automation file.

How to use it:
!!! Make sure you have *.rec and *.rec.mem files in the run directory (/opt/msv/bin/2013/DIT-168/project-template) 
python automation.py -r <recording_file> -c <ground_truth_file> or
python automation.py --recording <recording_file> --csvfile <ground_truth_file>'

example: python automation.py -r RightLaneMissing.rec -c RightLaneMissing.csv

recording file and ground truth file has to be in the run directory
which is by default : "/opt/msv/bin/2013/DIT-168/project-template"


