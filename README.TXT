
=========================================================================================
The data was collected  from  
https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip. 
==========================================================================================

Original description of the data set
==================================================================
Human Activity Recognition Using Smartphones Dataset
Version 1.0
==================================================================
Jorge L. Reyes-Ortiz, Davide Anguita, Alessandro Ghio, Luca Oneto.
Smartlab - Non Linear Complex Systems Laboratory
DITEN - Università degli Studi di Genova.
Via Opera Pia 11A, I-16145, Genoa, Italy.
activityrecognition@smartlab.ws
www.smartlab.ws
==================================================================

The experiments have been carried out with a group of 30 volunteers within an age bracket of 19-48 years. Each person performed six activities (WALKING, WALKING_UPSTAIRS, WALKING_DOWNSTAIRS, SITTING, STANDING, LAYING) wearing a smartphone (Samsung Galaxy S II) on the waist. Using its embedded accelerometer and gyroscope, we captured 3-axial linear acceleration and 3-axial angular velocity at a constant rate of 50Hz. The experiments have been video-recorded to label the data manually. The obtained dataset has been randomly partitioned into two sets, where 70% of the volunteers was selected for generating the training data and 30% the test data. 

The sensor signals (accelerometer and gyroscope) were pre-processed by applying noise filters and then sampled in fixed-width sliding windows of 2.56 sec and 50% overlap (128 readings/window). The sensor acceleration signal, which has gravitational and body motion components, was separated using a Butterworth low-pass filter into body acceleration and gravity. The gravitational force is assumed to have only low frequency components, therefore a filter with 0.3 Hz cutoff frequency was used. From each window, a vector of features was obtained by calculating variables from the time and frequency domain. See 'features_info.txt' for more details. 
=============================================================================================
Data cleaning
=============================================================================================

The data was extracted  and merged using the information in the Readme.text  inside of the zip file.
The test and train data sets were merged using the label ativity as a index. To the resulting data set was added the subject identification, finally the activity names were set up for each measurement in the data set.

The variables were renamed giving indication of the type of process. the fourier transformations have fourier and the eucledian has Mag name. The no transform data has no especial addition in the name. 

From the original dataset were selected only the variables showing mean values and standard deviations of the  accelometer and gyroscope signals, including the modified signals using fourier or other mathemathical approach. 

For each record it is provided:
======================================

The average of each signal of the accelerometer and giroscope  for each subject. the were measured in 50 hz and 20 hz respectivaly. 
The average of each signal  using Fourier transformation of the accelerometer and giroscope  for each subject.
The average of each signal  using Euclidean norm transformation of the accelerometer and giroscope  for each subject.

Additional information can be found in the codebook. 