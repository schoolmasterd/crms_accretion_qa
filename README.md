# CRMS Accretion QA/QC Tool

## Overview
The code takes as in a .csv of the data to be tested (_input data_) and a csv of historical values. It runs tests on the input data and outputs a report in .html format.

## How To Use
This repository contains the latest version of the code. 
1. Download this respository as a folder by clicking the green "<>Code" button, then "Download ZIP" from the drop-down menu. This will download a zip file to your machine. 
2. Next, extact the zip file, which will be called "crms\_accretion\_qa-main" to a working directory on your computer.
   * You can extract the file by right clicking the mouse on the file name and selecting the option "Extract All...". This will bring up an window to allow you to choose where the files will be extracted.
   * Remember where you put it, becuase you will need to enter the path to it later. As an example, let's choose to extract the repository to "C:/Users/donschoolmaster/crms_work/". This will create a folder on the _path_  "C:/Users/donschoolmaster/crms\_work/crms\_accretion\_qa-main" that will contain 4 folders: _Code_, _Data_, _Output_, and _Tools_.
   * When using the QA/QC Tool, there should never be a reason to open the _Tools_ folder.
3. Open the _Data_ folder and drop in the .csv for the data to be tested and an updated .csv of the database of the historical data (if necessary).
4. Open the _Code_ folder and double-click on the file "crms\_accretion\_qc\_v2.R"
5. Replace the code on line 6 that says:
```r
path<-"path/to/crms_accretion_qc-main"
```
with the path to which we extracted the repository. In this example:
```r
path<-"C:/Users/donschoolmaster/crms_work/crms_accretion_qa-main"
```
6. On line 10, update the name of the file containing the data to be tested that you placed in the _Data_ folder by changing the name of the file (the part in quotes). For example:
```r
input_file<-"CRMS_ACC_Su25.csv"
```
7. If you updated the historical data in the _Data_ folder. Change line 11 to reflect the name of the file you added:
```r
db_df<-"New_Historical_Data.csv"
```
8. Finally hit _Ctrl-a_ to select all of the code, then _Ctrl-r_ to run the code. 
9. The ouput report(s) will appear in the _Ouput_ folder. It's name will reflect the names of the input file.

## Notes
This script requires the R package _xtable_. If you have not already installed this package for R, you will be required to before running this script for the first time. To do so, click "Packages->Install Package(s)..." from the toolbar and scroll down the popup menu to select the package. Then click "OK" to install it.