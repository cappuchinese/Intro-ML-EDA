# Project Theme09: Introduction to Machine Learning
Pancreatic cancer can be hard to diagnose and is most of the time found when in its later stages. This research focuses on predicting the diagnosis based on urinary biomarkers.

## Installation
First clone this repository:
```bash
git clone --recurse-submodules https://github.com/cappuchinese/Intro-ML-EDA
```
The `--recurse-submodules` option is added to clone the wrapper repository for the model at once.

## Project structure
<pre><font color="#12488B"><b>.</b></font>
├── <font color="#12488B"><b>data</b></font> &#9632 Data files
│   ├── codebook.txt
│   ├── Data.csv
│   ├── Documentation.csv
│   ├── <font color="#12488B"><b>wekafiles</b></font> &#9632 All the files used in Weka
│   │   ├── base.exp
│   │   ├── benign_train.csv
│   │   ├── cleaned_data.csv
│   │   ├── control_train.csv
│   │   ├── optimization.exp
│   │   └── test.arff
│   └── <font color="#12488B"><b>weka_out</b></font> &#9632 All the output files from Weka
│       ├── base.csv
│       ├── benign.model
│       ├── control.model
│       ├── optimization.csv
│       ├── roc_benign.arff
│       └── roc_control.arff
├── LICENSE
├── <font color="#12488B"><b>log</b></font> &#9632 All the files to create the log
│   ├── header.sty
│   ├── Log.pdf
│   ├── Log.rmd
│   └── title.sty
├── README.md &#9632 This file
├── <font color="#12488B"><b>report</b></font> &#9632 All the files used to create the report
│   ├── header.sty
│   ├── references.rmd
│   ├── Report.pdf
│   ├── Report.rmd
│   └── title.sty
├── <font color="#12488B"><b>src</b></font> &#9632 Folder with all the R source code used in the rmd files
│   ├── data_cleaning.R
│   ├── functions.R
│   ├── log.R
│   └── report_vars.R
└── <font color="#12488B"><b>wrapper</b></font> &#9632 Submodule for the wrapper application
    ├── build.gradle &#9632 Gradle build file
    ├── <font color="#12488B"><b>data</b></font> &#9632 Contains a test data to test the application with
    │   └── test.arff &#9632 ARFF formatted file with instances to test with
    ├── settings.gradle &#9632 Gradle settings file
    └── <font color="#12488B"><b>src</b></font>
        └── <font color="#12488B"><b>main</b></font>
            ├── <font color="#12488B"><b>java</b></font>
            │   └── <font color="#12488B"><b>nl</b></font>
            │       └── <font color="#12488B"><b>bioinf</b></font>
            │           └── <font color="#12488B"><b>ljbhu</b></font>
            │               └── <font color="#12488B"><b>WrapperT9</b></font>
            │                   ├── ApacheCliOptionsProvider.java &#9632 Class for parsing command line arguments
            │                   ├── WekaRunner.java &#9632 Class to run Weka methods, classifying the unknown instances
            │                   └── WrapperMain.java &#9632 Class to run classes above
            └── <font color="#12488B"><b>resources</b></font> &#9632 Folder with the models used for the application
                ├── benign.model
                └── control.model

</pre>
