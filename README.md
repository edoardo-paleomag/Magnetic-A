# Magnetic-A - How to run locally

The documents and data within this git-hub folder form the basis for the Magnetic-A application available online at https://edoardodallanave.shinyapps.io/MagneticA/. The code for the application can be found in the documents “server.R” and “ui.R”. 

If you use this code for publication purposes, please cite (for now):

Dallanave, E. (2024). Assessing the reliability of paleomagnetic datasets using the R package PmagDiR. Scientific Reports, 14(1666). https://doi.org/10.1038/s41598-024-52001-x

A dedicated publication is under preparation!

#### If you will be processing a large amount of data using Magnetic-A - it is adventageous to run the program locally. To run Magnetic-A locally on your own computer follow the steps below.

### 1.DOWNLOAD R AND R-STUDIO. Instructions for downloads can be found at:
* R = https://www.r-project.org/
* R studio = https://www.rstudio.com/products/rstudio/download/
* Open R-Studio
* NOTE: R-Studio facilitate the interaction with R, but the same commands described in point 2 and 3 can be typed in the R Console.

### 2.INSTALLING THE REQUIRED PACKAGES (SKIP to step 2 IF THE PACKAGES ARE ALREADY ISNTALLED IN YOUR DEVICE): 
all package used by Magnetic-A are stored in CRAN (Except PmagDiR) and can be downloaded by typing in the console:

* install.packages("plyr")
* install.packages("dplyr")
* install.packages("shiny")
* install.packages("shinyWidgets")
* install.packages("DT")
* install.packages("shinyhelper")
* install.packages("glue")
* install.packages("tidyverse")
  
#### PmagDiR is stored in GitHub. To install it, devtools (package hosted in CRAN) is required. Please type:

* install.packages("devtools")
* library(devtools)
* install_github("edoardo-paleomag/PmagDiR")

### 3.WORKING WITH MAGNETIC-A

After installation, all packages are automatically called by launching Magnetic-A. To do so, type this:

* shiny::runGitHub("Magnetic-A", "edoardo-paleomag")

Alternatively, download the full Magnetic-A content in a folder on your device and type:

* shiny::runApp("/Path_to_the _folder/Name_of_the_folder/")

The latter option allows to operate offline.

For help and inquiries, please contact me: 

* edoardo.dallanave@unimi.it


