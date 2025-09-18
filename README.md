# Magnetic-A

The documents and data within this git-hub folder form the basis for the Magnetic-A application available online at https://edoardodallanave.shinyapps.io/MagneticA/. The code for the application can be found in the documents “server.R” and “ui.R”. 

Anyone using this code should acknowledge and cite (for now):

Dallanave, E. (2024). Assessing the reliability of paleomagnetic datasets using the R package PmagDiR. Scientific Reports, 14(1666). https://doi.org/10.1038/s41598-024-52001-x

A dedicated publication is under preparation!

If you will be processing a large amount of data using Magnetic-A - it is adventageous to run the program locally (offline). To run Magnetic-A locally on your own computer follow the steps below.

### Download both R and R-studio to your computer. Instructions for downloads can be found at:
* R = https://www.r-project.org/
* R studio = https://www.rstudio.com/products/rstudio/download/
* Open R-Studio

### INSTALLING THE REQUIRED PACKAGES (SKIP IF ALREADY DONE): 
all package used by Magnetic-A are stored in CRAN (Except PmagDiR) and can be downloaded by typing in the console:

* install.packages("plyr")
* install.packages("dplyr")
* install.packages("shiny")
* install.packages("shinyWidgets")
* install.packages("DT")
* install.packages("shinyhelper")
* install.packages("glue")
* install.packages("tidyverse")
  
#### PmagDiR is stored in GitHub. To install it, devtools is required. Please type:

* install.packages("devtools")
* library(devtools)
* install_github("edoardo-paleomag/PmagDiR")

### STARTING WITH MAGNETIC-A

UPLOAD THE REQUIRE PACKAGES: 

After installation, they must be called before starting Magnetic-A with these commands:

* library(plyr)
* library(dplyr)
* library(shiny)
* library(shinyWidgets)
* library(DT)
* library(shinyhelper)
* library(stats)
* library(glue)
* library(tidyverse)
* library(PmagDiR)

Magnetic-A is then opened by:

* shiny::runGitHub("Magnetic-A", "edoardo-paleomag")


