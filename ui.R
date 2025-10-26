
library(PmagDiR)
library(plyr)
library(dplyr)
library(shiny)
library(shinyWidgets)
library(DT)
library(shinyhelper)
library(stats)
library(glue)
library(tidyverse)


ui <- fluidPage(
  tabsetPanel(
    type = "tabs",
    tabPanel("Introduction and resources",
             sidebarLayout(
               sidebarPanel(
                 fluidRow(
                   column(12,img(src = "MagneticA_logo.png", width = "100%"))
                 ),
                 br(),
                 fluidRow(
                   column(12, h3(tags$a(href="https://edoardodallanave.wixsite.com/mysite", 
                                        "By Edoardo Dallanave", target="_blank")))
                 ),
                 #br(),
                 fluidRow(
                   column(12,h4("A R-Shiny-based complete toolbox for paleomagnetic analyses."))
                 ),
                 fluidRow(column(12,h5("• Paleomagnetic directions analysis"))),
                 fluidRow(column(12,h5("• Parametric and non-parametric statistics and test for consistency with field model"))),
                 fluidRow(column(12,h5("• Virtual geomagnetic pole analysis"))),
                 fluidRow(column(12,h5("• Multiple paleomagnetic poles analysis"))),
                 fluidRow(column(12,h5("• Magnetic polarity stratigraphy")))
               ),
               mainPanel(
                 fluidRow(
                   column(12,h4("Web version: ",tags$a(href="https://edoardodallanave.shinyapps.io/MagneticA/", 
                                                       "Magnetic-A")))
                 ),
                 br(),
                 fluidRow(
                   column(12,h4("Source and instructions: ",tags$a(href="https://github.com/edoardo-paleomag/Magnetic-A/blob/main/README.md",
                                                                   "click here.")))
                 ),
                 br(),
                 fluidRow(
                   column(12,h4("The User Guide is available as pdf file on my ",tags$a(href="https://edoardodallanave.wixsite.com/mysite", 
                                                                                        "personal Webpage")))
                 )
               )
             )
    ),
    tabPanel("Directions analysis",
             tabsetPanel(
               tabPanel("Vector end-points interpolation",
                        sidebarLayout(
                          sidebarPanel(width = 3,
                                       fluidRow(
                                         column(6,fileInput("All_Zijd",label = "Load demag data",multiple = T)),
                                         column(6,selectInput("Zijd_f_type",label = "File type",
                                                              choices = list("Magnetic-A"=1,"LASA"=2,"Bremen (.cor)"=3,"IODP JR6A Expanded"=4,"CIT multi-samples"=5,"pmd multi-samples"=6,"Example data"=7),selected = 1) %>%
                                                  helper(type = "inline",
                                                         title = "Format file",
                                                         content = c(
                                                           "Magnetic-A= file processed directly by the software without conversion; 11 comma-separated columns with: Sample,step,the nine remaining columns with (3x3x3) tilt corrected data, geographic coordinates data, and sample coordinates data in cartesian coordinates.",
                                                           "",
                                                           "LASA= file produced at the paleomagnetic laboratory of the Univeristy of Milan -Italy- (i.e., old Lamont file).",
                                                           "",
                                                           "Bremen (.cor)= file produced at the Bremen paleomagnetic laboratory (Germany).",
                                                           "",
                                                           "IODP JR6A Expanded= IODP spinner magnetometer (JR6) file downloaded by from the portal https://web.iodp.tamu.edu/LORE/.",
                                                           "",
                                                           "CIT multi-samples= Sample Data file as described in https://cires1.colorado.edu/people/jones.craig/PMag_Formats.html (multiple files can be selected).",
                                                           "",
                                                           "Example data= Some paleomagnetic directions from the South Ardo Paleocene record,
                                     Dallanave et al., 2012, https://doi.org/10.1016/j.palaeo.2012.04.007",
                                                           "",
                                                           "For any inquiries or request regarding additional file types, please contact me at edoardo.dallanave@unimi.it"),
                                                         size = "l",fade = T))
                                       ),
                                       fluidRow(
                                         column(6,selectInput("Zijd_Stereo_shift",label = "Diagram",
                                                              choices = list("N-Right"=1,"N-Up"=2, "Equal area"=3), selected = 1)),
                                         column(6,selectInput(inputId = "VEPcoordinates",label = "Coordinates",
                                                              choices = list("Specimen"=1,"Geographic"=2,"Tilt Corr."=3),selected = 3))
                                       ),
                                       fluidRow(
                                         column(6, actionButton(inputId = "Zijd_detail",label = "UNITS",width = "100%")),
                                         column(6, actionButton(inputId = "Zijd_detail2",label = "TAGS",width = "100%"))
                                       ),
                                       br(),
                                       br(),
                                       fluidRow(
                                         column(6,selectInput("anchor",label = "Interpolation",
                                                              choices = list("PCA Free"=1,"PCA Anch."=2,"PCA Or. Incl."=3,"PPCA Constr."= 6, "Fisher"=4, "G. Circle"=5),selected = 1)%>%
                                                  helper(type = "inline",
                                                         title = "Interpolation type",
                                                         content = c(
                                                           "Vector end-points interpolation can be performed by:",
                                                           "",
                                                           "PCA Free- PCA free from origin of demagnetization axes",
                                                           "",
                                                           "PCA Anch.- PCA with ellipsoid centered at the origin of the demagnetization axes.",
                                                           "THIS OPTION IS WARMLY DISCOURAGED UNLESS JUSTIFIED BY THE PPCA* TEST (available in main panel)",
                                                           "",
                                                           "PCA Or. Incl.- Includes origin of the demagnetization axes as demagnetization point",
                                                           "",
                                                           "PPCA Constr.- Constrained PCA*",
                                                           "",
                                                           "Fisher- Fisher spherical average of the vector end-points",
                                                           "",
                                                           "G. Circle- Interpolation of the vector end-points by a great circle",
                                                           "",
                                                           "*Heslop, D., Roberts, A.P. (2016). Analyzing paleomagnetic data: To anchor or not to anchor? Journal of Geophysical Research: Solid Earth, 121(11), 7742–7753. https://doi.org/10.1002/2016JB013387"
                                                         ),
                                                         size = "l",fade = T)),
                                         column(6, textInput("comp_name",label = "Component name",value = "Ch"))
                                       ),
                                       fluidRow(
                                         column(12,h4("List of loaded specimens"))
                                       ),
                                       fluidRow(
                                         column(12,DT::dataTableOutput("samples_list"))
                                       )
                          ),
                          mainPanel(
                            fluidRow(
                              actionButton(inputId = "del_VEPs",label = "Delete selection"),
                              actionButton(inputId = "restore_VEPs",label = "Restore data"),
                              actionButton(inputId = "PPCA",label = "Run Probabilistic PCA test"),
                              actionButton(inputId = "runVEPstat",label = "Run interpolation"),
                              actionButton(inputId = "save_PCA",label = "Save interpolation"),
                              downloadButton("export_PCA",label = "Export all saved directions")
                            ),
                            fluidRow(column(3,textOutput("Zijd_Unit")),
                                     column(9,textOutput("PCA_result"))
                            ),
                            column(11,plotOutput("zijderveld",brush = brushOpts(id = "plot_brush", fill = NA))),
                            column(1, DT::dataTableOutput("sampledat",width = 100))
                          )
                        )
               ),
               tabPanel("Export figure with all diagrams",
                        mainPanel(
                          fluidRow(
                            column(2, downloadButton("export_VEPs_figure",label = "Export figure",width="100%")),
                            column(10,h5("Please define Units & Tags for a complete visualization"))
                          ),
                          column(12,plotOutput(outputId = "All_VEP_diagrams")),
                        )
               ),
               tabPanel("All saved directions",
                        sidebarLayout(
                          sidebarPanel(width = 6,
                                       fluidRow(
                                         column(4,fileInput(inputId = "import_PCA",label = "Import saved directions")%>%
                                                  helper(type = "inline",
                                                         title = "Format file",
                                                         content = c(
                                                           "File as exported from Vector end-points interpolation page"),
                                                         size = "m",fade = T)),
                                         column(4,textInput(inputId = "sel_interpol_name",label = "Name of exported file",value = "Directions")),
                                         column(4,selectInput(inputId = "EAcoordinates",label = "Coordinates",
                                                              choices = list("Geographic"=1,"Tilt Corr."=2),selected = 2))
                                       ),
                                       fluidRow(DT::dataTableOutput(outputId = "saved_interpol"))
                          ),
                          mainPanel(width = 6,
                                    fluidRow(actionButton(inputId = "del_interpol",label = "Delete selection"),
                                             actionButton(inputId = "undel_interpol",label = "undo delete"),
                                             downloadButton("export_interpol",label = "Export selected directions"),
                                             downloadButton("export_AllDirs_stereo",label = "Export figure"),
                                             actionButton(inputId = "comb_DI_GC",label = "Combine DI & GC"),
                                             actionButton(inputId = "save_GC",label = "Add GC dirs to list"),
                                             actionButton(inputId = "GC_erase",label = "Clear GC dirs from plot"),
                                             actionButton(inputId = "showdiersEA",label = "Show dragged directions details")),
                                    plotOutput("saved_interpol_EA",brush = brushOpts(id="plot_click")))
                        )
               )
             )
    ),
    tabPanel("Directions display, filter & average",
             sidebarLayout(
               sidebarPanel(
                 fluidRow(
                   column(6,fileInput("file", label= "Directions file input")),
                   column(6,textInput("fileN",label = "Export name",value = "Site"))
                 ),
                 fluidRow(
                   column(8,selectInput("filetype", label = "Directions file type",
                                        choices = list("Dec, Inc "=1,"G_dec, G_inc, B_az, B_plunge"=2,"G_dec, G_inc, B_dec, B_inc"=3,
                                                       "S_d, S_i, G_d, G_i, B_d, B_i"=7,"Magnetic-A"=4, "Internal file"=5,"pmm file"=8,"Example file"=6),selected = 1) %>%
                            helper(type = "inline",
                                   title = "Format file",
                                   content = c(
                                     "Dec, Inc= declination and inclination in two comma separated columns, with optional third column with stratigraphic position, 
                                     for plotting the mangetic polarity stratigraphy.",
                                     "",
                                     "G_dec, G_inc, B_az, B_plunge= Geographic coordinates declination and inclination,
                                               followed by bedding azimuth (i.e., declination of plunge) and plunge, four comma separated columns,with optional fifth column with stratigraphic position, 
                                     for plotting the mangetic polarity stratigraphy.",
                                     "",
                                     "G_dec, G_inc, B_dec, B_inc= Geographic coordinates declination and inclination, followed by bedding corrected declination and inclination, four comma separated columns,with optional fifth column with stratigraphic position, 
                                     for plotting the mangetic polarity stratigraphy.",
                                     "",
                                     "S_d, S_i, G_d, G_i, B_d, B_i= Specimen, Geographic, and tilt corrected coordinates declination and inclination, six columns, with optional seventh column with sample position.",
                                     "",
                                     "Magnetic-A= File as exported from the directions analysis page.",
                                     "",
                                     "Internal file= Directions selected in the 'All saved samples' page.",
                                     "",
                                     "Example file= Paleomagnetic directions in geographic and tilt-coorected coordinates, with stratigraphic position, from the South Ardo* Paleocene record",
                                     "",
                                     "*Dallanave, E., Agnini, C., Muttoni, G., Rio, D. (2012). Paleocene magneto-biostratigraphy and climate-controlled rock magnetism from the Belluno Basin, Tethys Ocean, Italy. Palaeogeography, Palaeoclimatology, Palaeoecology, 337–338, 130–142. https://doi.org/10.1016/j.palaeo.2012.04.007"),
                                   size = "l",fade = T)
                   ),
                   column(4,selectInput("coord", label= "Coordinates",
                                        choices = list("Geographic"=1, "Tilt corr."=2,"Specimen"=3),selected = 2))
                 ),
                 fluidRow(
                   column(4,numericInput("lat",label="Site latitude",value=0)),
                   column(4,numericInput("long",label="Site longitude",value=0)),
                   column(4, numericInput(inputId = "known_f",label = "Flattening",value = 1,min = 0.1,max = 1) %>%
                            helper(type = "inline",
                                   title = "Optional flattening factor",
                                   content = c(
                                     "If known, a flattening factor can be typed in and applied to the directions. 
                                                     The synthetically 'unflattened' directions will be used in the next pages."
                                   ),
                                   size = "l",fade = T))
                 ),
                 fluidRow(
                   column(4,selectInput("fisher", label= "Mean",          
                                        choices = list("None"=1, "Fisher" = 2, "Elliptic" = 3,
                                                       "Inc. only single mode"=4,"Inc. only bimodal"=5,
                                                       "Arithm. single mode"=6, "Arithm. bimodal"=7),selected = 1)%>%
                            helper(type = "inline",
                                   title = "Mean direction type",
                                   content = c(
                                     "Direction clusters can be averaged by:",
                                     "",
                                     "Fisher- Standard Fisher (1953*) spherical mean.",
                                     "",
                                     "Elliptic- confidence ellipses is calculated following Deenen et al. (2011**).",
                                     "",
                                     "Inc. only- Inclination only average and 95% confidence (either single mode or bimodal) based on the Maximum likelihood solution of Arason and Levi (2010***).",
                                     "",
                                     "Arithm.- Inclination only arithmetic average.",
                                     "",
                                     "*Fisher, R. (1953). Dispersion on a sphere. Proceedings of the Royal Society of London, A217, 295–305.",
                                     "",
                                     "**Deenen, M. H. L., Langereis, C. G., van Hinsbergen, D. J. J., & Biggin, A. J. (2011). Geomagnetic secular variation and the statistics of palaeomagnetic directions. Geophysical Journal International, 186(2), 509–520. https://doi.org/10.1111/j.1365-246X.2011.05050.x",
                                     "",
                                     "***Arason, P., & Levi, S. (2010). Maximum likelihood solution for inclination-only data in paleomagnetism. Geophysical Journal International, 182(2), 753–771. https://doi.org/10.1111/j.1365-246X.2010.04671.x."
                                   ),
                                   size = "l",fade = T)),
                   column(4,selectInput("mode", label= "Mode",
                                        choices = list("Bimodal"=1, "Mode 1" = 2, "Mode 2" = 3, "All down"=4, "All up"=5),selected = 1)),
                   column(4,selectInput(inputId = "apply_known_f",label = "Apply f",
                                        choices = list("No"=1,"Yes"=2),selected = 1))
                 ),
                 fluidRow(
                   column(4,selectInput("colD", label= "Color down",
                                        choices = list("black"=1, "blue" = 2, "red" = 3,"green"=4),selected = 2)),
                   column(4,selectInput("colU", label= "Color up",
                                        choices = list("white"=1, "cyan" = 2, "pink" = 3,"light green"=4),selected = 2)),
                   column(4,selectInput("sym", label= "Symbol",
                                        choices = list("circle"=1, "square"=2, "diamond"=3,"triangle"=4),selected=1))
                 ),
                 fluidRow(
                   column(6,selectInput("cutoff", label= "Cut-off",
                                        choices = list("None"=1, "Vandamme dynamic" = 2, "Vandamme static" = 3,
                                                       "Fixed dynamic"=4, "Fixed static"=5,
                                                       "Cut up-pointing"=6,"Cut down-pointing"=7,"Cut fixed inc."=8),selected = 1) %>%
                            helper(type = "inline",
                                   title = "Cut-off",
                                   content = c(
                                     "Dynamic cut-off, contrary to the static, include the TK03.GAD-based estimate of the inclination shallowing
                                     within the reiterative process (see Dallanave, 2024* for details)",
                                     "NOTE: The use of any cut-off before applying the SVEI test for consistency with the THG24 field model is NOT RECOMMENDED!",
                                     "",
                                     "Vandamme: based on the algorithm of Vandamme, 1994**",
                                     "",
                                     "Static: cut all VGPs with an angular distance higher than the angle defined in the 'VGP fixed-filter radius' ",
                                     "",
                                     "Cut up- and down-pointing: cut either up-pointing or down-pointing directions",
                                     "",
                                     "Cut fixed inc.: cut all directions included in the angular interval defined by the 'Min inc. filt.' and 'Max inc. filt.' angles 
                                     (specifically compiled for azimuthally unoriented data (i.e., RCB IODP data)",
                                     "",
                                     "*Dallanave, E. (2024). Assessing the reliability of paleomagnetic datasets using the R package PmagDiR. Scientific Reports, 14(1666). https://doi.org/10.1038/s41598-024-52001-x",
                                     "",
                                     "**Vandamme, D. (1994). A new method to determine paleosecular variation. Physics of the Earth and Planetary Interiors, 85(1–2), 131–142. https://doi.org/10.1016/0031-9201(94)90012-4"),
                                   size = "l",fade = T)),
                   column(6, numericInput("VGP_fixed", label= "VGP fixed-filter radius", value=45)),
                 ),
                 fluidRow(
                   column(6, numericInput("MinInc",label = "Min Inc. filt.",value = 0)),
                   column(6, numericInput("MaxInc",label = "Max Inc. filt.",value = 0)),
                 ),
                 fluidRow(
                   tableOutput("stats")
                 ),
                 fluidRow(
                   h4(textOutput("inc_warn"))
                 ),
                 br(),
                 fluidRow(
                   column(12,actionButton("resetDir",label = "Delete input file", width = "100%"))
                 ),
                 br(),
               ),
               mainPanel(
                 fluidRow(downloadButton("exportG","Export graph"),
                          downloadButton("exportS","Export stat"),
                          downloadButton("exportDI","Export directions")),
                 column(1),
                 plotOutput("directions")
               )
             )
    ),
    tabPanel("Bootstrap statistics",
             tabsetPanel(
               tabPanel("Confidence ellipses",      
                        sidebarLayout(              
                          sidebarPanel(
                            fluidRow(
                              column(12,h4("Calculate non-parametric 95% confidence*"))),
                            br(),
                            fluidRow(
                              column(4,textInput("fileN_B95",label = "Export name",value = "Site")),
                              column(4,numericInput("B95nb", label="Bootstraps n.",value=10000)),
                              column(4,selectInput("B95_dirs",label = "Plot Dirs",choices = list("Yes"=1,"No"=2),selected = 1))),
                            fluidRow(
                              column(4,actionButton("B95_mode_1_go", label= "Run Mode 1",width = "100%")),
                              column(4,actionButton("B95_mode_2_go", label= "Run Mode 2",width = "100%")),
                              column(4,actionButton("B95_clear",label = "Clear plot",width = "100%"))),
                            br(),
                            fluidRow(
                              column(12,progressBar(
                                id = "B95_Mode1_b",
                                value = 0,total=10000,
                                title = "Bootstrap Mode 1",
                                display_pct = TRUE))),
                            fluidRow(
                              column(12,progressBar(
                                id = "B95_Mode2_b",
                                value = 0,total=10000,
                                title = "Bootstrap Mode 2",
                                display_pct = TRUE))),
                            fluidRow(
                              column(12,h4(textOutput(outputId = "B95_result_text1")))),
                            fluidRow(
                              column(12,h4(textOutput(outputId = "B95_result_text2")))),
                            fluidRow(
                              column(12,h4(textOutput(outputId = "notbimodal")))),
                            br(),
                            fluidRow(
                              column(12,h5("Average and confidence ellipses can be downloaded as coordinate points by clicking on 'Export ellipses'"))),
                            br(),
                            fluidRow(
                              column(12,h5("*Please cite: "), tags$a(href="https://doi.org/10.1029/2023JB026983", 
                                                                     "Heslop, D., Scealy, J.L., Wood, A.T.A., Tauxe, L., Roberts, A.P. (2023). JGR: SolidEarth, 128, e2023JB026983", target="_blank"))
                            )
                          ),
                          mainPanel(
                            fluidRow(
                              downloadButton("B95_graph","Export graph"),
                              downloadButton("B95_stat","Export ellipses")),
                            plotOutput("B95_test")
                          )
                        )
               ),
               tabPanel("Reversal test",         
                        sidebarLayout(
                          sidebarPanel(width=3,
                                       fluidRow(
                                         column(12,h4("Reversal Test*"))),
                                       br(),
                                       fluidRow(
                                         column(6,textInput("fileN_RT",label = "Export name",value = "Site")),
                                         column(6,numericInput("revnb", label="Bootstraps n.",value=10000))),
                                       fluidRow(
                                         column(12,actionButton("revgo", label= "Perform",width = "100%"))),
                                       br(),
                                       fluidRow(
                                         column(12,progressBar(
                                           id = "Rev_test_b",
                                           value = 0,total=10000,
                                           title = "Bootstrap",
                                           display_pct = TRUE))),
                                       br(),
                                       fluidRow(
                                         column(12,h4(textOutput(outputId = "CMDT_result1")))),
                                       fluidRow(
                                         column(12,h4(textOutput(outputId = "CMDT_result2")))),
                                       fluidRow(
                                         column(12,h4(textOutput(outputId = "CMDT_result3")))),
                                       fluidRow(
                                         column(12,h4(textOutput(outputId = "CMDT_result4")))),
                                       br(),
                                       fluidRow(
                                         column(12,h5("*Please cite: "), tags$a(href="https://doi.org/10.1029/2023JB026983", 
                                                                                "Heslop, D., Scealy, J.L., Wood, A.T.A., Tauxe, L., Roberts, A.P. (2023). JGR: SolidEarth, 128, e2023JB026983", target="_blank"))
                                       )
                          ),
                          mainPanel(
                            fluidRow(downloadButton("revexpG","Export graph"),
                                     downloadButton("CMDT_ellipsis",label = "Export Common direction and ellipsis")),
                            plotOutput("revtest")
                          )
                        )
               )
             )
    ),
    tabPanel("Distribution reliability (SVEI test)",
             tabsetPanel(
               tabPanel("SVEI test",
                        sidebarLayout(
                          sidebarPanel(
                            fluidRow(
                              column(12,h4("SVEI Test*"))),
                            br(),
                            fluidRow(
                              column(4,textInput(inputId = "sveiEXPname",label = "Export name",value = "Site")),
                              # column(4,selectInput(inputId = "model_name",label = "GGP model",
                              #                      choices = list("THG24"=1,"Tk03"=2,"CP88"=3,"QC96"=4,"CJ98"=5,"BCE19"=6),selected = 1)),
                              column(4,selectInput(inputId = "SVEI_k",label = "kappa",
                                                   choices = list("Infinite"=1,"50"=2,"100"=3),selected = 1)),
                              column(4,numericInput(inputId = "SVEI_n",label = "Simulations",value = 1000))
                            ),
                            fluidRow(
                              column(12, actionButton(inputId = "SVEIgo",label = "Perform SVEI test",width = "100%"))
                            ),
                            br(),
                            h4(textOutput("flatwarning3")),
                            fluidRow(
                              column(12,progressBar(
                                id = "svei_test_b",
                                value = 0,total=1000,
                                title = "Simulations",
                                display_pct = TRUE))
                            ),
                            br(),
                            fluidRow(
                              column(12,h5("*Perform SVEI test for consistency with the THG24 GGP model. Translated in R from the original svei.py."))),
                            fluidRow(
                              column(12,h5("Run the code locally for a faster bootstrap. Please find the link to the GitHub repository in the 'Introduction and resources' page."))),
                            fluidRow(
                              column(12,h5("Please cite: "), tags$a(href="https://doi.org/10.1029/2024JB029502", 
                                                                    "Tauxe, L., Heslop, D., Gilder, S.A. (2024). JGR: Solid Earth, 129, e2024JB029502.", target="_blank"))
                            )
                          ),
                          mainPanel(
                            fluidRow(downloadButton(outputId = "SVEIexp",label = "Download graph")),
                            plotOutput("SVEI_test_fig")
                          )
                        )
               ),
               tabPanel("Inclination flattening estimate",
                        sidebarLayout(
                          sidebarPanel(
                            fluidRow(column(12,h4("SVEI Test - inclination flattening*"))),
                            fluidRow(
                              column(12,h5("NOTE: This test can be SIGNIFICANTLY TIME DEMANDING. Lost of connection may result in annoying breaks of the process and data loss. 
                                         To avoid this, please use the original Jupyter notebook or run the R code locally."))),
                            br(),
                            fluidRow(
                              column(4, textInput(inputId = "SVEI_EI_expname",label = "Export name",value = "Site unflat")),
                              column(4,selectInput(inputId = "kappa_f",label = "kappa",
                                                   choices = list("Infinite"=1,"50"=2,"100"=3),selected = 2)),
                              column(4, numericInput(inputId = "SVEI_EI_nb",label = "Simulations",value = 1000))
                            ),
                            fluidRow(
                              column(4, numericInput(inputId = "SVEI_fi_1",label = "Initial f1",value = 1.0) %>%
                                       helper(type = "inline",
                                              title = "Setting the flattening parameters for the inclination flattening test",
                                              content = c(
                                                "To optimize the test and reduce the performing time, flattening targets can be modulated depending on the preliminary results.",
                                                "Three f series intervals can be set. If any of the f2 and f3 parameters are set as 0, only Initial, Target, and increments of f1 will be used."
                                              ),
                                              size = "m",fade = T)),
                              column(4, numericInput(inputId = "SVEI_ft_1",label = "Target f1",value = 0.3)),
                              column(4, numericInput(inputId = "SVEIfinc1",label = "Increment 1",value = 0.01)),
                            ),
                            fluidRow(
                              column(4, numericInput(inputId = "SVEI_fi_2",label = "Initial f2",value = 0.0)),
                              column(4, numericInput(inputId = "SVEI_ft_2",label = "Target f2",value = 0.0)),
                              column(4, numericInput(inputId = "SVEIfinc2",label = "Increment 2",value = 0.00)),
                            ),
                            fluidRow(
                              column(4, numericInput(inputId = "SVEI_fi_3",label = "Initial f3",value = 0.0)),
                              column(4, numericInput(inputId = "SVEI_ft_3",label = "Target f3",value = 0.0)),
                              column(4, numericInput(inputId = "SVEIfinc3",label = "Increment 3",value = 0.00)),
                            ),
                            fluidRow(
                              column(4, actionButton(inputId = "check_f",label = "Check f list",width = "100%")),
                              column(8, actionButton(inputId = "SVEI_EI_go",label = "Perform flattening test",width = "100%"))
                            ),
                            br(),
                            h4(textOutput("flatwarning2")),
                            fluidRow(
                              column(12,progressBar(
                                id = "svei_test_EI_b",
                                value = 0,total=1000,
                                title = "Simulations ",
                                display_pct = TRUE))
                            ),
                            br(),
                            fluidRow(
                              column(12,progressBar(
                                id = "f_increments",
                                value = 0,total=71,
                                title = "Total increments performed",
                                display_pct = TRUE))
                            ),
                            br(),
                            fluidRow(
                              column(12,h5("*Perform SVEI test for inclination flattening estimate by comparing progressively unflattened directions with the THG24 GGP model. Translated in R from the original svei.py."))),
                            fluidRow(
                              column(12,h5("Please cite: "), tags$a(href="https://doi.org/10.1029/2024JB029502", 
                                                                    "Tauxe, L., Heslop, D., Gilder, S.A. (2024). JGR: Solid Earth, 129, e2024JB029502.", target="_blank"))
                            )
                          ),
                          mainPanel(
                            fluidRow(
                              downloadButton(outputId = "SVEI_EI_exp",label = "Download graph"),
                              downloadButton(outputId = "SVEI_EI_tab_exp",label = "Download result table")),
                            plotOutput("SVEI_EI_test_fig")
                          )
                        )
               )
             )
    ),
    tabPanel("VGP and Pmag Poles",
             tabsetPanel(
               tabPanel("VGPs plot and average",
                        sidebarLayout(
                          sidebarPanel(width = 5,
                                       fluidRow(
                                         column(12,h4("Loaded directions  VGPs")),
                                       ),
                                       fluidRow(
                                         column(4,textInput("fileN_VGP",label = "Export name",value = "Site")),
                                         column(4,selectInput("intVGPflip",label = "Hemisphere",choices = list("North"=1,"South"=2),selected = 1)),
                                         column(4,selectInput("centercoord",label="Center of plot",
                                                              choices=list("Automatic"= 1,"Manual"= 2), selected=1))
                                       ),
                                       fluidRow(
                                         column(4,numericInput("centerlat",label="Center lat", value=90)),
                                         column(4,numericInput("centerlong",label="Center long", value=0)),
                                         column(4,numericInput("intGrid",label = "Grid", value = 30))
                                       ),
                                       fluidRow(
                                         column(4,selectInput("coastyesno", label = "Coastline",
                                                              choices = list("Yes"=1,"no"=2),selected=1)),
                                         column(4,selectInput("vgpscolor", label= "Color",
                                                              choices= list("black"=1,"blue"=2,"green"=3,"pink"=4,"purple"=5,"brown"=6,"red"=7,"yellow"=8,"cyan"=9, "gray"=10,"white"= 11), selected=3)),
                                         column(4,selectInput("vgpssymbol",label = "Symbol",
                                                              choices = list("circle"=1, "square"=2, "diamond"=3,"triangle"=4),selected=1))
                                       ),
                                       fluidRow(
                                         column(4,selectInput("plotA95", label="Statistic",
                                                              choices=list("None"=1,"Fisher"=2,"Bootstrap"=3),selected=1)),
                                         column(4,numericInput("vgpbootn",label="Bootstraps n.",value=2000)),
                                         column(4,selectInput("VGPtype", label = "VGPs to export",
                                                              choices = list("Single mode"=1,"Bimodal"=2,"Rotated"=3),selected = 1))
                                       ),
                                       h4(textOutput("geowarning")),
                                       h4(textOutput("coordwarning")),
                                       h4(textOutput("flatwarning")),
                                       br(),
                                       fluidRow(
                                         column(12,progressBar(
                                           id = "vgpboot",
                                           value = 0,total=1000,
                                           title = "VGPs bootstrap",
                                           display_pct = TRUE))
                                       ),
                                       fluidRow(
                                         column(12,actionButton("saveVGP",label = "Add to List of loaded VGPs",width = "100%"))
                                       ),
                                       br(),
                                       h4("Preliminary list of loaded VGPs:"),
                                       h5("(Interactive list on Analysis)"),
                                       br(),
                                       tableOutput("Ext_VGP_list3")
                          ),
                          mainPanel(width = 7,
                                    fluidRow(
                                      downloadButton("VGPs_S","Export Pole"),
                                      downloadButton("VGPs_Exp","Export VGPs"),
                                      downloadButton("VGPs_G","Export graph")
                                    ),
                                    fluidRow(column(12,textOutput("fishpole"))
                                    ),
                                    column(1),
                                    plotOutput("VGPplot")
                          )
                        )
               ),
               tabPanel("External VGP",
                        sidebarLayout(
                          sidebarPanel(width = 5,
                                       fluidRow(
                                         column(12,h4("Externally loaded VGPs")),
                                       ),
                                       fluidRow(
                                         column(4,fileInput("vgpfile", label = "Load VGPs file") %>%
                                                  helper(type = "inline",
                                                         title = "Format file",
                                                         content = c(
                                                           "File consists of two comma separated columns, with (any text) header, containing:",
                                                           "",
                                                           "1- Longitude of the VGP",
                                                           "2- Latitude of the VGP"
                                                         ),
                                                         size = "m",fade = T)),
                                         column(4,textInput("EVGP_sitename", label = "VGPs name",value = "Locality")),
                                         column(4,selectInput("VGP_ext_center",label="Center of plot",
                                                              choices=list("Automatic"= 1,"Manual"= 2), selected=1)),
                                       ),
                                       fluidRow(
                                         column(3,numericInput("VGP_ext_clat",label="Center lat", value=90)),
                                         column(3,numericInput("VGP_ext_clong",label="Center long", value=0)),
                                         column(3,numericInput(inputId = "VGP_ext_grid",label = "Grid",value = 30)),
                                         column(3,selectInput("VGP_ext_coast", label = "Coastline",
                                                              choices = list("Yes"=1,"no"=2),selected=1)),
                                       ),
                                       fluidRow(
                                         column(3,selectInput("EVGPcolor", label= "Color",
                                                              choices= list("black"=1,"blue"=2,"green"=3,"pink"=4,"purple"=5,"brown"=6,"red"=7,"yellow"=8,"cyan"=9,"gray"=10, "white"=11), selected=3)),
                                         column(3,selectInput("EVGPsymbol", label= "Symbol",
                                                              choices = list("circle"=1, "square"=2, "diamond"=3,"triangle"=4),selected=1)),
                                         column(3,selectInput("MVGP_stat", label="Statistic",
                                                              choices=list("None"=1,"Fisher"=2,"Bootstrap"=3),selected=1)),
                                         column(3,numericInput("MVGPnb_ext", label = "Bootstrap n.", value = 2000))
                                       ),
                                       fluidRow(
                                         column(4,selectInput("VGP_ext_mode",label = "Hemisphere",choices = list("North"=1,"South"=2),selected = 1)),
                                         column(4,selectInput("VGP_ext_cut_type",label = "Cut-off",choices = list("None"=1,"Vandamme"=2,"Fixed"=3),selected = 1)),
                                         column(4,numericInput("VGP_ext_cutoff",label = "VGP fixed-filter radius",value = 45))
                                       ),
                                       fluidRow(
                                         column(12,progressBar(
                                           id = "Ext_vgpboot",
                                           value = 0,total=2000,
                                           title = "VGP bootstrap",
                                           display_pct = TRUE))
                                       ),
                                       tableOutput("MVGPpolestat"),
                                       fluidRow(
                                         column(6,actionButton("resetMVGP",label = "Clear current VGP", width = "100%")),
                                         column(6,actionButton("saveMVGP",label = "Add to List of loaded VGPs",width = "100%"))
                                       ),
                                       br(),
                                       h4("Preliminary list of loaded VGPs:"),
                                       h5("(Interactive list on Analysis)"),
                                       br(),
                                       tableOutput("Ext_VGP_list")
                          ),
                          mainPanel(width = 7,
                                    fluidRow(
                                      downloadButton("Ext_VGP_G","Export graph"),
                                      downloadButton("VGP_site_stat","Export current site stat")
                                    ),
                                    column(1),
                                    plotOutput("Ext_VGP_plot")
                          )
                        )
               ),
               tabPanel("VGP simulator",
                        sidebarLayout(
                          sidebarPanel(width = 5,
                                       fluidRow(
                                         column(12,h4("VGP (Fisherian) simulator")),
                                       ),
                                       fluidRow(
                                         column(6, textInput("SVGPname",label = "VGPs name",value = "VGP_sim")),
                                         column(3,numericInput("SVGPlon",label = "Longitude",value = 0)),
                                         column(3,numericInput("SVGPlat",label = "Latitude",value = 90,max = 90,min = -90))
                                       ),
                                       fluidRow(
                                         column(3,numericInput("SVGPN",label = "N",value = 100)),
                                         column(3,numericInput("SVGPk",label = "K",value = 20)),
                                         column(3,numericInput("k_tol",label = "K tolerance",value = 0.5) %>%
                                                  helper(type = "inline",
                                                         title = "K tolerance",
                                                         content = c("Maximum difference between selected and simulated k. If set 0, it does not apply any tolerance test."),
                                                         size = "m",fade = T)),
                                         column(3,selectInput("SVGP_center",label="Center of plot",
                                                              choices=list("Automatic"= 1,"Manual"= 2), selected=1))
                                       ),
                                       fluidRow(
                                         column(3,numericInput("SVGP_clat",label="Center lat", value=90)),
                                         column(3,numericInput("SVGP_clong",label="Center long", value=0)),
                                         column(3, numericInput(inputId = "SVGP_grid",label = "Grid",value = 30)),
                                         column(3,selectInput("SVGP_coast", label = "Coastline",
                                                              choices = list("Yes"=1,"no"=2),selected=1)),
                                       ),
                                       fluidRow(
                                         column(3,selectInput("SVGPcolor", label= "Color",
                                                              choices= list("black"=1,"blue"=2,"green"=3,"pink"=4,"purple"=5,"brown"=6,"red"=7,"yellow"=8,"cyan"=9,"gray"=10, "white"=11), selected=2)),
                                         column(3,selectInput("SVGPsymbol", label= "Symbol",
                                                              choices = list("circle"=1, "square"=2, "diamond"=3,"triangle"=4),selected=1)),
                                         column(3,selectInput("SVGP_stat", label="Statistic",
                                                              choices=list("None"=1,"Fisher"=2,"Bootstrap"=3),selected=2)),
                                         column(3,numericInput("SVGPnb", label = "Bootstrap n.", value = 2000))
                                       ),
                                       fluidRow(
                                         column(12,progressBar(
                                           id = "SVGPboot",
                                           value = 0,total=2000,
                                           title = "VGP bootstrap",
                                           display_pct = TRUE))
                                       ),
                                       tableOutput("SVGPstat"),
                                       fluidRow(
                                         column(6, actionButton("SVGPgo",label = "GENERATE VGP",width = "100%")),
                                         column(6,actionButton("saveSVGP",label = "Add to List of loaded VGPs",width = "100%"))
                                       ),
                                       br(),
                                       h4("Preliminary list of loaded VGPs:"),
                                       h5("(Interactive list on Analysis)"),
                                       br(),
                                       tableOutput("Ext_VGP_list2")
                                       
                          ),
                          mainPanel(width = 7,
                                    fluidRow(
                                      downloadButton("SVGP_G","Export graph"),
                                      downloadButton("SVGP_list","Export VGP"),
                                      downloadButton("SVGP_S","Export VGP stat")
                                    ),
                                    column(1),
                                    plotOutput("SVGP_plot")
                          )
                        )
               ),
               tabPanel("VGP rotator",
                        sidebarLayout(
                          sidebarPanel(width = 5,
                                       fluidRow(
                                         column(5,h4("VGPs Euler Rotation")%>%
                                                  helper(type = "inline",
                                                         title = "Info",
                                                         content = c("It works ONLY on one site at a time"),
                                                         size = "m",fade = T))
                                       ),
                                       fluidRow(
                                         column(6, textInput("eul_name",label = "VGPs name",value = "VGP_Rotated")),
                                         column(6,selectInput("eul_center",label="Center of plot",
                                                              choices=list("Automatic"= 1,"Manual"= 2), selected=1))
                                       ),
                                       fluidRow(
                                         column(3,numericInput("eul_clat",label="Center lat", value=90)),
                                         column(3,numericInput("eul_clong",label="Center long", value=0)),
                                         column(3,numericInput(inputId = "RVGP_grid",label = "Grid",value = 30)),
                                         column(3,selectInput("eul_coast", label = "Coastline",
                                                              choices = list("Yes"=1,"no"=2),selected=1)),
                                       ),
                                       fluidRow(
                                         column(4, numericInput("eul_long",label = "EPole Long",value = 0,min = 0,max = 360)),
                                         column(4, numericInput("eul_lat",label = "EPole Lat",value = 90,min = -90,max = 90)),
                                         column(4, numericInput("eul_rot",label = "Rotation",value = 0,min = 0,max = 360)),
                                       ),
                                       fluidRow(
                                         column(4,selectInput("eulPlotType",label = "Type",
                                                              choices = list("VGPs"=1,"Fisher"=2,"Bootstrapped"=3),selected = 1)),
                                         column(4,selectInput("eulcolor", label= "Color",
                                                              choices= list("black"=1,"blue"=2,"green"=3,"pink"=4,"purple"=5,"brown"=6,"red"=7,"yellow"=8,"cyan"=9,"gray"=10, "white"=11), selected=8)),
                                         column(4,selectInput("eulsymbol", label= "Symbol",
                                                              choices = list("circle"=1, "square"=2, "diamond"=3,"triangle"=4),selected=1))
                                       ),
                                       fluidRow(
                                         column(6, actionButton("eulerrot",label = "Rotate",width = "100%")),
                                         column(6, actionButton("eulersave",label = "Add to List of loaded VGPs", width = "100%"))
                                       ),
                                       br(),
                                       tableOutput("eul_temp_table"),
                                       fluidRow(
                                         column(5,h4("List of loaded VGPs"))
                                       ),
                                       br(),
                                       fluidRow(
                                         column(12,DT::dataTableOutput("MVGPlist2"))
                                       ),
                          ),
                          mainPanel(width=7,
                                    fluidRow(
                                      downloadButton("euler_G","Export graph"),
                                      downloadButton("euler_vgp","Export rotated VGP")
                                    ),
                                    column(1),
                                    plotOutput("eulerplot")
                          )
                        )
                        
               ),
               tabPanel("Analysis - 1 - Combine VGPs",
                        sidebarLayout(
                          sidebarPanel(width = 5,
                                       fluidRow(
                                         column(8,h4("Multiple VGPs Analysis"))
                                       ),
                                       fluidRow(
                                         column(4,textInput("fileN_MVGP",label = "Merged VGPs Name",value = "M-VGPs")),
                                         column(4,selectInput("MVGP_names_YN",label = "Plot poles name",
                                                              choices = list("No"=1,"Yes"=2),selected = 1)),
                                         column(4,selectInput("MVGP_center",label="Center of plot",
                                                              choices=list("Automatic"= 1,"Manual"= 2), selected=1))
                                       ),
                                       fluidRow(
                                         column(3,numericInput("MVGP_clat",label="Center lat", value=90)),
                                         column(3,numericInput("MVGP_clong",label="Center long", value=0)),
                                         column(3,numericInput("MultiVGPGrid",label = "Grid", value = 30)),
                                         column(3,selectInput("MVGP_coast", label = " Coastline",
                                                              choices = list("Yes"=1,"no"=2),selected=1))
                                       ),
                                       fluidRow(
                                         column(4,selectInput("MVGPsPlotType",label = "Type",
                                                              choices = list("VGPs"=1,"Fisher"=2,"Bootstrapped"=3),selected = 1)),
                                         column(4,selectInput("MVGP_Pole_Stat", label = "Multi VGPs average",
                                                              choices = list("None"=1,"Fisher of VGPs"=2,"Bootstrap of VGPs"=3), selected = 1)),
                                         column(4,numericInput("MVGPnb", label = "Bootstrap n.", value = 2000))
                                       ),
                                       fluidRow(
                                         column(6,selectInput("MVGP_aver_sym", label = "Mean symbol",
                                                              choices = list("circle"=1, "square"=2, "diamond"=3,"triangle"=4),selected=1)),
                                         column(6,selectInput("MVGP_aver_color", label = "Mean color",
                                                              choices= list("black"=1,"blue"=2,"green"=3,"pink"=4,"purple"=5,"brown"=6,"red"=7,"yellow"=8,"cyan"=9,"gray"=10,"white"=11), selected=10))
                                       ),
                                       fluidRow(
                                         column(12,progressBar(
                                           id = "Mvgpboot",
                                           value = 0,total=2000,
                                           title = "VGP bootstrap",
                                           display_pct = TRUE))
                                       ),
                                       br(),
                                       fluidRow(
                                         column(6,actionButton("add_MVGPs",label = "Add merged VGPs to list",width = "100%")),
                                         column(6,actionButton("deletevgp",label = "Delete selected VGPs",width = "100%"))
                                       ),
                                       fluidRow(
                                         column(12,h4("List of loaded VGPs"))
                                       ),
                                       br(),
                                       fluidRow(
                                         column(12,DT::dataTableOutput("MVGPlist"))
                                       ),
                          ),
                          mainPanel(width=7,
                                    fluidRow(
                                      downloadButton("MVGP_G","Export graph"),
                                      downloadButton("VGP_ALL_stat","Export selected VGPs average"),
                                      downloadButton("MVGP_list",label = "Export Pole list"),
                                      downloadButton("Down_ALL_VGPs",label = "Export selected VGPs")
                                    ),
                                    fluidRow(column(12,textOutput("MVGP_ALLVGPS_stat"))
                                    ),
                                    column(1),
                                    plotOutput("MVGP_plot")
                          )
                        )
               ),
               tabPanel("Analysis - 2 - Add Pmag poles & Fisher mean",
                        sidebarLayout(
                          sidebarPanel(width = 5,
                                       fluidRow(
                                         column(8,h4("External poles list"))
                                       ),
                                       fluidRow(
                                         column(6,fileInput("extrapolelist",label = "Load pole list") %>%
                                                  helper(type = "inline",
                                                         title = "Format file",
                                                         content = c(
                                                           "File as exported from this page (Export Pole list):",
                                                           "",
                                                           "Five columns, comma separated, with (any text) header. Columns (left to right) consist of:",
                                                           "1- Locality name",
                                                           "2- Symbol color",
                                                           "3- Symbol (c= circle,d= diamond, t= triangle,s= square)",
                                                           "4- Pole longitude",
                                                           "5- Pole latitude",
                                                           "6- Confidence angle (A95)"
                                                         ),
                                                         size = "m",fade = T)),
                                         column(6,selectInput("addextreanames",label = "Plot pole names",
                                                              choices = list("No"=1,"Yes"=2),selected = 1))
                                       ),
                                       fluidRow(
                                         column(6, actionButton("addextrapolelist",label = "ADD LOADED FILE TO LIST",width = "100%")),
                                         column(6, actionButton(inputId = "PmagPole_manual",label = "ADD POLE MANUALLY",width = "100%"))
                                       ),
                                       br(),
                                       fluidRow(
                                         column(6, actionButton("rotPmagPole", label = "ROTATE POLE",width = "100%")),
                                         column(6, actionButton("deleteextrapole",label = "DELETE SELECTED POLES",width = "100%"))
                                       ),
                                       br(),
                                       fluidRow(
                                         column(12,h4("Interpolation of all selected poles"))
                                       ),
                                       fluidRow(
                                         column(3,selectInput("extrapolesfisher",label = "Statistic",
                                                              choices = list("None"=1,"Fisher"=2,"Great circle"=3),selected = 1)),
                                         column(3,selectInput("extrameansymbol",label = "Average symbol",
                                                              choices = list("circle"=1, "square"=2, "diamond"=3,"triangle"=4),selected = 1)),
                                         column(3,selectInput("extrameancolor", label= "Average color",
                                                              choices= list("black"=1,"blue"=2,"green"=3,"pink"=4,"purple"=5,"brown"=6,"red"=7,"yellow"=8,"cyan"=9,"gray"=10, "white"=11), selected=2)),
                                         column(3,textInput("extreameanname",label = "Name",value = "Ext_F_mean")),
                                       ),
                                       br(),
                                       fluidRow(
                                         column(12,actionButton(inputId = "add_F_2_ext_list",label = "Add average to list of external poles",width = "100%"))
                                       ),
                                       br(),
                                       fluidRow(
                                         column(6,h4("List of external poles"))
                                       ),
                                       br(),
                                       fluidRow(
                                         column(12,DT::dataTableOutput("EP_list"))
                                       ),
                          ),
                          mainPanel(width = 7,
                                    fluidRow(
                                      downloadButton("MVGP_G2",label = "Export graph"),
                                      downloadButton("Ext_pole_fisher_S",label = "Export average"),
                                      downloadButton("ExportPoleList",label = "Export Pole list")
                                    ),
                                    fluidRow(column(12,textOutput("extpolefisher"))
                                    ),
                                    column(1),
                                    plotOutput("MVGP_plot2"))
                        )
               ),
               tabPanel("Analysis - 3 - Add APWP",
                        sidebarLayout(
                          sidebarPanel(width = 5,
                                       fluidRow(
                                         column(8,h4("Add APWP"))
                                       ),
                                       fluidRow(
                                         column(4,selectInput("APWP", label = "APWP",
                                                              choices = list("None"=1,"V2023"=2,"T2012"=3,"Custom"=4),selected=1)),
                                         column(4,selectInput("frameV23", label= "V2023 frames",
                                                              choices= list("South Africa"=1,"North America"=2,
                                                                            "South America"=3,"Europe"=4,
                                                                            "India"=5,"Australia"=6,"Antarctica"=7,
                                                                            "Pacific (0-80Ma)"=8,"Iberia (0-80Ma)"=9), selected=1)),
                                         column(4,selectInput("frameT12", label= "T2012 frames",
                                                              choices= list("South Africa"= 1,"North America"=2,
                                                                            "Europe"=3,"India"=4,"Amazonia"=5,
                                                                            "Australia"=6,"East Antarctica"=7), selected=1))
                                       ),
                                       fluidRow(
                                         column(4,fileInput("customAPWP",label = "Custom APWP")%>%
                                                  helper(type = "inline",
                                                         title = "Format file",
                                                         content = c(
                                                           "Custom APWP file consists of four columns, comma separated, with (any text) header:",
                                                           "",
                                                           "1- Age (Ma)",
                                                           "2- Pole longitude",
                                                           "3- Pole latitude",
                                                           "4- Confidence angle (A95)"
                                                         ),
                                                         size = "m",fade = T)),
                                         column(4,numericInput("apwp_Y",label = "APWP min age",value = 0)),
                                         column(4,numericInput("apwp_O",label = "APWP max age",value = 320))
                                       ),
                                       fluidRow(
                                         column(12,h5("Please cite: "), tags$a(href="https://doi.org/10.1016/j.earscirev.2023.104547", 
                                                                               "(V2023): Vaes, B. et al. (2023). Earth-Science Reviews, 245(104547), 1–35.", target="_blank")),
                                         column(12, tags$a(href="http://linkinghub.elsevier.com/retrieve/pii/S0012825212000797", 
                                                           "(T2012): Torsvik, T.H., et al. (2012). Earth-Science Reviews, 114(3–4), 325–368.", target="_blank"))
                                       )
                          ),
                          mainPanel(
                            width = 7,
                            fluidRow(
                              downloadButton("MVGP_G3",label = "Export graph")
                            ),
                            column(1),
                            plotOutput("MVGP_plot3")
                          )
                        )
               ),
             )
    ),
    tabPanel("Magnetic polarity",
             sidebarLayout(
               sidebarPanel(width = 3,
                            fluidRow(
                              column(12,fileInput(inputId = "depth_file",label = "Upload depth file",width = "100%") %>%
                                       helper(type = "inline",
                                              title = "Format file",
                                              content = c(
                                                "In case of direction analysis performed with this platform, the depth of the samples is not included in the direction result file. A depth file can be uploaded here.",
                                                "",
                                                "Type 1: two columns with sample code and depth, comma-separated values (.csv). Samples code MUST be the same",
                                                "",
                                                "Type 2: Spinner or ex-spinner file downloaded from LIMS (https://web.iodp.tamu.edu/LORE/), in case of discrete specimen data generated on the JOIDES Resolution. This file is must be the same uploaded in the Vector end-points interpolation window.",
                                                "",
                                                "The file type does not need to be selected, the script recognizes the file depending on the number of columns."),
                                              size = "l",fade = T))
                            ),
                            h4(textOutput("warndepth")),
                            br(),
                            fluidRow(
                              column(12,textInput("fileN_mgstr",label = "Export name",value = "Site"))
                            ),
                            fluidRow(
                              column(6,numericInput("baseMS",label = "Base plot",value = NULL)),
                              column(6,numericInput("topMS",label = "Top plot",value = NULL))
                            ),
                            fluidRow(
                              column(6,selectInput(inputId = "revdepth",label = "Reverse depth",choices = list("No"=1,"Yes"=2),selected = 1)),
                              column(6,textInput(inputId = "depthUnit",label = "Unit",value = "m"))
                            ),
                            fluidRow(
                              column(6,numericInput("hGrid",label = "Vertical grid",value = 10)),
                              column(6,selectInput("colmgstr", label= "Point color",
                                                   choices = list("black"=1,"blue"=2,"green"=3,"pink"=4,"purple"=5,"brown"=6,
                                                                  "red"=7,"yellow"=8,"cyan"=9,"gray"=10,"white"=11),selected = 7))
                            ),
                            fluidRow(
                              column(6,numericInput("Doffset",label = "Decl. offset (°)",value = 0)),
                              column(6,selectInput(inputId = "VGP_inc",label = "VGP/Incl.",choices = list("Use VGP"=1,"Use Inc. (N.Hem.)"=2,"Use Inc. (S.Hem.)"=3),selected = 1))
                            ),
                            br(),
                            fluidRow(
                              column(6,downloadButton("mgstr",label= "Export graph")),
                              column(6,downloadButton("revTab",label="Export Rev."))
                            )
               ),
               mainPanel(
                 plotOutput("magstrat")
               )
             )
    ),
    tabPanel("Site map",
             sidebarLayout(
               sidebarPanel(width = 3,
                            fluidRow(
                              column(6,textInput("siteText", label= "Internal site name",value = "")),
                              column(6,fileInput("sitefile", label= "Sites file") %>%
                                       helper(type = "inline",
                                              title = "Format file",
                                              content = c(
                                                "Site file consists of file columns, comma separated, with (any text) header:",
                                                "",
                                                "1- Site name",
                                                "2- Site longitude",
                                                "3- Site latitude",
                                                "4- Symbol (c= circle,d= diamond, t= triangle,s= square)",
                                                "5- Symbol color"
                                              ),
                                              size = "l",fade = T))
                            ),
                            fluidRow(
                              column(6,selectInput("siteCol", label= "Color",
                                                   choices = list("black"=1, "blue" = 2, "red" = 3,"green"=4,"purple"=5,"white"=6),selected = 3)),
                              column(6,selectInput("siteSym", label= "Symbol",
                                                   choices = list("circle"=1, "square"=2, "diamond"=3,"triangle"=4),selected=1))),
                            fluidRow(
                              column(6,selectInput("gridSpace",label="Map grid",
                                                   choices=list("No grid"=1,"10°"=2,"15°"=3,"30°"=4,
                                                                "45°"=5, "90°"=6), selected=4)%>%
                                       helper(type = "inline",
                                              title = "Map details",
                                              content = c(
                                                "The map is drawn using the Kavrayskiy VII compromise pseudocylindrical projection."),
                                              size = "m",fade = T)
                              ),
                              column(6,selectInput("gridCent", label= "Center meridian",
                                                   choices = list("Greenwich"=1, "Anti Greenwich"=2),selected=1))
                              
                            ),
                            fluidRow(
                              column(4,selectInput("landCol", label= "Land color",
                                                   choices = list("black"=1, "gray" = 2,"light gray"=3, "green" = 4,"darkgreen"=5,"light brown"=6,"brown"=7),selected = 3)),
                              column(4,selectInput("seaCol", label= "Sea Color",
                                                   choices = list("cyan"=1, "light cyan"=2, "light green"=3,"white"=4,"light gray"=5),selected=4)),
                              column(4,selectInput("gridCol", label= "Grid color",
                                                   choices = list("black"=1, "gray" = 2, "light gray" = 3,"blue"=4,"light blue"=5),selected = 2))
                            ),
                            br(),
                            fluidRow(
                              column(12,actionButton("mapgo", label= "Plot - Refresh",width = "100%"))
                            ),
                            br(),
                            fluidRow(
                              column(12,actionButton("resetsitesfile",label = "Delete sites file", width = "100%"))
                            )
               ),
               mainPanel(
                 fluidRow(
                   column(3,downloadButton("mapG","Export map"))
                 ),
                 fluidRow(
                   column(12,plotOutput("geomap"))
                 )
               )
             )
    )
  )
)
