
library(PmagDiR)
library(plyr)
library(dplyr)
library(shiny)
library(shinyWidgets)
library(DT)
library(shinyhelper)
library(shinyjqui)
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
                   column(12,h4("A R-Shiny-based comprehensive toolbox for paleomagnetic analyses."))
                 ),
                 fluidRow(column(12,h5("• Paleomagnetic directions analysis"))),
                 fluidRow(column(12,h5("• Parametric and non-parametric statistics and test for consistency with field model"))),
                 fluidRow(column(12,h5("• Virtual geomagnetic pole analysis"))),
                 fluidRow(column(12,h5("• Multiple paleomagnetic poles analysis"))),
                 fluidRow(column(12,h5("• Magnetic polarity stratigraphy")))
               ),
               mainPanel(
                 fluidRow(
                   column(12, h2("Magnetic-A 2.0"))
                 ),
                 br(),
                 fluidRow(
                   column(12, h3("What's new?"))
                 ),
                 fluidRow(
                   column(12,h3("• Updated and simplified VGPs and Pmag Poles part"))
                 ),                 
                 fluidRow(
                   column(12,h3("• Draggable interactive pop-up windows"))
                 ),
                 fluidRow(
                   column(12,h4("Non-parametric fold test is under development"))
                 ),
                 fluidRow(
                   column(12,h4("Version 1.0 still available at ",tags$a(href="https://edoardodallanave.shinyapps.io/MagneticA/", 
                                                                         "Magnetic-A")))
                 ),
                 fluidRow(
                   column(12,h4("Or you can run Magnetic-A locally (and faster!)  by following the instructions ",tags$a(href="https://github.com/edoardo-paleomag/Magnetic-A/blob/main/README.md",
                                                                                                                         "in the GitHub repository where it is stored.")))
                 ),
                 fluidRow(
                   column(12,h4("The User Guide is available as pdf file on my ",tags$a(href="https://edoardodallanave.wixsite.com/mysite", 
                                                                                        "personal Webpage")))
                 )
               )
             )
    ),
    tabPanel("VEPs analysis",
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
                                                              choices = list("PCA Free"=1,"PCA Anch."=2,"PCA Or. Incl."=3,"PCA Constr."= 6, "Fisher"=4, "G. Circle"=5),selected = 1)%>%
                                                  helper(type = "inline",
                                                         title = "Interpolation type",
                                                         content = c(
                                                           "Vector end-points interpolation can be performed by:",
                                                           "",
                                                           "PCA Free: PCA free from origin of demagnetization axes",
                                                           "",
                                                           "PCA Anch.: PCA with ellipsoid centered at the origin of the demagnetization axes.",
                                                           "THIS OPTION IS WARMLY DISCOURAGED UNLESS JUSTIFIED BY THE PROBABILISTIC PCA* TEST (available in main panel)",
                                                           "",
                                                           "PCA Or. Incl.: Includes origin of the demagnetization axes as demagnetization point",
                                                           "",
                                                           "PCA Constr.: Constrained PCA*",
                                                           "",
                                                           "Fisher: Fisher spherical average of the vector end-points",
                                                           "",
                                                           "G. Circle: Interpolation of the vector end-points by a great circle",
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
                                       fluidRow(
                                         column(12,h5("The component name is editable, double-click on the cell."))
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
    tabPanel("Directions display & average",
             tabsetPanel(
               tabPanel("Directions display & average",
                        sidebarLayout(
                          sidebarPanel(width = 5,
                                       h4("Please assign different Site names to different datasets!"),
                                       fluidRow(
                                         column(6,fileInput("file", label= "Load directions file")),
                                         column(6,textInput("fileN",label = "Site name",value = "Site"))
                                       ),
                                       fluidRow(
                                         column(6,selectInput("filetype", label = "Directions file type",
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
                                                           "S_d, S_i, G_d, G_i, B_d, B_i= Specimen, Geographic, and tilt corrected coordinates declination and inclination, six columns, with optional seventh column with sample position, for plotting the mangetic polarity stratigraphy..",
                                                           "",
                                                           "Magnetic-A= File as exported from the directions analysis page.",
                                                           "",
                                                           "Internal file= Directions selected in the 'All saved samples' sub-page of the 'VEPs analysis' page.",
                                                           "",
                                                           "Example file= Paleomagnetic directions in geographic and tilt-coorected coordinates, with stratigraphic position, from the South Ardo* Paleocene record",
                                                           "",
                                                           "*Dallanave, E., Agnini, C., Muttoni, G., Rio, D. (2012). Paleocene magneto-biostratigraphy and climate-controlled rock magnetism from the Belluno Basin, Tethys Ocean, Italy. Palaeogeography, Palaeoclimatology, Palaeoecology, 337–338, 130–142. https://doi.org/10.1016/j.palaeo.2012.04.007"),
                                                         size = "l",fade = T)
                                         ),
                                         column(6,selectInput("coord", label= "Coordinates",
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
                                                              choices = list("None"=1, "Fisher" = 2,"Fisher (no split)"=8, "Elliptic" = 3,
                                                                             "Inc. only single mode"=4,"Inc. only bimodal"=5,
                                                                             "Arithm. single mode"=6, "Arithm. bimodal"=7),selected = 1)%>%
                                                  helper(type = "inline",
                                                         title = "Mean direction type",
                                                         content = c(
                                                           "Direction clusters can be averaged by:",
                                                           "",
                                                           "Fisher- Standard Fisher (1953*) spherical mean (splits automatically the two modes).",
                                                           "",
                                                           "Fisher (no split)- Standard Fisher (1953*) spherical mean (uses all directions without splitting the modes).",
                                                           "",
                                                           "Elliptic- confidence ellipse is calculated following Deenen et al. (2011**).",
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
                                         column(3,selectInput("colD", label= "Color down",
                                                              choices = list("black"=1, "blue" = 2, "red" = 3,"green"=4),selected = 2)),
                                         column(3,selectInput("colU", label= "Color up",
                                                              choices = list("white"=1, "cyan" = 2, "pink" = 3,"light green"=4),selected = 2)),
                                         column(3,selectInput("sym", label= "Symbol",
                                                              choices = list("circle"=1, "square"=2, "diamond"=3,"triangle"=4),selected=1)),
                                         column(3,selectInput(inputId = "addGAD",label = "GAD",
                                                              choices = list("No"=1,"GAD"=2,"GAD & circle"=3),selected = 1))
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
                          ),
                          mainPanel(width = 7,
                                    fluidRow(downloadButton("exportG","Export graph"),
                                             downloadButton("exportS","Export stat"),
                                             downloadButton("exportDI","Export directions"),
                                             actionButton(inputId = "WatsRand",label = "Watson's test of randomness"),
                                             actionButton(inputId = "cutDirs",label = "Delete dragged directions"),
                                             actionButton(inputId = "restoreDirs",label = "Restore all directions")),
                                    column(1),
                                    plotOutput("directions",brush = brushOpts(id = "plot_brush2"))
                          )
                        )
               ),
               tabPanel("Plot multiple directions sets ",
                        sidebarLayout(
                          sidebarPanel(width = 5,
                                       fluidRow(
                                         column(12,h4("Plots multiple directions sets"))),
                                       br(),
                                       fluidRow(
                                         column(6,fileInput(inputId = "extDirsFile",label = "Load directions file")%>%
                                                  helper(type = "inline",
                                                         title = "Format file",
                                                         content = c(
                                                           "Declination and inclination of the directions in two comma separated columns, with (any) header. Any other column is ignored."),
                                                         size = "m",fade = T)),
                                         column(3,numericInput("GAD_lat", label= "GAD Latitude",value = 0)),
                                         column(3,selectInput(inputId = "addGAD_2",label = "GAD",
                                                              choices = list("No"=1,"GAD"=2,"GAD & circle"=3),selected = 1))
                                       ),
                                       fluidRow(
                                         column(6, actionButton(inputId = "ADD_Dirs",label = "ADD MAIN SET TO LIST",width = "100%")),
                                         column(6, actionButton(inputId = "Del_Dirs",label = "DELETE ENTRY",width = "100%"))
                                       ),
                                       br(),
                                       fluidRow(
                                         column(12,h5("Color and symbol are editable, double-click on the cell."))
                                       ),
                                       fluidRow(
                                         column(12,h6("Valid symbols: c= circle, d= diamond, s= square, t= triangle. Any color accepted by R can be typed in."))
                                       ),
                                       br(),
                                       fluidRow(
                                         column(12,DT::dataTableOutput("multiDirsTab"))
                                       )
                          ),
                          mainPanel(width = 7,
                                    fluidRow(
                                      downloadButton("multiDirs_1","Export graph")),
                                    column(1),
                                    plotOutput("MultiFish1")
                          )
                        )
               ),
               tabPanel("Add average directions",
                        sidebarLayout(
                          sidebarPanel(width = 5,
                                       fluidRow(
                                         column(12,h4("Plots multiple mean directions and 95% confidence"))),
                                       br(),
                                       fluidRow(
                                         column(6,fileInput(inputId = "multiFishFile",label = "Load parametric file") %>%
                                                  helper(type = "inline",
                                                         title = "File format",
                                                         content = c(
                                                           "Six formats are accepted:",
                                                           "",
                                                           "1) and 2) Average Fisher* or elliptic** statistic as exported from the 'Directions display & average' sub-page of Magnetic-A. DO NOT CHANGE the columns name of the file before importing!",
                                                           "",
                                                           "The remaining 4 formats can be compiled manually, all as .csv file with (any) columns header: ",
                                                           "",
                                                           "3) Standard Fisher: 3 columns with declination, inclination, a95 (colors and symbol are set automatically)",
                                                           "",
                                                           "4) Elliptic: 4 columns with declination, inclination, a95_dec, a95_inc (colors and symbol are set automatically)",
                                                           "",
                                                           "5 and 6) Same format as above, but with 3 more columns (total columns= 6 & 7 respectively), with symbol, color of symbol, color of lines",
                                                           "",
                                                           "Accepted symbols: c=circle, d=diamond, t=triangle, s= square; color= full name of the desired color (black, white, cyan, etc)",
                                                           "",
                                                           "",
                                                           "*Fisher, R. (1953). Proceedings of the Royal Society of London, A217, 295–305.",
                                                           
                                                           "**Deenen, M.H.L. et al. (2011). Geophysical Journal International, 186(2), 509–520. https://doi.org/10.1111/j.1365-246X.2011.05050.x"),
                                                         size = "l",fade = T)),
                                         column(6,fileInput(inputId = "multiBootFile",label = "Load non-parametric file")%>%
                                                  helper(type = "inline",
                                                         title = "File format",
                                                         content = c(
                                                           "Accepts files as exported from the 'Bootstrap statistic' page, determined adopting the non-parametric approach of Heslop et al. (2023)*",
                                                           "",
                                                           "1) 4 columns (csv), with (any) header. Columns 1 & 2, only one cell each, with averaged declination and inclination; columns 3 & 4 with declination and inclination of the 95% confidence ellipse points (no limit in the number of rows).",
                                                           "",
                                                           "2) 8 columns (csv), with any header. Same as above, with 2 average directions (Dec1, Inc1, Ellips_dec1, Ellips_inc1, Dec2, Inc2, Ellips_dec2, Ellips_inc2",
                                                           "",
                                                           "*Heslop, D., Scealy, J.L., Wood, A.T.A., Tauxe, L., Roberts, A.P. (2023). JGR: Solid Earth, 128(8), 1–12. https://doi.org/10.1029/2023JB026983"),
                                                         size = "l",fade = T))
                                       ),
                                       fluidRow(
                                         column(6,actionButton(inputId = "MultiFishDetails",label = "ADD ENTRY MANUALLY",width = "100%")),
                                         column(6,actionButton(inputId = "cutMultiFish",label = "DELETE ENTRY",width = "100%"))
                                       ),
                                       br(),
                                       fluidRow(
                                         column(12,h5("The parametric average directions table is fully editable. Within the non-parametric averages table, only colors and symbols are editable. Double-click on the cell to edit."))
                                       ),
                                       fluidRow(
                                         column(12,h6("Valid symbols: c= circle, d= diamond, s= square, t= triangle. Any color accepted by R can be typed in."))
                                       ),
                                       br(),
                                       fluidRow(
                                         column(12,h5(textOutput(outputId = "TableParametric")))),
                                       fluidRow(
                                         column(12,DT::dataTableOutput("multiFishTab"))
                                       ),
                                       br(),
                                       fluidRow(
                                         column(12,h5(textOutput(outputId = "TableBoots")))),
                                       fluidRow(
                                         column(12,DT::dataTableOutput("multiBootTab")))
                          ),
                          mainPanel(width = 7,
                                    fluidRow(
                                      downloadButton("multiDirs_2","Export graph")),
                                    column(1),
                                    plotOutput("MultiFish2")
                          )
                        )
               )
             )
    ),
    tabPanel("Bootstrap statistics",
             tabsetPanel(
               tabPanel("Confidence ellipse",      
                        sidebarLayout(              
                          sidebarPanel(
                            fluidRow(
                              column(12,h4("Calculate non-parametric 95% confidence"))),
                            br(),
                            fluidRow(
                              column(12,h5("Please cite: "), tags$a(href="https://doi.org/10.1029/2023JB026983", 
                                                                    "Heslop, D., Scealy, J.L., Wood, A.T.A., Tauxe, L., Roberts, A.P. (2023). JGR: SolidEarth, 128, e2023JB026983", target="_blank"))
                            ),
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
                              column(12,h5("Average directions and confidence ellipse can be downloaded as coordinate points by clicking on 'Export ellipse'")))
                          ),
                          mainPanel(
                            fluidRow(
                              downloadButton("B95_graph","Export graph"),
                              downloadButton("B95_stat","Export ellipse")),
                            plotOutput("B95_test")
                          )
                        )
               ),
               tabPanel("Reversal test",         
                        sidebarLayout(
                          sidebarPanel(width=3,
                                       fluidRow(
                                         column(12,h4("Reversal Test"))
                                       ),
                                       fluidRow(
                                         column(12,h5("Checks if the two modes flipped on the same hemisphere of the equal area share a common mean direction at a 95% confidence"))
                                       ),
                                       br(),
                                       fluidRow(
                                         column(12,h5("Please cite: "), tags$a(href="https://doi.org/10.1029/2023JB026983", 
                                                                               "Heslop, D., Scealy, J.L., Wood, A.T.A., Tauxe, L., Roberts, A.P. (2023). JGR: SolidEarth, 128, e2023JB026983", target="_blank"))
                                       ),
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
                          ),
                          mainPanel(
                            fluidRow(downloadButton("revexpG","Export graph"),
                                     downloadButton("CMDT_ellipsis",label = "Export Common direction and ellipse")),
                            plotOutput("revtest")
                          )
                        )
               ),
               tabPanel("Common mean direction test of two datasets",         
                        sidebarLayout(
                          sidebarPanel(width=3,
                                       fluidRow(
                                         column(12,h4("Common mean direction test"))
                                       ),
                                       fluidRow(
                                         column(12,h5("Checks if the set of directions loaded in the main page and another set loaded here share a common mean direction at a 95% confidence"))
                                       ),
                                       br(),
                                       fluidRow(
                                         column(12,h5("Please cite: "), tags$a(href="https://doi.org/10.1029/2023JB026983", 
                                                                               "Heslop, D., Scealy, J.L., Wood, A.T.A., Tauxe, L., Roberts, A.P. (2023). JGR: SolidEarth, 128, e2023JB026983", target="_blank"))
                                       ),
                                       br(),
                                       fluidRow(
                                         column(12,fileInput(inputId = "CMDT_2_file",label = "Load second set"))
                                       ),
                                       fluidRow(
                                         column(12,actionButton(inputId = "show_DI2",label = "Show second set",width = "100%"))
                                       ),
                                       br(),
                                       fluidRow(
                                         column(6,textInput("fileN_RT2",label = "Export name",value = "CMDT")),
                                         column(6,numericInput("revnb2", label="Bootstraps n.",value=10000))),
                                       fluidRow(
                                         column(12,actionButton("revgo2", label= "Perform",width = "100%"))),
                                       br(),
                                       fluidRow(
                                         column(12,progressBar(
                                           id = "Rev_test_b2",
                                           value = 0,total=10000,
                                           title = "Bootstrap",
                                           display_pct = TRUE))),
                                       br(),
                                       fluidRow(
                                         column(12,h4(textOutput(outputId = "CMDT_result1_2")))),
                                       fluidRow(
                                         column(12,h4(textOutput(outputId = "CMDT_result2_2")))),
                                       fluidRow(
                                         column(12,h4(textOutput(outputId = "CMDT_result3_2")))),
                                       fluidRow(
                                         column(12,h4(textOutput(outputId = "CMDT_result4_2")))),
                          ),                                
                          mainPanel(
                            fluidRow(downloadButton("revexpG2","Export graph"),
                                     downloadButton("CMDT_ellipsis2",label = "Export Common direction and ellipse")
                            ),
                            plotOutput("revtest2")
                          )
                        )
               )
             )
    ),
    tabPanel("SVEI test",
             tabsetPanel(
               tabPanel("SVEI test",
                        sidebarLayout(
                          sidebarPanel(
                            fluidRow(
                              column(12,h4("SVEI Test"))),
                            fluidRow(
                              column(12,h5("Perform SVEI test for consistency with the THG24 GGP model. Translated in R from the original svei.py."))),
                            fluidRow(
                              column(12,h5("Please cite: "), tags$a(href="https://doi.org/10.1029/2024JB029502", 
                                                                    "Tauxe, L., Heslop, D., Gilder, S.A. (2024). JGR: Solid Earth, 129, e2024JB029502.", target="_blank"))
                            ),
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
                            h4(textOutput("geowarning3")),
                            fluidRow(
                              column(12,progressBar(
                                id = "svei_test_b",
                                value = 0,total=1000,
                                title = "Simulations",
                                display_pct = TRUE))
                            )
                          ),
                          mainPanel(
                            fluidRow(downloadButton(outputId = "SVEIexp",label = "Export graph")),
                            plotOutput("SVEI_test_fig")
                          )
                        )
               ),
               tabPanel("Inclination flattening estimate",
                        sidebarLayout(
                          sidebarPanel(
                            fluidRow(column(12,h4("SVEI Test - inclination flattening"))),
                            br(),
                            fluidRow(
                              column(12,h5("Perform SVEI test for inclination flattening estimate by comparing progressively unflattened directions with the THG24 GGP model. Translated in R from the original svei.py."))),
                            fluidRow(
                              column(12,h5("Please cite: "), tags$a(href="https://doi.org/10.1029/2024JB029502", 
                                                                    "Tauxe, L., Heslop, D., Gilder, S.A. (2024). JGR: Solid Earth, 129, e2024JB029502.", target="_blank"))
                            ),
                            br(),
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
                            h4(textOutput("geowarning2")),
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
                            )
                          ),
                          mainPanel(
                            fluidRow(
                              downloadButton(outputId = "SVEI_EI_exp",label = "Export graph"),
                              downloadButton(outputId = "SVEI_EI_tab_exp",label = "Export result table")),
                            plotOutput("SVEI_EI_test_fig")
                          )
                        )
               )
             )
    ),
    tabPanel("TK03.GAD E/I Test",
             sidebarLayout(
               sidebarPanel(width = 3,
                            fluidRow(
                              column(12,h4("TK03.GAD E/I Test"))),
                            fluidRow(
                              column(12,h5("This page performs the test for inclination flattening of paleomagnetic directions based on the TK03.GAD field model (Elongation/Inclination test).
                                           It is faster than the SVEI test, but it is based on an older paleosecular variation model and has less strict reliability criteria."))),
                            br(),
                            fluidRow(
                              column(12,h5("Please refer to: "), tags$a(href="https://doi.org/10.7916/D81N89JT", 
                                                                        "Tauxe, L., Kent, D.V. (2004). In Channell, J.E.T., Kent, D.V.,Lowrie, W., Meert,J.E.T. (Eds.), Timescales of the Paleomagnetic Field, Geophys. Monogr. (Vol. 145, pp. 101–115).", target="_blank"))
                            ),
                            br(),
                            fluidRow(
                              column(12,textInput("fileN_FF",label = "Export name",value = "Site"))
                            ),
                            fluidRow(
                              column(6,selectInput("ffindyesnoboot", label="Bootstrap?",
                                                   choices = list("No"= 1, "Yes" = 2),selected = 1)),
                              column(6,numericInput("ffindboot", label="Bootstraps number",value=1000))
                            ),
                            fluidRow(
                              column(12,actionButton("ffindgo", label= "Perform",width = "100%"))
                            ),
                            br(),
                            h4(textOutput("flatwarning4")),
                            h4(textOutput("geowarning4")),
                            br(),
                            fluidRow(
                              column(12,progressBar(
                                id = "ffindbootstrap",
                                value = 0,total=1000,
                                title = "Valid bootstraps",
                                display_pct = TRUE
                              ))
                            ),
                            fluidRow(h5(textOutput("validboots"))),
                            br(),
                            fluidRow(
                              tableOutput("ffindStat")
                            )
               ),
               mainPanel(
                 fluidRow(
                   downloadButton("ffindG","Export graph"),
                   downloadButton("ffindS","Export stat")
                 ),
                 column(1),
                 plotOutput("ffindgraph")
               )
             )
    ),
    tabPanel("VGP and Pmag Poles",
             sidebarLayout(
               sidebarPanel(width = 6,
                            fluidRow(
                              column(3,textInput("fileN_VGP",label = "Export name",value = "VGP_&_Poles")),     
                              column(3,selectInput(inputId = "VGPsType",label = "Type",
                                                   choices = list("VGPs"=1,"Fisher"=2,"Bootstrapped"=3),selected = 1)),
                              column(3,selectInput("MVGP_names_YN",label = "Plot poles name",
                                                   choices = list("No"=1,"Yes"=2),selected = 1)),
                              column(3,selectInput("MVGP_coast", label = " Coastline",
                                                   choices = list("Yes"=1,"no"=2),selected=1))
                            ),
                            fluidRow(
                              column(3,selectInput("MVGP_center",label="Center of plot",
                                                   choices=list("Automatic"= 1,"Manual"= 2), selected=1)),
                              column(3,numericInput("MVGP_clat",label="Center lat", value=90)),
                              column(3,numericInput("MVGP_clong",label="Center long", value=0)),
                              column(3,numericInput("MultiVGPGrid",label = "Grid", value = 30))
                            ),
                            fluidRow(
                              column(3, h4("Action buttons")%>%
                                       helper(type = "inline",
                                              title = "Buttons description",
                                              content = c(
                                                "Internal: generate VGPs from directions in 'Directions display & average' window.",
                                                "",
                                                "External: load VGPs file.",
                                                "",
                                                "Simulate: simulates fisheran VGPs from known parameters.",
                                                "",
                                                "Merge VGPs: merge different VGPs sets into one.",
                                                "",
                                                "Pmag Pole: paleomagnetic pole(s) manually or from list exported from this window.",
                                                "",
                                                "Rotate: rotate VGPs or poles applying Euler parameters.",
                                                "",
                                                "Fisher A95: calculate and add to list Fisher mean and parameters.",
                                                "",
                                                "APWP: add on plot APWP either from Magnetic-A or from file (four columns, comma separated, with any header: 1-Age, 2-Long, 3-Lat, 4-A95).",
                                                "",
                                                "Locality: add any geopoint onto the plot.",
                                                "",
                                                "Small circle: add circles to plot, defined by center longitude and latitude, and radius.",
                                                "",
                                                "Great circle: calculate pole of plane and plot great circles through selected poles and (if requested) localities",
                                                "",
                                                "Delete: delete selected entries from list."
                                              ),
                                              size = "m",fade = T))
                            ),
                            fluidRow(
                              column(3, actionButton(inputId = "inter_VGPs",label = "Internal",width = "100%")),
                              column(3, actionButton(inputId = "ext_VGPs",label = "External",width = "100%")),
                              column(3, actionButton(inputId = "sim_VGPs",label = "Simulate",width = "100%")),
                              column(3,actionButton(inputId = "merg_VGPs",label = "Merge",width = "100%")),
                            ),
                            br(),
                            fluidRow(
                              column(3,actionButton("add_PPole",label = "Pmag Pole",width = "100%")),
                              column(3, actionButton(inputId = "rot_VGPs",label = "Rotate",width = "100%")),
                              column(3,actionButton(inputId = "add_A95",label = "Fisher A95",width = "100%")),
                              column(3,actionButton(inputId = "add_apwp",label = "APWP",width = "100%")),
                            ),
                            br(),
                            fluidRow(
                              column(3,actionButton(inputId = "localitydetails",label = "Locality",width = "100%")),
                              column(3,actionButton(inputId = "smCircle",label = "Small circle",width = "100%")),
                              column(3,actionButton(inputId = "add_GCircle",label = "Great circle",width = "100%")),
                              column(3,actionButton("deletevgp",label = "Delete",width = "100%")),
                            ),
                            br(),
                            h4("List of loaded VGPs and Poles:"),
                            fluidRow(
                              column(12,DT::dataTableOutput("VGPs_List")),
                            ),
                            fluidRow(
                              column(12,h5("Name, color, and symbol are editable, double-click on the cell.")),
                            ),
                            fluidRow(
                              column(12,h6("Valid symbols: c= circle, d= diamond, s= square, t= triangle. Any color accepted by R can be typed in.")),
                            ),
                            br(),
                            
               ),
               mainPanel(width = 6,
                         fluidRow(
                           actionButton(inputId = "plusSize",label = "Bigger"),
                           actionButton(inputId = "minusSize",label = "Smaller"),
                           downloadButton("VGPs_G","Export graph"),           
                           downloadButton("VGPs_table","Export Pole Table"),
                           downloadButton("VGPs_Sets","Export selected VGPs sets"),
                           downloadButton(outputId = "VGPs_stats",label = "Export selected stats"),
                         ),
                         # column(1),
                         plotOutput("MVGP_plot")
               )
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
                              column(6,numericInput("Doffset",label = "Decl. offset (0-360°)",value = 0)),
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

