server <- function(input, output){
  
  #activate helpers
  observe_helpers()
  
  ############ VECTOR END-POINTS MODULE##########
  
  #Function for plottin zijderveld used in 2 screens
  #function plotting the vector end point diagram and return values without order of magnitude
  zijderveld <- function(specim,selected_steps,coordinates=1,orient=1,d_tick=0.25,ticks=T){
    
    if(coordinates==1){specim <- specim[,c(1,2,3,4,5)]}
    else if(coordinates==2){specim <- specim[,c(1,2,6,7,8)]}
    else if(coordinates==3){specim <- specim[,c(1,2,9,10,11)]}
    
    colnames(specim) <- c("sample","step","x","y","z")
    #invert z for plotting correct positive and negative
    specim[,5] <- -specim[,5]
    #fix data if North is right and W is up
    if(orient==1){
      specim[,4] <- -specim[,4]
    }
    #fix data if North is up and E is right
    if(orient==2){
      temp <- specim[,3]
      specim[,3] <- specim[,4]
      specim[,4] <- temp
    }
    #eliminate order of magnitudes from values
    OM <- log10(max(abs(specim[,3:5])))
    OM <- ifelse(OM<0,floor(OM), ceiling(OM))
    specim[,3:5] <- (specim[,3:5])/(10**OM)
    #save order of magnitude in reactive file
    
    #define min and max of x
    if(max(specim[,3])<0 && min(specim[,3])<0){
      x_max <- 0
      x_min <-  min(specim[,3])
    }else if(max(specim[,3])>0 && min(specim[,3])>0){
      x_max <-  max(specim[,3])
      x_min <- 0
    }else{
      x_max <- max(specim[,3])
      x_min <- min(specim[,3])
    }
    #define min and max of y
    if(max(specim[,4:5])<0 && min(specim[,4:5])<0){
      y_max <- 0
      y_min <- min(specim[,4:5])
    }else if(max(specim[,4:5])>0 && min(specim[,4:5])>0){
      y_max <-  max(specim[,4:5])
      y_min <- 0
    }else{
      y_max <- max(specim[,4:5])
      y_min <- min(specim[,4:5])
    }
    
    #plot empty diagram
    plot(NA,asp=1,
         xlim=c(x_min,x_max),
         ylim=c(y_min,y_max),
         axes=F,xlab="",ylab="",
         #add axes
         panel.first= c(arrows(x0 = x_min,y0 = 0,
                               x1 = x_max,y1 = 0,length = 0),
                        arrows(x0 = 0,y0 = y_min,
                               x1 = 0,y1 = y_max, length = 0))
    )
    #ask if ticks are wanted
    if(ticks==T){
      #custom function drawing only the ticks
      draw_ticks <- function(x0, y0, x1, y1){
        t = atan2(y1-y0, x1-x0)
        a1 = pi+t+pi/2
        a2 = pi+t-pi/2
        e1x = x1 + ((x_max-x_min)/100)*cos(a1)
        e1y = y1 + ((x_max-x_min)/100)*sin(a1)
        e2x = x1 + ((x_max-x_min)/100)*cos(a2)
        e2y = y1 + ((x_max-x_min)/100)*sin(a2)
        lines(c(e1x,x1,e2x),c(e1y,y1,e2y))
      }
      #define number of ticks on x and y
      #d_tick is the interval of each unit (*10^OM) subdivision
      x_ticks_l <- floor((-x_min)/d_tick)
      x_ticks_r <- floor((x_max)/d_tick)
      for(i in 0:x_ticks_l){
        draw_ticks(x0 = (-((i-1)*d_tick)),y0 = 0,
                   x1 = (-((i)*d_tick)),y1 = 0)
      }
      for(i in 0:x_ticks_r){
        draw_ticks(x0 = (((i-1)*d_tick)),y0 = 0,
                   x1 = (((i)*d_tick)),y1 = 0)
      }
      
      y_ticks_d <- floor((-y_min)/d_tick)
      y_ticks_u <- floor((y_max)/d_tick)
      for(i in 0:y_ticks_d){
        draw_ticks(y0 = (-((i-1)*d_tick)),x0 = 0,
                   y1 = (-((i)*d_tick)),x1 = 0)
      }
      for(i in 0:y_ticks_u){
        draw_ticks(y0 = (((i-1)*d_tick)),x0 = 0,
                   y1 = (((i)*d_tick)),x1 = 0)
      }
    }
    
    #plots vector end points
    points(x=specim[,3],y=specim[,4],type="o",pch=21,bg="black",cex=1.5)
    points(x=specim[,3],y=specim[,5],type="o",pch=21,bg="white",cex=1.5)
    if(!is.null(input$labelaxis)){
      labels_seq <- seq(1,nrow(specim),as.numeric(input$labelspace))
      labels_file <- specim[labels_seq[-1],] #cut step 0 to add NRM
      if(input$labelaxis==1) {text(x=labels_file[,3], y=labels_file[,5],labels=paste(labels_file[,2],input$demagunit,sep = ""),pos=input$labelpos)}
      if(input$labelaxis==2) {text(x=labels_file[,3], y=labels_file[,4],labels=paste(labels_file[,2],input$demagunit,sep = ""),pos=input$labelpos)}
    }
    #plot NRM as square
    points(x=specim[1,3],y=specim[1,4],pch=22,bg="black",cex=1.8)
    points(x=specim[1,3],y=specim[1,5],pch=22,bg="white",cex=1.8)
    if(!is.null(input$labelaxis)){
      if(input$labelaxis==1) {text(x=specim[1,3], y=specim[1,5],labels=ifelse(specim[1,2]==0,"NRM",paste(specim[1,2],input$demagunit,sep = "")),pos=input$labelpos,cex=1.2)}
      if(input$labelaxis==2) {text(x=specim[1,3], y=specim[1,4],labels=ifelse(specim[1,2]==0,"NRM",paste(specim[1,2],input$demagunit,sep = "")),pos=input$labelpos,cex=1.2)}
    }
    #highlight selected steps
    if(length(selected_steps)){
      Ssteps <- specim[selected_steps,]
      points(x=Ssteps[,3],y=Ssteps[,4],pch=21,bg="red",cex=1.8)
      points(x=Ssteps[,3],y=Ssteps[,5],pch=21,col="red" ,bg="yellow",cex=1.8)
      if(selected_steps[1]==1){
        points(x=specim[1,3],y=specim[1,4],pch=22,bg="red",cex=2.1)
        points(x=specim[1,3],y=specim[1,5],pch=22,col="red",bg="yellow",cex=2.1)
      }
    }
    
    
    #add coordinates
    if(orient==1){
      text(x = x_max,y=0,"N", pos=4, cex=1.2)
      text(x = 0,y=y_max,"W, Up", pos=3, cex=1.2)
    }
    if(orient==2){
      text(x = x_max,y=0,"E", pos=4, cex=1.2)
      text(x = 0,y=y_max,"N, Up", pos=3, cex=1.2)
    }
    #create list for returning both data without order of magnitude AND order of magnitude
    result <- list(specim)
    result$OM <- OM
    return(result)
  }
  
  #interpolating line function
  interpol_line <- function(anchor,Zijd_shift,coordinates){
    if(anchor==2){
      if(length(specim$DiR_da)){
        if(Zijd_shift==1){
          m_xy <- -specim$DiR_da[coordinates,8]
          m_xz <- -specim$DiR_da[coordinates,9]
        }else if(Zijd_shift==2){
          m_xy <- 1/(specim$DiR_da[coordinates,8])
          m_xz <- -specim$DiR_da[coordinates,10]
        }
        #plot curve corrected for center of mass
        curve((m_xy*x),add = T,col="blue", lty=2,lwd=2)
        curve((m_xz*x),add = T,col="blue", lty=2,lwd=2)
      }
    }
    else if(anchor==1){
      if(length(specim$DiR_df)){
        if(Zijd_shift==1){
          m_xy <- -specim$DiR_df[coordinates,8]
          m_xz <- -specim$DiR_df[coordinates,9]
          #recalculate x0 y0 and z0 removing OM
          x0 <- specim$DiR_df[coordinates,5]/(10**specim$OM)
          y0 <- -specim$DiR_df[coordinates,6]/(10**specim$OM)
          z0 <- -specim$DiR_df[coordinates,7]/(10**specim$OM)
        }else if(Zijd_shift==2){
          m_xy <- 1/(specim$DiR_df[coordinates,8])
          m_xz <- -specim$DiR_df[coordinates,10]
          #recalculate x0 y0 and z0 removing OM switching axes x and y!!
          x0 <- specim$DiR_df[coordinates,6]/(10**specim$OM)
          y0 <- specim$DiR_df[coordinates,5]/(10**specim$OM)
          z0 <- -specim$DiR_df[coordinates,7]/(10**specim$OM)
        }
        #plot curve corrected for center of mass
        curve(((m_xy*(x-x0))+y0),add = T,col="blue", lty=2,lwd=2)
        curve(((m_xz*(x-x0))+z0),add = T,col="blue", lty=2,lwd=2)
      }
    }
    else if(anchor==3){
      if(length(specim$DiR_doi)){
        if(Zijd_shift==1){
          m_xy <- -specim$DiR_doi[coordinates,8]
          m_xz <- -specim$DiR_doi[coordinates,9]
          #recalculate x0 y0 and z0 removing OM
          x0 <- specim$DiR_doi[coordinates,5]/(10**specim$OM)
          y0 <- -specim$DiR_doi[coordinates,6]/(10**specim$OM)
          z0 <- -specim$DiR_doi[coordinates,7]/(10**specim$OM)
        }else if(Zijd_shift==2){
          m_xy <- 1/(specim$DiR_doi[coordinates,8])
          m_xz <- -specim$DiR_doi[coordinates,10]
          #recalculate x0 y0 and z0 removing OM switching axes x and y!!
          x0 <- specim$DiR_doi[coordinates,6]/(10**specim$OM)
          y0 <- specim$DiR_doi[coordinates,5]/(10**specim$OM)
          z0 <- -specim$DiR_doi[coordinates,7]/(10**specim$OM)
        }
        #plot curve corrected for center of mass
        curve(((m_xy*(x-x0))+y0),add = T,col="blue", lty=2,lwd=2)
        curve(((m_xz*(x-x0))+z0),add = T,col="blue", lty=2,lwd=2)
      }else{NULL}
    }
    else if(anchor==6){
      if(length(specim$DiR_C)){
        if(Zijd_shift==1){
          m_xy <- -specim$DiR_C[coordinates,8]
          m_xz <- -specim$DiR_C[coordinates,9]
          #recalculate x0 y0 and z0 removing OM
          x0 <- specim$DiR_C[coordinates,5]/(10**specim$OM)
          y0 <- -specim$DiR_C[coordinates,6]/(10**specim$OM)
          z0 <- -specim$DiR_C[coordinates,7]/(10**specim$OM)
        }else if(Zijd_shift==2){
          m_xy <- 1/(specim$DiR_C[coordinates,8])
          m_xz <- -specim$DiR_C[coordinates,10]
          #recalculate x0 y0 and z0 removing OM switching axes x and y!!
          x0 <- specim$DiR_C[coordinates,6]/(10**specim$OM)
          y0 <- specim$DiR_C[coordinates,5]/(10**specim$OM)
          z0 <- -specim$DiR_C[coordinates,7]/(10**specim$OM)
        }
        #plot curve corrected for center of mass
        curve(((m_xy*(x-x0))+y0),add = T,col="blue", lty=2,lwd=2)
        curve(((m_xz*(x-x0))+z0),add = T,col="blue", lty=2,lwd=2)
      }
    }
  }
  
  #function plotting equal area
  plot_equal_area <- function(VEP_dat,VEP_coord){
    if(VEP_coord==3){VEP_temp <- VEP_dat[,c(1,2,9,10,11)]}
    if(VEP_coord==2){VEP_temp <- VEP_dat[,c(1,2,6,7,8)]}
    if(VEP_coord==1){VEP_temp <- VEP_dat[,c(1,2,3,4,5)]}
    
    #functions converting cartesian to spherical
    c2sD <- function(x,y) {(atan2(y,x))*(180/pi)}
    c2sI <- function(x,y,z) {(asin(z/(sqrt((x^2)+(y^2)+(z^2)))))*(180/pi)}
    c2sInt <- function(x,y,z) {sqrt((x^2)+(y^2)+(z^2))}
    
    
    #functions converting inc(x) and dec(y) into equal area
    a2cx <- function(x,y) {sqrt(2)*sin((PmagDiR::d2r(90-x))/2)*sin(PmagDiR::d2r(y))}
    a2cy <- function(x,y) {sqrt(2)*sin((PmagDiR::d2r(90-x))/2)*cos(PmagDiR::d2r(y))}
    
    #add spherical coord columns for table
    dat <- VEP_temp[,-1]
    dat$D <- (round(c2sD(VEP_temp[,3],VEP_temp[,4]),digits = 1))%%360
    dat$I <- round(c2sI(VEP_temp[,3],VEP_temp[,4],VEP_temp[,5]),digits = 1)
    dat$Int <- formatC(c2sInt(VEP_temp[,3],VEP_temp[,4],VEP_temp[,5]), format = "e", digits = 2)
    dat <- subset(dat,select = c(1,5,6,7))
    #add x and y coordinates for equal area plot
    dat$x <- a2cx(abs(dat$I),dat$D)
    dat$y <- a2cy(abs(dat$I),dat$D)
    #copy cartesian coordinates to reactive file for equal area use
    specim$cart <- dat
    
    #draw base equal area from PmagDiR
    PmagDiR::equalarea()
    
    #graphical function connecting two points on a sphere with great circle segment
    connect_GC <- function(DI){
      #functions converting inc(x) and dec(y) into equal area
      a2cx <- function(x,y) {sqrt(2)*sin((PmagDiR::d2r(90-x))/2)*sin(PmagDiR::d2r(y))}
      a2cy <- function(x,y) {sqrt(2)*sin((PmagDiR::d2r(90-x))/2)*cos(PmagDiR::d2r(y))}
      #functions spherical (Dec=x, Inc=y) to Cartesian
      s2cx <- function(x,y) {cos(PmagDiR::d2r(x))*cos(PmagDiR::d2r(y))}
      s2cy <- function(x,y) {sin(PmagDiR::d2r(x))*cos(PmagDiR::d2r(y))}
      s2cz <- function(y) {sin(PmagDiR::d2r(y))}
      for(i in 2:nrow(DI)){
        #data are data frame 2X2 with dec and inc
        data <- DI[(i-1):i,]
        colnames(data) <- c("dec", "inc")
        
        ##NEXT PART CALCULATES POLE OF GREAT CIRCLE
        #directions in Cartesian coordinates
        data$x <- cos(PmagDiR::d2r(data$dec))*cos(PmagDiR::d2r(data$inc))
        data$y <- sin(PmagDiR::d2r(data$dec))*cos(PmagDiR::d2r(data$inc))
        data$z <- sin(PmagDiR::d2r(data$inc))
        #averaged Cartesian coordinates
        x_av <- mean(data$x)
        y_av <- mean(data$y)
        z_av <- mean(data$z)
        #elements of the distribution matrix
        T_elements <- c(sum((data$x)*(data$x)),sum(data$x*data$y),sum(data$x*data$z),
                        sum(data$y*data$x),sum(data$y*data$y),sum(data$y*data$z),
                        sum(data$z*data$x),sum(data$z*data$y),sum(data$z*data$z))
        #distribution matrix
        T <- matrix(T_elements,nrow=3, byrow=TRUE)
        #calculate and copy eigenvalues and vectors
        T_e <- eigen(T)
        T_vec <- T_e$vectors
        #coordinate of V3
        V3inc <- PmagDiR::r2d(asin(T_vec[3,3]/(sqrt((T_vec[1,3]^2)+(T_vec[2,3]^2)+(T_vec[3,3]^2)))))
        V3dec <- (PmagDiR::r2d(atan2(T_vec[2,3],T_vec[1,3])))%%360
        if(V3inc<0){
          V3dec <- (V3dec+180)%%360
          V3inc <- abs(V3inc)
        }
        #place both points on the horizontal plane
        data_horiz <- PmagDiR::bed_DI(DI = data[,1:2],in_file = FALSE,bed_az = (V3dec+180)%%360,
                                      bed_plunge = (90-V3inc))
        if(max(data_horiz[,1])-min(data_horiz[,1])<180){
          GCS_h <- data.frame(seq(min(data_horiz[,1]),max(data_horiz[,1]),0.5))
          colnames(GCS_h) <- "dec"
          GCS_h$inc <- rep(0)
        }else if((max(data_horiz[,1])-min(data_horiz[,1])>=180)){
          GCS_h <- data.frame((seq(-(360-max(data_horiz[,1])),min(data_horiz[,1]),0.5)))
          colnames(GCS_h) <- "dec"
          GCS_h$inc <- rep(0)
        }
        #place points and connecting great circle back in position
        GCS <- PmagDiR::bed_DI(DI = GCS_h,in_file = F,bed_az = V3dec,bed_plunge = (90-V3inc))
        colnames(GCS) <- c("dec","inc")
        #separate hemispheres
        GCS$em <- ifelse(GCS$inc<=0,-1,1)
        GCS$inc <- abs(GCS$inc)
        GCS$x <- a2cx(GCS$inc,GCS$dec)
        GCS$y <- a2cy(GCS$inc,GCS$dec)
        GCS_upper <- dplyr::filter_all(GCS,all_vars(GCS$em<0))
        GCS_lower <- dplyr::filter_all(GCS,all_vars(GCS$em>0))
        #plot great circles
        points(x = GCS_upper$x,y=GCS_upper$y,type="l", col="black",lty=2)
        points(x = GCS_lower$x,y=GCS_lower$y,type="l", col="black")
      }
    }
    
    #first draw connecting cirlces
    connect_GC(specim$cart[,2:3])
    
    #add points
    points(x=specim$cart[,5],
           y=specim$cart[,6],pch=21, bg=ifelse(specim$cart[,3]<0,"white","black"),
           cex=1.4)
    # Plot demag labels
    if(!is.null(input$labelaxis) && input$labelaxis!=3){
      labels_seq <- seq(1,nrow(specim$cart),as.numeric(input$labelspace))
      labels_file <- specim$cart[labels_seq[-1],] #cut step 0 to add NRM
      text(x=labels_file[,5], y=labels_file[,6],labels=paste(labels_file[,1],input$demagunit,sep = ""),pos=input$labelpos)
    }
    #plots NRM
    points(x=specim$cart[1,5],
           y=specim$cart[1,6],pch=22, bg=ifelse(specim$cart[1,3]<0,"white","black"),
           cex=1.8)
    if(!is.null(input$labelaxis) && input$labelaxis!=3){
      text(x=specim$cart[1,5], y=specim$cart[1,6],labels=ifelse(specim$cart[1,1]==0,"NRM",paste(specim$cart[1,1],input$demagunit,sep = "")),pos=input$labelpos,cex=1.2)
    }
    
    #highlight selected points
    if(length(selectedVEP())){
      Hightlight <- specim$cart[selectedVEP(),]
      points(x=Hightlight[,5],
             y=Hightlight[,6],pch=21, col=ifelse(Hightlight[,3]<0,"red","black"),
             bg=ifelse(Hightlight[,3]<0,"yellow","red"),
             cex=1.6)
      if(selectedVEP()[1]==1){
        points(x=Hightlight[1,5],
               y=Hightlight[1,6],pch=22, col=ifelse(Hightlight[1,3]<0,"red","black"),
               bg=ifelse(Hightlight[1,3]<0,"yellow","red"),
               cex=2)
      }
    }
    #plot fisher stat
    if(length(specim$DiR_f)){
      PmagDiR::plot_a95(specim$DiR_f[VEP_coord,1],specim$DiR_f[VEP_coord,2],specim$DiR_f[VEP_coord,3],
                        on_plot = T,symbol = "d",col_d = "purple",col_u = "pink")
    }
    if(length(specim$DiR_p)){
      specim$DiR_p <- as.data.frame(specim$DiR_p)
      PmagDiR::plot_plane(specim$DiR_p[VEP_coord,1],specim$DiR_p[VEP_coord,2],on_plot = T,col_cU = "blue",col_cD = "blue",symbol = "d",col_d = "purple")
    }
  }
  
  #read file if present, reset if requested
  #reads files depending on the format selected, and normalise with NRMnorm
  sample_list <- reactive({
    if (is.null(input$All_Zijd) && input$Zijd_f_type!=7){
      return(NULL)
    }
    if(input$Zijd_f_type==1){
      read.csv(file = input$All_Zijd$datapath)
    }
    #LASA file
    else if(input$Zijd_f_type==2){
      dat <- read.table(input$All_Zijd$datapath,header = F,skip = 6)
      dat <- dat[,-c(3,4)]
      dat_PmagDiR <- data.frame(matrix(ncol=11,nrow = nrow(dat)))
      colnames(dat_PmagDiR) <- c("sample","step","Sx","Sy","Sz","Gx","Gy","Gz","Bx","By","Bz")
      dat_PmagDiR[,1:2] <- dat[,1:2]
      dat_PmagDiR[,3:5] <- PmagDiR::s2c(DI = dat[,4:5],J = dat[,3])
      dat_PmagDiR[,6:8] <- PmagDiR::s2c(DI = dat[,6:7],J = dat[,3])
      dat_PmagDiR[,9:11] <- PmagDiR::s2c(DI = dat[,8:9],J = dat[,3])
      
      #file is in emu e-4, next convert in Am2
      dat_PmagDiR[,3:11] <- dat_PmagDiR[,3:11]*(10**-7)
    }else if(input$Zijd_f_type==3){
      #Bremen cor file
      dat <- read.table(input$All_Zijd$datapath,header = F,skip = 1)
      dat_PmagDiR <- data.frame(matrix(ncol = 11,nrow = nrow(dat)))
      colnames(dat_PmagDiR) <- c("Sample","Step","Sx","Sy","Sz","Gx","Gy","Gz","Bx","By","Bz")
      dat_PmagDiR[,1:2] <- dat[,1:2]
      dat_PmagDiR[,3:5] <- dat[,3:5]
      dat_PmagDiR[,6:8] <- dat[,3:5]
      dat_PmagDiR[,9:11] <- dat[,3:5]
      #converts in Am2
      dat_PmagDiR[,3:11] <- dat_PmagDiR[,3:11]*(10**-3)
    }else if(input$Zijd_f_type==4){
      #load IODP Spinner data
      dat <- read.csv(input$All_Zijd$datapath)
      #eliminate IRM if present
      eliminateIRM <- which(dat[,19]=="IRM")
      if(length(eliminateIRM)){dat <- dat[-eliminateIRM,]}
      temp_file <- data.frame(matrix(ncol=12,nrow = nrow(dat)))
      colnames(temp_file) <- c("CSF_A","sample","step","Sx","Sy","Sz","Gx","Gy","Gz","Bx","By","Bz")
      temp_file[,1] <- dat[,9]
      #create specimens code
      temp_file[,2] <- paste(paste(dat[,1],paste(dat[,2],dat[,3],sep=""),
                                   paste(dat[,4],dat[,5],sep = ""),
                                   paste(dat[,6],dat[,7],sep = ""),sep="-"),
                             paste(dat[,8],(dat[,8]+2),sep = "/"),sep = ";")
      temp_file[,3] <- dat[,20]
      temp_file[,4:6] <- PmagDiR::s2c(DI = dat[,12:11],J = dat[,15])
      temp_file[,7:9] <- PmagDiR::s2c(DI = dat[,14:13],J = dat[,15])
      temp_file[,10:12] <- PmagDiR::s2c(DI = dat[,14:13],J = dat[,15])
      temp_file <- temp_file[order(temp_file[,3]),]
      temp_file <- temp_file[order(temp_file[,1]),]
      dat_PmagDiR <- temp_file[,-1]
      #return(dat_PmagDiR)
    }else if(input$Zijd_f_type==5){
      #CIT multisamples
      dat_PmagDiR <- data.frame(matrix(ncol = 11,nrow = 0))
      colnames(dat_PmagDiR) <- c("Sample","Step","Sx","Sy","Sz","Gx","Gy","Gz","Bx","By","Bz")
      for(i in 1:length(input$All_Zijd[,1])){
        #read first and second row and count columns
        f_row <- read.table(input$All_Zijd[[i, 'datapath']],header = F,skip = 2,nrows = 1)
        s_row <- read.table(input$All_Zijd[[i, 'datapath']],header = F,skip = 3,nrows = 1)
        if(ncol(f_row)<ncol(s_row)){
          f_row <- cbind(f_row[1,1],NA,f_row[1,2:ncol(f_row)])
          dat <- read.table(input$All_Zijd[[i, 'datapath']],header = F,skip = 3)
          colnames(f_row) <- colnames(dat)
          dat <- rbind(f_row,dat)
        }else if(ncol(f_row)==ncol(s_row)){dat <- read.table(input$All_Zijd[[i, 'datapath']],header = F,skip = 2)}
        specimen <- input$All_Zijd[[i, 'name']]
        temp_file <- data.frame(matrix(ncol = 11,nrow = nrow(dat)))
        colnames(temp_file) <- c("Sample","Step","Sx","Sy","Sz","Gx","Gy","Gz","Bx","By","Bz")
        temp_file[,1] <- rep(specimen)
        temp_file[,2] <- dat[,2]
        temp_file[,3:5] <- PmagDiR::s2c(DI = dat[,9:10],J = dat[,7])
        temp_file[,6:8] <- PmagDiR::s2c(DI = dat[,3:4],J = dat[,7])
        temp_file[,9:11] <- PmagDiR::s2c(DI = dat[,5:6],J = dat[,7])
        dat_PmagDiR <- rbind(dat_PmagDiR,temp_file)
      }
      #return(dat_PmagDiR)
    }else if(input$Zijd_f_type==6){
      #Longyun multisamples
      dat_PmagDiR <- data.frame(matrix(ncol = 11,nrow = 0))
      colnames(dat_PmagDiR) <- c("Sample","Step","Sx","Sy","Sz","Gx","Gy","Gz","Bx","By","Bz")
      #must read all selected file
      for(i in 1:length(input$All_Zijd[,1])){
        #read name from first row
        first_row <- read.table(input$All_Zijd[[i, 'datapath']],header = F,skip = 1,nrows = 1)
        specimen <- first_row[1,1]
        #read demag file
        dat <- read.table(input$All_Zijd[[i, 'datapath']],header = F,skip = 3)
        #create temp converted file
        temp_file <- data.frame(matrix(ncol = 11,nrow = nrow(dat)))
        colnames(temp_file) <- c("Sample","Step","Sx","Sy","Sz","Gx","Gy","Gz","Bx","By","Bz")
        #populate temp_fil
        temp_file[,1] <- rep(specimen)
        temp_file[,2] <- dat[,1]
        temp_file[,3:5] <- PmagDiR::s2c(DI = dat[,6:7],J = dat[,5])
        temp_file[,6:8] <- PmagDiR::s2c(DI = dat[,6:7],J = dat[,5])
        temp_file[,9:11] <- PmagDiR::s2c(DI = dat[,8:9],J = dat[,5])
        dat_PmagDiR <- rbind(dat_PmagDiR,temp_file)
      }
      #return(dat_PmagDiR)
      #example file
    }else if(input$Zijd_f_type==7){
      dat_PmagDiR <- PmagDiR::Ardo_diRs_example
      #file is in emu e-4, next convert in Am2
      dat_PmagDiR[,3:11] <- dat_PmagDiR[,3:11]*(10**-7)
    }
    #acts only if Unit window has been opened before
    if(!is.null(input$NRMnormYN)){
      #normalise per volume and convert cm^3 in m^3
      if(input$NRMnormYN==2){dat_PmagDiR[,3:11] <- dat_PmagDiR[,3:11]/(input$NRMvolume*(10**-6))}
      #normalise per mass and convert g in kg
      if(input$NRMnormYN==3){dat_PmagDiR[,3:11] <- dat_PmagDiR[,3:11]/(input$NRMmass*(10**-3))}  
    }
    #return manipulated values   
    return(dat_PmagDiR)
  })
  
  #create reactive file
  specim <- reactiveValues(specim=NULL)
  
  #isolate specimen names and make table
  specimens <- reactive({
    if(!is.null(sample_list())){
      dat <- sample_list()
      specimens <- data.frame(unique(dat[,1]))
      colnames(specimens) <- "specimens"
      specim$list <- specimens
    }
  })
  
  #send table to UI
  output$samples_list <- DT::renderDataTable(specimens(), server = F,
                                             selection="single",
                                             rownames=F,
                                             options=list(searching=F))
  
  #isolate specimen selected on list side of the Zijd.
  isolated_specimen <- reactive({
    #depopulate stat temp result files
    specim$DiR_f <- NULL
    specim$DiR_p <- NULL
    specim$DiR_da <- NULL
    specim$DiR_df <- NULL
    specim$DiR_doi <- NULL
    specim$DiR_C <- NULL
    samp <- input$samples_list_rows_selected
    if(length(samp)){
      req(sample_list())
      dat <- sample_list()
      specim$specim <- dat[dat[,1]==specim$list[samp,1],]
    }
  })
  
  #depopulate stat temp result files when new sample is selected
  observeEvent(input$samples_list_rows_selected,{
    #depopulate stat temp result files
    specim$DiR_f <- NULL
    specim$DiR_p <- NULL
    specim$DiR_da <- NULL
    specim$DiR_df <- NULL
    specim$DiR_doi <- NULL
    specim$DiR_C <- NULL
  })
  
  #restore specimen
  observeEvent(input$restore_VEPs,{
    specim$specim <- isolated_specimen()
    specim$selectedVEP <- NULL
    specim$selectedVEP <- NULL
    specim$selectedVEP_t <- NULL
    specim$selectedVEP_BW <- NULL
    specim$selectedVEP_BB <- NULL
    specim$selectedVEP_stereo <- NULL
    specim$DiR_f <- NULL
    specim$DiR_p <- NULL
    specim$DiR_da <- NULL
    specim$DiR_df <- NULL
    specim$DiR_doi <- NULL
    specim$DiR_d <- NULL
    specim$DiR_C <- NULL
  })
  
  #table data of VEP
  output$sampledat <- DT::renderDataTable({
    #if no samples are selected returns null
    samp <- input$samples_list_rows_selected
    if(length(samp)){
      dat <- data.frame(specim$specim[,2])
      colnames(dat) <- "Step"
      dat
    }else{NULL}
  }, server = FALSE, rownames=FALSE,options=list(dom='t',sort=FALSE,
                                                 "drawCallback" = JS("function(settings) {var table = this.api();table.rows().nodes().to$().css('font-size', '12px');}"),
                                                 paging=FALSE),class=list(stripe=FALSE))
  
  
  #selection of vector end-points
  selectedVEP <- reactive({
    #select points from step list
    specim$selectedVEP_t <- input$sampledat_rows_selected
    if(input$Zijd_Stereo_shift==1 || input$Zijd_Stereo_shift==2){
      specim$selectedVEP_stereo <- NULL
      #brush white points
      selectedVEP_BW_temp <- rownames(brushedPoints(specim$specim_no_OM, input$plot_brush, xvar = "x", yvar = "z"))
      specim$selectedVEP_BW <- which(rownames(specim$specim) %in% selectedVEP_BW_temp)
      #brush black points
      selectedVEP_BB_temp <- rownames(brushedPoints(specim$specim_no_OM, input$plot_brush, xvar = "x", yvar = "y"))
      specim$selectedVEP_BB <- which(rownames(specim$specim) %in% selectedVEP_BB_temp)
    } else if(input$Zijd_Stereo_shift==3){
      specim$selectedVEP_BW <- NULL
      specim$selectedVEP_BB <- NULL
      #brush stereo points
      selectedVEP_stereo_temp <- rownames(brushedPoints(specim$cart, input$plot_brush, xvar = "x", yvar = "y"))
      specim$selectedVEP_stereo <- which(rownames(specim$specim) %in% selectedVEP_stereo_temp)
    }
    #create single file with all selected points
    specim$selectedVEP <- sort(unique(c(specim$selectedVEP_t,specim$selectedVEP_BW,specim$selectedVEP_BB,specim$selectedVEP_stereo)))
    return(specim$selectedVEP)
  })
  
  #delete steps
  observeEvent(input$del_VEPs,{
    todelete <- selectedVEP()
    if(length(todelete)){
      specim$specim <- specim$specim[-todelete,]
      specim$selectedVEP <- NULL
      specim$selectedVEP_t <- NULL
      specim$selectedVEP_BW <- NULL
      specim$selectedVEP_BB <- NULL
      specim$selectedVEP_stereo <- NULL
    }
  })
  
  #define size of plot, or equal area is too big
  size_plot <- reactive({
    if(input$Zijd_Stereo_shift==1 || input$Zijd_Stereo_shift==2){size <- 800}
    if(input$Zijd_Stereo_shift==3){size <- 600}
    return(size)
  })
  
  # display a modal dialog with a header, textinput and action buttons
  observeEvent(input$Zijd_detail, {
    req(sample_list())
    req(specimens())
    #defined some pre-compiled parameters
    if(input$Zijd_f_type==2 || input$Zijd_f_type==7){
      normtext <- "This window is pre-compiled for LASA file type. NRM is in A/m assuming standard 11.15 cm^3 samples"
      Svolume <- 11.15
      StepUnit <- "°C"
      Normalization <- 2
    }else if(input$Zijd_f_type==3){
      normtext <- "This window is pre-compiled for Bremen (.cor) file. Data are in A/m assuming 8 cm^3 volume"
      Svolume <- 8.0
      StepUnit <- "mT"
      Normalization <- 2
    }else{
      normtext <- "If data are already normalized, select Normalization = No. Volume and Mass are automatically converted in m^3 and kg."
      Svolume <- 0
      StepUnit <- "mT"
      Normalization <- 1
    }
    
    showModal(jqui_draggable(modalDialog(
      size = "l",
      tags$h2('Enter NRM and demagnetization unit'),
      tags$h4(normtext),
      br(),
      fluidRow(
        column(4,textInput(inputId = "textunit",label = "NRM unit",value = "A/m")),
        column(4,numericInput(inputId = "NRMvolume",label = "Volume (cm^3)",value = Svolume)),
        column(4,numericInput(inputId = "NRMmass",label = "Mass (g)",value = 0)),
      ),
      fluidRow(
        column(4,selectInput(inputId = "NRMnormYN",label = "Normalization",choices = list("No"=1,"By volume"=2,"By mass"=3),selected = Normalization)),
        column(4,textInput(inputId = "demagunit",label = "Demag. unit",value = StepUnit)),
        column(4,selectInput("VEPticks",label = "Ticks",
                             choices = list("x0.05"=1,"x0.1"=2,"x0.25"=3,"x0.5"=4,"x1.0"=5,"No ticks"=6),selected = 4))
      ),
      easyClose = TRUE,
      footer=tagList(
        modalButton('close')
      )
    ),options = list(cancel = ".shiny-input-container")))
  })
  
  # display a modal dialog with a header, textinput and action buttons
  observeEvent(input$Zijd_detail2, {
    req(sample_list())
    req(specimens())
    showModal(jqui_draggable(modalDialog(
      size = "m",
      tags$h2('Enter tags details'),
      fluidRow(
        column(4,selectInput(inputId = "labelaxis",label = "Labels plane",
                             choices = list("Vertical"=1,"Horizontal"=2, "No labels"=3),selected = 1)),
        column(4,selectInput(inputId = "labelpos",label = "Labels position",
                             choices = list("Below"=1,"Left"=2,"Above"=3,"Right"=4),selected = 1)),
        column(4,selectInput(inputId = "labelspace",label = "Labels spacing",
                             choices = list("Every 1"=1,"Every 2"=2, "Every 3"=3,"Every 4"=4),selected = 3))
      ),
      easyClose = TRUE,
      footer=tagList(
        modalButton('close')
      )
    ),options = list(cancel = ".shiny-input-container")))
  })
  
  
  #send Vector end point or equal area fig to UI
  output$zijderveld <- renderPlot({
    #does not send fig if file is not selected
    req(isolated_specimen())
    coord <- input$VEPcoordinates
    
    #plot Zijderveld
    if(input$Zijd_Stereo_shift==1 || input$Zijd_Stereo_shift==2){
      #add ticks and unit only if details window is opened
      if(!is.null(input$VEPticks)){
        if(input$VEPticks==1){d_tick=0.05}
        if(input$VEPticks==2){d_tick=0.1}
        if(input$VEPticks==3){d_tick=0.25}
        if(input$VEPticks==4){d_tick=0.5}
        if(input$VEPticks==5){d_tick=1.0}
        ticks <- TRUE
        if(input$VEPticks==6){ticks=FALSE}
      }else{
        ticks <- TRUE
        d_tick <- 0.5
      }
      
      #save value with no order of magnitude and plot Zijderveld
      Zijdervel_res <- zijderveld(specim = specim$specim,
                                  selected_steps = selectedVEP(),
                                  coordinates = input$VEPcoordinates,
                                  orient = input$Zijd_Stereo_shift,d_tick = d_tick,ticks = ticks)
      
      #save data without OM
      specim$specim_no_OM <- Zijdervel_res[[1]]
      #save order of magnitude
      specim$OM <- Zijdervel_res[[2]]
      
      #save coordinates as number otherwise is a character and uses it for interpolation line
      #coordinates <- as.numeric(input$VEPcoordinates)
      
      #add interpolation lines
      if(length(specim$DiR_da)||length(specim$DiR_df)||length(specim$DiR_doi) || length(specim$DiR_C)){
        
        #run interpolating line function
        interpol_line(anchor = input$anchor,Zijd_shift = input$Zijd_Stereo_shift,coordinates=as.numeric(input$VEPcoordinates))
      }
    }
    #work on equal area plot
    else if(input$Zijd_Stereo_shift==3){
      
      #run function
      plot_equal_area(VEP_dat = specim$specim,VEP_coord = input$VEPcoordinates)
    }
    
    #save plot for export figure
    specim$savedplot <- recordPlot()
  }, height = reactive({size_plot()}))
  
  #creates reactive file for saving steps of PCA
  specim$saved_steps <- data.frame(matrix(ncol=1,nrow=0))
  
  # creates reactive file for probabilistic PCA, calculated always to be paste in the result file
  PPCA_prob <- reactiveValues(pHa=NULL)
  
  #calculate PCA or fisher and save steps selection            
  observeEvent(input$runVEPstat,{
    req(specim$specim)
    req(length(selectedVEP())>1)
    
    #calculate constrained directions senus Heslop+Roberts 2016 PPCA to paste in the result line. Uses tc data (coordinates are not-relevant)
    PPCA_results <- PmagDiR::PPCA_HR16(VEPs = specim$specim[selectedVEP(),9:11])
    PPCA_prob$pHa <- PPCA_results[[4]]
    PPCA_prob$pHc <- PPCA_results[[5]]
    
    DiR <- NULL
    c <- input$anchor
    if(c==1 || c==2 || c==3 || c==5 || c==6){
      #calculate PCA-derived direction and MAD from demagnetization steps
      #VEPs is expressed in Cartesian coordinates x,y,z
      run_PCA <- function(VEPs,anchor) {
        data <- VEPs
        colnames(data) <- c("x", "y","z")
        
        #averaged Cartesian coordinates
        x_av <- mean(data$x)
        y_av <- mean(data$y)
        z_av <- mean(data$z)
        #copy coordinates for anchored directions or great circle
        if(anchor==1) {
          #calculate coordinates with new center of mass for PCA
          data$xn <- data$x-x_av
          data$yn <- data$y-y_av
          data$zn <- data$z-z_av
        }
        else if (anchor==2){
          data$xn <- data$x
          data$yn <- data$y
          data$zn <- data$z
        }
        else if(anchor==3) {
          #includes origin and calculate new center of mass
          newrow <- c(0,0,0)
          data <- rbind(data,newrow)
          data$xn <- data$x-x_av
          data$yn <- data$y-y_av
          data$zn <- data$z-z_av
        }
        else if (anchor==5){
          #if great circle, data must be transformed in unit vectors as suggested by MF ME 1988 (EPSL87)
          #I use the PmagDiR::c2s and s2c funtions, made for dec inc, and eliminating vector length
          data_spherical <- PmagDiR::c2s(data)
          data <- PmagDiR::s2c(data_spherical)
          data$xn <- data$x
          data$yn <- data$y
          data$zn <- data$z
        }
        #elements of the distribution matrix
        T_elements <- c(sum((data$xn)*(data$xn)),sum(data$xn*data$yn),sum(data$xn*data$zn),
                        sum(data$yn*data$xn),sum(data$yn*data$yn),sum(data$yn*data$zn),
                        sum(data$zn*data$xn),sum(data$zn*data$yn),sum(data$zn*data$zn))
        
        Tm <- matrix(T_elements,3, 3)
        T_e <- eigen(Tm)
        T_vec <- T_e$vectors
        T_val <- T_e$value
        
        #interpolate line through points
        if(anchor==1 || anchor== 2 || anchor==3){
          #calculate dec inc of max variance
          Vdec <- (PmagDiR::r2d(atan2(T_vec[2,1],T_vec[1,1])))%%360
          Vinc <- PmagDiR::r2d(asin(T_vec[3,1]/(sqrt((T_vec[1,1]^2)+(T_vec[2,1]^2)+(T_vec[3,1]^2)))))
          
          #flipping V1 module, if directions goes opposite to vector tip
          tip <- c(data[1,1]-data[nrow(data),1],data[1,2]-data[nrow(data),2],data[1,3]-data[nrow(data),3])
          tipdec <- (PmagDiR::r2d(atan2(tip[2],tip[1])))%%360
          tipinc <- PmagDiR::r2d(asin(tip[3]/(sqrt((tip[1]^2)+(tip[2]^2)+(tip[3]^2)))))
          deltadec_tip_V1<- abs(tipdec-Vdec)
          dist_tip_V1 <- PmagDiR::r2d(acos((sin(PmagDiR::d2r(tipinc))*sin(PmagDiR::d2r(Vinc)))+
                                             (cos(PmagDiR::d2r(tipinc))*cos(PmagDiR::d2r(Vinc))*cos(PmagDiR::d2r(deltadec_tip_V1)))))
          if(dist_tip_V1>90){
            Vdec <- (Vdec+180)%%360
            Vinc <- -Vinc
          }
          #calculate max ang dev of line
          MAD <- PmagDiR::r2d(atan(sqrt(((T_val[2])+(T_val[3]))/T_val[1])))
          
          #calculate x y z coordinates of V1
          V1_x <- cos(PmagDiR::d2r(Vdec))*cos(PmagDiR::d2r(Vinc))
          #next because zijderveld y axis is down pointing
          V1_y <- (sin(PmagDiR::d2r(Vdec))*cos(PmagDiR::d2r(Vinc)))
          V1_z <- (sin(PmagDiR::d2r(Vinc)))
          
          #calculate inclination of interpolating lines
          m_xy <- V1_y/V1_x
          m_xz <- V1_z/V1_x
          m_yz <- V1_z/V1_y
          
        }
        if(anchor==6){
          #PPCA is calculated always at the top of the function, for having probablistic values, always pasted in the result line
          Vdec <- PPCA_results[[1]][3,1]
          Vinc <- PPCA_results[[1]][3,2]
          MAD <- PPCA_results[[1]][3,3]
          #calculate x y z coordinates of V1
          V1_x <- cos(PmagDiR::d2r(Vdec))*cos(PmagDiR::d2r(Vinc))
          #next because zijderveld y axis is down pointing
          V1_y <- (sin(PmagDiR::d2r(Vdec))*cos(PmagDiR::d2r(Vinc)))
          V1_z <- (sin(PmagDiR::d2r(Vinc)))
          
          #calculate inclination of interpolating lines
          m_xy <- V1_y/V1_x
          m_xz <- V1_z/V1_x
          m_yz <- V1_z/V1_y
        }
        if(anchor==5){
          #calculate dec inc of min variance that is the pole of the plane or circle
          Vdec <- (PmagDiR::r2d(atan2(T_vec[2,3],T_vec[1,3])))%%360
          Vinc <- PmagDiR::r2d(asin(T_vec[3,3]/(sqrt((T_vec[1,3]^2)+(T_vec[2,3]^2)+(T_vec[3,3]^2)))))
          #flip pole if negative
          if(Vinc<0){
            Vdec <- (Vdec+180)%%360
            Vinc <- abs(Vinc)
          }
          MAD <- PmagDiR::r2d(atan(sqrt((T_val[3]/T_val[2])+(T_val[3]/T_val[1]))))
        }
        
        #number of data points
        N <- nrow(data)
        #create result file
        dirs <- cbind(Vdec,Vinc,MAD,N)
        colnames(dirs) <- c("Dec", "Inc","MAD","N")
        
        #add cartesian coordinates of V1 for calculating interpolating lines, only if line, and center of mass in original OM
        if(anchor==1 || anchor==2 || anchor==3 || anchor==6){
          dirs <- cbind(dirs,x_av,y_av,z_av,m_xy,m_xz,m_yz)
        }
        return(dirs)
      }
      
      #create file with selected VEPs from GUI
      VEPs_for_PCA_sp <- specim$specim[selectedVEP(),3:5]
      VEPs_for_PCA_geo <- specim$specim[selectedVEP(),6:8]
      VEPs_for_PCA_tc <- specim$specim[selectedVEP(),9:11]
      
      
      #performs PCA
      DiR_sp <- run_PCA(VEPs = VEPs_for_PCA_sp, anchor = input$anchor)
      DiR_geo <- run_PCA(VEPs = VEPs_for_PCA_geo, anchor = input$anchor)
      DiR_tc <- run_PCA(VEPs = VEPs_for_PCA_tc, anchor = input$anchor)
      
    } else if(c==4){
      dat_sp <- specim$specim[selectedVEP(),3:5]
      dat_geo <- specim$specim[selectedVEP(),6:8]
      dat_tc <- specim$specim[selectedVEP(),9:11]
      
      dat_sp$dec <- (PmagDiR::r2d(atan2(dat_sp[,2],dat_sp[,1])))%%360
      dat_sp$inc <- PmagDiR::r2d(asin(dat_sp[,3]/(sqrt((dat_sp[,1]^2)+(dat_sp[,2]^2)+(dat_sp[,3]^2)))))
      
      dat_geo$dec <- (PmagDiR::r2d(atan2(dat_geo[,2],dat_geo[,1])))%%360
      dat_geo$inc <- PmagDiR::r2d(asin(dat_geo[,3]/(sqrt((dat_geo[,1]^2)+(dat_geo[,2]^2)+(dat_geo[,3]^2)))))
      
      dat_tc$dec <- (PmagDiR::r2d(atan2(dat_tc[,2],dat_tc[,1])))%%360
      dat_tc$inc <- PmagDiR::r2d(asin(dat_tc[,3]/(sqrt((dat_tc[,1]^2)+(dat_tc[,2]^2)+(dat_tc[,3]^2)))))
      
      DiR_sp <- PmagDiR::fisher(dat_sp[,4:5],export = F)
      DiR_geo <- PmagDiR::fisher(dat_geo[,4:5],export = F)
      DiR_tc <- PmagDiR::fisher(dat_tc[,4:5],export = F)
    }                                      
    #compile different files for Zijderveld graph
    if(c==1){
      #copy result on reactive file and empty others
      specim$DiR_df <- rbind(DiR_sp,DiR_geo,DiR_tc)
      specim$DiR_C <- NULL
      specim$DiR_da <- NULL
      specim$DiR_doi <- NULL
      specim$DiR_f <- NULL
      specim$DiR_p <- NULL
    }
    else if(c==2){
      specim$DiR_df <- NULL
      specim$DiR_da <- rbind(DiR_sp,DiR_geo,DiR_tc)
      specim$DiR_doi <- NULL
      specim$DiR_C <- NULL
      specim$DiR_f <- NULL
      specim$DiR_p <- NULL
    }
    else if(c==3){
      specim$DiR_df <- NULL
      specim$DiR_da <- NULL
      specim$DiR_doi <- rbind(DiR_sp,DiR_geo,DiR_tc)
      specim$DiR_f <- NULL
      specim$DiR_C <- NULL
      specim$DiR_p <- NULL
    }
    else if(c==4){
      specim$DiR_f <- rbind(DiR_sp,DiR_geo,DiR_tc)
      specim$DiR_df <- NULL
      specim$DiR_C <- NULL
      specim$DiR_da <- NULL
      specim$DiR_doi <- NULL
      specim$DiR_p <- NULL
    }
    else if(c==5){
      specim$DiR_p <- rbind(DiR_sp,DiR_geo,DiR_tc)
      specim$DiR_df <- NULL
      specim$DiR_C <- NULL
      specim$DiR_da <- NULL
      specim$DiR_doi <- NULL
      specim$DiR_f <- NULL
    }
    else if(c==6){
      specim$DiR_C <- rbind(DiR_sp,DiR_geo,DiR_tc)
      specim$DiR_p <- NULL
      specim$DiR_df <- NULL
      specim$DiR_da <- NULL
      specim$DiR_doi <- NULL
      specim$DiR_f <- NULL
    }
    
    #save demagnetization steps in a temporary file, used if save is requested
    specim$saved_steps_temp <- paste(specim$specim[selectedVEP(),2],collapse = ",")
  })
  
  #window with test for Probabilisitc PCA Anchor or not to Anchor Heslop+Roberts 2016
  observeEvent(input$PPCA, {
    req(length(selectedVEP())>1)
    if(input$VEPcoordinates==1){VEPs <- specim$specim[selectedVEP(),3:5]}
    if(input$VEPcoordinates==2){VEPs <- specim$specim[selectedVEP(),6:8]}
    if(input$VEPcoordinates==3){VEPs <- specim$specim[selectedVEP(),9:11]}
    PPCA_results <- PmagDiR::PPCA_HR16(VEPs = VEPs)
    output$PPCA_result_table <- renderTable({
      #remove N, not useful
      PPCA_results$All_directions[,-4]
    },rownames = T)
    output$verdict_a <- renderText({
      PPCA_results$verdict_a
    })
    output$verdict_c <- renderText({
      PPCA_results$verdict_c
    })
    
    # display a modal dialog with a header, textinput and action buttons
    showModal(jqui_draggable(modalDialog(
      size= "m",
      tags$h2('Probabilistic PCA'),
      tags$h4("Models:"),
      fluidRow(tableOutput("PPCA_result_table")),
      tags$h4("Result from probabilistic analysis:"),
      fluidRow(column(12,textOutput("verdict_a"))),
      fluidRow(column(12,textOutput("verdict_c"))),
      br(),
      tags$h5("*Please cite: "), tags$a(href=" https://doi.org/10.1002/2016JB013387", 
                                        "Heslop & Roberts (2016), JGR: Solid Earth, 121, 7742–7753", target="_blank"),
      easyClose = TRUE,
      footer=tagList(
        modalButton('close')
      )
    ),options = list(cancel = ".shiny-input-container")))
  })
  
  # creates text with results
  PCA_result <- reactive({
    c <- input$anchor
    coordinates <- as.numeric(input$VEPcoordinates)
    #assign same name to different files
    if(c==1) {if(length(specim$DiR_df)){specim$DiR_d <- specim$DiR_df}else{specim$DiR_d <- NULL}}
    if(c==2) {if(length(specim$DiR_da)) {specim$DiR_d <- specim$DiR_da}else{specim$DiR_d <- NULL}}
    if(c==3) {if(length(specim$DiR_doi)) {specim$DiR_d <- specim$DiR_doi}else{specim$DiR_d <- NULL}}
    if(c==4) {if(length(specim$DiR_f)) {specim$DiR_d <- specim$DiR_f}else{specim$DiR_d <- NULL}}
    if(c==5) {if(length(specim$DiR_p)) {specim$DiR_d <- specim$DiR_p}else{specim$DiR_p <- NULL}}
    if(c==6) {if(length(specim$DiR_C)) {specim$DiR_d <- specim$DiR_C}else{specim$DiR_p <- NULL}}
    
    if(length(specim$DiR_d)){
      if(c==1 || c==2 || c==3 || c==5 || c==6){
        PCA_text <- paste("N= ",round(specim$DiR_d[coordinates,4],digits = 0),", ",
                          "Decl.= ",round(specim$DiR_d[coordinates,1],digits = 2),", ",
                          "Incl.= ",round(specim$DiR_d[coordinates,2],digits = 2),", ",
                          "M.A.D.= ", round(specim$DiR_d[coordinates,3],digits = 2),sep = "")
      }
      else if(c==4){
        PCA_text <- paste("N= ",round(specim$DiR_d[coordinates,4],digits = 0),", ",
                          "Decl.= ",round(specim$DiR_d[coordinates,1],digits = 2),", ",
                          "Incl.= ",round(specim$DiR_d[coordinates,2],digits = 2),", " ,
                          "a95= ",round(specim$DiR_d[coordinates,3],digits = 2),", ",
                          "k=  ",round(specim$DiR_d[coordinates,6],digits = 2), sep="")
      }
    }
    else{PCA_text <- NULL}
    return(PCA_text)
  })
  
  #send results to figure
  output$PCA_result <- renderText({
    req(PCA_result())
    PCA_result()
  })
  
  #creates reactive result file
  specim$PCA_result_file <- data.frame(matrix(ncol=16,nrow = 0))
  
  #save results in reactive file
  observeEvent(input$save_PCA,{
    #DiR_d contains the results no matter the interpolation
    req(specim$DiR_d)
    c <- input$anchor
    temp_result <- data.frame(matrix(ncol=16,nrow = 1))
    colnames(temp_result) <- c("Sample","N","S_Dec","S_Inc","G_Dec","G_Inc","B_Dec","B_Inc","MAD","a95","k","Type","Comp","Steps","p(Ha|D)","p(Hc|D)")
    temp_result[1,1] <- unique(specim$specim[,1])
    temp_result[1,2] <- round(specim$DiR_d[1,4],digits = 0)
    temp_result[1,3] <- round(specim$DiR_d[1,1],digits = 2)
    temp_result[1,4] <- round(specim$DiR_d[1,2],digits = 2)
    temp_result[1,5] <- round(specim$DiR_d[2,1],digits = 2)
    temp_result[1,6] <- round(specim$DiR_d[2,2],digits = 2)
    temp_result[1,7] <- round(specim$DiR_d[3,1],digits = 2)
    temp_result[1,8] <- round(specim$DiR_d[3,2],digits = 2)
    temp_result[1,9] <- ifelse(c==4,"",round(specim$DiR_d[1,3],digits = 2))
    temp_result[1,10] <- ifelse(c==4,round(specim$DiR_d[1,3],digits = 2),"")
    temp_result[1,11] <- ifelse(c==4,round(specim$DiR_d[1,6],digits = 2),"")
    if(c==1) {Type <- "PCA_F"}
    if(c==2) {Type <- "PCA_A"}
    if(c==3) {Type <- "PCA_OI"}
    if(c==4) {Type <- "Fisher"}
    if(c==5) {Type <- "GC"}
    if(c==6) {Type <- "PPCA_C"}
    temp_result[1,12] <- Type
    temp_result[1,13] <- input$comp_name
    #add steps of all interpreted samples
    temp_result[1,14] <- specim$saved_steps_temp
    temp_result[1,15] <- PPCA_prob$pHa
    temp_result[1,16] <- PPCA_prob$pHc
    specim$PCA_result_file <- rbind(specim$PCA_result_file,temp_result)
  })
  
  #save interpretation and steps file as .csv text
  output$export_PCA <- downloadHandler(
    filename = function() {
      paste("Interpolated_directions_", Sys.Date(), ".csv", sep="")
    },
    content = function(file) {
      write.csv(specim$PCA_result_file, file,row.names = FALSE)
    }
  )
  
  #reactive name of sample for saving figure
  sampleName <- reactive({
    if(input$Zijd_Stereo_shift==1 || input$Zijd_Stereo_shift==2){samplename <- paste(specim$specim[1,1],"_VEP_")}
    else if(input$Zijd_Stereo_shift==3){samplename <- paste(specim$specim[1,1],"_EA_")}
    return(samplename)
  })
  
  
  #create text file with unit
  zijd_unit <- reactive({
    #if sample is not yet selected, does not generate unit
    req(isolated_specimen())    
    #if window with details is not opened, does not plot ticks
    if(!is.null(input$textunit)){
      if(input$Zijd_Stereo_shift==3 || input$VEPticks==6 || length(isolated_specimen())==0){
        z_unit <- NULL
      }else if(input$Zijd_Stereo_shift==1 || input$Zijd_Stereo_shift==2){
        if(input$VEPticks==1) fract <- "0.05 E"
        if(input$VEPticks==2) fract <- "0.1 E"
        if(input$VEPticks==3) fract <- "0.25 E"
        if(input$VEPticks==4) fract <- "0.5 E"
        if(input$VEPticks==5) fract <- "1 E"
        z_unit <- paste("Unit: ",fract,specim$OM," ",input$textunit,sep = "")
      }
    }else{z_unit <- "Please define units in 'UNITS' & 'TAGS'"}
    return(z_unit)
  })
  
  #print unit of Vector end points axes
  output$Zijd_Unit <- renderText({
    req(zijd_unit())
    zijd_unit()
  })
  
  ####### ALL SAVED SAMPLES PAGE
  #import external file and attche it to internal result file if exists
  observeEvent(input$import_PCA,{
    specim$tab_result_ext <- read.csv(file = input$import_PCA$datapath)
    if(ncol(specim$tab_result_ext)==14){
      temp <- specim$tab_result_ext
      temp$pa <- rep(0)
      temp$pd <- rep(0)
      showModal(jqui_draggable(modalDialog(
        size="m",
        tags$h3("Warning"),
        tags$h5("Bayesian PPCA values p(Ha|D) and p(Hc|D) missing from file, probably exported from an old version of Magnetic-A. Added columns with 0"),
        easyClose = TRUE,
        footer=tagList(
          modalButton('close')
        )
      ),options = list(cancel = ".shiny-input-container")))
      specim$tab_result_ext <- temp
    }
    colnames(specim$tab_result_ext) <- c("Sample","N","S_Dec","S_Inc",
                                         "G_Dec","G_Inc","B_Dec","B_Inc",
                                         "MAD","a95","k","Type","Comp",
                                         "Steps","p(Ha|D)","p(Hc|D)")
    
    if(!is.null(specim$PCA_result_file)) {
      specim$PCA_result_file <- rbind(specim$PCA_result_file,specim$tab_result_ext)
    }
  })
  
  
  TAB <- reactive({
    if(nrow(specim$PCA_result_file)==0){return(NULL)}
    else{
      result_table <- specim$PCA_result_file[,-c(2,3,4,11,14,15,16)]
      return(result_table)
    }
  })
  
  #turn table in a interactive table
  output$saved_interpol <- DT::renderDataTable({
    req(TAB())
    #next allows to modify on ly one value of the table, which is the component name
    datatable(TAB(), rownames=F, extension= 'Scroller',
              editable = list(target="cell", disable= list(columns=c(0,1,2,3,4,5,6,7))),
              options=list(dom='t',sort=T, paging=T,
                           deferRender = TRUE,
                           scrollY = 600,
                           scroller = TRUE,
                           "drawCallback" = JS("function(settings) {var table = this.api();table.rows().nodes().to$().css('font-size', '12px');}"),
                           initComplete = JS(
                             "function(settings, json) {",
                             "$(this.api().table().header()).css({'font-size': '85%'});",
                             "}")),
              class=list(stripe=FALSE))%>%
      formatStyle(
        columns = c("Sample","G_Dec","G_Inc","B_Dec","B_Inc",
                    "MAD","a95","Type","Comp"), 
        color = 'black'   # Colore del testo per le celle editabili
      )
  })
  
  #modify original table by changing component name
  observeEvent(input$saved_interpol_cell_edit, {
    info <- input$saved_interpol_cell_edit
    #current values in table
    modified_data <- specim$PCA_result_file  
    #update values (plus one otherwise in paste to the wrong column. Do not ask me why, it's like a chopper flying without spinning blades)
    modified_data[info$row, (info$col+5)] <- info$value 
    #update table
    specim$PCA_result_file <- modified_data
  })
  
  #delete selected directions permanently
  observeEvent(input$del_interpol,{
    to_delete <- input$saved_interpol_rows_selected
    if(length(input$saved_interpol_rows_selected)>0){
      #creates backup
      specim$PCA_result_file_BU <- specim$PCA_result_file
      specim$PCA_result_file <- specim$PCA_result_file[-to_delete,]
    }
  })
  
  #undo delete restoring backup
  observeEvent(input$undel_interpol,{
    if(nrow(specim$PCA_result_file_BU)>0){
      specim$PCA_result_file <- specim$PCA_result_file_BU
    }
  })
  
  #export only selected directions
  output$export_interpol <- downloadHandler(
    filename = function() {
      paste(input$sel_interpol_name,"_", Sys.Date(), ".csv", sep="")
    },
    content = function(file) {
      sel <- input$saved_interpol_rows_selected
      data <- specim$PCA_result_file[sel,]
      write.csv(data, file,row.names = FALSE)
    }
  )
  
  #export equal area of all plotted directions
  output$export_AllDirs_stereo <- downloadHandler(
    filename = function() {
      paste(input$sel_interpol_name,"_", Sys.Date(), ".pdf", sep="")
    },
    content = function(file) {
      pdf(file, onefile = TRUE,width = 7,height = 7)
      replayPlot(specim$all_dirs_equarea_plot)
      dev.off()
    }
  )
  
  #combines dirs and great circles
  observeEvent(input$comb_DI_GC,{
    req(TAB())
    dat <- input$saved_interpol_rows_selected
    dirs_full <- specim$PCA_result_file
    dirs_selected <- dirs_full[dat,]
    if(length(dat)>0){
      dirs_temp <- dirs_selected[dirs_selected$Type!="GC",]
      if(input$EAcoordinates==1){dirs <- dirs_temp[,5:6]}
      else if(input$EAcoordinates==2){dirs <- dirs_temp[,7:8]}
      circles_temp <- dirs_selected[dirs_selected$Type=="GC",]
      if(input$EAcoordinates==1){circles <- circles_temp[,5:6]}
      else if(input$EAcoordinates==2){circles <- circles_temp[,7:8]}
    }
    if(nrow(circles)>0){
      #create empty directions file if no directions are selected
      if(nrow(dirs)>0){
        DI <- dirs
      }else{
        DI <- data.frame(matrix(ncol = 2,nrow = 0))
        colnames(DI) <- c("dec","inc")
      }
      #save some details to fill table
      specim$GC_firstCols <- data.frame(circles_temp[,1:2])
      specim$GC_lastCols <- data.frame(circles_temp[,9:14])
      
      #apply function from PmagDiR
      specim$GC_directions <- PmagDiR::comb_GC_dirs(dirs = DI,poles = circles)
    }
  })
  
  #erase file if coordinate are changed
  observeEvent(input$EAcoordinates,{specim$GC_directions <- NULL})
  
  #erase GC file if asked
  observeEvent(input$GC_erase,{specim$GC_directions <- NULL})
  
  #save file to list
  observeEvent(input$save_GC,{
    req(specim$GC_directions)
    #create empty tab
    temp_table <- data.frame(matrix(ncol=16,nrow = nrow(specim$GC_directions)))
    colnames(temp_table) <- c("Sample","N","S_Dec","S_Inc","G_Dec","G_Inc","B_Dec","B_Inc","MAD","a95","k","Type","Comp","Steps","p(Ha|D)","p(Hc|D)")
    #paste details copied above
    temp_table[1:nrow(temp_table),1:2] <- specim$GC_firstCols
    temp_table[1:nrow(temp_table),9:16] <- specim$GC_lastCols
    if(input$EAcoordinates==2){
      temp_table[1:nrow(temp_table),7:8] <- round(specim$GC_directions, digits = 2)
    } else if(input$EAcoordinates==1){
      temp_table[1:nrow(temp_table),5:6] <- round(specim$GC_directions, digits = 2)
    }
    temp_table[,12] <- rep("Dir")
    specim$PCA_result_file <- rbind(specim$PCA_result_file,temp_table)
    specim$GC_directions <- NULL
  })
  
  #create reactive file for display details on dragged dirs
  dirsEA <- reactiveValues(df=NULL)
  
  #plot directions and circles taking data from table of results
  output$saved_interpol_EA <- renderPlot({
    req(TAB())
    dat <- input$saved_interpol_rows_selected
    dirs_full <- TAB()
    dirs_selected <- dirs_full[dat,]
    
    #PREPARE SELECTING POINTS FOR PLOTTING INVISIBLE POINTS FOR DRAGGING IN EQUAL AREA WINDOW (BELOW)
    #functions converting inc(x) and dec(y) into equal area
    a2cx <- function(x,y) {sqrt(2)*sin((PmagDiR::d2r(90-x))/2)*sin(PmagDiR::d2r(y))}
    a2cy <- function(x,y) {sqrt(2)*sin((PmagDiR::d2r(90-x))/2)*cos(PmagDiR::d2r(y))}
    #check coordinates
    if(input$EAcoordinates==1){
      selection_2_convert <- dirs_selected[,2:3]}
    else if(input$EAcoordinates==2){
      selection_2_convert <- dirs_selected[,4:5]}
    #add invisible points to select with drag
    dirs_selected$x <- a2cx(abs(selection_2_convert[,2]),selection_2_convert[,1])
    dirs_selected$y <- a2cy(abs(selection_2_convert[,2]),selection_2_convert[,1])
    
    #cut great circles temporarily
    dirs_temp <- dirs_selected[dirs_selected$Type!="GC",]
    if(input$EAcoordinates==1){dirs <- dirs_temp[,2:3]}
    else if(input$EAcoordinates==2){dirs <- dirs_temp[,4:5]}
    
    #plots directions
    PmagDiR::plot_DI(dirs)
    #add great circles
    circles_temp <- dirs_selected[dirs_selected$Type=="GC",]
    if(nrow(circles_temp>=1)){
      if(input$EAcoordinates==1){circles <- circles_temp[,2:3]}
      else if(input$EAcoordinates==2){circles <- circles_temp[,4:5]}
      #plot plane is designed for a single circle so it has to be reiterated
      for(i in 1:nrow(circles)){
        PmagDiR::plot_plane(circles[i,1],circles[i,2],on_plot = TRUE,col_cU = "blue",col_cD = "blue",symbol = "d",col_d = "yellow")
      }
    }
    if(is.null(specim$GC_directions)==FALSE){PmagDiR::plot_DI(DI = specim$GC_directions,on_plot = T,
                                                              col_d = "red",col_u = "pink",symbol = "t")}
    
    #plot invisible points to select with drag
    points(x = dirs_selected$x,y=dirs_selected$y,col=NA)
    
    #select points
    df <- brushedPoints(dirs_selected,input$plot_click,xvar = "x",yvar = "y")
    if(length(df)){
      if(input$EAcoordinates==1){PmagDiR::plot_DI(df[,2:3],col_d = "red",col_u = "yellow",on_plot = T)}
      else if(input$EAcoordinates==2){PmagDiR::plot_DI(df[,4:5],col_d = "red",col_u = "yellow",on_plot = T)}
    }
    dirsEA$df <- df
    specim$all_dirs_equarea_plot <- recordPlot()
  },height = 700,width = 700)
  
  #show directions dragged in figure in an extra window
  observeEvent(input$showdiersEA,{
    showModal(jqui_draggable(modalDialog(
      size="l",
      title = "Directions selected on the equal area:",
      renderTable(dirsEA$df[,-c(10,11)]),
      easyClose = TRUE,
      footer=tagList(
        modalButton('close')
      )
    ),options = list(cancel = ".shiny-input-container")))
  })
  
  ####### END OF ALL SAVED SAMPLES PAGE
  
  ####### FIGURE WITH ALL DIAGRAM FROM A SINGLE SAMPLE FOR EXPORT AND PUBLICATION
  #plot Vector end points
  output$All_VEP_diagrams <- renderPlot({
    req(specim$specim)
    #add ticks and unit only if details window is opened
    if(!is.null(input$VEPticks)){
      if(input$VEPticks==1){d_tick=0.05}
      if(input$VEPticks==2){d_tick=0.1}
      if(input$VEPticks==3){d_tick=0.25}
      if(input$VEPticks==4){d_tick=0.5}
      if(input$VEPticks==5){d_tick=1.0}
      ticks <- TRUE
      if(input$VEPticks==6){ticks=FALSE}
    }else{ticks <- FALSE}
    par(fig=c(0,0.65,0,1))
    #save value with no order of magnitude and plot Zijderveld
    Zijdervel_res <- zijderveld(specim = specim$specim,
                                selected_steps = selectedVEP(),
                                coordinates = input$VEPcoordinates,
                                #if equal area is selected in first page, it forces N-right
                                orient = ifelse(input$Zijd_Stereo_shift==3,1,input$Zijd_Stereo_shift),
                                d_tick = d_tick,ticks = ticks)
    #save data without OM
    specim$specim_no_OM <- Zijdervel_res[[1]]
    #save order of magnitude
    specim$OM <- Zijdervel_res[[2]]
    
    #save coordinates as number otherwise is a character and uses it for interpolation line
    coordinates <- as.numeric(input$VEPcoordinates)
    #run interpolating line function, auto N-Up if equalarea is selected in panel 1
    interpol_line(anchor = input$anchor,Zijd_shift = ifelse(input$Zijd_Stereo_shift==3,1,input$Zijd_Stereo_shift),
                  coordinates=as.numeric(input$VEPcoordinates))
    
    par(fig=c(0.65,1,0.35,0.95),new=T)
    plot_equal_area(VEP_dat = specim$specim,VEP_coord = input$VEPcoordinates)
    
    par(fig=c(0.5,1,0.78,1),new=T)
    plot(NA,xlim=c(0,1),ylim=c(0,1),xaxt="n",yaxt="n",xlab="",ylab="", axes=F)
    
    #define type to put in text
    if(input$anchor==1) {fit <- "PCA_F"}
    if(input$anchor==2) {fit <- "PCA_A"}
    if(input$anchor==3) {fit <- "PCA_OI"}
    if(input$anchor==4) {fit <- "Fisher"}
    if(input$anchor==5) {fit <- "GC"}
    if(input$anchor==6) {fit <- "PPCA_C"}
    text(x=1,y=0.81, paste("Sample ",specim$specim[1,1],", NRM= ",
                           formatC(sqrt(specim$specim[1,3]^2+specim$specim[1,4]^2+specim$specim[1,5]^2),digits = 2)," ",
                           input$textunit,", Fit: ",fit,sep=""),pos=2)
    text(x = 1,y=0.5,PCA_result(),pos=2)
    text(x = 1,y=0.19,zijd_unit(),pos=2)
    
    par(fig=c(0.65,1,0,0.45),new=T)
    plot(x = specim$specim[,2],y = sqrt(specim$specim[,3]^2+specim$specim[,4]^2+specim$specim[,5]^2),
         type="o",pch=21,bg="black",cex=1.3,
         xlab=paste("Demangetization step (", input$demagunit,")",sep=""),
         ylab=paste("NRM (",input$textunit,")", sep=""))
    if(length(selectedVEP())){
      points(x = specim$specim[selectedVEP(),2],y = sqrt(specim$specim[selectedVEP(),3]^2+specim$specim[selectedVEP(),4]^2+specim$specim[selectedVEP(),5]^2),
             type="p",pch=21,bg="red",cex=1.6)
    }
    specim$all_diagrams <- recordPlot()
  }, height = 800,width = 1200)
  
  
  #download figure
  output$export_VEPs_figure <- downloadHandler(
    filename = function() {
      paste(sampleName(), Sys.Date(), ".pdf", sep="")
    },
    content = function(file) {
      pdf(file, onefile = TRUE, width = 15, height = 11)
      replayPlot(specim$all_diagrams)
      dev.off()
    })
  
  ############ Takes direction input file and fix it depending on commands
  
  #create reactive file for stat
  F_stat <- reactiveValues(result=NULL)
  
  #creates reactive value for checking if file is uploaded
  Dirs <- reactiveValues(Dirs = NULL)
  #check for uploaded file and save name
  observeEvent(input$file,{Dirs$Dirs <- "uploaded"})
  observeEvent(input$file,{
    file <- input$file
    Dirs$dirsFileName <- file$name
  })
  #read file if present, reset if requested
  input_file <- reactive({
    #take internal file if exists and selected
    if(input$filetype==5){
      sel <- input$saved_interpol_rows_selected
      dat <- specim$PCA_result_file[sel,]
      Dirs$dat <- dat
    }
    else if(input$filetype==6){
      dat <- PmagDiR::Ardo_Geo_PmagDiR
      Dirs$dat <- dat
    }else{
      if (is.null(Dirs$Dirs)) {
        return(NULL)
      } else if (Dirs$Dirs == 'uploaded') {
        if(input$filetype==8){
          Dirs$dat <- read.csv(file = input$file$datapath, skip = 3,header = F)
        }else{Dirs$dat <- read.csv(file = input$file$datapath)}
      }  
    }
  })
  
  #fix dirs coordinate depending on input, file ncol, different cutoff
  fix_DI <- function(input_file,file=input$filetype,
                     Slat=input$lat, Slong=input$long,
                     coord=input$coord, cutoff=input$cutoff,
                     VGP_fixed=input$VGP_fixed, MinInc=input$MinInc, MaxInc=input$MaxInc){
    DIRS <- input_file
    if(file==1){
      if(coord==1 || coord==2 || coord==3){
        DI <- DIRS
      }
    }
    if(file==2){
      if(coord==1 || coord==3){DI <- DIRS}
      if(coord==2){DI <- PmagDiR::bed_DI(DIRS)}
    }
    if(file==3){
      #save extra temporary file for using bed coordinates for cutoff but plot Geo coord
      DIBB <- DIRS
      if(coord==1 || coord==3){DI <- DIRS[,-c(3,4)]}
      if(coord==2){DI <- DIRS[,-c(1,2)]}
    }
    #MagneticA format
    if(file==4){                         
      #save extra temporary file for using bed coordinates for cutoff but plot Geo coord
      DIBB <- DIRS
      if(coord==1){DI <- DIRS[,5:6]}
      if(coord==2){DI <- DIRS[,7:8]}
      if(coord==3){DI <- DIRS[,3:4]}
    }
    #uses internal file with line selected in the "all saved directions" page
    if(file==5){                    #this also needs to be fixed for internal file
      req(specim$PCA_result_file)
      DIBB <- DIRS
      if(coord==1){DI <- DIRS[,5:6]}
      if(coord==2){DI <- DIRS[,7:8]}
      if(coord==3){DI <- DIRS[,3:4]}
    }
    #uses Ardo example file
    if(file==6){
      if(coord==1 || coord==3){DI <- DIRS}
      if(coord==2){DI <- PmagDiR::bed_DI(DIRS)}
    }
    if(file==7){
      DIBB <- DIRS
      if(coord==1){DI <- DIRS[,-c(1,2,5,6)]}
      if(coord==2){DI <- DIRS[,-c(1,2,3,4)]}
      if(coord==3){DI <- DIRS[,-c(3,4,5,6)]}
    }
    #reads file for Longyun
    if(file==8){
      DIBB <- DIRS
      if(coord==1){DI <- DIRS[,5:6]}
      if(coord==2){DI <- DIRS[,9:10]}
      #there is no sample coordinates in the file, takes geo
      if(coord==3){DI <- DIRS[,5:6]}
    }
    #apply cutoff & filters
    #file type of Ardo
    if((file==2 || file==6) && (coord==1 || coord==3)){geo=TRUE}else{geo=FALSE}
    #in case of dirsfile type 3 and geo coordinates takes bedding coordinate to use as filter after cutoff
    if(file==3 && coord==1){
      if(cutoff>=2 && cutoff<=5){
        DI <- DIRS[,3:4]
      }
    }
    #in case of dirsfile type webDiR and geo or specimen coordinates takes bedding coordinate to use as filter after cutoff
    if(file==4 || file==5){
      if(cutoff>=2 && cutoff<=5){
        if(coord==1 || coord==3){DI <- DIRS[,7:8]}
      }
    }
    if(file==7){
      if(cutoff>=2 && cutoff<=5){
        if(coord==1 || coord==3){DI <- DIRS[,5:6]}
      }
    }
    #Longyung file, operates only on TC coordintes for VGPs based cutoffs
    if(file==8){
      if(cutoff>=2 && cutoff<=5){
        if(coord==1 || coord==3){DI <- DIRS[,9:10]}
      }
    }
    
    if(cutoff==2){DI <- PmagDiR::cut_DI(DI = DI,lat=Slat,long = Slong,geo = geo,Shiny = T)}
    else if(cutoff==3){DI <- PmagDiR::cut_DI(DI = DI,lat=Slat,long = Slong,inc_f = F,geo = geo,Shiny = T)}
    else if(cutoff==4){DI <- PmagDiR::cut_DI(DI = DI,VD=F,cutoff = VGP_fixed ,lat=Slat,long = Slong,geo = geo,Shiny = T)}
    else if(cutoff==5){DI <- PmagDiR::cut_DI(DI = DI,VD=F,cutoff = VGP_fixed ,lat=Slat,long = Slong, inc_f=F,geo = geo,Shiny = T)}
    else if(cutoff==6){DI <- DI[DI[,2]>0,]}
    else if(cutoff==7){DI <- DI[DI[,2]<0,]}
    else if(cutoff==8){
      DI1 <- DI[DI[,2]<MinInc,]
      DI2 <- DI[DI[,2]>MaxInc,]
      DI <- rbind(DI1,DI2)
    }
    #in case of dirsfile type 2 and geo coordinates gives back geo coordinate dirs filtered by rownames after cutoff
    if(file==3 && coord==1){DI <- DIBB[rownames(DI),1:2]}
    #if internal or Magnetica restore proper coordinates after filtering tilt coorected
    if(file==4 || file==5){
      if(coord==1){DI <- DIBB[rownames(DI),5:6]}
      if(coord==3){DI <- DIBB[rownames(DI),3:4]}
    }
    #if file with Specimen cords restore proper coordinates after filtering tilt corrected
    if(file==7){
      if(coord==1){DI <- DIBB[rownames(DI),3:4]}
      if(coord==3){DI <- DIBB[rownames(DI),1:2]}
    }
    #if Longyun file with Specimen cords restore proper coordinates after filtering tilt corrected
    if(file==8){
      if(coord==1 || coord==3){DI <- DIBB[rownames(DI),5:6]}
    }
    if(input$mode==2){DI <- PmagDiR::common_DI(DI)}
    if(input$mode==3){DI <- PmagDiR::common_DI(DI,down = F)}
    if(input$mode==4){
      for(i in 1:nrow(DI)){
        if(DI[i,2]<0){
          DI[i,1] <- (DI[i,1]+180)%%360
          DI[i,2] <- abs(DI[i,2])
        }
      }
    }else if(input$mode==5){
      for(i in 1:nrow(DI)){
        if(DI[i,2]>=0){
          DI[i,1] <- (DI[i,1]+180)%%360
          DI[i,2] <- -(DI[i,2])
        }
      }
    }
    if(input$apply_known_f==2) {DI <- PmagDiR::unflat_DI(DI,f = input$known_f,export = F)}
    DI <- na.omit(DI)
    return(DI)
  }
  
  #warning for flattening activated in other pages
  flatwarn <- reactive({ifelse(input$apply_known_f==2,"WARNING: Apply f in directions page is active","")})
  output$flatwarning <- renderText({flatwarn()})
  output$flatwarning2 <- renderText({flatwarn()})
  output$flatwarning3 <- renderText({flatwarn()})
  output$flatwarning4 <- renderText({flatwarn()})
  
  #warning for not tilt coordinates in different pages
  geowarn <- reactive({ifelse(input$coord!=2,"WARNING: data are not in Tilt Corrected coordinates","")})
  output$geowarning <- renderText({geowarn()})
  output$geowarning2 <- renderText({geowarn()})
  output$geowarning3 <- renderText({geowarn()})
  output$geowarning4 <- renderText({geowarn()})
  
  
  ############ DIRECTIONS DISPILAY & AVERAGE##########
  
  ####### DIRECTIONS DISPILAY & AVERAGE SUBPAGE
  #modified fisher_plot function
  fisher_plot_S <- function(DI, plot=TRUE, col_d="red",col_u="white",col_l="black",symbol="c",auto_split=TRUE) {
    data <- DI
    data <- na.omit(data)
    data <- data[,1:2]
    colnames(data) <- c("dec", "inc")
    if(auto_split==TRUE){
      #directions in Cartesian coordinates
      data$x <- NA
      data$y <- NA
      data$z <- NA
      data[,3:5] <- PmagDiR::s2c(data[,1:2])
      #averaged Cartesian coordinates
      x_av <- mean(data$x)
      y_av <- mean(data$y)
      z_av <- mean(data$z)
      #elements of the distribution matrix
      T_elements <- c(sum((data$x)*(data$x)),sum(data$x*data$y),sum(data$x*data$z),
                      sum(data$y*data$x),sum(data$y*data$y),sum(data$y*data$z),
                      sum(data$z*data$x),sum(data$z*data$y),sum(data$z*data$z))
      #distribution matrix
      T <- matrix(T_elements,nrow=3, byrow=TRUE)
      #calculate and copy eigenvalues and vectors
      T_e <- eigen(T,symmetric = TRUE)
      T_vec <- T_e$vectors
      T_val <- T_e$values
      #calculate dec inc of max variance
      V1inc <- PmagDiR::r2d(asin(T_vec[3,1]/(sqrt((T_vec[1,1]^2)+(T_vec[2,1]^2)+(T_vec[3,1]^2)))))
      V1dec <- (PmagDiR::r2d(atan2(T_vec[2,1],T_vec[1,1])))%%360
      #next  calculates difference between dec_inc and average
      data$Dec_aver <- rep(V1dec)
      data$Inc_aver <- rep(V1inc)
      data$delta <- abs(data$dec-data$Dec_aver)
      data$diff <- PmagDiR::r2d(acos((sin(PmagDiR::d2r(data$inc))*sin(PmagDiR::d2r(data$Inc_aver)))+
                                       (cos(PmagDiR::d2r(data$inc))*cos(PmagDiR::d2r(data$Inc_aver))*cos(PmagDiR::d2r(data$delta)))))
      #Isolate modes
      if(any(data$diff<=90)){
        mode1 <- as.data.frame(data$dec[data$diff<=90])
        mode1$inc <- data$inc[data$diff<=90]
        colnames(mode1) <- c("dec","inc")
      }
      if(any(data$diff>90)){
        mode2 <- as.data.frame(data$dec[data$diff>90])
        mode2$inc <- data$inc[data$diff>90]
        colnames(mode2) <- c("dec","inc")
      }
      if(exists("mode1")==TRUE) {fisher_M1 <- fisher(mode1)}
      if(exists("mode2")==TRUE) {fisher_M2 <- fisher(mode2)}
      if(plot==TRUE){
        if(exists("mode1")==TRUE){PmagDiR::plot_a95(fisher_M1[1,1],fisher_M1[1,2],fisher_M1[1,3],
                                                    on_plot = TRUE,symbol=symbol, col_d = col_d,
                                                    col_u=col_u,col_l=col_l)}
        if(exists("mode2")==TRUE){PmagDiR::plot_a95(fisher_M2[1,1],fisher_M2[1,2],fisher_M2[1,3],
                                                    on_plot = TRUE,symbol=symbol, col_d = col_d,
                                                    col_u=col_u,col_l=col_l)}
      }
      data_M12 <- PmagDiR::common_DI(data)
      fisher_M12 <- PmagDiR::fisher(data_M12)
    }else{ #forces fisher for all directions without splitting
      data_M12 <- data
      fisher_M12 <- PmagDiR::fisher(data_M12)
      PmagDiR::plot_a95(fisher_M12[1,1],fisher_M12[1,2],fisher_M12[1,3],
                        on_plot = TRUE,symbol=symbol, col_d = col_d,
                        col_u=col_u,col_l=col_l)
    }
    #plot text with results
    Dec <- round(fisher_M12[1,1],digits=2)
    Inc <- round(fisher_M12[1,2],digits=2)
    a <- round(fisher_M12[1,3],digits=2)
    N <- round(fisher_M12[1,4],digits=2)
    
    #creates table for Shiny
    S_results <- as.data.frame(matrix(ncol=6, nrow=3))
    colnames(S_results) <- c("dec", "inc", "a95", "N","R","k")
    
    if(auto_split==TRUE){
      if(any(data$diff<=90)) {
        S_results[1,] <- fisher_M1
      }
      if(any(data$diff>90)) {
        S_results[2,] <- fisher_M2
      }
      if(exists("fisher_M1")==TRUE | exists("fisher_M2")==TRUE) {
        S_results[3,] <- fisher_M12
      }
      
    }else{S_results[3,] <- fisher_M12} #forces fisher for all directions without splitting
    
    rownames(S_results) <- c("Mode 1","Mode 2","All")
    S_results <- S_results[,-5]
    S_results <- na.omit(S_results)
    if(nrow(S_results)==2) S_results <- S_results[1,]
    
    return(S_results)
  }
  
  #modified ellips_plot function
  ellips_plot_S <- function(DI,lat=0,long=0, plot=TRUE, col_d="red",col_u="white",col_l="black",symbol="c"){
    #functions converting inc(x) and dec(y) into equal area
    a2cx <- function(x,y) {sqrt(2)*sin((PmagDiR::d2r(90-x))/2)*sin(PmagDiR::d2r(y))}
    a2cy <- function(x,y) {sqrt(2)*sin((PmagDiR::d2r(90-x))/2)*cos(PmagDiR::d2r(y))}
    data <- DI
    #cut lines with empty cells
    data <- na.omit(data)
    data <- data[,1:2]
    colnames(data) <- c("dec", "inc")
    #directions in Cartesian coordinates
    data$x <- cos(PmagDiR::d2r(data$dec))*cos(PmagDiR::d2r(data$inc))
    data$y <- sin(PmagDiR::d2r(data$dec))*cos(PmagDiR::d2r(data$inc))
    data$z <- sin(PmagDiR::d2r(data$inc))
    #averaged Cartesian coordinates
    x_av <- mean(data$x)
    y_av <- mean(data$y)
    z_av <- mean(data$z)
    #elements of the distribution matrix
    T_elements <- c(sum((data$x)*(data$x)),sum(data$x*data$y),sum(data$x*data$z),
                    sum(data$y*data$x),sum(data$y*data$y),sum(data$y*data$z),
                    sum(data$z*data$x),sum(data$z*data$y),sum(data$z*data$z))
    #distribution matrix
    T <- matrix(T_elements,nrow=3, byrow=TRUE)
    #calculate and copy eigenvalues and vectors
    T_e <- eigen(T,symmetric = TRUE)
    T_vec <- T_e$vectors
    T_val <- T_e$values
    #calculate dec inc of max variance
    V1inc <- PmagDiR::r2d(asin(T_vec[3,1]/(sqrt((T_vec[1,1]^2)+(T_vec[2,1]^2)+(T_vec[3,1]^2)))))
    V1dec <- (PmagDiR::r2d(atan2(T_vec[2,1],T_vec[1,1])))%%360
    #next  calculates difference between dec_inc and average
    data$Dec_aver <- rep(V1dec)
    data$Inc_aver <- rep(V1inc)
    data$delta <- abs(data$dec-data$Dec_aver)
    data$diff <- PmagDiR::r2d(acos((sin(PmagDiR::d2r(data$inc))*sin(PmagDiR::d2r(data$Inc_aver)))+
                                     (cos(PmagDiR::d2r(data$inc))*cos(PmagDiR::d2r(data$Inc_aver))*cos(PmagDiR::d2r(data$delta)))))
    #Isolate modes
    if(any(data$diff<=90)){mode1 <- data[data$diff<=90,1:2]}
    if(any(data$diff>90)){mode2 <- data[data$diff>90,1:2]}
    #calculate ellipses
    if(exists("mode1")==TRUE && nrow(mode1)>1) {ellips_M1 <- PmagDiR::ellips_DI(mode1, lat=lat, long=long)}
    if(exists("mode2")==TRUE && nrow(mode2)>1) {ellips_M2 <- PmagDiR::ellips_DI(mode2, lat=lat, long=long)}
    if(plot==TRUE){ 
      if(exists("mode1")==TRUE){PmagDiR::generate_ellips(ellips_M1[1,1],ellips_M1[1,2],ellips_M1[1,3],ellips_M1[1,4],
                                                         on_plot = TRUE,symbol=symbol, col_d = col_d,
                                                         col_u=col_u,col_l=col_l)}
      if(exists("mode2")==TRUE){PmagDiR::generate_ellips(ellips_M2[1,1],ellips_M2[1,2],ellips_M2[1,3],ellips_M2[1,4],
                                                         on_plot = TRUE,symbol=symbol, col_d = col_d,
                                                         col_u=col_u,col_l=col_l)}
    }
    data_M12 <- PmagDiR::common_DI(data)
    ellips_M12 <- PmagDiR::ellips_DI(data_M12,lat=lat, long=long)
    #plot text with results
    N <- ellips_M12[1,5]
    Dec <- round(ellips_M12[1,1],digits=2)
    Inc <- round(ellips_M12[1,2],digits=2)
    Delta_dec <- round(ellips_M12[1,3],digits=2)
    Delta_inc <- round(ellips_M12[1,4],digits=2)
    
    #set file for export in shiny
    S_result <- as.data.frame(matrix(ncol=5,nrow=3))
    colnames(S_result) <- c("dec", "inc","a95 dec","a95 inc","N")
    rownames(S_result) <- c("Mode 1","Mode 2", "All")
    
    if(any(data$diff<=90)) {
      S_result[1,] <- ellips_M1
    }
    if(any(data$diff>90)) {
      S_result[2,] <- ellips_M2
    }
    if(any(data$diff>90)) {
      S_result[3,] <- ellips_M12
    }
    S_result <- na.omit(S_result)
    if(nrow(S_result)==2) S_result <- S_result[1,]
    return(S_result)
  }
  
  #perform watson's test for randomnes
  observeEvent(input$WatsRand,{
    #stop without file
    req(Dirs$dat)
    
    #import data
    DI <- fix_DI(input_file= Dirs$dat)           
    
    #apply function
    result <- PmagDiR::Watson_Random(DI)
    
    #watson's test result
    showModal(jqui_draggable(modalDialog(
      size= "m",
      tags$h2("Watson's test of randomness"),
      br(),
      tags$h4(paste("N:",result[[1]])),
      tags$h4(paste("R:",round(result[[2]],digits = 2))),
      tags$h4(paste("R_critical:",round(result[[3]],digits = 2))),
      tags$h4(paste(result[[4]])),
      easyClose = TRUE,
      footer=tagList(
        modalButton('close')
      )
    ),options = list(cancel = ".shiny-input-container")))
  })
  
  #function that select directions from screen
  selectedDIR <- reactive({
    DI_2_drag <- fix_DI(Dirs$dat)
    #PREPARE SELECTING POINTS
    #plot invisible points to select with drag
    DI_2_drag$x <- PmagDiR::a2cx(abs(DI_2_drag[,2]),DI_2_drag[,1])
    DI_2_drag$y <- PmagDiR::a2cy(abs(DI_2_drag[,2]),DI_2_drag[,1])
    #plot invisible dirs for selection
    points(x = DI_2_drag$x,y=DI_2_drag$y,col=NA)
    #select points
    selected_directions <- as.integer(rownames(brushedPoints(df = DI_2_drag,brush = input$plot_brush2,xvar = "x",yvar = "y")))
    return(selected_directions)
  })
  
  #delete selected directions         
  observeEvent(input$cutDirs,{
    req(Dirs$dat)
    req(Dirs$to_delete)
    #act differently for example data otherwise does not work. Do not ask me why, like this it works. it is likely because of the fix_DI function a bit messed up
    if(input$filetype==6 && input$coord==2){
      Dirs$dat <- Dirs$dat[-Dirs$to_delete,]
    }else{
      #Dirs$to_delete comes from the renderplot part
      to_delete <- as.character(Dirs$to_delete)
      Dirs$dat <- Dirs$dat[!(row.names(Dirs$dat) %in% to_delete),]
    }
  })
  
  #restore directions from file
  observeEvent(input$restoreDirs,{
    req(input_file())
    Dirs$dat <- input_file()
  })
  
  #equal area function
  plot_dirs <- function(DI,Slat=input$lat,Slong=input$long,mode=input$mode,
                        colD=input$colD,colU=input$colU,sym=input$sym,GAD=input$addGAD){
    #file with possible error message from inclination only routine
    inc_warn <- NULL
    #define colors Down-pointing
    if(colD==1) colD <- "black"
    if(colD==2) colD <- "blue"
    if(colD==3) colD <- "red"
    if(colD==4) colD <- "dark green"
    
    #define color Up-pointing
    if(colU==1) colU <- "white"
    if(colU==2) colU <- "cyan"
    if(colU==3) colU <- "pink"
    if(colU==4) colU <- "light green"
    
    #define symbol
    if(sym==1) sym <- "c"
    if(sym==2) sym <- "s"
    if(sym==3) sym <- "d"
    if(sym==4) sym <- "t"
    
    PmagDiR::plot_DI(DI,col_d = colD,col_u = colU, symbol = sym)
    #plot GAD if requested
    if(GAD==2){PmagDiR::plot_GAD(lat = Slat,on_plot = T,size = 0.18,col_u = "yellow")}
    if(GAD==3){PmagDiR::plot_GAD(lat = Slat,on_plot = T,circle = T,size=0.18,col_u = "yellow")}
    
    #plot statistic, with warning message if any from Arason+Levi2010 algorythm, or assign NULL in all other cases
    #inc warn must be created in the environment normally to cover the one created by PmagDiR
    if(input$fisher==2){
      assign("inc_warn",NULL, envir = .GlobalEnv)
      F_stat$result <- fisher_plot_S(DI)
    }else if(input$fisher==8){
      assign("inc_warn",NULL, envir = .GlobalEnv)
      F_stat$result <- fisher_plot_S(DI,auto_split = FALSE)
    }else if(input$fisher==3){
      assign("inc_warn",NULL, envir = .GlobalEnv)
      F_stat$result <- ellips_plot_S(DI,lat = Slat,long = Slong)
    }else if(input$fisher==4){
      assign("inc_warn",NULL, envir = .GlobalEnv)
      F_stat$result <- PmagDiR::inc_plot(DI = DI,bimodal = F,print = F,export = F,save = F,Shiny = T)
    }else if(input$fisher==5){
      assign("inc_warn",NULL, envir = .GlobalEnv)
      F_stat$result <- PmagDiR::inc_plot(DI = DI,bimodal = T,print = F,export = F,save = F,Shiny = T)
    }else if(input$fisher==6){
      assign("inc_warn",NULL, envir = .GlobalEnv)
      F_stat$result <- PmagDiR::inc_plot(DI = DI,bimodal = F,print = F,export = F,save = F,arith_stat = T,Shiny = T)
    }else if(input$fisher==7){
      assign("inc_warn",NULL, envir = .GlobalEnv)
      F_stat$result <- PmagDiR::inc_plot(DI = DI,bimodal = T,print = F,export = F,save = F,arith_stat = T,Shiny = T)
    }else{
      F_stat$result <- NULL
      assign("inc_warn",NULL, envir = .GlobalEnv)
    }
  }
  #send plot to UI
  output$directions <- renderPlot({
    #avoid errors if long and lat are missing
    req(input$lat)
    req(input$long)
    req(input_file())
    
    #apply plotting function
    plot_dirs(fix_DI(Dirs$dat))
    
    
    #select points dragging from screen
    Dirs$to_delete <- selectedDIR()   
    
    #select and plot all dragged points. It chooses rownames otherwise it mixed up with rowindex
    if(length(Dirs$to_delete)){
      temp <- fix_DI(Dirs$dat)
      Selection2plot <- temp
      Selection2plot[,] <- NA
      for(i in Dirs$to_delete){
        i <- as.character(i)
        for(l in 1:nrow(temp)){
          if(rownames(temp[l,])==i){Selection2plot[l,] <- temp[l,]}
        }
      }
      Selection2plot <- na.omit(Selection2plot)
      PmagDiR::plot_DI(Selection2plot,col_d = "red",col_u = "yellow",on_plot = T)
    }
    
    #record plot
    DirsPlot <- recordPlot()
    
    #create directions stats table
    output$stats <- renderTable({
      F_stat$result
    },rownames=T, digits=1)
    
    #write inc_only problem if any
    if(exists("inc_warn")==T){
      output$inc_warn <- renderText({
        inc_warn
      })
    }
    
    #export stat
    output$exportS <- downloadHandler(
      filename = function() {
        paste(input$fileN,"_", Sys.Date(), "_stat.csv", sep="")
      },
      content = function(file) {
        write.csv(round(F_stat$result, digits = 2), file)
      }
    )
    
    #export equal_area graph
    output$exportG <- downloadHandler(
      filename = function() {
        paste(input$fileN,"_", Sys.Date(), ".pdf", sep="")
      },
      content = function(file) {
        pdf(file, onefile = TRUE,width = 9,height = 9)
        replayPlot(DirsPlot)
        dev.off()
      }
    )
    
    #export DI
    output$exportDI <- downloadHandler(
      filename = function() {
        paste(input$fileN,"_", Sys.Date(), "_directions.csv", sep="")
      },
      content = function(file) {
        DI <- fix_DI(Dirs$dat)
        if(input$mode==1){DI <- DI}
        if(input$mode==2){DI <- common_DI(DI)}
        if(input$mode==3){DI <- common_DI(DI,down = F)}
        DI <- na.omit(DI)
        write.csv(round(DI, digits=2),row.names = F, file)
      }
    )
  },width = 700,height = 700)
  ####### END OF DIRECTIONS DISPILAY & AVERAGE SUBPAGE
  
  ####### MULTIPLE DIRECTION SETS AND AVERAGED DIRECTIONS PLOT SUBPAGES
  ###create reactive file table parametric
  #direction sets table to display
  multiDirsTab <- reactiveValues(table_sets=NULL) 
  #direction sets list
  multiDirsList <- reactiveValues(sets=NULL)
  #multi parametric average
  multiFish <- reactiveValues(table_p=NULL)
  #multi non-parametric average
  multiBoot <- reactiveValues(table_b=NULL)
  #list with non-parametric ellipses
  multiEllips <- reactiveValues(ellips=NULL)
  #file for recording plot to save
  multiDirs <- reactiveValues(plot=NULL)
  
  #import directions set from file
  observeEvent(input$extDirsFile, {
    #save name for later rowname use
    fileName <- input$extDirsFile$name
    
    #import file
    ext_Dirs_file <- read.csv(file = input$extDirsFile$datapath,header = T)
    #remove empty rows to avoid wrong N
    ext_Dirs_file <- na.omit(ext_Dirs_file)
    
    #create and populate temporary file for table
    temp <- data.frame(matrix(ncol=5,nrow = 1))
    colnames(temp) <- c("Name","N","Sym.","Col. D.","Col. U.")
    temp[1,1] <- fileName
    temp[1,2] <- nrow(ext_Dirs_file)
    temp[1,3] <- "c"
    temp[1,4] <- "black"
    temp[1,5] <- "white"
    
    #Directions set is saved elsewhere in a reactive list. Need to create an unique name for it with file and dec inc of first row
    ExtDirsName_1 <- paste(fileName,"_",ext_Dirs_file[1,1],"_",ext_Dirs_file[1,2],sep = "")
    
    #depending of the lenth of the exising list it make an index for the new set
    ExtDirsListIndex <- length(multiDirsList$sets)
    #send set to the list
    multiDirsList$sets[[ExtDirsListIndex+1]] <- ext_Dirs_file[,1:2]
    #change name of the set in the list
    names(multiDirsList$sets)[ExtDirsListIndex+1] <- ExtDirsName_1
    
    #name is also assign to the row, so the set can be called again to plot.
    rownames(temp) <- ExtDirsName_1
    
    #if table is null makes it, or add new rows if it exists
    if(is.null(multiDirsTab$table_sets)){
      multiDirsTab$table_sets <- temp
    }else{multiDirsTab$table_sets <- rbind(multiDirsTab$table_sets,temp)}
    multiDirsTab$table_sets <- unique(multiDirsTab$table_sets)
  })
  
  #import main set from DD&A window
  observeEvent(input$ADD_Dirs,{
    #take directions 
    req(Dirs$dat)
    DI <- fix_DI(Dirs$dat)
    req(nrow(DI)>0)
    #save name as typed in main window for later rowname use
    fileName <- input$fileN
    
    #create and populate temporary file for table
    temp <- data.frame(matrix(ncol=5,nrow = 1))
    colnames(temp) <- c("Name","N","Sym.","Col. D.","Col. U.")
    temp[1,1] <- fileName
    temp[1,2] <- nrow(DI)
    temp[1,3] <- "c"
    temp[1,4] <- "black"
    temp[1,5] <- "white"
    
    #Directions set is saved elsewhere in a reactive list. Need to create an unique name for it with file and dec inc of first row
    ExtDirsName_1 <- paste(fileName,"_",DI[1,1],"_",DI[1,2],sep = "")
    
    #depending of the lenth of the exising list it make an index for the new set
    ExtDirsListIndex <- length(multiDirsList$sets)
    #send set to the list
    multiDirsList$sets[[ExtDirsListIndex+1]] <- DI[,1:2]
    #change name of the set in the list
    names(multiDirsList$sets)[ExtDirsListIndex+1] <- ExtDirsName_1
    
    #name is also assign to the row, so the set can be called again to plot.
    rownames(temp) <- ExtDirsName_1
    
    #if table is null makes it, or add new rows if it exists
    if(is.null(multiDirsTab$table_sets)){
      multiDirsTab$table_sets <- temp
    }else{multiDirsTab$table_sets <- rbind(multiDirsTab$table_sets,temp)}
    multiDirsTab$table_sets <- unique(multiDirsTab$table_sets)
  })
  
  #send multiDirsTab to UI and make it editable cell by cell
  output$multiDirsTab <- DT::renderDataTable({
    req(multiDirsTab$table_sets)  
    datatable(multiDirsTab$table_sets, editable = list(target="cell", disable= list(columns=c(0,1))),rownames = FALSE,
              options = list(
                dom = 't',                    # 't' mostra solo la tabella, senza l'intestazione e ricerca
                paging = FALSE,               # Disabilita la paginazione
                searching = FALSE              # Disabilita la ricerca
              ))  %>%
      formatStyle(
        columns = c("Name","N","Sym.","Col. D.","Col. U."), 
        color = 'black'   # Colore del testo per le celle editabili
      )
  })
  
  #modify table through UI
  observeEvent(input$multiDirsTab_cell_edit, {
    info <- input$multiDirsTab_cell_edit
    #current values in table
    modified_data <-  multiDirsTab$table_sets  
    #update values (plus one otherwise in paste to the wrong column. Do not ask me why, it's like a chopper flying without spinning blades)
    modified_data[info$row, (info$col+1)] <- info$value 
    #update table
    multiDirsTab$table_sets <- modified_data
  })
  
  #delete entry from multi dirs tab
  observeEvent(input$Del_Dirs,{
    if(!is.null(multiDirsTab$table_sets)){
      d <- input$multiDirsTab_rows_selected
      if(length(d)){multiDirsTab$table_sets <- multiDirsTab$table_sets[-d,]}
      if(nrow(multiDirsTab$table_sets)==0){multiDirsTab$table_sets <- NULL}
    }#does not delete from list as it is not relevant if data stays there temporarily
  })
  
  #import parametric average from file
  observeEvent(input$multiFishFile,{
    #save name for later rowname use
    fileName <- input$multiFishFile$name
    
    #import file
    ext_MFish_file <- read.csv(file = input$multiFishFile$datapath,header = T)
    
    #create temporary file with same row number
    temp <- data.frame(matrix(ncol=7,nrow = nrow(ext_MFish_file)))
    colnames(temp) <- c("Dec.","Inc.","a95(dec)","a95_inc","Sym.","S. Col","L. Col")
    
    
    #create the rest depending on the file (columns number)
    #only standard Fisher
    if(ncol(ext_MFish_file)==3){
      temp[,1] <- ext_MFish_file[,1]
      temp[,2] <- ext_MFish_file[,2]
      temp[,3] <- ext_MFish_file[,3]
      temp[,4] <- 0
      temp[,5] <- "c"
      temp[,6] <- ifelse(temp[,2]<0,"white","black")
      temp[,7] <- "black"
    }
    #Deenan elliptic
    else if(ncol(ext_MFish_file)==4){
      temp[,1] <- ext_MFish_file[,1]
      temp[,2] <- ext_MFish_file[,2]
      temp[,3] <- ext_MFish_file[,3]
      temp[,4] <- ext_MFish_file[,4]
      temp[,5] <- "c"
      temp[,6] <- ifelse(temp[,2]<0,"white","black")
      temp[,7] <- "black"
    }
    #Standard Fisher+symbol and color
    else if(ncol(ext_MFish_file)==6){
      #this is for Magnetic-A fisher-exported files 
      if(all(names(ext_MFish_file)==c("X","dec","inc","a95","N","k"))){
        temp[,1] <- ext_MFish_file[,2]
        temp[,2] <- ext_MFish_file[,3]
        temp[,3] <- ext_MFish_file[,4]
        temp[,4] <- 0
        temp[,5] <- "c"
        temp[,6] <- ifelse(temp[,2]<0,"white","black")
        temp[,7] <- "black"
      }
      #magnetic-A elliptic
      else if(all(names(ext_MFish_file)==c("X","dec","inc","a95.dec","a95.inc","N"))){
        temp[,1] <- ext_MFish_file[,2]
        temp[,2] <- ext_MFish_file[,3]
        temp[,3] <- ext_MFish_file[,4]
        temp[,4] <- ext_MFish_file[,5]
        temp[,5] <- "c"
        temp[,6] <- ifelse(temp[,2]<0,"white","black")
        temp[,7] <- "black"
      }
      #any other 6 columns file
      else{
        temp[,1] <- ext_MFish_file[,1]
        temp[,2] <- ext_MFish_file[,2]
        temp[,3] <- ext_MFish_file[,3]
        temp[,4] <- 0
        temp[,5] <- ext_MFish_file[,4]
        temp[,6] <- ext_MFish_file[,5]
        temp[,7] <- ext_MFish_file[,6]
      }
    }
    #Deenan elliptic+ all symbol and color
    else if(ncol(ext_MFish_file)==7){
      temp[,1] <- ext_MFish_file[,1]
      temp[,2] <- ext_MFish_file[,2]
      temp[,3] <- ext_MFish_file[,3]
      temp[,4] <- ext_MFish_file[,4]
      temp[,5] <- ext_MFish_file[,5]
      temp[,6] <- ext_MFish_file[,6]
      temp[,7] <- ext_MFish_file[,7]
    } 
    #check if multifish reactive file exists and merge or create
    if(is.null(multiFish$table_p)){
      multiFish$table_p <- temp
    }else{multiFish$table_p <- rbind(multiFish$table_p,temp)}
    
  })
  
  #import non-parametric data from file
  observeEvent(input$multiBootFile,{
    #save name for later rowname use
    fileName <- input$multiBootFile$name
    
    #import file
    ext_Boot_file <- read.csv(file = input$multiBootFile$datapath,header = T)
    
    #create and populate temporary file of table
    temp <- data.frame(matrix(ncol=5,nrow = 2))
    colnames(temp) <- c("Dec.","Inc.","Sym.","S. Col","L. Col")
    temp[1,1:2] <- round(ext_Boot_file[1,1:2],digits = 1)
    temp[1,3] <- "c"
    temp[1,4] <- ifelse(temp[1,2]<0,"white","black")
    temp[1,5] <- "black"
    
    #ellipsis is saved elsewhere in a reactive list. Need to create an unique name for it with file and dec inc
    ellipsName_1 <- paste(fileName,"_",temp[1,1],"_",temp[1,2],sep = "")
    
    #depending of the lenth of the exising list it make an index for the new ellipse
    ellipsListIndex <- length(multiEllips$ellips)
    #send ellipse to the list
    multiEllips$ellips[[ellipsListIndex+1]] <- ext_Boot_file[,3:4]
    #change name of the ellipse in the list
    names(multiEllips$ellips)[ellipsListIndex+1] <- ellipsName_1
    
    #name is also assign to the row, so the ellipsis can be called again to plot.
    rownames(temp) <- c(ellipsName_1,"to_change")
    
    #if it is a file with two modes, first condition is to avoid crash with wrong file
    if(ncol(ext_Boot_file)>4 && !is.na(ext_Boot_file[1,5])){
      temp[2,1:2] <- round(ext_Boot_file[1,5:6], digits = 1)
      temp[2,3] <- "c"
      temp[2,4] <- ifelse(temp[2,2]<0,"white","black")
      temp[2,5] <- "black"
      #name of ellipsis
      ellipsName_2 <- paste(fileName,"_",temp[2,1],"_",temp[2,2],sep = "")             
      multiEllips$ellips[[ellipsListIndex+2]] <- ext_Boot_file[,7:8]
      names(multiEllips$ellips)[ellipsListIndex+2] <- ellipsName_2
      #rename rows of the table
      rownames(temp) <- c(ellipsName_1,ellipsName_2)                             
    }
    
    #delete NA row in case one mode only
    temp <- na.omit(temp)
    
    #if table is null makes it, or add new rows if it exists
    if(is.null(multiBoot$table_b)){
      multiBoot$table_b <- temp
    }else{multiBoot$table_b <- rbind(multiBoot$table_b,temp)}
    multiBoot$table_b <- unique(multiBoot$table_b)
  })
  
  #open window to enter fisher mean details manually
  observeEvent(input$MultiFishDetails, {
    # display a modal dialog with a header, textinput and action buttons
    showModal(jqui_draggable(modalDialog(
      tags$h2('Enter average direction details'),
      fluidRow(
        column(3,numericInput("MFdec",label = "Declination",value = 0)),
        column(3,numericInput("MFinc",label = "Inclination",value = 0)),
        column(3,numericInput("MFa95",label = "a95 (Dec)*",value = 0)),
        column(3,numericInput("MFa95_inc",label = "a95 Inc",value = 0))
      ),
      fluidRow(
        column(4,selectInput("MFsym", label= "Symbol",
                             choices = list("circle"=1, "square"=2, "diamond"=3,"Triangle"=4),selected=1)),
        column(4,selectInput("MFcolor_s", label= "Symbol color",
                             choices= list("black"=1,"blue"=2,"green"=3,"darkgreen"=12,"pink"=4,"purple"=5,"brown"=6,"red"=7,"yellow"=8,"cyan"=9,"gray"=10, "white"=11), selected=1)),
        column(4,selectInput("MFcolor_l", label= "Line color",
                             choices= list("black"=1,"blue"=2,"green"=3,"darkgreen"=12,"pink"=4,"purple"=5,"brown"=6,"red"=7,"yellow"=8,"cyan"=9,"gray"=10, "white"=11), selected=1))
      ),
      br(),
      tags$h5('*In case of circular (standard Fisher) 95% confidence, fill only a95 (Dec) and leave a95 Inc zero. In case of elliptic 95% confidence [Deenan et al. GJI 186(2), 509-520, 2013] fill both Dec and Inc 95%'),
      br(),
      fluidRow(
        column(12, actionButton(inputId = "addMultiFish",label = "ADD TO LIST",width = "100%"))
      ),
      easyClose = TRUE,
      footer=tagList(
        modalButton('close')
      )
    ),options = list(cancel = ".shiny-input-container")))
  })
  
  #add manually typed average direction to list
  observeEvent(input$addMultiFish,{
    #check if list already exists
    if(is.null(multiFish$table_p)==T){
      multiFish$table_p <- data.frame(matrix(ncol=7,nrow = 0))
      colnames(multiFish$table_p) <- c("Dec.","Inc.","a95(dec)","a95_inc","Sym.","S. Col","L. Col")
    }
    #temporary row
    temp <- data.frame(matrix(ncol=7,nrow = 0))
    colnames(temp) <- c("Dec.","Inc.","a95(dec)","a95_inc","Sym.","S. Col","L. Col")
    
    #select symbol of dir
    if(input$MFsym==1) MFsym <- "c"
    if(input$MFsym==2) MFsym <- "s"
    if(input$MFsym==3) MFsym <- "d"
    if(input$MFsym==4) MFsym <- "t"
    
    #choose color of point
    if(input$MFcolor_s==1) MFcolor_s <- "black"
    if(input$MFcolor_s==2) MFcolor_s <- "blue"
    if(input$MFcolor_s==3) MFcolor_s <- "green"
    if(input$MFcolor_s==12) MFcolor_s <- "darkgreen"
    if(input$MFcolor_s==4) MFcolor_s <- "pink"
    if(input$MFcolor_s==5) MFcolor_s <- "purple"
    if(input$MFcolor_s==6) MFcolor_s <- "brown"
    if(input$MFcolor_s==7) MFcolor_s <- "red"
    if(input$MFcolor_s==8) MFcolor_s <- "yellow"
    if(input$MFcolor_s==9) MFcolor_s <- "cyan"
    if(input$MFcolor_s==10) MFcolor_s <- "gray"
    if(input$MFcolor_s==11) MFcolor_s <- "white"
    #color of line
    if(input$MFcolor_l==1) MFcolor_l <- "black"
    if(input$MFcolor_l==2) MFcolor_l <- "blue"
    if(input$MFcolor_l==3) MFcolor_l <- "green"
    if(input$MFcolor_l==12) MFcolor_l <- "darkgreen"
    if(input$MFcolor_l==4) MFcolor_l <- "pink"
    if(input$MFcolor_l==5) MFcolor_l <- "purple"
    if(input$MFcolor_l==6) MFcolor_l <- "brown"
    if(input$MFcolor_l==7) MFcolor_l <- "red"
    if(input$MFcolor_l==8) MFcolor_l <- "yellow"
    if(input$MFcolor_l==9) MFcolor_l <- "cyan"
    if(input$MFcolor_l==10) MFcolor_l <- "gray"
    if(input$MFcolor_l==11) MFcolor_l <- "white"
    
    #fill temporary row
    temp[1,1] <- input$MFdec
    temp[1,2] <- input$MFinc
    temp[1,3] <- input$MFa95
    temp[1,4] <- input$MFa95_inc
    temp[1,5] <- MFsym
    temp[1,6] <- MFcolor_s
    temp[1,7] <- MFcolor_l
    
    #combine with entries
    multiFish$table_p <- rbind(multiFish$table_p,temp)
  })                                           
  
  #send table to UI and make it editable cell by cell
  output$multiFishTab <- DT::renderDataTable({
    req(multiFish$table_p)  
    datatable(multiFish$table_p, editable = TRUE,rownames = FALSE,
              options = list(
                dom = 't',                    # 't' mostra solo la tabella, senza l'intestazione e ricerca
                paging = FALSE,               # Disabilita la paginazione
                searching = FALSE              # Disabilita la ricerca
              ))  %>%
      formatStyle(
        columns = c("Dec.","Inc.","a95(dec)","a95_inc","Sym.","S. Col","L. Col"), 
        color = 'black'   # Colore del testo per le celle editabili
      )
  })
  
  #send table of non-parametric averages to UI
  output$multiBootTab <- DT::renderDataTable({
    req(multiBoot$table_b)  
    datatable(multiBoot$table_b, editable = list(target="cell", disable= list(columns=c(0,1))),rownames = FALSE,
              options = list(
                dom = 't',                    # 't' mostra solo la tabella, senza l'intestazione e ricerca
                paging = FALSE,               # Disabilita la paginazione
                searching = FALSE              # Disabilita la ricerca
              ))  %>%
      formatStyle(
        columns = c("Dec.","Inc.","Sym.","S. Col","L. Col"), 
        color = 'black'   # Colore del testo per le celle editabili
      )
  })
  
  #shows table title if present
  output$TableParametric <- renderText(ifelse(!is.null(multiFish$table_p),"Parametric average list",""))
  output$TableBoots <- renderText(ifelse(!is.null(multiBoot$table_b),"Non-parametric average list",""))
  
  #delete entry from multi fisher list
  observeEvent(input$cutMultiFish,{
    if(!is.null(multiFish$table_p)){
      d <- input$multiFishTab_rows_selected
      if(length(d)){multiFish$table_p <- multiFish$table_p[-d,]}
      if(nrow(multiFish$table_p)==0){multiFish$table_p <- NULL}
    }
    #delete also ellipsis
    if(!is.null(multiBoot$table_b)){
      b <- input$multiBootTab_rows_selected
      if(length(b)){
        multiBoot$table_b <- multiBoot$table_b[-b,]
      }
      if(nrow(multiBoot$table_b)==0){multiBoot$table_b <- NULL}
    }
  })
  
  #modify table through UI
  observeEvent(input$multiFishTab_cell_edit, {
    info <- input$multiFishTab_cell_edit
    #current values in table
    modified_data <- multiFish$table_p  
    #update values (plus one otherwise in paste to the wrong column. Do not ask me why, it's like a chopper flying without spinning blades)
    modified_data[info$row, (info$col+1)] <- info$value 
    #update table
    multiFish$table_p <- modified_data
  })
  
  #modify table through UI
  observeEvent(input$multiBootTab_cell_edit, {
    info <- input$multiBootTab_cell_edit
    #current values in table
    modified_data <-  multiBoot$table_b  
    #update values (plus one otherwise in paste to the wrong column. Do not ask me why, it's like a chopper flying without spinning blades)
    modified_data[info$row, (info$col+1)] <- info$value 
    #update table
    multiBoot$table_b <- modified_data
  })
  
  #generate equal area and plot it, one for sub-page
  output$MultiFish1 <- renderPlot({
    #plot empty stereo
    PmagDiR::equalarea()
    #plot directions sets
    if(!is.null(multiDirsTab$table_sets)){
      f <- input$multiDirsTab_rows_selected
      if(length(f)){
        for(n in f){
          nome_riga <- rownames(multiDirsTab$table_sets)[n]
          PmagDiR::plot_DI(DI = multiDirsList$sets[[nome_riga]][,1:2],
                           symbol = multiDirsTab$table_sets[n,3],
                           col_d = multiDirsTab$table_sets[n,4],
                           col_u = multiDirsTab$table_sets[n,5],
                           on_plot = T)
        }
      }
    }
    #plot fisher
    if(!is.null(multiFish$table_p)){
      d <- input$multiFishTab_rows_selected
      if(length(d)){
        for(i in d){
          if(multiFish$table_p[i,4]==0){
            PmagDiR::plot_a95(D = multiFish$table_p[i,1],
                              I = multiFish$table_p[i,2],
                              a = multiFish$table_p[i,3],
                              symbol = multiFish$table_p[i,5],
                              col_l = multiFish$table_p[i,7],
                              col_d = multiFish$table_p[i,6],
                              col_u = multiFish$table_p[i,6],
                              on_plot = T)
          }
          else if(multiFish$table_p[i,4]!=0){
            PmagDiR::generate_ellips(D = multiFish$table_p[i,1],
                                     I = multiFish$table_p[i,2],
                                     delta_dec = multiFish$table_p[i,3],
                                     delta_inc = multiFish$table_p[i,4],
                                     symbol = multiFish$table_p[i,5],
                                     col_l = multiFish$table_p[i,7],
                                     col_d = multiFish$table_p[i,6],
                                     col_u = multiFish$table_p[i,6],
                                     on_plot = T)
          }
        }
      }
    }
    #plot non-parametric ellipsis
    if(!is.null(multiBoot$table_b)){
      b <- input$multiBootTab_rows_selected
      if(length(b)){
        for(l in b){
          nome_riga <- rownames(multiBoot$table_b)[l]
          PmagDiR::plot_B95(D = multiBoot$table_b[l,1],
                            I = multiBoot$table_b[l,2],
                            B_conf = multiEllips$ellips[[nome_riga]][,1:2],
                            col_d = multiBoot$table_b[l,4],
                            col_u = multiBoot$table_b[l,4],
                            col_l = multiBoot$table_b[l,5],
                            symbol = multiBoot$table_b[l,3],
                            on_plot = T)
        }
      }
    }
    if(input$addGAD_2==2){PmagDiR::plot_GAD(lat = input$GAD_lat,on_plot = T,size = 0.18,col_u = "yellow")}
    if(input$addGAD_2==3){PmagDiR::plot_GAD(lat = input$GAD_lat,on_plot = T,circle = T,size=0.18,col_u = "yellow")}
    #record plot for exporting
    multiDirs$plot <- recordPlot()
  },width = 700,height = 700)
  
  output$MultiFish2 <- renderPlot({
    #plot empty stereo
    PmagDiR::equalarea()
    #plot directions sets
    if(!is.null(multiDirsTab$table_sets)){
      f <- input$multiDirsTab_rows_selected
      if(length(f)){
        for(n in f){
          nome_riga <- rownames(multiDirsTab$table_sets)[n]
          PmagDiR::plot_DI(DI = multiDirsList$sets[[nome_riga]][,1:2],
                           symbol = multiDirsTab$table_sets[n,3],
                           col_d = multiDirsTab$table_sets[n,4],
                           col_u = multiDirsTab$table_sets[n,5],
                           on_plot = T)
        }
      }
    }
    #plot fisher
    if(!is.null(multiFish$table_p)){
      d <- input$multiFishTab_rows_selected
      if(length(d)){
        for(i in d){
          if(multiFish$table_p[i,4]==0){
            PmagDiR::plot_a95(D = multiFish$table_p[i,1],
                              I = multiFish$table_p[i,2],
                              a = multiFish$table_p[i,3],
                              symbol = multiFish$table_p[i,5],
                              col_l = multiFish$table_p[i,7],
                              col_d = multiFish$table_p[i,6],
                              col_u = multiFish$table_p[i,6],
                              on_plot = T)
          }
          else if(multiFish$table_p[i,4]!=0){
            PmagDiR::generate_ellips(D = multiFish$table_p[i,1],
                                     I = multiFish$table_p[i,2],
                                     delta_dec = multiFish$table_p[i,3],
                                     delta_inc = multiFish$table_p[i,4],
                                     symbol = multiFish$table_p[i,5],
                                     col_l = multiFish$table_p[i,7],
                                     col_d = multiFish$table_p[i,6],
                                     col_u = multiFish$table_p[i,6],
                                     on_plot = T)
          }
        }
      }
    }
    #plot non-parametric ellipsis
    if(!is.null(multiBoot$table_b)){
      b <- input$multiBootTab_rows_selected
      if(length(b)){
        for(l in b){
          nome_riga <- rownames(multiBoot$table_b)[l]
          PmagDiR::plot_B95(D = multiBoot$table_b[l,1],
                            I = multiBoot$table_b[l,2],
                            B_conf = multiEllips$ellips[[nome_riga]][,1:2],
                            col_d = multiBoot$table_b[l,4],
                            col_u = multiBoot$table_b[l,4],
                            col_l = multiBoot$table_b[l,5],
                            symbol = multiBoot$table_b[l,3],
                            on_plot = T)
        }
      }
    }
    if(input$addGAD_2==2){PmagDiR::plot_GAD(lat = input$GAD_lat,on_plot = T,size = 0.18,col_u = "yellow")}
    if(input$addGAD_2==3){PmagDiR::plot_GAD(lat = input$GAD_lat,on_plot = T,circle = T,size=0.18,col_u = "yellow")}
    #record plot for exporting
    multiDirs$plot <- recordPlot()
  },width = 700,height = 700)
  
  #export plot _1
  output$multiDirs_1 <- downloadHandler(
    filename = function() {
      paste("Multi_sets_", Sys.Date(), ".pdf", sep="")
    },
    content = function(file) {
      pdf(file, onefile = TRUE,width = 10,height = 10)
      replayPlot(multiDirs$plot)
      dev.off()
    }
  )
  #export plot _2
  output$multiDirs_2 <- downloadHandler(
    filename = function() {
      paste("Multi_sets_", Sys.Date(), ".pdf", sep="")
    },
    content = function(file) {
      pdf(file, onefile = TRUE,width = 10,height = 10)
      replayPlot(multiDirs$plot)
      dev.off()
    }
  )
  ####### END OF MULTIPLE DIRECTIONS AND AVERAGED PLOT SUBPAGES
  ############ END OF DIRECTIONS DISPILAY & AVERAGE
  
  ############ BOOTSTRAPPED CONFIDENCE MODULE###############
  #create reactive file to append things
  B95 <- reactiveValues(result_1=NULL)
  
  #function that split data in modes and calculate ellipses
  B95_calculation <- function(n_boots=input$B95nb,p=0.05,mode=1){ 
    DI <- fix_DI(Dirs$dat)
    DI <- na.omit(DI)
    
    dat <- DI[,1:2]
    colnames(dat) <- c("dec", "inc")
    #directions in Cartesian coordinates
    dat$x <- NA
    dat$y <- NA
    dat$z <- NA
    dat[,3:5] <- PmagDiR::s2c(DI = dat[,1:2])
    #calculate interpolation of all data set
    Ta_temp <- as.matrix(dat[,3:5])
    Ta <- t(Ta_temp) %*% Ta_temp
    Ta <- Ta/nrow(dat)
    T_e <- eigen(Ta,symmetric = TRUE)
    T_vec <- T_e$vectors
    T_val <- T_e$value
    
    #calculate dec inc of max variance
    V1inc <- PmagDiR::r2d(asin(T_vec[3,1]/(sqrt((T_vec[1,1]^2)+(T_vec[2,1]^2)+(T_vec[3,1]^2)))))
    V1dec <- (PmagDiR::r2d(atan2(T_vec[2,1],T_vec[1,1])))%%360
    
    #next  calculates difference between dec_inc and average
    dat$Dec_aver <- rep(V1dec)
    dat$Inc_aver <- rep(V1inc)
    dat$delta <- abs(dat$dec-dat$Dec_aver)
    dat$diff <- PmagDiR::r2d(acos((sin(PmagDiR::d2r(dat$inc))*sin(PmagDiR::d2r(dat$Inc_aver)))+
                                    (cos(PmagDiR::d2r(dat$inc))*cos(PmagDiR::d2r(dat$Inc_aver))*cos(PmagDiR::d2r(dat$delta)))))
    #Isolate modes
    m1ind <- as.numeric(which(dat$diff<=90), arr.ind = TRUE)
    m2ind <- as.numeric(which(dat$diff>90), arr.ind = TRUE)
    
    #terminate if distribution is not bimodal
    if(length(m1ind)<10){Mode1 <- NULL}else{Mode1 <- dat[m1ind,1:2]}
    
    if(length(m2ind)<10){Mode2 <- NULL}else{Mode2 <- dat[m2ind,1:2]}
    
    if(mode==1){
      if (is.null(Mode1)==T){
        output$notbimodal <- renderText({paste("Distribution not biomodal, or N (mode) <10")})
        return(NULL)
      }else{
        B95_result <- PmagDiR::Boots_conf_DI(DI = Mode1,n_boots = n_boots,p = p, mode=mode,shiny=TRUE)
        B95_result$DI <- Mode1
        output$notbimodal <- NULL
        return(B95_result)
      }
    }
    else if(mode==2){
      if (is.null(Mode2)==T){
        output$notbimodal <- renderText({paste("Distribution not biomodal, or N (mode) <10")})
        return(NULL)
      }else{
        B95_result <- PmagDiR::Boots_conf_DI(DI = Mode2,n_boots = n_boots,p = p, mode=mode,shiny=TRUE)
        B95_result$DI <- Mode2
        output$notbimodal <- NULL
        return(B95_result)
      }
    }
  }
  
  #perform calculation
  observeEvent(input$B95_mode_1_go,{
    B95$result_1 <- B95_calculation(mode=1)
  })
  observeEvent(input$B95_mode_2_go,{
    B95$result_2 <- B95_calculation(mode=2)
  })
  
  #clear analysis and plot
  observeEvent(input$B95_clear,{
    B95$result_1 <- NULL
    B95$result_2 <- NULL
  })
  
  #plotting function
  plot_B95_all <- function(){
    if(!is.null(B95$result_1) || !is.null(B95$result_2)) {PmagDiR::equalarea()}
    
    #define colors Down-pointing
    if(input$colD==1) colD <- "black"
    if(input$colD==2) colD <- "blue"
    if(input$colD==3) colD <- "red"
    if(input$colD==4) colD <- "dark green"
    
    #define color Up-pointing
    if(input$colU==1) colU <- "white"
    if(input$colU==2) colU <- "cyan"
    if(input$colU==3) colU <- "pink"
    if(input$colU==4) colU <- "light green"
    
    #define symbol
    if(input$sym==1) sym <- "c"
    if(input$sym==2) sym <- "s"
    if(input$sym==3) sym <- "d"
    if(input$sym==4) sym <- "t"
    
    if(!is.null(B95$result_1)){
      if(input$B95_dirs==1){PmagDiR::plot_DI(B95$result_1[["DI"]],on_plot = T,symbol = sym,col_d = colD,col_u = colU)}
      PmagDiR::plot_B95(B95$result_1[["aver_DI"]][["dec"]],
                        B95$result_1[["aver_DI"]][["inc"]],
                        B_conf = B95$result_1[["ellipses"]],
                        on_plot = TRUE)
    }
    if(!is.null(B95$result_2)){
      if(input$B95_dirs==1){PmagDiR::plot_DI(B95$result_2[["DI"]],on_plot = T,symbol = sym,col_d = colD,col_u = colU)}
      PmagDiR::plot_B95(B95$result_2[["aver_DI"]][["dec"]],
                        B95$result_2[["aver_DI"]][["inc"]],
                        B_conf = B95$result_2[["ellipses"]],
                        on_plot = TRUE)
    }
  }
  
  #send plot to UI
  output$B95_test <- renderPlot({
    plot_B95_all()
  },width = 700,height = 700)
  
  
  #Export B95 statistic, average dec inc and dec inc of ellipses
  output$B95_stat <- downloadHandler(
    filename = function() {
      paste(input$fileN_B95,"_B95_conf_", Sys.Date(), ".csv", sep="")
    },
    content = function(file) {
      B95_table_to_export <- matrix(ncol = 8,nrow = 181)
      colnames(B95_table_to_export) <- c("M1_dec","M1_inc","E1_dec","E1_inc","M2_dec","M2_inc","E2_dec","E2_inc")
      if(!is.null(B95$result_1)){
        B95_table_to_export[1,1]<- B95$result_1[["aver_DI"]][["dec"]]
        B95_table_to_export[1,2]<- B95$result_1[["aver_DI"]][["inc"]]
        B95_table_to_export[1:181,3]<- B95$result_1[["ellipses"]][["dec"]]
        B95_table_to_export[1:181,4]<- B95$result_1[["ellipses"]][["inc"]]
      }
      if(!is.null(B95$result_2)){
        B95_table_to_export[1,5]<- B95$result_2[["aver_DI"]][["dec"]]
        B95_table_to_export[1,6]<- B95$result_2[["aver_DI"]][["inc"]]
        B95_table_to_export[1:181,7]<- B95$result_2[["ellipses"]][["dec"]]
        B95_table_to_export[1:181,8]<- B95$result_2[["ellipses"]][["inc"]]
      }
      write.csv(as.data.frame(B95_table_to_export),file,row.names = F)
    }
  )
  
  output$B95_graph <- downloadHandler(
    filename = function() {
      paste(input$fileN_B95,"_B95_conf_", Sys.Date(), ".pdf", sep="")
    },
    content = function(file) {
      pdf(file, onefile = TRUE,width = 10,height = 10)
      plot_B95_all()
      dev.off()
    }
  )
  
  output$B95_result_text1 <- renderText({
    if(!is.null(B95$result_1)){
      paste("Mode 1 - Dec:", round(B95$result_1[["aver_DI"]][["dec"]],digits = 1),"Inc: ",round(B95$result_1[["aver_DI"]][["inc"]],digits = 1))
    }else{NULL}
  })  
  output$B95_result_text2 <- renderText({
    if(!is.null(B95$result_2)){
      paste("Mode 1 - Dec:", round(B95$result_2[["aver_DI"]][["dec"]],digits = 1),"Inc: ",round(B95$result_2[["aver_DI"]][["inc"]],digits = 1))
    }else{NULL}
  })
  ############ END OF BOOTSTRAPPED CONFIDENCE MODULE
  
  ############ REVERSAL TEST MODULE#############
  #create result reactive file
  CMDT <- reactiveValues(result=NULL)
  
  #create function that reversal test and save statistics and graph
  revtest_plot <- eventReactive(input$revgo,{
    
    #reversal test
    revtest_funct <- function(){
      DI <- fix_DI(Dirs$dat)
      RVresult <- PmagDiR::CMDT_H23(DI,n_boots=input$revnb,Shiny=T)
      return(RVresult)
    }
    
    #save statistic and makes plot
    CMDT$result <- revtest_funct()
    #record plot
    revPlot <- recordPlot()
    #Export reversal test graphic
    output$revexpG <- downloadHandler(
      filename = function() {
        paste(input$fileN_RT,"_revtest_", Sys.Date(), ".pdf", sep="")
      },
      content = function(file) {
        pdf(file, onefile = TRUE,width = 15,height = 10)
        replayPlot(revPlot)
        dev.off()
      }
    )
  })
  
  #Export reversal test stats
  output$CMDT_ellipsis <- downloadHandler(
    filename = function() {
      paste(input$fileN_RT,"_CM_ellipsis_", Sys.Date(), "_stat.csv", sep="")
    },
    content = function(file) {
      if(!is.null(CMDT$result)){
        CMD_ellips <- matrix(ncol = 4,nrow = 181)
        colnames(CMD_ellips) <- c("mean_dec","mean_inc","ell_dec","ell_inc")
        CMD_ellips[1,1] <- round(CMDT$result[["mean_direction"]][["dec"]]%%360,digits = 2)
        CMD_ellips[1,2] <- round(CMDT$result[["mean_direction"]][["inc"]],digits = 2)
        CMD_ellips[,3] <- CMDT$result[["ellipsis"]][["dec"]]%%360
        CMD_ellips[,4] <- CMDT$result[["ellipsis"]][["inc"]]
        write.csv(CMD_ellips, file,row.names = F)
      }
    }
  )
  
  
  output$CMDT_result1 <- renderText({
    if(!is.null(CMDT$result)){paste("V3 obs.:", round(CMDT$result[["CMDT_value"]],digit=2))}
  })
  output$CMDT_result2 <- renderText({
    if(!is.null(CMDT$result)){paste("V3 crit.:", round(CMDT$result[["CMDT_critical_value"]], digit=2))}
  })
  output$CMDT_result3 <- renderText({
    if(!is.null(CMDT$result)){paste(ifelse(CMDT$result[["CMDT_value"]]>CMDT$result[["CMDT_critical_value"]],
                                           "Test not passed","Test passed"))}
  })
  output$CMDT_result4 <- renderText({
    if(!is.null(CMDT$result[["mean_direction"]])){
      paste("Comm. Dec.:",round((CMDT$result[["mean_direction"]][["dec"]])%%360, digits = 1),"-","Comm. Inc.:",
            round(CMDT$result[["mean_direction"]][["inc"]], digits = 1))}
  })
  
  #execute reversal test
  output$revtest <- renderPlot({
    revtest_plot()
  },width = 1200,height = 700)
  ############ END OF REVERSAL TEST MODULE
  
  ############ CMDT OF TWO DATASETS MODULE##############
  second_DI <- observeEvent(input$CMDT_2_file,{
    dat <- read.csv(input$CMDT_2_file$datapath)
    CMDT$DI2 <- dat
  })
  
  #create function that reversal test and save statistics and graph
  revtest_plot2 <- eventReactive(input$revgo2,{
    #reversal test
    CMDT_funct <- function(){
      req(Dirs$dat)
      req(CMDT$DI2)
      DI <- fix_DI(Dirs$dat)
      DI2 <- CMDT$DI2
      DI2 <- DI2[,1:2]
      RVresult2 <- PmagDiR::CMDT_H23_2DiRs(DI = DI,DI2 = DI2,n_boots=input$revnb2,Shiny=T)
      return(RVresult2)
    }
    
    #save statistic and makes plot
    CMDT$result2 <- CMDT_funct()
    #record plot
    revPlot2 <- recordPlot()
    #Export reversal test graphic
    output$revexpG2 <- downloadHandler(
      filename = function() {
        paste(input$fileN_RT2,"_CMDT_", Sys.Date(), ".pdf", sep="")
      },
      content = function(file) {
        pdf(file, onefile = TRUE,width = 15,height = 10)
        replayPlot(revPlot2)
        dev.off()
      }
    )
  })
  
  #Export reversal test stats
  output$CMDT_ellipsis2 <- downloadHandler(
    filename = function() {
      paste(input$fileN_RT2,"_CM_ellipsis_", Sys.Date(), "_stat.csv", sep="")
    },
    content = function(file) {
      if(!is.null(CMDT$result2)){
        CMD_ellips <- matrix(ncol = 4,nrow = 181)
        colnames(CMD_ellips) <- c("mean_dec","mean_inc","ell_dec","ell_inc")
        CMD_ellips[1,1] <- round(CMDT$result2[["mean_direction"]][["dec"]]%%360,digits = 2)
        CMD_ellips[1,2] <- round(CMDT$result2[["mean_direction"]][["inc"]],digits = 2)
        CMD_ellips[,3] <- CMDT$result2[["ellipsis"]][["dec"]]%%360
        CMD_ellips[,4] <- CMDT$result2[["ellipsis"]][["inc"]]
        write.csv(CMD_ellips, file,row.names = F)
      }
    }
  )
  output$CMDT_DI2 <- renderPlot({
    PmagDiR::plot_DI(DI = CMDT$DI2)
  },width = 550,height = 550)
  
  observeEvent(input$show_DI2,{
    req(CMDT$DI2)
    showModal(jqui_draggable(modalDialog(
      size="m",
      tags$h4("Equal area of the second set of directions"),
      plotOutput(outputId = "CMDT_DI2",height = 520),
      br(),
      easyClose = TRUE,
      footer=tagList(
        modalButton('close')
      )
    ),options = list(cancel = ".shiny-input-container")))
  })
  
  output$CMDT_result1_2 <- renderText({
    if(!is.null(CMDT$result2)){paste("V3 obs.:", round(CMDT$result2[["CMDT_value"]],digit=2))}
  })
  output$CMDT_result2_2 <- renderText({
    if(!is.null(CMDT$result2)){paste("V3 crit.:", round(CMDT$result2[["CMDT_critical_value"]], digit=2))}
  })
  output$CMDT_result3_2 <- renderText({
    if(!is.null(CMDT$result2)){paste(ifelse(CMDT$result2[["CMDT_value"]]>CMDT$result2[["CMDT_critical_value"]],
                                            "Test not passed","Test passed"))}
  })
  output$CMDT_result4_2 <- renderText({
    if(!is.null(CMDT$result2[["mean_direction"]])){
      paste("Comm. Dec.:",round((CMDT$result2[["mean_direction"]][["dec"]])%%360, digits = 1),"-","Comm. Inc.:",
            round(CMDT$result2[["mean_direction"]][["inc"]], digits = 1))}
  })
  
  #execute CMDT test
  output$revtest2 <- renderPlot({
    revtest_plot2()
  },width = 1200,height = 700)
  
  ############ END OF CMDT OF TWO DATASETS MODULE
  
  
  ##################### SVEI MODULE ###################
  # Restituisce la lista con i parametri del modello GGP richiesto 
  GGPmodels <- function(model = 'THG24') {
    CP88 <- data.frame(t(c(g10 = -30, g20 = -1.8, g30 = 0.0, sig10 = 3.0, sig11 = 3.0,
                           sig20 = 0.0, sig21 = 0.0, sig22 = 0.0, alpha = 27.7, beta = 1.0, name = 'CP88')))
    QC96 <- data.frame(t(c(g10 = -30, g20 = -1.2, g30 = 0.0, sig10 = 3.0, sig11 = 3.0,
                           sig20 = 1.3, sig21 = 4.3, sig22 = 1.3, alpha = 27.7, beta = 1.0, name = 'QC96')))
    CJ98 <- data.frame(t(c(g10 = -30, g20 = -1.5, g30 = 0.0, sig10 = 11.72, sig11 = 1.67,
                           sig20 = 1.16, sig21 = 4.06, sig22 = 1.16, alpha = 15.0, beta = 1.0, name = 'CJ98')))
    TK03 <- data.frame(t(c(g10 = -18, g20 = 0.0, g30 = 0.0, sig10 = 0.0, sig11 = 0.0,
                           sig20 = 0.0, sig21 = 0.0, sig22 = 0.0, alpha = 7.5, beta = 3.8, name = 'TK03')))
    BCE19 <- data.frame(t(c(g10 = -18, g20 = 0.0, g30 = 0.0, sig10 = 0.0, sig11 = 0.0,
                            sig20 = 0.0, sig21 = 0.0, sig22 = 0.0, alpha = 6.7, beta = 4.2, name = 'BCE19')))
    THG24 <- data.frame(t(c(g10 = -18, g20 = -0.1, g30 = 0.4, sig10 = 0.0, sig11 = 0.0,
                            sig20 = 0.0, sig21 = 0.0, sig22 = 0.0, alpha = 7.25, beta = 3.75, name = 'THG24')))
    models <- list(CP88, QC96, CJ98, TK03, BCE19, THG24)
    for (m in models) {
      if (m$name == model) {
        #convert numbers from character to numbers
        m[,1:10] <- as.numeric(m[,1:10])
        return(m)
      }
    }
  }         
  
  #Calculates the zonal field for a given latitude and returns the horizontal and vertical components Btetha, Br.
  zonal <- function(lat,g10=-18,G2=0.0,G3=0.0,G4=0.0,a_r=1.0) {
    # Parameters:
    # lat: Latitude in degrees
    # g10: Gauss coefficient g10
    # G2: Gauss coefficient G2
    # G3: Gauss coefficient G3 
    # G4: Gauss coefficient G4 
    # a_r: Earth radius divided by the distance from the center of the Earth 
    # Returns:
    # tuple: List containing the following elements:
    # Bttot (float): Total horizontal component Btetha.
    # Brtot (float): Total vertical component Br.
    
    Theta<-90-lat
    g20<-G2*g10
    g30<-G3*g10
    g40<-G4*g10
    costheta<-cos(pi/180*Theta)
    sintheta<-sin(pi/180*Theta)
    Br1<-2*(a_r^3)*g10*costheta
    Br2<-(3*g20/2)*(a_r^4)*(3*costheta^2-1)
    Br3<-(a_r^5)*(2*g30)*(5*costheta^3-3*costheta)
    Br4<-(a_r^6)*(5*g40/8)*(35*costheta^4-30*costheta^2+3)
    Bt1<-(a_r^3)*g10*sintheta
    Bt2<-(a_r^4)*(3*g20)*sintheta*costheta
    Bt3<-(a_r^5)*(g30/2)*(15*sintheta*costheta^2-3*sintheta)
    Bt4<-(a_r^6)*(g40/2)*(35*sintheta*costheta^3-15*sintheta*costheta)
    Brtot<-Br1+Br2+Br3+Br4
    Bttot<-Bt1+Bt2+Bt3+Bt4
    return(list(Bttot=Bttot,Brtot=Brtot))
  }
  
  #Calculates the Time Average Field (TAF) for a given GGP model dictionary.
  m_TAF <- function(GGPmodel="THG24", lat) {
    # Parameters:
    #   GGPmodel (dict): GGP model dictionary containing the following keys:
    # 'g10': Gauss coefficient g10.
    # 'g20': Gauss coefficient g20.
    # 'g30': Gauss coefficient g30.
    # 
    # lat: Latitude in degrees.
    # 
    # Returns:
    #   ndarray: Array `m` representing the time average field with shape (3,).
    # m[0]: Bx component (negative Btetha).
    # m[1]: By component (always 0).
    # m[2]: Bz component (negative Br).
    GGPmodel <- GGPmodels(GGPmodel)
    
    z <- zonal(lat = lat,g10 =GGPmodel[1],G2 = GGPmodel[2]/GGPmodel[1],G3 = GGPmodel[3]/GGPmodel[1])
    m <- matrix(ncol = 1,nrow = 3)
    m[1,1] <- -z[["Bttot"]][["g10"]]
    m[2,1] <- 0
    m[3,1] <- -z[["Brtot"]][["g10"]]
    return(m)
  }
  
  # Calcola la varianza nella direzione theta (Nord-Sud) del campo magnetico.
  # Uses the s_lm2() and dP_lm_dt() functions.
  sig_bt2 <- function(l,alpha,beta,theta,sig10_2,sig11_2,sig20_2,sig21_2,sig22_2) {
    sum_l<-0
    for(i in 1:l) {
      A<-s_lm2(i,0,alpha,beta,sig10_2,sig11_2,sig20_2,sig21_2,sig22_2)*(dP_lm_dt(i,0,theta)^2)
      sum_m<-0
      for(j in 1:i) {
        B<-(factorial(i-j)/factorial(i+j))*s_lm2(i,j,alpha,beta,sig10_2,sig11_2,sig20_2,sig21_2,sig22_2)*(dP_lm_dt(i,j,theta)^2)
        sum_m<-sum_m+B
      }
      sum_l<-sum_l+A+2*sum_m
    }
    return(sum_l)
  }
  
  #Calculate the covariance between Br and Btheta for a given maximum degree and co-latitude.
  # Uses the s_lm2(), P_lm(), and dP_lm_dt() functions.
  cov_br_bt <- function(l,alpha,beta,theta,sig10_2,sig11_2,sig20_2,sig21_2,sig22_2) {
    sum_l<-0
    for(i in 1:l) {
      A<-s_lm2(i,0,alpha,beta,sig10_2,sig11_2,sig20_2,sig21_2,sig22_2)*P_lm(i,0,theta)*dP_lm_dt(i,0,theta)
      sum_m<-0
      for(j in 1:i) {
        B<-(factorial(i-j)/factorial(i+j))*s_lm2(i,j,alpha,beta,sig10_2,sig11_2,sig20_2,sig21_2,sig22_2)*P_lm(i,j,theta)*dP_lm_dt(i,j,theta)
        sum_m<-sum_m+B
      }
      sum_l<-sum_l-(i+1)*(A+2*sum_m)
    }
    return(sum_l)
  }
  
  # Calcola la varianza nella direzione phi (Est-Ovest) del campo magnetico.
  # Uses the s_lm2() and P_lm() functions.
  sig_bph2 <- function(l,alpha,beta,theta,sig10_2,sig11_2,sig20_2,sig21_2,sig22_2) {
    sum_l<-0
    if(theta==0) {theta<-1.745329e-08}
    for(i in 1:l) {
      sum_m<-0
      for(j in 1:i) {
        B<-(j^2)*(factorial(i-j)/factorial(i+j))*s_lm2(i,j,alpha,beta,sig10_2,sig11_2,sig20_2,sig21_2,sig22_2)*(P_lm(i,j,theta)^2)
        sum_m<-sum_m+B
      }
      sum_l<-sum_l+2*sum_m/(sin(theta)^2)
    }
    return(sum_l)
  }
  
  
  #Calculate the variance in the r-direction for the magnetic field components.
  # Uses the s_lm2() and P_lm() functions.
  sig_br2 <- function(l,alpha,beta,theta,sig10_2,sig11_2,sig20_2,sig21_2,sig22_2) {
    sum_l<-0
    for(i in 1:l) {
      A<-((i+1)^2)*s_lm2(i,0,alpha,beta,sig10_2,sig11_2,sig20_2,sig21_2,sig22_2)*(P_lm(i,0,theta)^2)
      sum_m<-0
      for(j in 1:i) {
        B<-(factorial(i-j)/factorial(i+j))*s_lm2(i,j,alpha,beta,sig10_2,sig11_2,sig20_2,sig21_2,sig22_2)*(P_lm(i,j,theta)^2)
        sum_m<-sum_m+B
      }
      sum_l<-sum_l+A+((i+1)^2)*2*sum_m
    }
    return(sum_l)
  }
  
  # Calcolo della varianza per ciascun coefficiente di Gauss
  s_lm2 <- function(l, m, alpha, beta, sig10_2, sig11_2, sig20_2, sig21_2, sig22_2) {
    c_a <- 0.547
    if(((l-m)/2-floor((l-m)/2))==0) {
      s_lm2 <- ((c_a^(2*l))*(alpha^2))/((l+1)*(2*l+1))
    }else{
      s_lm2 <- ((c_a^(2*l))*((alpha*beta)^2))/((l+1)*(2*l+1))
    }
    if (l==1 && m==0 && sig10_2>0){s_lm2 <- sig10_2}
    if (l==1 && m==1 && sig11_2>0){s_lm2 <- sig11_2}
    if (l==2 && m==0 && sig20_2>0){s_lm2 <- sig20_2}
    if (l==2 && m==1 && sig21_2>0){s_lm2 <- sig21_2}
    if (l==2 && m==2 && sig22_2>0){s_lm2 <- sig22_2}
    return(s_lm2)
  }
  
  #Calcolo della funzione associata di Legendre
  P_lm <- function(l,m,theta) {
    A <- sin(theta)^m/2^l
    sum_p_lm <- 0
    for (t in 0:floor((l-m)/2)) {
      B <- (-1)^t * factorial(2*l-2*t)
      C <- factorial(t) * factorial(l-t)*factorial(l-m-2*t)
      D <- l-m-2*t
      sum_p_lm <- sum_p_lm + (B/C)*cos(theta)^D
    }
    return(A*sum_p_lm)
  }
  
  # Calcolo della derivata della funzione associata di Legendre rispetto a theta
  dP_lm_dt <- function(l,m,theta) {
    A <- (sin(theta)^m) / (2^l)
    if (m == 0) {
      A2 <- 0
    } else {
      A2 <- m * (sin(theta)^(m-1))*cos(theta)/(2^l)
    }
    sum_p_lm <- 0
    sum_p_lm2 <- 0
    
    for (t in 0:floor((l-m)/2)) {
      B <- (-1)^t * factorial(2*l-2*t)
      C <- factorial(t)*factorial(l-t)*factorial(l-m-2*t)
      D <- l-m-2*t
      sum_p_lm <- sum_p_lm+(B/C) * (cos(theta)^D)
    }
    
    for (t in 0:floor((l-m)/2)) {
      B <- ((-1)^t)*factorial(2*l-2*t)
      C <- factorial(t)*factorial(l-t)*factorial(l-m-2*t)
      D <- l-m-2*t
      sum_p_lm2 <- sum_p_lm2+sin(theta)*(B*D/C)*(cos(theta)^(D-1))
    }
    
    return(A2*sum_p_lm-A*sum_p_lm2)
  }
  
  #Calculate the covariance matrix using the provided GGP model and latitude.
  # Uses the sig_bt2(), cov_br_bt(), sig_bph2(), and sig_br2() functions.
  Cov_modelo <- function(GGPmodel="THG24",lat,degree) {
    GGPmodel <- GGPmodels(model = GGPmodel)
    sig10_2<-GGPmodel$sig10^2
    sig11_2<-GGPmodel$sig11^2
    sig20_2<-GGPmodel$sig20^2
    sig21_2<-GGPmodel$sig21^2
    sig22_2<-GGPmodel$sig22^2
    alpha<-GGPmodel$alpha
    beta<-GGPmodel$beta
    theta<-pi/180*(90-lat)
    Cov<-matrix(0,3,3)
    
    Cov[1,1]<-sig_bt2(degree,alpha,beta,theta,sig10_2,sig11_2,sig20_2,sig21_2,sig22_2)       
    Cov[1,3]<-cov_br_bt(degree,alpha,beta,theta,sig10_2,sig11_2,sig20_2,sig21_2,sig22_2)
    Cov[2,2]<-sig_bph2(degree,alpha,beta,theta,sig10_2,sig11_2,sig20_2,sig21_2,sig22_2)
    Cov[3,1]<-cov_br_bt(degree,alpha,beta,theta,sig10_2,sig11_2,sig20_2,sig21_2,sig22_2)
    Cov[3,3]<-sig_br2(degree,alpha,beta,theta,sig10_2,sig11_2,sig20_2,sig21_2,sig22_2)
    return(Cov)
  }
  
  #requires m_TAF and Cov_modelo
  GGP_vMF_cdfs <- function(GGPmodel="THG24", lat, degree, flat=1, kappa=-1, n=2e6) {
    n <- as.integer(n)
    m <- m_TAF(GGPmodel, lat)       
    Cov <- Cov_modelo(GGPmodel, lat, degree)     
    XYZ <- MASS::mvrnorm(n, mu=m, Sigma=Cov) * 1000
    XYZ <- XYZ/sqrt(rowSums(XYZ^2))
    
    if(flat < 1) {
      DI <- PmagDiR::c2s(XYZ)           
      DI<- PmagDiR::flat_DI(DI, f=flat)  
      XYZ <- PmagDiR::s2c(DI)
    }
    
    if(kappa > 0) {
      C <- diag(1 / kappa, 3)
      XYZ <- XYZ + MASS::mvrnorm(n, mu=c(0,0,0), Sigma=C)
      XYZ <- XYZ / sqrt(rowSums(XYZ^2))
    }
    
    DI <- PmagDiR::c2s(XYZ)
    DI[DI[,1] > 180, 1] <- DI[DI[,1] > 180, 1] - 360
    
    nI <- 181
    nD <- 361
    I0 <- seq(-90, 90, length.out=nI)
    D0 <- seq(-180, 180, length.out=nD)
    Ic <- c()
    for (I in I0) {
      Ic <- c(Ic, sum(DI[,2] <= I) / n)
    }
    Dc <- c()
    for (D in D0) {
      Dc <- c(Dc, sum(DI[,1] <= D) / n)
    }  
    Icdf <- splinefun(I0*pi/180, Ic, method="monoH.FC")
    Dcdf <- splinefun(D0*pi/180, Dc, method="monoH.FC")
    
    return(list(Icdf=Icdf, Dcdf=Dcdf))
  }   
  
  GGPrand <- function(GGPmodel="THG24", lat, n, degree = 8) {
    m <- m_TAF(GGPmodel=GGPmodel, lat=lat)                  
    Cov <- Cov_modelo(GGPmodel=GGPmodel, lat=lat, degree=degree)   
    X <- MASS::mvrnorm(n = n, mu = m, Sigma = Cov) * 1000
    DI <- PmagDiR::c2s(X)
    return(DI)
  }
  
  
  # Anderson-Darling test per la distribuzione delle inclinazioni
  # Is: inclinazioni osservate in gradi
  # Icdf: funzione cumulativa (CDF) in radianti
  AD_inc <- function(Is, Icdf) {
    # Parameters:
    # Is (array-like): Array of observed inclinations in degrees.
    # Icdf (callable): Pchip function representing the cumulative distribution function (CDF) of inclinations for a given latitude.
    # 
    # Returns:
    # Test statistic (A2) for inclinations.
    # 
    # Notes:
    # The Anderson-Darling (AD) test is a statistical test used to assess whether a sample of data comes from a specific probability distribution. This function calculates the AD test statistic for the distribution of inclinations based on the observed inclinations and the CDF of inclinations.
    # The AD test statistic measures the discrepancy between the observed data and the expected distribution. A larger AD test statistic indicates a greater discrepancy, suggesting that the observed data may not follow the expected distribution.
    # It is important to note that this function assumes the input inclinations are in degrees and converts them to radians internally for consistency with the CDF function.
    
    Is <- sort(Is * pi / 180)  # ordina e converte in radianti
    ns <- length(Is)
    
    C <- Icdf(Is)  # valori CDF per i dati ordinati
    i <- 1:ns
    S <- sum((2 * i - 1) / ns * (log(C) + log(1 - rev(C))))
    
    A2 <- -ns - S
    return(A2)
  }
  
  # Anderson-Darling test per la distribuzione delle declinazioni
  # Ds: declinazioni osservate in gradi
  # Dcdf: funzione CDF (in radianti) per le declinazioni
  AD_dec <- function(Ds, Dcdf) {
    # Parameters:
    #   Ds: declinations in degrees.
    # Dcdf: Pchip function representing the cumulative distribution function (CDF) of declinations for a given latitude.
    # 
    # Returns:
    #   Test statistic (A2) for declinations.
    
    Ds[Ds > 180] <- Ds[Ds > 180] - 360  # normalizza nel range [-180, 180]
    Ds <- sort(Ds * pi / 180)  # ordina e converti in radianti
    ns <- length(Ds)
    
    C <- Dcdf(Ds)  # valuta la CDF
    i <- 1:ns
    S <- sum((2 * i - 1) / ns * (log(C) + log(1 - rev(C))))
    
    A2 <- -ns - S
    return(A2)
  }
  
  #Perform the Anderson-Darling (AD) test on observed inclinations and declinations and Monte Carlo simulation to estimate 95% confidence bounds on V2dec and E
  svei_test <- function(DI, model_name = 'THG24', degree = 8, kappa = -1, num_sims = 1000,Shiny=F,EI_test=F,Plot=T) {
    #   Returns:
    #   kappa (float): kappa used in simulations
    #   H: 0 if the null hypothesis cannot be rejected, 1 otherwise.
    #   A2I: Test statistic for inclinations.
    #   A2D: Test statistic for declinations.
    #   pID: Representation of combined p-values (can be used for minimization to optimize latitude).
    #   lat: Latitude of the site.
    #   V2dec: Declination of minor principle component of data set. 
    #   V2sim_min, V2sim_max : Bounds on V2dec from Monte Carlo simulation
    #   V2_result : 0 if V2dec not in bounds, 1 if in bounds (consistent)
    #   E: Elongation (tau2/tau3) of data set 
    #   Esim_min, Esim_max : Bounds on elongation from Monte Carlo simulation
    #   E_result : 0 if V2dec not in bounds, 1 if in bounds (consistent)
    
    A2ref <- c(0.025, 0.050, 0.075, 0.100, 0.125, 0.150, 0.175, 0.200, 0.225, 0.250, 0.275,
               0.300, 0.325, 0.350, 0.375, 0.400, 0.425, 0.450, 0.475, 0.500, 0.525, 0.550,
               0.575, 0.600, 0.625, 0.650, 0.675, 0.700, 0.750, 0.800, 0.850, 0.900, 0.950,
               1.000, 1.050, 1.100, 1.150, 1.200, 1.250, 1.300, 1.350, 1.400, 1.450, 1.500,
               1.550, 1.600, 1.650, 1.700, 1.750, 1.800, 1.850, 1.900, 1.950, 2.000, 2.050,
               2.100, 2.150, 2.200, 2.250, 2.300, 2.350, 2.400, 2.450, 2.500, 2.550, 2.600,
               2.650, 2.700, 2.750, 2.800, 2.850, 2.900, 2.950, 3.000, 3.050, 3.100, 3.150,
               3.200, 3.250, 3.300, 3.350, 3.400, 3.450, 3.500, 3.550, 3.600, 3.650, 3.700,
               3.750, 3.800, 3.850, 3.900, 3.950, 4.000, 4.050, 4.100, 4.150, 4.200, 4.250,
               4.300, 4.350, 4.400, 4.500, 4.600, 4.700, 4.800, 4.900, 5.000, 5.500, 6.000,
               7.000, 8.000)
    
    pref <- c(1.0000,1.0000,1.0000,1.0000,0.9997,0.9986,0.9958,0.9904,0.9820,0.9704,
              0.9557,0.9382,0.9183,0.8964,0.8731,0.8487,0.8236,0.7981,0.7724,0.7468,
              0.7214,0.6964,0.6719,0.6480,0.6247,0.6070,0.5801,0.5588,0.5185,0.4810,
              0.4463,0.4142,0.3846,0.3573,0.3320,0.3088,0.2873,0.2676,0.2497,0.2323,
              0.2167,0.2027,0.1889,0.1765,0.1650,0.1543,0.1444,0.1352,0.1266,0.1186,
              0.1112,0.1043,0.0979,0.0918,0.0862,0.0810,0.0761,0.0715,0.0672,0.0632,
              0.0595,0.0559,0.0526,0.0496,0.0466,0.0439,0.0414,0.0390,0.0367,0.0346,
              0.0326,0.0308,0.0290,0.0274,0.0258,0.0244,0.0230,0.0217,0.0205,0.0193,
              0.0182,0.0172,0.0163,0.0154,0.0145,0.0137,0.0130,0.0122,0.0116,0.0109,
              0.0103,0.0098,0.0092,0.0087,0.0083,0.0078,0.0078,0.0070,0.0066,0.0062,
              0.0059,0.0056,0.0050,0.0045,0.0040,0.0036,0.0032,0.0029,0.0017,0.0010,
              0.0003,0.0001)
    
    #select models
    GGPmodel <- GGPmodels(model_name)
    
    lat <- NA
    
    DI <- DI[,1:2]
    DI <- na.omit(DI)
    
    #convert data to common mode down pointing
    DIc <- PmagDiR::common_DI(DI,down = T,export = F)
    pars <- PmagDiR::Principal_DiR(DIc)
    
    #find average direction
    fpars <- PmagDiR::fisher(DIc)
    fpars <- fpars[,1:2]
    # if (fpars$dec > 90 && fpars$dec < 270) {
    #   fpars$dec <- (fpars$dec-180)%%360
    # }
    rot_block <- data.frame(matrix(ncol = 2,nrow = nrow(DI)))
    rot_block[,1] <- (DIc[,1]-fpars$dec)%%360
    rot_block[,2] <- DIc[,2]
    
    DI <- rot_block
    Ds <- rot_block[,1]
    Is <- rot_block[,2]
    
    n <- nrow(DI)
    if (n < 5) stop("Insufficient data: N must be ≥ 5")
    
    if(is.na(lat)) {lat <- PmagDiR::findlat(fpars[1,2])}
    
    # Get empirical CDFs from GGP model
    cdfs <- GGP_vMF_cdfs(GGPmodel=model_name, lat=lat, degree=degree, kappa = kappa)  
    Icdf <- cdfs$Icdf
    Dcdf <- cdfs$Dcdf
    
    A2I <- AD_inc(Is, Icdf)                                
    pI <- approx(A2ref, pref, xout = A2I, rule = 2)$y
    
    A2D <- AD_dec(Ds, Dcdf)                                
    pD <- approx(A2ref, pref, xout = A2D, rule = 2)$y
    
    pID <- min(pI, pD)
    H <- ifelse(A2I < 3.07 && A2D < 3.07, 0, 1)
    
    #must remove the declination deviation from V2 dec
    V2dec <- (pars[["vectors"]][["V2dec"]]-pars[["vectors"]][["V1dec"]])%%360
    #V2dec <- (V2dec-fpars$dec)%%360
    if (V2dec < 90 || V2dec > 270) V2dec <- (V2dec-180)%%360
    E <- pars[["values"]][2] / pars[["values"]][3]
    N <- nrow(DIc)                                           
    
    V2decs <- numeric(num_sims)
    Es <- numeric(num_sims)
    count <- 0
    for (sim in 1:num_sims) {
      count <- count+1
      DI <- GGPrand(GGPmodel=model_name, lat=lat, N, degree=degree)        
      if (kappa > 0) {
        fish_DI <- matrix(nrow = N, ncol = 2)
        for (i in 1:N) {
          temp <- PmagDiR::fisher(PmagDiR::fisher_DI_generator(N = 4,k = kappa,Dec = DI[i,1],Inc = DI[i, 2]))
          fish_DI[i,1] <- temp[1,1]
          fish_DI[i,2] <- temp[1,2]
        }
        DI <- data.frame(fish_DI)
        colnames(DI) <- c("dec","inc")
      }
      
      mcpars <- PmagDiR::Principal_DiR(DI)  
      v2 <- mcpars[["vectors"]][["V2dec"]]
      if (v2 < 90 || v2 > 270) v2 <- (v2 - 180)%%360
      V2decs[sim] <- v2
      Es[sim] <- mcpars[["values"]][2]/mcpars[["values"]][3]
      if(Shiny==TRUE){
        updateProgressBar(
          #depends if using for distribution shape or inclination flattening
          id=ifelse(EI_test==F,"svei_test_b","svei_test_EI_b"),
          title = "Simulations",
          value=sim,total=num_sims
        )}else{print(paste(count," simulations done."))}
    }
    
    V2decs <- sort(V2decs)
    Es <- sort(Es)
    
    V2sim_min <- ifelse(floor(num_sims * 0.025)==0,V2decs[1],V2decs[floor(num_sims * 0.025)])
    V2sim_max <- V2decs[ceiling(num_sims * 0.975)]
    Esim_min <- ifelse(floor(num_sims * 0.025)==0,Es[1],Es[floor(num_sims * 0.025)])
    Esim_max <- Es[ceiling(num_sims * 0.975)]
    
    V2_result <- ifelse(V2dec >= V2sim_min && V2dec <= V2sim_max, 1, 0)
    E_result <- ifelse(E >= Esim_min && E <= Esim_max, 1, 0)
    
    if(Plot==T){    
      #graphical part by Edo
      #prepara dati D e I
      D0 <- seq(-180, 180, 1)
      # CDF empirica per Declinazione
      Decdf <- numeric(length(D0))
      #trasforma Ds in -180,180
      Ds <- ifelse(Ds>180,Ds-360,Ds)
      for (i in seq_along(D0)) {
        Decdf[i] <- mean(Ds <= D0[i])
      }
      I0 <- seq(-90, 90, 1)
      # CDF empirica per Inclinazione
      Iecdf <- numeric(length(I0))
      for (i in seq_along(I0)) {
        Iecdf[i] <- mean(Is <= I0[i])
      }
      #upleft quadrant
      par(fig=c(0,0.5,0.5,1))
      par(mar=c(1.8,1.8,1.8,1.8))
      PmagDiR::plot_DI(DI =rot_block,title = paste("Field model:",model_name))
      #upright quadrant
      par(fig=c(0.5,1,0.5,1),new=T)
      par(mar=c(4,4, 4, 4))
      plot(D0, Dcdf(D0*pi/180), 
           xlim=c(-180,180),ylim=c(0,1),
           type="l",col = "red", lwd = 2,
           xaxp=c(-180,180,6),
           xlab = "Decl. (°)",ylab = "Cumulative probability")
      lines(D0, Decdf, col = "black", lty = 2,lwd=1.5)
      text(x=-180,y = 0.87,labels = "Test result:",cex=0.9,pos=4)
      text(x = -180,y = 0.8,
           labels = paste("Decl.: ",
                          ifelse(A2D<3.07,"Positive","Negative"),sep = ""),
           col = "red",cex=0.9,pos=4)
      text(x=-180,y=0.73,
           labels = paste("Incl.: ",
                          ifelse(A2I<3.07,"Positive","Negative"),sep = ""),
           col="blue",cex=0.9,pos=4)
      
      par(new = TRUE)
      plot(x = I0,y = Icdf(I0*pi/180),
           type="l",col="blue",lwd=2,
           xlim=c(-90,90),ylim=c(0,1),
           xlab=NA,ylab = NA,axes = FALSE)
      lines(x = I0,y = Iecdf,col="black",lty=2,lwd=1.5)
      axis(3,col = "black",xaxp=c(-90,90,4))
      mtext("Incl. (°)",side=3,line=3)
      
      #downleft quadrant
      par(fig=c(0,0.5,0.05,0.55),new=T)
      par(mar=c(4,4, 4, 4))
      #define v2 dec min and max
      V2xmin <- ifelse(min(V2decs)>V2dec,V2dec,(min(V2decs)))
      V2xmax <- ifelse(max(V2decs)<V2dec,V2dec,max(V2decs))
      plot(x=V2decs, y=(1:length(V2decs))/length(V2decs),
           type = "l",col="purple",lwd=2,
           xlim = c(V2xmin,V2xmax),
           xlab = "Simulated E Decl.(°)",
           ylab="Cumulative distribution")
      rect(xleft = V2sim_min,0,xright = V2sim_max,ytop = 1,col = rgb(1,0,0,0.25),border = NA)
      #  abline(v=c(V2sim_min,V2sim_max),col="black",lty=2)
      text(x = V2sim_min-1,y = 0.3,
           labels = paste("Lower conf.: ", round(V2sim_min,digits = 1),"°",sep=""), 
           pos = 3, srt=90, cex=0.8, col="black")
      text(x = V2sim_max-1,y = 0.3,
           labels = paste("Upper conf.: ", round(V2sim_max,digits = 1),"°",sep=""), 
           pos = 3, srt=90, cex=0.8, col="black")
      abline(v = V2dec,col="darkgreen",lwd=2)
      text(x = V2dec-1,y = 0.7,
           labels = paste("E Decl.: ", round(V2dec,digits = 1),"°",sep=""), 
           pos = 3, srt=90, cex=0.8, col="darkgreen")
      
      #downright quadrant
      par(fig=c(0.5,1,0.05,0.55),new=T)
      Exmin <- ifelse(min(Es)>E,E,(min(Es)))
      Exmax <- ifelse(max(Es)<E,E,max(Es))
      
      plot(x = Es,y = (1:length(Es))/length(Es),
           type = "l",col="purple",lwd=2,
           xlim = c(Exmin, Exmax),
           xlab = "Simulated E",ylab = "Cumulative distribution")
      rect(xleft = Esim_min,ybottom = 0,xright = Esim_max,ytop = 1,col = rgb(1,0,0,0.25), border = NA )
      abline(v = E,col="darkgreen",lwd=2)
      text(x = E-0.02,y=0.7,labels = paste("Real E: ",round(E, digits = 2),sep=""),
           pos=3,srt=90,cex=0.8, col = "darkgreen")
      #  abline(v=c(Esim_min,Esim_max), lty=2,col="black")
      text(x = Esim_min-0.02,y = 0.3,
           labels = paste("Lower conf.: ", round(Esim_min,digits = 2),sep=""), 
           pos = 3, srt=90, cex=0.8, col="black")
      text(x = Esim_max-0.02,y = 0.3,
           labels = paste("Upper conf.: ", round(Esim_max,digits = 2),sep=""), 
           pos = 3, srt=90, cex=0.8, col="black")
    }
    #return results
    return(list(
      kappa = kappa,
      lat = round(lat, 1),
      A2D = round(A2D, 4),
      A2I = round(A2I, 4),
      pID = round(pID, 4),
      H = H,
      V2dec = round(V2dec, 1),
      V2sim_min = round(V2sim_min, 1),
      V2sim_max = round(V2sim_max, 1),
      E = round(E, 4),
      Esim_min = round(Esim_min, 4),
      Esim_max = round(Esim_max, 4),
      V2_result = V2_result,
      E_result = E_result
    ))
  }
  
  #reactive function
  SVEI_testPlot <- eventReactive(input$SVEIgo,{
    #set parameters
    #MODEL NAME IS DEACTIVATED, IT WORKS WITH THG24 BY DEFAULT
    # if(input$model_name==1) modelname <- "THG24"
    # if(input$model_name==2) modelname <- "TK03"
    # if(input$model_name==3) modelname <- "CP88"
    # if(input$model_name==4) modelname <- "QC96"
    # if(input$model_name==5) modelname <- "CJ98"
    # if(input$model_name==6) modelname <- "BCE19"
    if(input$SVEI_k==1) sveikappa <- -1
    if(input$SVEI_k==2) sveikappa <- 50
    if(input$SVEI_k==3) sveikappa <- 100
    
    DI <- fix_DI(Dirs$dat)
    SVEI_result <- svei_test(DI = DI,
                             model_name = "THG24",
                             kappa = sveikappa,
                             num_sims = input$SVEI_n, Shiny=T)
  })
  
  output$SVEI_test_fig <- renderPlot({
    SVEI_testPlot()
    SVEI_plot <- recordPlot()
    output$SVEIexp <- downloadHandler(
      filename = function() {
        paste(input$sveiEXPname,"_SVEI_test_", Sys.Date(), ".pdf", sep="")
      },
      content = function(file) {
        pdf(file, onefile = TRUE,width = 12,height = 10.5)
        replayPlot(SVEI_plot)
        dev.off()
      }
    )
  },width = 850,height = 700)
  
  ######### INCLINATION FLATTENING SUBROUTINE
  
  #function for the inclination flattening routine
  find_flatteR <- function(DI,kappa =-1, model_name = 'THG24',flat_max1=1,flat_min1=0.3,flat_incr1=0.01,
                           flat_max2=0.0,flat_min2=0.0,flat_incr2=0.0,
                           flat_max3=0.0,flat_min3=0.0,flat_incr3=0.0,
                           num_sims = 1000,Shiny=F) {
    DI <- DI[,1:2]
    DI <- na.omit(DI)
    model <- GGPmodels(model_name)  
    # flat_min <- flat_min
    # flat_incr <- flat_incr
    flats1 <- seq(flat_max1, flat_min1, by = -flat_incr1)
    flats <- flats1
    if((flat_max2==0.0 | flat_min2==0.0 | flat_incr2==0.0)==FALSE){
      flats2 <- seq(flat_max2, flat_min2, by = -flat_incr2)
      flats <- c(flats,flats2)
    }
    if((flat_max3==0.0 | flat_min3==0.0 | flat_incr3==0.0)==FALSE){
      flats3 <- seq(flat_max3, flat_min3, by = -flat_incr3)
      flats <- c(flats,flats3)
    }
    flats <- unique(flats)
    flats <- sort(flats,decreasing = T)
    #convert data to common mode down pointing
    DIc <- PmagDiR::common_DI(DI,down = T,export = F)
    #find average direction
    pars <- PmagDiR::fisher(DIc)
    pars <- pars[,1:2]
    if (pars$dec > 90 && pars$dec < 270) {
      pars$dec <- (pars$dec-180)%%360
    }
    rot_block <- data.frame(matrix(ncol = 2,nrow = nrow(DI)))
    rot_block[,1] <- (DIc[,1]-pars$dec)%%360
    rot_block[,2] <- DIc[,2]
    
    results <- data.frame(matrix(ncol=16,nrow=0))
    colnames(results) <- c("flat","kappa","A2D","A2I","H",
                           "lat","inc","pID","V2","V2min","V2max",
                           "E","Emin","Emax","V2_results","E_results")
    incr_counts <- 0
    for (flat in flats) {
      incr_counts <- incr_counts+1
      unflat_block <- PmagDiR::unflat_DI(DI = rot_block,f = flat)
      res <- svei_test(unflat_block, kappa = kappa, num_sims = num_sims, model_name = model_name, Plot=F,EI_test = T,Shiny=T)
      results_t <- data.frame(matrix(flat))
      colnames(results_t) <- "flat"
      results_t$kappa <- res$kappa
      results_t$A2D <- res$A2D
      results_t$A2I <- res$A2I
      results_t$H <- res$H
      results_t$lat <- res$lat
      results_t$inc <- PmagDiR::findinc(res$lat)  
      results_t$pID <- res$pID
      results_t$V2 <- res$V2dec
      results_t$V2min <- res$V2sim_min
      results_t$V2max <- res$V2sim_max
      results_t$E <- res$E
      results_t$Emin <- res$Esim_min
      results_t$Emax <- res$Esim_max
      results_t$V2_results<- res$V2_result
      results_t$E_results <- res$E_result
      results <- rbind(results,results_t)
      if(Shiny==T) {
        updateProgressBar(
          #depends if using for distribution shape or inclination flattening
          id="f_increments",
          title = "Total increments performed",
          value=incr_counts,total=length(flats)
        )}else{print(paste(flat," f factor applied."))}
    }
    good_flats <- data.frame(results[which(results$H==0 & results$V2_results==1 & results$E_results==1, arr.ind = T),1])
    good_incs <- data.frame(results[which(results$H==0 & results$V2_results==1 & results$E_results==1, arr.ind = T),7])
    
    #plotting part
    #four diagrams, corrected directions elsewhere
    #Top left, V2Dec vs Flattening
    par(fig=c(0,0.5,0.5,1))
    par(mar=c(4,4,4,4))
    #define plot with v2min and v2max for ylim, flatmax and min for xlim reversed
    plot(NA,xlim=c(max(flats),min(flats)),ylim=c(90,270),yaxp=c(90,270,4),
         xlab="Flattening factor",ylab="E Decl. (°)")
    abline(h = 180,lty=2)
    if(nrow(good_flats)>0) rect(xleft = good_flats[1,1],ybottom = 90,
                                xright = good_flats[nrow(good_flats),1],ytop = 270,
                                col = rgb(1,0,0,0.25), border = NA)
    lines(x = flats,y = results$V2max,lty=1,lwd=2,col="red")
    lines(x = flats,y = results$V2min,lty=1,lwd=2,col="red")
    lines(x = flats,y = results$V2,lwd=2, col="purple")
    
    #top right, E vs Flattening
    par(fig=c(0.5,1,0.5,1),new=TRUE)
    par(mar=c(4,4, 4, 4))
    plot(NA,xlim=c(max(flats),min(flats)),ylim=c(1,max(results$Emax)),
         xlab="Flattening factor",ylab="E")
    if(nrow(good_flats)>0) rect(xleft = good_flats[1,1],ybottom = 1,
                                xright = good_flats[nrow(good_flats),1],ytop = max(results$Emax),
                                col = rgb(1,0,0,0.25), border = NA)
    lines(x = flats,y = results$E,lwd=2,col="purple")
    lines(x = flats,y = results$Emin,lwd=2,col="red")
    lines(x = flats,y = results$Emax,lwd=2,col="red")
    
    #downleft, A2D and A2I vs flattening
    par(fig=c(0,0.5,0,0.5),new=TRUE)
    par(mar=c(4,4, 4, 4))
    plot(NA,xlim=c(max(flats),min(flats)),
         ylim=c(0,max(c(results$A2D,results$A2I))),
         xlab="Flattening factor",ylab="A2D (red),A2I (blue)")
    abline(h = 3.07, lty=2)
    if(nrow(good_flats)>0) rect(xleft = good_flats[1,1],ybottom = 0,
                                xright = good_flats[nrow(good_flats),1],
                                ytop = max(c(results$A2D,results$A2I)),
                                col = rgb(1,0,0,0.25), border = NA)
    lines(x=flats,y = results$A2D, lwd=2, col="red")
    lines(x=flats,y = results$A2I, lwd=2, col="blue")
    
    #downright, pID and inc vs flattening
    par(fig=c(0.5,1,0,0.5),new=TRUE)
    par(mar=c(4,4, 4, 4))
    plot(NA,xlim=c(max(flats),min(flats)),
         ylim=c(0,max(results$pID)+0.1),
         xlab="Flattening factor",ylab="pID")
    abline(h=0.025, lty=2)
    if(nrow(good_flats)>0) rect(xleft = good_flats[1,1],ybottom = 0,
                                xright = good_flats[nrow(good_flats),1],
                                ytop = max(results$pID)+0.1,
                                col = rgb(1,0,0,0.25), border = NA)
    lines(x = flats,y = results$pID,lwd=2, col="purple")
    #probable f is not the max of pID but the average of max and min
    #f <- results[which(results[,8]==max(results[,8]),arr.ind = T),1]
    f <- min(good_flats)+((max(good_flats)-min(good_flats))/2)
    inc_corr <- min(good_incs)+((max(good_incs)-min(good_incs))/2)
    par(new=T)
    plot(x=flats,y = results$inc,
         xlim=c(max(flats),min(flats)),lwd=2,
         xlab="", ylab="", axes=F,type="l", col="darkgreen")
    axis(4,col="black")
    mtext("Incl. (°)", side = 4,line=2.5)
    #text with result
    par(fig=c(0.48,1,0.33,0.8), new=T)
    plot(NA,xlim=c(0,1),ylim=c(0,1),xlab="",ylab="",axes=F)
    text(x = 0,y=0.05,labels=paste("f= ",f,"(",min(good_flats),",",max(good_flats),")",
                                   "; ","Incl.= ",round(inc_corr,digits = 1),
                                   "(",round(min(good_incs),digits = 1),",",
                                   round(max(good_incs),digits = 1),")",sep=""), pos=4)
    return(results)
  }
  
  #reactive function
  SVEI_EI_testPlot <- eventReactive(input$SVEI_EI_go,{
    DI <- fix_DI(Dirs$dat)
    if(input$kappa_f==1){k_4_ftest <- -1}
    if(input$kappa_f==2){k_4_ftest <- 50}
    if(input$kappa_f==3){k_4_ftest <- 100}
    SVEI_EI_result <- find_flatteR(DI = DI,
                                   flat_max1=input$SVEI_fi_1,flat_min1=input$SVEI_ft_1,flat_incr1=input$SVEIfinc1,
                                   flat_max2=input$SVEI_fi_2,flat_min2=input$SVEI_ft_2,flat_incr2=input$SVEIfinc2,
                                   flat_max3=input$SVEI_fi_3,flat_min3=input$SVEI_ft_3,flat_incr3=input$SVEIfinc3,
                                   num_sims = input$SVEI_EI_nb,
                                   kappa=k_4_ftest,
                                   Shiny = T)
    return(SVEI_EI_result)
  })
  
  #window showing list of applied f
  observeEvent(input$check_f, {
    # display a modal dialog with a header, textinput and action buttons
    showModal(jqui_draggable(modalDialog(
      size = "l",
      tags$h2('Series of applied f'),
      fluidRow(
        column(12,textOutput(outputId = "f_series"))
      ),
      easyClose = TRUE,
      footer=tagList(
        modalButton('close')
      )
    ),options = list(cancel = ".shiny-input-container")))
  })
  
  #print sequence of flattening factor to window above
  output$f_series <- renderText({
    flat_max1=input$SVEI_fi_1
    flat_min1=input$SVEI_ft_1
    flat_incr1=input$SVEIfinc1
    flat_max2=input$SVEI_fi_2
    flat_min2=input$SVEI_ft_2
    flat_incr2=input$SVEIfinc2
    flat_max3=input$SVEI_fi_3
    flat_min3=input$SVEI_ft_3
    flat_incr3=input$SVEIfinc3
    flats1 <- seq(flat_max1, flat_min1, by = -flat_incr1)
    flats <- flats1
    if((flat_max2==0.0 | flat_min2==0.0 | flat_incr2==0.0)==FALSE){
      flats2 <- seq(flat_max2, flat_min2, by = -flat_incr2)
      flats <- c(flats,flats2)
    }
    if((flat_max3==0.0 | flat_min3==0.0 | flat_incr3==0.0)==FALSE){
      flats3 <- seq(flat_max3, flat_min3, by = -flat_incr3)
      flats <- c(flats,flats3)
    }
    flats <- unique(flats)
    flats <- sort(flats,decreasing = T)
  })
  
  #send plot to UI
  output$SVEI_EI_test_fig <- renderPlot({
    EI_table <- SVEI_EI_testPlot()
    SVEI_EI_Plot <- recordPlot()
    output$SVEI_EI_exp <- downloadHandler(
      filename = function() {
        paste(input$SVEI_EI_expname,"_SVEI_incFlat_test_", Sys.Date(), ".pdf", sep="")
      },
      content = function(file) {
        pdf(file, onefile = TRUE,width = 12,height = 10.5)
        replayPlot(SVEI_EI_Plot)
        dev.off()
      }
    )
    output$SVEI_EI_tab_exp <- downloadHandler(
      filename = function() {
        paste(input$SVEI_EI_expname,"_SVEI_Inclination_flattening_", Sys.Date(), ".csv", sep="")
      },
      content = function(file) {
        if(!is.null(EI_table)){
          write.csv(EI_table, file,row.names = F)
        }
      }
    )
  },width = 850,height = 700)
  ##################### END OF SVEI MODULE
  
  
  ############ TK03.GAD INCLINATION FLATTENING MODULE###############
  #Function that correct inclination shallowing after tk03.GAD model
  
  #create reactive file
  ffind <- reactiveValues(results=NULL)
  ffind_boot_plot <- eventReactive(input$ffindgo,{
    #main function adapted
    #perform routine
    ffind_boot_funct <- function(){
      DI <- fix_DI(Dirs$dat)
      PmagDiR::ffind_boot_S(DI,nb=input$ffindboot,bootstrap=input$ffindyesnoboot)
    }
    #save statistic and makes plot
    ffind$results <- ffind_boot_funct()
    #total number of simulations
    output$validboots <- renderText({paste("Total number of simulations:",ffind$results[[4]])})
    
    #prepare and plot table
    output$ffindStat <- renderTable({ffind$results[[2]][1:2,]},rownames = T,
                                    caption="Inc= inclination, E_dec= declination of elongation. Exported file includes f, N, Elongation.")
  })
  #execute ffind_boot test
  output$ffindgraph <- renderPlot({
    ffind_boot_plot()
    ffindPlot <- recordPlot()
    #Export graphic
    output$ffindG <- downloadHandler(
      filename = function() {
        paste(input$fileN_FF,"_ffind_", Sys.Date(), ".pdf", sep="")
      },
      content = function(file) {
        pdf(file, onefile = TRUE,width = 12,height = 11)
        replayPlot(ffindPlot)
        dev.off()
      }
    )
    output$ffindS <- downloadHandler(
      filename = function() {
        paste(input$fileN_FF,"_ffind_", Sys.Date(), ".csv", sep="")
      },
      content = function(file) {
        write.csv(ffind$results[[2]],file)
      }
    )
  },width = 800,height = 700)
  ############ END OF TK03.GAD INCLINATION FLATTENING MODULE
  
  
  ############ VIRTUAL GEOMAGNETIC POLES MODULE###########
  #create service environment
  MVGP_temp <- new.env()
  assign("MVGP_temp", MVGP_temp, envir = .GlobalEnv)
  
  #create reactive files for saving external VGPs
  VGP_saved <- reactiveValues(list = NULL)
  MVGP_list <- reactiveValues(vgps = NULL)
  
  #modified VGP plot function ; VGPint defines if VGP calculation are coming from VGP screen 1,2, or 3
  plot_VGP_S <- function(VGP,lat=90,long=0,grid=30,plot_vgp=TRUE, symbol="c",cex=1,
                         col_sym_out="black", col="black",col_f="cyan",col_boot=rgb(1,0,0,0.15),
                         on_plot=FALSE,auto_cent=TRUE,coast=T, title="",save=TRUE,A95=FALSE,B95=FALSE,nb=1000,VGPint=1,plot=T){
    
    #functions converting long & lat to xy
    c2x <- function(lon,lat) {cos(d2r(lat))*sin(d2r(lon-lon0))}
    c2y <- function(lon,lat) {(cos(d2r(lat0))*sin(d2r(lat)))-(sin(d2r(lat0))*cos(d2r(lat))*cos(d2r(lon-lon0)))}
    #cut is cosin of c, when negative is behind projections, needs to be cut
    cut <- function(lon,lat) {(sin(d2r(lat0))*sin(d2r(lat)))+(cos(d2r(lat0))*cos(d2r(lat))*cos(d2r(lon-lon0)))}
    
    
    #manipulate data
    colnames(VGP) <- c("lon","lat")
    vgpsN <- PmagDiR::common_DI(VGP,down = ifelse(mean(VGP$lat)<0,FALSE,TRUE))
    PPole <- PmagDiR::fisher(vgpsN)
    #fix point of view
    if(auto_cent==FALSE){
      #center of proj is Lon0 & Lat0
      lon0 <- long
      lat0 <- lat
    }else{
      lon0 <- PPole[1,1]
      lat0 <- PPole[1,2]
    }
    if(plot==T){
      if(on_plot==FALSE){
        plot(NA, xlim=c(-1,1), ylim=c(-1,1), asp=1,
             xlab="", xaxt="n",ylab="", yaxt="n", axes=FALSE)
        PmagDiR::sph_ortho(lat=lat0,long=lon0,grid=grid,coast=coast, title=title)
      }
    }
    
    coord <- as.data.frame(lon0)
    coord$lat0 <- lat0
    VGP$x <- c2x(lon = VGP$lon,lat = VGP$lat)
    VGP$y <- c2y(lon = VGP$lon,lat = VGP$lat)
    VGP$cut <- cut(lon = VGP$lon,VGP$lat)
    #select symbol
    if(symbol=="c") {pch <- 21}
    else if(symbol=="s") {pch <- 22}
    else if(symbol=="d") {pch <- 23}
    else if(symbol=="t") {pch <- 24}
    
    if(plot==T){
      #plot points if requested
      if(plot_vgp==T){
        points(VGP$x,VGP$y,pch=pch,
               col=col_sym_out,
               bg=ifelse(VGP$cut>0,col,"white"), cex=cex)
      }
    }
    
    #uses PmagDiR for plotting A95
    if(plot==T){
      if(A95==TRUE){
        PmagDiR::plot_PA95(lon = PPole[1,1],lat = PPole[1,2],A = PPole[1,3],lon0 = lon0,lat0 = lat0,on_plot = TRUE,symbol = symbol,col_f = col_f,size=1.5)
      }
    }
    
    #calculate bootstreapped VGPs
    if(B95==TRUE){
      bootlonlat <- as.data.frame(matrix(ncol = 2,nrow = 0))
      n <- 0
      #number of bootrstrap
      nb <- nb
      repeat{
        n <- n+1
        VGPb <- PmagDiR::boots_DI(VGP)
        VGPb_av <- PmagDiR::fisher(VGPb)
        blon <- VGPb_av[1,1]
        blat <- VGPb_av[1,2]
        blonlat <- as.data.frame(t(c(blon,blat)))
        bootlonlat <- rbind(bootlonlat,blonlat)
        x <- PmagDiR::c2x(blon,blat,centLon = lon0)
        y <- PmagDiR::c2y(blon,blat,centLon = lon0,centLat = lat0)
        cutt <- PmagDiR::cut(blon,blat,centLon = lon0,centLat = lat0)
        
        #update progress bar of VGP calculated from directions internally
        if(VGPint==1){
          updateProgressBar(
            id="vgpboot",
            title = "VGPs bootstrap",
            value=n,total=nb,
          )
        }else if(VGPint==2){
          #Update progress bar of VGP external
          updateProgressBar(
            id="Ext_vgpboot",
            title = "VGPs bootstrap",
            value=n,total=nb,
          )
        }else if(VGPint==3){
          updateProgressBar(
            id="Mvgpboot",
            title = "VGPs bootstrap",
            value=n,total=nb,
          )
        }else if(VGPint==4){
          updateProgressBar(
            id="SVGPboot",
            title = "VGPs bootstrap",
            value=n,total=nb,
          )
        }
        
        if(n>=nb) break
        #ADD bootstrapping counter
      }
      colnames(bootlonlat) <- c("vgp_lon","vgp_lat")
      #calculate angluar distances
      bootlonlat$Plon <- rep(PPole[1,1])
      bootlonlat$Plat <- rep(PPole[1,2])
      bootlonlat$delta <- abs(bootlonlat$vgp_lon-bootlonlat$Plon)
      bootlonlat$diff <- PmagDiR::r2d(acos((sin(PmagDiR::d2r(bootlonlat$vgp_lat))*sin(PmagDiR::d2r(bootlonlat$Plat)))+
                                             (cos(PmagDiR::d2r(bootlonlat$vgp_lat))*cos(PmagDiR::d2r(bootlonlat$Plat))*cos(PmagDiR::d2r(bootlonlat$delta)))))
      ang_dis <- as.data.frame(bootlonlat$diff)
      ang_dis <- (ang_dis[order(ang_dis[,1]),])
      conf <- 0.95
      Uconf <- round(nb*conf,digits=0)
      angular_conf <- ang_dis[Uconf]
      
      #add cartesian coordinates
      bootlonlat$x <- PmagDiR::c2x(bootlonlat$vgp_lon,bootlonlat$vgp_lat,centLon = lon0)
      bootlonlat$y <- PmagDiR::c2y(bootlonlat$vgp_lon,bootlonlat$vgp_lat,centLon = lon0,centLat = lat0)
      bootlonlat$cutt <- PmagDiR::cut(bootlonlat$vgp_lon,bootlonlat$vgp_lat,centLon = lon0,centLat = lat0)
      
      if(plot==T){
        #plot bootstrapped data
        points(bootlonlat$x,bootlonlat$y,pch=ifelse(bootlonlat$cutt>0,16,1),col=col_boot)
        PPole_x <- PmagDiR::c2x(PPole[1,1],PPole[1,2],centLon = lon0)
        PPole_y <- PmagDiR::c2y(PPole[1,1],PPole[1,2],centLon = lon0,centLat = lat0)
        PPole_cut <- PmagDiR::cut(PPole[1,1],PPole[1,2],centLon = lon0,centLat = lat0)
        #plot average
        if(PPole_cut>0){
          points(PPole_x,PPole_y, pch=pch,cex=1.5, col="black",
                 bg= col_f)
        }else{
          points(PPole_x,PPole_y, pch=pch,cex=1.5, col="black",
                 bg= "white")
        }
      }
    }
    
    Plot_VGP_result <- list(0)
    Paleopole_F <- cbind(PPole[,4],PPole[,1:3],PPole[,6])
    colnames(Paleopole_F) <- c("N","Long","Lat","A95","K")
    rownames(Paleopole_F) <- input$fileN
    if(B95==TRUE){
      Paleopole_B <- data.frame(matrix(ncol=4,nrow=1))
      Paleopole_B[1,1] <- PPole[1,4]
      Paleopole_B[1,2:3] <- PPole[1,1:2]
      Paleopole_B[1,4] <- angular_conf
      colnames(Paleopole_B) <- c("N","Long","Lat","B95")
      rownames(Paleopole_B) <- input$fileN
    }
    
    Plot_VGP_result[[1]] <- Paleopole_F
    if(B95==TRUE){
      Plot_VGP_result[[2]] <- Paleopole_B
      Plot_VGP_result[[3]] <- bootlonlat[,1:2]
    }
    return(Plot_VGP_result)
  }
  
  #function that built list of sites
  sites_list <- function(){
    MVGP_list_t <- data.frame(matrix(nrow=0,ncol = 9))
    colnames(MVGP_list_t) <- c("Loc.","Col", "Sym","N","Lon","Lat","A95","B95","K")
    #check global vgp list
    if(length(VGP_saved$list)){
      sites <- ls(VGP_saved$list)
      #populate list
      for(i in 1:length(sites)){
        newTabLine <- data.frame(t(c(sites[i],
                                     VGP_saved$list[[sites[i]]][[1]][1,3],
                                     VGP_saved$list[[sites[i]]][[1]][1,4],
                                     nrow(VGP_saved$list[[sites[i]]][[1]]),
                                     round(VGP_saved$list[[sites[i]]][[2]][[1]][1,2], digits=1),
                                     round(VGP_saved$list[[sites[i]]][[2]][[1]][1,3], digits=1),
                                     round(VGP_saved$list[[sites[i]]][[2]][[1]][1,4], digits=1),
                                     round(VGP_saved$list[[sites[i]]][[2]][[2]][1,4], digits=1),
                                     round(VGP_saved$list[[sites[i]]][[2]][[1]][1,5], digits=1))))     
        colnames(newTabLine) <- c("Loc.","Col", "Sym","N","Lon","Lat","A95","B95","K")
        MVGP_list_t <- rbind(MVGP_list_t,newTabLine)
      }
    }
    if(!is.null(Added_poles$list)){
      MVGP_list_t <- rbind(MVGP_list_t,Added_poles$list)
    }
    if(nrow(MVGP_list_t>0)) {return(MVGP_list_t)}
  }
  
  #create reactive value
  Pole <- reactiveValues(FishPole = NULL)
  IVGP <- reactiveValues(VGP_list=NULL)
  
  
  ###########################MODAL DIALOG WITH INTERNAL VGPs DETAILS ##################
  ####opens modal window
  observeEvent(input$inter_VGPs, {
    # display a modal dialog with a header, textinput and action buttons
    showModal(jqui_draggable(modalDialog(size = "l",
                                         tags$h3('Enter VGPs details calculated from internal directions'),
                                         br(),
                                         fluidRow(
                                           column(3,selectInput("intVGPflip",label = "Hemisphere",choices = list("North"=1,"South"=2),selected = 1)),
                                           column(3,selectInput("vgpscolor", label= "Color",
                                                                choices= list("black"=1,"blue"=2,"green"=3,"pink"=4,"purple"=5,"brown"=6,"red"=7,"yellow"=8,"cyan"=9, "gray"=10,"white"= 11), selected=3)),
                                           column(3,selectInput("vgpssymbol",label = "Symbol",
                                                                choices = list("circle"=1, "square"=2, "diamond"=3,"triangle"=4),selected=1)),
                                           column(3,selectInput("VGPtype", label = "VGPs to export",
                                                                choices = list("Single mode"=1,"Bimodal"=2,"Rotated"=3),selected = 1))
                                         ),
                                         fluidRow(
                                           column(3,numericInput("vgpbootn",label="Bootstraps n.",value=2000)),
                                           
                                           column(9,progressBar(
                                             id = "vgpboot",
                                             value = 0,total=1000,
                                             title = "VGPs bootstrap",
                                             display_pct = TRUE))
                                         ),
                                         #warning messages for particular situations
                                         h4(textOutput("geowarning")),
                                         h4(textOutput("coordwarning")),
                                         h4(textOutput("flatwarning")),
                                         br(),
                                         fluidRow(
                                           column(6,actionButton("saveVGP",label = "Add to List of loaded VGPs and Poles",width = "100%")),
                                           column(6, downloadButton("VGPs_Exp",label = "Export VGPs",style = "width:100%;")),
                                         ),
                                         #result of statistic 
                                         h5(textOutput("fishpole")),
                                         #plot here internal VGPs
                                         fluidRow(
                                           column(1),
                                           plotOutput(outputId = "VGPplot")
                                         ),
                                         #make window deeper
                                         br(),
                                         br(),
                                         br(),
                                         br(),
                                         br(),
                                         br(),
                                         br(),
                                         br(),
                                         br(),
                                         br(),
                                         br(),
                                         br(),      
                                         easyClose = TRUE,
                                         footer=tagList(
                                           modalButton('close')
                                         ) 
    ),options = list(cancel = ".shiny-input-container")))
  })
  
  ### VGP calculation and plotting Part
  output$VGPplot <- renderPlot({
    req(Dirs$dat)
    DIrs<- fix_DI(Dirs$dat)
    #create file with all VGPs
    reactiveValuesToList(IVGP)
    ifelse(input$intVGPflip==1,
           IVGP$VGP_list[[1]] <- PmagDiR::VGP_DI(DI = DIrs,export = FALSE,Prnt = FALSE,lat = input$lat, long = input$long,type="VGPsN"),
           IVGP$VGP_list[[1]] <- PmagDiR::flip_DI(PmagDiR::VGP_DI(DI = DIrs,export = FALSE,Prnt = FALSE,lat = input$lat, long = input$long,type="VGPsN"))
    )    
    IVGP$VGP_list[[2]] <- PmagDiR::VGP_DI(DI = DIrs,export = FALSE,Prnt = FALSE,lat = input$lat, long = input$long,type="VGPs")
    IVGP$VGP_list[[3]] <- PmagDiR::VGP_DI(DI = DIrs,export = FALSE,Prnt = FALSE,lat = input$lat, long = input$long,type="VGPsR")
    VGPS <- IVGP$VGP_list[[1]]
    
    #select color
    if(input$vgpscolor==1 || is.null(input$vgpscolor)) IVGP$vgpscolor <- "black"
    else if(input$vgpscolor==2) IVGP$vgpscolor <- "blue"
    else if(input$vgpscolor==3) IVGP$vgpscolor <- "green"
    else if(input$vgpscolor==4) IVGP$vgpscolor <- "pink"
    else if(input$vgpscolor==5) IVGP$vgpscolor <- "purple"
    else if(input$vgpscolor==6) IVGP$vgpscolor <- "brown"
    else if(input$vgpscolor==7) IVGP$vgpscolor <- "red"
    else if(input$vgpscolor==8) IVGP$vgpscolor <- "yellow"
    else if(input$vgpscolor==9) IVGP$vgpscolor <- "cyan"
    else if(input$vgpscolor==10) IVGP$vgpscolor <- "gray"
    else if(input$vgpscolor==11) IVGP$vgpscolor <- "white"
    
    #select symbol
    if(input$vgpssymbol==1 || is.null(input$vgpssymbol)) IVGP$vgpssymbol <- "c"
    else if(input$vgpssymbol==2) IVGP$vgpssymbol <- "s"
    else if(input$vgpssymbol==3) IVGP$vgpssymbol <- "d"
    else if(input$vgpssymbol==4) IVGP$vgpssymbol <- "t"
    
    #plot data and save stat
    Pole$FishPole <- plot_VGP_S(VGP = VGPS,
                                col = IVGP$vgpscolor,symbol = IVGP$vgpssymbol,nb=input$vgpbootn,
                                A95 = T,
                                B95 =F,
                                #B95 = boot_run,
                                VGPint = 1)
    #warning for site coordinates not set
    coordwarn <- ifelse(input$lat==0 | input$long==0, "WARNING: site latitude and longitude are set as zero","")
    output$coordwarning <- renderText({coordwarn})
    
    output$fishpole <- renderText({
      Polestat <- paste("N: ",Pole$FishPole[[1]][1,1],",",
                        " Long: ",round(Pole$FishPole[[1]][1,2], digits = 1),",",
                        " Lat: ", round(Pole$FishPole[[1]][1,3], digits = 1),",",
                        " A95: ",round(Pole$FishPole[[1]][1,4],digits = 1),",",
                        " K: ",round(Pole$FishPole[[1]][1,5], digits = 1),
                        sep = "")
      Polestat_out <- paste("Average Pole: ", Polestat)
      Polestat_out
    })
    
    output$VGPs_Exp <- downloadHandler(
      filename = function(){
        paste(input$fileN,
              if(input$VGPtype==1){"_VGPs_SingleMod_"}
              else if(input$VGPtype==2){"_VGP_Rev_"}
              else if(input$VGPtype==3){"_VGP_Rot_"},
              Sys.Date(), ".csv", sep="")
      },
      content = function(file) {
        if(input$VGPtype==1){write.csv(round(IVGP$VGP_list[[1]], digits = 2),file, row.names = F)}
        else if(input$VGPtype==2){write.csv(round(IVGP$VGP_list[[2]], digits = 2),file, row.names = F)}
        else if(input$VGPtype==3){write.csv(round(IVGP$VGP_list[[3]], digits = 2),file, row.names = F)}
      }
    )
  }, width = 710, height = 710)
  
  
  #send INTERNAL VGP to list and reactive VGP_saved_list
  observeEvent(input$saveVGP,{
    req(IVGP$VGP_list[[1]])
    VGP <- IVGP$VGP_list[[1]]
    colnames(VGP) <- c("Long","Lat")
    VGP$color <- IVGP$vgpscolor 
    VGP$symbol <- IVGP$vgpssymbol
    
    VGPsaved <- list()
    VGPsaved[[1]] <- VGP
    #if statistics does not exists, it makes it
    if(length(Pole$FishPole)<=1){
      VGPsaved[[2]] <- plot_VGP_S(VGP = VGP,A95 = T,B95 = T,nb = input$vgpbootn,VGPint = 1,plot=F)
    }else{VGPsaved[[2]] <- Pole$FishPole}
    VGP_saved$list[[input$fileN]] <- VGPsaved
    MVGP_list$vgps <- sites_list()
  })
  
  ################################### External and Multiple vgps part
  extVGP <- reactiveValues(loaded="NO")
  observeEvent(input$vgpfile,{
    extVGP$loaded <- "YES"
  })
  #erease file if modal dialog is opened again
  observeEvent(input$ext_VGPs,{
    extVGP$loaded <- "NO"
  })
  
  #read file if present, reset if requested
  VGPfile <- reactive({
    if(extVGP$loaded=="YES"){
      read.csv(file = input$vgpfile$datapath)
    }else{return(NULL)}
  })
  
  
  #########################MODAL DIALOG WITH EXTERNAL VGPs DETAILS################
  #create External VGPs reactive value for color and symbol and average
  EVGP <- reactiveValues(EVGPcolor=NULL)
  
  #Open window
  observeEvent(input$ext_VGPs, {
    # display a modal dialog with a header, textinput and action buttons
    showModal(jqui_draggable(modalDialog(size = "l",
                                         tags$h3('Enter VGPs details from external file'),
                                         br(),
                                         fluidRow(
                                           column(3,fileInput("vgpfile", label = "Load VGPs file")),
                                           column(3,textInput("EVGP_sitename", label = "VGPs name",value = "Locality")),
                                           column(3,selectInput("VGP_ext_mode",label = "Hemisphere",choices = list("North"=1,"South"=2),selected = 1)),
                                           column(3,selectInput("EVGPcolor", label= "Color",
                                                                choices= list("black"=1,"blue"=2,"green"=3,"pink"=4,"purple"=5,"brown"=6,"red"=7,"yellow"=8,"cyan"=9,"gray"=10, "white"=11), selected=3)),
                                           
                                         ),
                                         fluidRow(
                                           column(3,selectInput("EVGPsymbol", label= "Symbol",
                                                                choices = list("circle"=1, "square"=2, "diamond"=3,"triangle"=4),selected=1)),
                                           column(3,selectInput("VGP_ext_cut_type",label = "Cut-off",choices = list("None"=1,"Vandamme"=2,"Fixed"=3),selected = 1)),
                                           column(3,numericInput("VGP_ext_cutoff",label = "VGP filter radius",value = 45)),
                                           column(3,numericInput("EVGPnb", label = "Bootstrap n.", value = 2000))
                                         ),
                                         fluidRow(
                                           column(12,progressBar(
                                             id = "Ext_vgpboot",
                                             value = 0,total=2000,
                                             title = "VGP bootstrap",
                                             display_pct = TRUE))
                                         ),
                                         fluidRow(
                                           column(12,actionButton("saveEVGP",label = "Add to List of loaded VGPs and Poles",width = "100%")),
                                         ),
                                         #result of statistic 
                                         h5(textOutput("Ext_fishpole")),
                                         fluidRow(
                                           column(1),
                                           plotOutput(outputId = "Ext_VGP_plot")
                                         ),
                                         #makes window deeper
                                         br(),
                                         br(),
                                         br(),
                                         br(),
                                         br(),
                                         br(),
                                         br(),
                                         br(),
                                         br(),
                                         br(),
                                         br(),
                                         br(),      
                                         easyClose = TRUE,
                                         footer=tagList(
                                           modalButton('close'))
    ),options = list(cancel = ".shiny-input-container"))
    )
  })
  
  #function that plots EXTERNAL vgp and save plotting detail in temporary environment
  plot_current_VGP <- function(){
    VGP <- VGPfile()
    colnames(VGP) <- c("Long","Lat")
    #decide how to plot poles (north, or sud)
    if(input$VGP_ext_mode==1){VGP <- PmagDiR::common_DI(VGP)}
    if(input$VGP_ext_mode==2){VGP <- PmagDiR::common_DI(VGP,down = FALSE)}
    
    #apply cutoff if requested
    if(input$VGP_ext_cut_type!=1){VGP <- PmagDiR::cut_VGP(VGP = VGP,VD=ifelse(input$VGP_ext_cut_type==2,TRUE,FALSE),cutoff = input$VGP_ext_cutoff)}
    
    #choose color
    if(input$EVGPcolor==1) EVGP$EVGPcolor <- "black"
    if(input$EVGPcolor==2) EVGP$EVGPcolor <- "blue"
    if(input$EVGPcolor==3) EVGP$EVGPcolor <- "green"
    if(input$EVGPcolor==4) EVGP$EVGPcolor <- "pink"
    if(input$EVGPcolor==5) EVGP$EVGPcolor <- "purple"
    if(input$EVGPcolor==6) EVGP$EVGPcolor <- "brown"
    if(input$EVGPcolor==7) EVGP$EVGPcolor <- "red"
    if(input$EVGPcolor==8) EVGP$EVGPcolor <- "yellow"
    if(input$EVGPcolor==9) EVGP$EVGPcolor <- "cyan"
    if(input$EVGPcolor==10) EVGP$EVGPcolor <- "gray"
    if(input$EVGPcolor==11) EVGP$EVGPcolor <- "white"
    
    #choose symbol
    if(input$EVGPsymbol==1) EVGP$EVGPsymbol <- "c"
    if(input$EVGPsymbol==2) EVGP$EVGPsymbol <- "s"
    if(input$EVGPsymbol==3) EVGP$EVGPsymbol <- "d"
    if(input$EVGPsymbol==4) EVGP$EVGPsymbol <- "t"
    
    #Plot external vgp with no statistics, it will send it through add button to main list to download
    EVGP$Pole <- plot_VGP_S(VGP = VGP,
                            auto_cent = TRUE,
                            coast = TRUE,
                            col = EVGP$EVGPcolor, symbol = EVGP$EVGPsymbol,
                            A95 = T,
                            B95 = F,
                            VGPint=2)
  }
  
  #send figures to ui external VGP
  output$Ext_VGP_plot <- renderPlot({
    req(VGPfile())
    plot_current_VGP()
    
    #record plot
    Ext_VGP_plot <- recordPlot()
  },width = 710, height = 710)
  
  #send fisher  OF EXTERNAL POLE on modal window                     
  output$Ext_fishpole <- renderText({
    if(extVGP$loaded=="YES"){
      E_Polestat <- paste("N: ",EVGP$Pole[[1]][1,1],",",
                          " Long: ",round(EVGP$Pole[[1]][1,2], digits = 1),",",
                          " Lat: ", round(EVGP$Pole[[1]][1,3], digits = 1),",",
                          " A95: ",round(EVGP$Pole[[1]][1,4],digits = 1),",",
                          " K: ",round(EVGP$Pole[[1]][1,5], digits = 1),
                          sep = "")
      E_Polestat_out <- paste("Average Pole: ", E_Polestat)
      E_Polestat_out
    }
  })
  
  #send external VGP to list and reactive VGP_saved_list
  observeEvent(input$saveEVGP,{
    req(VGPfile())
    VGP <- VGPfile()
    colnames(VGP) <- c("Long","Lat")
    #fix north or south hemisphere as requested in vgp enter page
    if(input$VGP_ext_mode==1){VGP <- PmagDiR::common_DI(VGP)}
    if(input$VGP_ext_mode==2){VGP <- PmagDiR::common_DI(VGP,down = FALSE)}
    
    #apply cutoff if requested
    if(input$VGP_ext_cut_type!=1){VGP <- PmagDiR::cut_VGP(VGP = VGP,VD=ifelse(input$VGP_ext_cut_type==2,TRUE,FALSE),cutoff = input$VGP_ext_cutoff)}
    VGP$color <- EVGP$EVGPcolor
    VGP$symbol <- EVGP$EVGPsymbol
    
    VGPsaved <- list()
    VGPsaved[[1]] <- VGP
    #if statistics does not exists, it makes it
    # if(length(EVGP$Pole)<=1){
    VGPsaved[[2]] <- plot_VGP_S(VGP = VGP,A95 = T,B95 = T,nb = input$EVGPnb,VGPint = 2,plot=F)
    # }else{VGPsaved[[2]] <- EVGP$Pole}
    VGP_saved$list[[input$EVGP_sitename]] <- VGPsaved
    
    MVGP_list$vgps <- sites_list()
  })
  
  #########################MODAL DIALOG WITH SIMULATED VGPs DETAILS##########
  #####open modal window
  observeEvent(input$sim_VGPs,{
    # display a modal dialog with a header, text input and action buttons
    showModal(jqui_draggable(modalDialog(size = "l",
                                         tags$h3("Generate a set of Fìsherian VGPs with known Longitude, Latitude, and K"),
                                         br(),
                                         fluidRow(
                                           column(3, textInput("SVGPname",label = "VGPs name",value = "VGP_sim")),
                                           column(3,selectInput("SVGPcolor", label= "Color",
                                                                choices= list("black"=1,"blue"=2,"green"=3,"pink"=4,"purple"=5,"brown"=6,"red"=7,"yellow"=8,"cyan"=9,"gray"=10, "white"=11), selected=2)),
                                           column(3,selectInput("SVGPsymbol", label= "Symbol",
                                                                choices = list("circle"=1, "square"=2, "diamond"=3,"triangle"=4),selected=1)),
                                           column(3,numericInput("SVGPN",label = "N",value = 100))
                                           # column(3,selectInput("SVGP_stat", label="Statistic",
                                           #                      choices=list("None"=1,"Fisher"=2,"Bootstrap"=3),selected=1)),
                                         ),
                                         fluidRow(
                                           column(3,numericInput("SVGPlon",label = "Longitude",value = 0)),
                                           column(3,numericInput("SVGPlat",label = "Latitude",value = 90,max = 90,min = -90)),
                                           column(3,numericInput("SVGPk",label = "K",value = 20)),
                                           column(3,numericInput("k_tol",label = "K tol.",value = 0.1,min = 0.02)),
                                         ),
                                         fluidRow(
                                           column(3,numericInput("SVGPnb", label = "Bootstrap n.", value = 2000)),
                                           column(9,progressBar(
                                             id = "SVGPboot",
                                             value = 0,total=2000,
                                             title = "VGP bootstrap",
                                             display_pct = TRUE))
                                         ),                        
                                         fluidRow(
                                           column(6,actionButton("SVGPgo",label = "Generate VGPs",width = "100%")),
                                           column(6,actionButton("saveSVGP",label = "Add to List of loaded VGPs and Poles",width = "100%")),
                                         ),
                                         #result of statistic 
                                         h5(textOutput("Sim_fishpole")),     
                                         fluidRow(
                                           column(1),
                                           plotOutput(outputId = "SVGP_plot")
                                         ),
                                         #makes window deeper
                                         br(),
                                         br(),
                                         br(),
                                         br(),
                                         br(),
                                         br(),
                                         br(),
                                         br(),
                                         br(),
                                         br(),
                                         br(),
                                         br(),      
                                         easyClose = TRUE,
                                         footer=tagList(
                                           modalButton('close')
                                         ) 
    ),options = list(cancel = ".shiny-input-container")))
  })
  
  #function that generates vgp with Fisher pars
  SVGP_generator <- function(N,k,lon,lat,k_tol){     
    #sub-function generating random long lat
    fisherDiR <- function(k){
      L <- exp(-2*k)
      a <- runif(1)*(1-L)+L
      f <- sqrt(-log(a)/(2*k))
      latitude <- 90-PmagDiR::r2d(2*asin(f))
      longitude <- PmagDiR::r2d(2*pi*runif(1))
      return(c(longitude, latitude))
    }
    #reiterate until k is within the tolerance
    repeat{
      result <- data.frame(matrix(ncol = 2,nrow = 0))
      colnames(result) <- c("lon", "lat")
      for(i in 1:N){
        decinc_temp <- data.frame(t(fisherDiR(k)))
        colnames(decinc_temp) <- c("lon", "lat")
        result <- rbind(result,decinc_temp)
      }
      AverLongLat <- PmagDiR::fisher(result)
      fixed_data <- PmagDiR::bed_DI(result,in_file = F,
                                    bed_az = (AverLongLat[1,1]+180)%%360,bed_plunge =90-AverLongLat[1,2])
      final_VGP <- PmagDiR::bed_DI(fixed_data,in_file = F,
                                   bed_az = lon,bed_plunge =90-lat)
      colnames(final_VGP) <- c("Long","Lat")
      stat <- PmagDiR::fisher(final_VGP)
      k_test <- stat[1,6]
      #check for tolerance
      if(k_tol==0) break
      if(abs(k_test-k)<=k_tol) break
    }
    return(final_VGP)
  }
  
  #creates reactive files
  SVGP <- reactiveValues(vgps = NULL)
  SVGP <- reactiveValues(vgps_old = NULL)
  SVGPS <- reactiveValues(stat = NULL)
  
  #Delete file if simulating window is opened again
  observeEvent(input$sim_VGPs,{
    SVGP$vgps <- NULL
  })
  
  #populate reactive file
  observeEvent(input$SVGPgo,{
    SVGP$vgps <- SVGP_generator(N = input$SVGPN,k = input$SVGPk,lon = input$SVGPlon,lat = input$SVGPlat,k_tol=input$k_tol)
  })
  
  #function that generate plot
  plot_SVGP <- function(){
    req(SVGP$vgps)
    VGP <- SVGP$vgps
    
    #choose color
    if(input$SVGPcolor==1) SVGPcolor <- "black"
    if(input$SVGPcolor==2) SVGPcolor <- "blue"
    if(input$SVGPcolor==3) SVGPcolor <- "green"
    if(input$SVGPcolor==4) SVGPcolor <- "pink"
    if(input$SVGPcolor==5) SVGPcolor <- "purple"
    if(input$SVGPcolor==6) SVGPcolor <- "brown"
    if(input$SVGPcolor==7) SVGPcolor <- "red"
    if(input$SVGPcolor==8) SVGPcolor <- "yellow"
    if(input$SVGPcolor==9) SVGPcolor <- "cyan"
    if(input$SVGPcolor==10) SVGPcolor <- "gray"
    if(input$SVGPcolor==11) SVGPcolor <- "white"
    assign("SVGP_color",SVGPcolor, envir = MVGP_temp)
    
    #choose symbol
    if(input$SVGPsymbol==1) SVGPsymbol <- "c"
    if(input$SVGPsymbol==2) SVGPsymbol <- "s"
    if(input$SVGPsymbol==3) SVGPsymbol <- "d"
    if(input$SVGPsymbol==4) SVGPsymbol <- "t"
    assign("SVGP_symbol",SVGPsymbol, envir = MVGP_temp)
    
    SVGPS$stats <- plot_VGP_S(VGP = VGP,
                              coast = TRUE,
                              auto_cent = TRUE,
                              col = SVGPcolor,symbol = SVGPsymbol,nb = input$SVGPnb,
                              A95 = T,
                              B95 = F,
                              VGPint = 4)
  }
  
  #send current VGP to list and reactive VGP_saved_list        
  observeEvent(input$saveSVGP,{
    req(SVGP$vgps)
    VGP <- SVGP$vgps
    colnames(VGP) <- c("Long","Lat")
    VGP$color <- MVGP_temp[["SVGP_color"]]
    VGP$symbol <- MVGP_temp[["SVGP_symbol"]]
    VGPsaved <- list()
    VGPsaved[[1]] <- VGP
    #if statistics does not exists, it makes it
    # if(length(SVGPS$stats)<=1){
    VGPsaved[[2]] <- plot_VGP_S(VGP = VGP,A95 = T,B95 = T,nb = input$SVGPnb,VGPint = 4,plot=F)
    # }else{VGPsaved[[2]] <- SVGPS$stats}
    VGP_saved$list[[input$SVGPname]] <- VGPsaved
    MVGP_list$vgps <- sites_list()
  })
  
  #send SIMULATED FIGURE TO UI
  output$SVGP_plot <- renderPlot({
    plot_SVGP()
  }, width = 710,height = 710)
  
  #send SIMULATED VGPs fisher or bootstrap OF EXTERNAL POLE on main window                     
  output$Sim_fishpole <- renderText({
    req(SVGPS$stats)
    S_Polestat <- paste("N: ",SVGPS$stats[[1]][1,1],",",
                        " Long: ",round(SVGPS$stats[[1]][1,2], digits = 1),",",
                        " Lat: ", round(SVGPS$stats[[1]][1,3], digits = 1),",",
                        " A95: ",round(SVGPS$stats[[1]][1,4],digits = 1),",",
                        " K: ",round(SVGPS$stats[[1]][1,5], digits = 1),
                        sep = "")
    S_Polestat_out <- paste("Average Pole: ", S_Polestat)
    S_Polestat_out
  })
  
  #########################MODAL DIALOG WITH ROTATED VGPs DETAILS##########
  
  #####opens modal window
  observeEvent(input$rot_VGPs,{
    # display a modal dialog with a header, text input and action buttons
    showModal(jqui_draggable(modalDialog(size = "l",
                                         tags$h3("Rotate selected VGPs with known Euler parameters (E_Pole, Rotation)"),
                                         br(),
                                         fluidRow(
                                           column(4,selectInput("eulcolor", label= "Color",
                                                                choices= list("black"=1,"blue"=2,"green"=3,"pink"=4,"purple"=5,"brown"=6,"red"=7,"yellow"=8,"cyan"=9,"gray"=10, "white"=11), selected=8)),
                                           column(4,selectInput("eulsymbol", label= "Symbol",
                                                                choices = list("circle"=1, "square"=2, "diamond"=3,"triangle"=4),selected=1)),
                                           column(4,selectInput("eulPlotType",label = "Type",
                                                                choices = list("VGPs"=1,"Fisher"=2,"Bootstrapped"=3),selected = 1)),
                                         ),
                                         fluidRow(
                                           column(4, numericInput("eul_long",label = "E_Pole Long",value = 0,min = 0,max = 360)),
                                           column(4, numericInput("eul_lat",label = "E_Pole Lat",value = 90,min = -90,max = 90)),
                                           column(4, numericInput("eul_rot",label = "Rotation",value = 0,min = 0,max = 360)),
                                         ),
                                         fluidRow(
                                           column(6, actionButton("eulerrot",label = "Rotate",width = "100%")),
                                           column(6, actionButton("eulersave",label = "Add to List of loaded VGPs and Poles", width = "100%"))
                                         ),
                                         fluidRow(
                                           column(1),
                                           plotOutput(outputId = "eulerplot")
                                         ),
                                         #makes window deeper
                                         br(),
                                         br(),
                                         br(),
                                         br(),
                                         br(),
                                         br(),
                                         br(),
                                         br(),
                                         br(),
                                         br(),
                                         br(),
                                         br(),      
                                         easyClose = TRUE,
                                         footer=tagList(
                                           modalButton('close')
                                         ) 
    ),options = list(cancel = ".shiny-input-container")))
  })
  
  #plot VGPs selected from list for euler rotation
  plot_selected_VGP_eul <- function(r,lat0,lon0){
    #selects what to plot
    if(input$eulPlotType==1){
      plot_VGP_S(VGP=VGP_saved$list[[MVGP_list$vgps[r,1]]][[1]][,1:2],
                 lat = lat0,
                 long = lon0,
                 auto_cent = F, on_plot = T,
                 col = MVGP_list$vgps[r,2],               
                 symbol = MVGP_list$vgps[r,3])
    }else if(input$eulPlotType==2){
      #plot fisher means and not VGPs if requested
      PmagDiR::plot_PA95(lon = VGP_saved$list[[MVGP_list$vgps[r,1]]][[2]][[1]][1,2],
                         lat = VGP_saved$list[[MVGP_list$vgps[r,1]]][[2]][[1]][1,3],
                         A = VGP_saved$list[[MVGP_list$vgps[r,1]]][[2]][[1]][1,4],
                         lat0 = lat0,lon0 = lon0,
                         col_A = rgb(0,0,1,0.15),
                         symbol = MVGP_list$vgps[r,3],     
                         col_f = MVGP_list$vgps[r,2],
                         on_plot = T)
    }else if(input$eulPlotType==3){
      plot_VGP_S(VGP=VGP_saved$list[[MVGP_list$vgps[r,1]]][[2]][[3]][,1:2],
                 lat = lat0,long = lon0,
                 auto_cent = F, on_plot = T,
                 col = rgb(1,0,0,0.1),col_sym_out = rgb(1,0,0,0.1),
                 symbol = "c")
      PmagDiR::plot_PA95(lon = VGP_saved$list[[MVGP_list$vgps[r,1]]][[2]][[1]][1,2],
                         lat = VGP_saved$list[[MVGP_list$vgps[r,1]]][[2]][[1]][1,3],
                         A = 0,
                         lon0 = lon0,lat0 = lat0,
                         col_f = MVGP_list$vgps[r,2],       
                         symbol = MVGP_list$vgps[r,3],
                         size = 1.2,on_plot = T)
    }
  }
  
  #creates reactive file
  vgprotate <- reactiveValues(new=NULL)
  A95rotate <- reactiveValues(new=NULL)
  
  #empty rotated file when new pop up opens
  observeEvent(input$rot_VGPs,{
    vgprotate$new <- NULL
    A95rotate$new <- NULL
  })
  
  #function rotating the data and stats                     
  observeEvent(input$eulerrot,{                         
    n <- input$VGPs_List_rows_selected
    if(length(n)){
      #since A95rotate$new is treated like a table, it is created, if nrow is 0 then is made NULL in the end
      A95rotate$new <- data.frame(matrix(ncol=9,nrow = 0))
      colnames(A95rotate$new) <- c("Loc.","Col","Sym","N","Lon","Lat","A95","B95","K")
      for(r in n){
        #assign name to new dataset, original plus _Rt
        NAME <- paste(MVGP_list$vgps[r,1],"_Rt",sep = "")
        #choose color
        if(input$eulcolor==1) eulcolor <- "black"
        if(input$eulcolor==2) eulcolor <- "blue"
        if(input$eulcolor==3) eulcolor <- "green"
        if(input$eulcolor==4) eulcolor <- "pink"
        if(input$eulcolor==5) eulcolor <- "purple"
        if(input$eulcolor==6) eulcolor <- "brown"
        if(input$eulcolor==7) eulcolor <- "red"
        if(input$eulcolor==8) eulcolor <- "yellow"
        if(input$eulcolor==9) eulcolor <- "cyan"
        if(input$eulcolor==10) eulcolor <- "gray"
        if(input$eulcolor==11) eulcolor <- "white"
        
        #select symbol of average
        if(input$eulsymbol==1) eulsymbol <- "c"
        if(input$eulsymbol==2) eulsymbol <- "s"
        if(input$eulsymbol==3) eulsymbol <- "d"
        if(input$eulsymbol==4) eulsymbol <- "t"
        
        
        if(MVGP_list$vgps[r,8]==""){
          #Added_poles$list
          temp <- MVGP_list$vgps[r,]
          #next because otherwise gives error
          temp$Lon <- as.numeric(temp$Lon)
          temp$Lat <- as.numeric(temp$Lat)
          temp$A95 <- as.numeric(temp$A95)
          assign("butta",input$eul_long,.GlobalEnv)
          temp[1,5:6] <- round(PmagDiR::rot_DI(temp[1,5:6],               
                                               P_long = input$eul_long,
                                               P_lat = input$eul_lat,
                                               rot = input$eul_rot),digits = 2)
          temp[1,2:3] <- t(c(eulcolor,eulsymbol))
          temp[1,1] <- paste(temp[1,1],"_Rt",sep="")
          A95rotate$new <- rbind(A95rotate$new,temp)
        }else{
          #new list with single rotated entry
          vgp_rot <- list()
          
          
          #copy file to rotate in new list
          vgp_rot[[1]] <- VGP_saved$list[[MVGP_list$vgps[r,1]]][[1]]
          #rotate VGPs
          vgp_rot[[1]][,1:2] <- PmagDiR::rot_DI(Lonlat = vgp_rot[[1]][,1:2],
                                                P_long = input$eul_long,P_lat = input$eul_lat,
                                                rot = input$eul_rot)
          #copy color and symbol to file for saving it in list
          vgp_rot[[1]][,3] <- rep(eulcolor)
          vgp_rot[[1]][,4] <- rep(eulsymbol)
          
          
          #copy original VGPs stats to be rotated
          vgp_rot[[2]] <- VGP_saved$list[[MVGP_list$vgps[r,1]]][[2]]
          #rotate fisher
          vgp_rot[[2]][[1]][1,2:3] <- PmagDiR::rot_DI(Lonlat = vgp_rot[[2]][[1]][1,2:3],
                                                      P_long = input$eul_long,P_lat = input$eul_lat,rot = input$eul_rot)
          #rotate B95
          vgp_rot[[2]][[2]][1,2:3] <- PmagDiR::rot_DI(Lonlat = vgp_rot[[2]][[2]][1,2:3],
                                                      P_long = input$eul_long,P_lat = input$eul_lat,rot = input$eul_rot)
          #rotate bootstrapped dirs
          vgp_rot[[2]][[3]][,1:2] <- PmagDiR::rot_DI(Lonlat = vgp_rot[[2]][[3]][,1:2],
                                                     P_long = input$eul_long,P_lat = input$eul_lat,rot = input$eul_rot)
          #transfer data to reactive file
          vgprotate$new[[NAME]] <- vgp_rot
        }
      }
    }
    #since A95rotate$new is treated like a table, it is created but made null if nrow is zero
    if(nrow(A95rotate$new)==0){A95rotate$new <- NULL}
  })
  
  #function adding rotated data to list                 
  observeEvent(input$eulersave,{
    if(!is.null(vgprotate$new)){
      VGP_saved$list <- append(VGP_saved$list,vgprotate$new)
    }
    if(!is.null(A95rotate$new)){
      Added_poles$list <- rbind(Added_poles$list,A95rotate$new)
    }
    MVGP_list$vgps <- sites_list()
    vgprotate$new <- NULL
    A95rotate$new <- NULL
  })
  
  #send figure to ui
  output$eulerplot <- renderPlot({
    req(MVGP_list$vgps)
    r <- input$VGPs_List_rows_selected
    if(length(r)){
      centerLat <-  as.numeric(MVGP_list$vgps[r[1],6])
      centerLong <- as.numeric(MVGP_list$vgps[r[1],5])
      
      #plot empty spherical orthographic
      PmagDiR::sph_ortho(lat = centerLat,long = centerLong,
                         coast = T)        
      #plot original selected data
      for(i in r){
        if(MVGP_list$vgps[i,8]==""){
          PmagDiR::plot_PA95(lon = as.numeric(MVGP_list$vgps[i,5]),
                             lat = as.numeric(MVGP_list$vgps[i,6]),
                             A = as.numeric(MVGP_list$vgps[i,7]),
                             lon0 = centerLong,
                             lat0 = centerLat,
                             col_f = MVGP_list$vgps[i,2],
                             symbol = MVGP_list$vgps[i,3],
                             on_plot = TRUE)
        }else{
          plot_selected_VGP_eul(r = i,lat0 = centerLat,lon0 = centerLong)
        }
      }
      #check if there are rotated data
      if(!is.null(vgprotate$new)){ 
        #it plots the  the rotated entries one by one
        for(i in 1:length(vgprotate$new)){
          #isolates the dataset
          Rt_Data_to_plot <- vgprotate$new[[i]]
          if(input$eulPlotType==1){
            plot_VGP_S(VGP = Rt_Data_to_plot[[1]],
                       lat = centerLat,
                       long = centerLong,
                       col= Rt_Data_to_plot[[1]][1,3],
                       symbol = Rt_Data_to_plot[[1]][1,4],
                       on_plot = T, auto_cent = F)
          }else if(input$eulPlotType==2){
            PmagDiR::plot_PA95(lon = Rt_Data_to_plot[[2]][[1]][1,2],
                               lat = Rt_Data_to_plot[[2]][[1]][1,3],
                               A = Rt_Data_to_plot[[2]][[1]][1,4],
                               lat0 = centerLat,
                               lon0 = centerLong,
                               col_A = rgb(1,0,1,0.15),
                               symbol = Rt_Data_to_plot[[1]][1,4],
                               col_f = Rt_Data_to_plot[[1]][1,3],
                               on_plot = T)
          }else if(input$eulPlotType==3){
            plot_VGP_S(VGP=Rt_Data_to_plot[[2]][[3]][,1:2],
                       lat = centerLat,
                       long = centerLong,
                       auto_cent = F, on_plot = T,
                       col = rgb(1,0,0,0.1),col_sym_out = rgb(1,0,0,0.1),
                       symbol = "c")
            PmagDiR::plot_PA95(lon = Rt_Data_to_plot[[2]][[2]][1,2],
                               lat = Rt_Data_to_plot[[2]][[2]][1,3],
                               A = 0,
                               lon0 = centerLong,
                               lat0 = centerLat,
                               col_f = Rt_Data_to_plot[[1]][1,3],
                               symbol = Rt_Data_to_plot[[1]][1,4],
                               size = 1.2,on_plot = T)
          }
        }
      }
      if(!is.null(A95rotate$new)){
        for(i in 1:nrow(A95rotate$new)){
          PmagDiR::plot_PA95(lon = as.numeric(A95rotate$new[i,5]),
                             lat = as.numeric(A95rotate$new[i,6]),
                             A = as.numeric(A95rotate$new[i,7]),
                             lon0 = centerLong,
                             lat0 = centerLat,on_plot = T,
                             col_f = A95rotate$new[i,2],
                             symbol = A95rotate$new[i,3])
        }
      }
    }
    
    
  },width = 710, height = 710)
  
  #########################MODAL DIALOG WITH MERGED VGPs DETAILS##########
  ####Opens modal window
  observeEvent(input$merg_VGPs,{
    # display a modal dialog with a header, text input and action buttons
    showModal(jqui_draggable(modalDialog(size = "l",
                                         tags$h3('Merge multiple VGPs sets into a single set'),
                                         br(),
                                         fluidRow(
                                           column(3,textInput("fileN_MVGP",label = "M-VGPs Name",value = "M-VGPs")),
                                           column(3,selectInput("MVGPsPlotType",label = "Type plotted",
                                                                choices = list("VGPs"=1,"Fisher"=2,"Bootstrapped"=3),selected = 1)),
                                           column(3,selectInput("MVGP_aver_sym", label = "Symbol",
                                                                choices = list("circle"=1, "square"=2, "diamond"=3,"triangle"=4),selected=1)),
                                           column(3,selectInput("MVGP_aver_color", label = "Color",
                                                                choices= list("black"=1,"blue"=2,"green"=3,"pink"=4,"purple"=5,"brown"=6,"red"=7,"yellow"=8,"cyan"=9,"gray"=10,"white"=11), selected=7)),
                                         ),
                                         br(),
                                         fluidRow(
                                           column(3,numericInput("MVGPnb", label = "Bootstrap n.", value = 2000)),
                                           column(9,progressBar(
                                             id = "Mvgpboot",
                                             value = 0,total=2000,
                                             title = "VGPs bootstrap",
                                             display_pct = TRUE))
                                         ),
                                         fluidRow(
                                           column(12,actionButton("add_MVGPs",label = "Add to list of loaded VGPs and Poles",width = "100%")),
                                         ),
                                         #result of statistic 
                                         h5(textOutput("MVGP_ALLVGPS_stat")),
                                         fluidRow(
                                           column(1),
                                           plotOutput(outputId = "MergeVGP_plot")
                                         ),
                                         #makes window deeper
                                         br(),
                                         br(),
                                         br(),
                                         br(),
                                         br(),
                                         br(),
                                         br(),
                                         br(),
                                         br(),
                                         br(),
                                         br(),
                                         br(),      
                                         easyClose = TRUE,
                                         footer=tagList(
                                           modalButton('close')
                                         ) 
    ),options = list(cancel = ".shiny-input-container")))  
  })
  
  #POLE output editable list
  output$VGPs_List <- DT::renderDataTable({
    req(MVGP_list$vgps)  
    datatable(MVGP_list$vgps, editable = list(target="cell", disable= list(columns=c(3,4,5,6,7,8))),
              rownames = F)%>%
      formatStyle(
        columns = c("Loc.","Col","Sym","N","Lon","Lat","A95","B95","K"), 
        color = 'black'   # Colore del testo per le celle editabili
      )
  })
  
  #Modify poles table throught UI               
  observeEvent(input$VGPs_List_cell_edit, {
    info <- input$VGPs_List_cell_edit
    #current values in table
    modified_data <-  MVGP_list$vgps  
    #update values (plus one otherwise in paste to the wrong column. Do not ask me why)
    modified_data[info$row, (info$col+1)] <- info$value 
    #select old data to change also list
    old_data <- MVGP_list$vgps[info$row,]
    #select only old name for VGP list modification
    old_Loc_name <- MVGP_list$vgps[info$row,1]
    #update table
    MVGP_list$vgps <- modified_data
    #MODIFY ALSO LIST WITH NEW NAME, COLOR AND SYMBOL
    #if it is a paleomagnetic pole comparing old line with saved list, if it is the same it changes with new
    if(MVGP_list$vgps[info$row,8]==""){
      for(i in 1:nrow(Added_poles$list)){
        if(all(Added_poles$list[i,]==old_data)){
          Added_poles$list[i,] <- MVGP_list$vgps[info$row,]
        }
      }
    }else{
      #if it is a VGPs file
      names(VGP_saved$list)[names(VGP_saved$list)==old_Loc_name] <- MVGP_list$vgps[info$row,1]
      VGP_saved$list[[MVGP_list$vgps[info$row,1]]][[1]][["color"]] <- MVGP_list$vgps[info$row,2]
      VGP_saved$list[[MVGP_list$vgps[info$row,1]]][[1]][["symbol"]] <- MVGP_list$vgps[info$row,3]
      assign("butta",VGP_saved$list,.GlobalEnv)
    }
  })
  
  #delete VGP or POLE from reactive lists
  observeEvent(input$deletevgp,{                      
    e <- input$VGPs_List_rows_selected
    for(i in e){
      #if is an external paleopole it deletes it from the loaded list
      if(MVGP_list$vgps[i,8]==""){
        Added_poles$list <- Added_poles$list[-which(Added_poles$list[,1]==MVGP_list$vgps[i,1],arr.ind = T),] 
      } 
      #eliminate selected names from global list
      VGP_saved$list <- VGP_saved$list[names(VGP_saved$list) %in% MVGP_list$vgps[i,1]==F]
    }
    MVGP_list$vgps <- sites_list()
  })
  
  #merge different VGPs into one and create statistics
  observeEvent(input$add_MVGPs,{
    #Vanno ridefiniti qui se no mi da un errore
    #choose color of average
    if(input$MVGP_aver_color==1) MrVGP$col_f <- "black"
    if(input$MVGP_aver_color==2) MrVGP$col_f <- "blue"
    if(input$MVGP_aver_color==3) MrVGP$col_f <- "green"
    if(input$MVGP_aver_color==4) MrVGP$col_f <- "pink"
    if(input$MVGP_aver_color==5) MrVGP$col_f <- "purple"
    if(input$MVGP_aver_color==6) MrVGP$col_f <- "brown"
    if(input$MVGP_aver_color==7) MrVGP$col_f <- "red"
    if(input$MVGP_aver_color==8) MrVGP$col_f <- "yellow"
    if(input$MVGP_aver_color==9) MrVGP$col_f <- "cyan"
    if(input$MVGP_aver_color==10) MrVGP$col_f <- "gray"
    if(input$MVGP_aver_color==11) MrVGP$col_f <- "white"
    
    #select symbol of average
    if(input$MVGP_aver_sym==1) MrVGP$MVGPaversym <- "c"
    if(input$MVGP_aver_sym==2) MrVGP$MVGPaversym <- "s"
    if(input$MVGP_aver_sym==3) MrVGP$MVGPaversym <- "d"
    if(input$MVGP_aver_sym==4) MrVGP$MVGPaversym <- "t"
    
    s <- input$VGPs_List_rows_selected
    
    #rebuild name for checking if stats exist and not replicate
    MVGP_name <- "MVGP_stat"
    
    if(length(s)){
      MergedVGP <- list()
      MergedVGP_t <- data.frame(matrix(ncol = 2,nrow = 0))
      colnames(MergedVGP_t) <- c("Long","Lat")
      for(i in s){
        if(MVGP_list$vgps[i,8]!=""){
          VGP <- VGP_saved$list[[MVGP_list$vgps[i,1]]][[1]][,1:2]
          colnames(VGP) <- c("Long","Lat")
          MergedVGP_t <- rbind(MergedVGP_t,VGP)
          #fill the file name with all sites selected
          MVGP_name <- paste(MVGP_name,MVGP_list$vgps[i,1],sep="_")
        }
      }
      MergedVGP_t$Col <- rep(MrVGP$col_f)
      MergedVGP_t$Sym <- rep(MrVGP$MVGPaversym)
      MergedVGP[[1]] <- MergedVGP_t
      #add fisher and Bootstrap statistics and bootstrapped data (to fix with condition if boot already exists) ######################!!!!!!!!!!
      MergedVGP[[2]] <- plot_VGP_S(VGP = MergedVGP_t[,1:2],
                                   A95 = T,
                                   B95 = T,
                                   nb = input$MVGPnb,
                                   VGPint = 3,
                                   plot=F)
      #add to VGP saved list
      VGP_saved$list[[input$fileN_MVGP]] <- MergedVGP
      # }
      
      #add to interactive and preliminary tables
      MVGP_list$vgps <- sites_list()
    }
  })
  
  #send Merged stereo figure to UI
  output$MergeVGP_plot <- renderPlot({
    all_poles_plotter(MVGP_ModalDialog = TRUE,Type=input$MVGPsPlotType)
  },width = 710, height = 710)
  
  #########################MODAL DIALOG WITH APWP DETAILS##########
  observeEvent(input$add_apwp,{
    showModal(jqui_draggable(modalDialog(size = "m",
                                         tags$h3('Add Apparent Polar Wander Path to the plot'),
                                         br(),
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
                                           column(4,fileInput("customAPWP",label = "Custom APWP")),
                                           column(4,numericInput("apwp_Y",label = "APWP min age",value = 0)),
                                           column(4,numericInput("apwp_O",label = "APWP max age",value = 320))
                                         ),
                                         br(),
                                         fluidRow(
                                           column(12,h5("Compiled available global synthetic APWPs: "), tags$a(href="https://doi.org/10.1016/j.earscirev.2023.104547", 
                                                                                                               "(V2023): Vaes, B. et al. (2023). Earth-Science Reviews, 245(104547), 1–35.", target="_blank")),
                                           column(12, tags$a(href="http://linkinghub.elsevier.com/retrieve/pii/S0012825212000797", 
                                                             "(T2012): Torsvik, T.H., et al. (2012). Earth-Science Reviews, 114(3–4), 325–368.", target="_blank"))
                                         ),    
                                         easyClose = TRUE,
                                         footer=tagList(
                                           modalButton('close')
                                         ) 
    ),options = list(cancel = ".shiny-input-container")))  
  })
  
  #########################MODAL DIALOG WITH LOCALITY LIST#########
  #add localities to list for plotting
  #create list file
  localities <- reactiveValues(list=NULL)
  
  #open window with locality plot details
  observeEvent(input$localitydetails, {
    # display a modal dialog with a header, textinput and action buttons
    showModal(jqui_draggable(modalDialog(size = "m",
                                         tags$h2('Enter Locality details'),
                                         tags$h5('Default name and coordinates are from Direction display & average window'),
                                         fluidRow(
                                           column(4,textInput("LocName",label = "Name",value = input$fileN)),
                                           column(4,numericInput("LocLat",label = "Latitude",value = input$lat,min = -90,max = 90)),
                                           column(4,numericInput("LocLong",label = "Longitude",value = input$long,min = 0,max = 180))
                                         ),
                                         fluidRow(
                                           column(4,selectInput("LocSymbol", label= "Symbol",
                                                                choices = list("circle"=1, "square"=2, "diamond"=3,"Triangle"=4),selected=1)),
                                           column(4,selectInput("LocColor", label= "Symbol color",
                                                                choices= list("black"=1,"blue"=2,"green"=3,"pink"=4,"purple"=5,"brown"=6,"red"=7,"yellow"=8,"cyan"=9,"gray"=10, "white"=11), selected=7)),
                                           column(4,numericInput("LocSize",label = "Symbol size",value = 1)),
                                         ),
                                         fluidRow(
                                           column(6, actionButton("addlocality",label = "Add to list",width = "100%")),
                                           column(6, actionButton("cutlocality",label = "Delete from list",width = "100%"))
                                         ),
                                         br(),
                                         fluidRow(
                                           column(12,DT::dataTableOutput("LocList"))
                                         ),
                                         easyClose = TRUE,
                                         footer=tagList(
                                           modalButton('close')
                                         )
    ),options = list(cancel = ".shiny-input-container")))
  })
  
  #Add locality details to list
  observeEvent(input$addlocality,{
    #check if list already exists
    if(is.null(localities$list)==T){
      localities$list <- data.frame(matrix(ncol=6,nrow = 0))
      colnames(localities$list) <- c("Name","Lat","Long","Sym","Col","Size")
    }
    #choose color of pole
    if(input$LocColor==1) LocColor <- "black"
    if(input$LocColor==2) LocColor <- "blue"
    if(input$LocColor==3) LocColor <- "green"
    if(input$LocColor==4) LocColor <- "pink"
    if(input$LocColor==5) LocColor <- "purple"
    if(input$LocColor==6) LocColor <- "brown"
    if(input$LocColor==7) LocColor <- "red"
    if(input$LocColor==8) LocColor <- "yellow"
    if(input$LocColor==9) LocColor <- "cyan"
    if(input$LocColor==10) LocColor <- "gray"
    if(input$LocColor==11) LocColor <- "white"
    
    #select symbol of pole
    if(input$LocSymbol==1) LocSymbol <- "c"
    if(input$LocSymbol==2) LocSymbol <- "s"
    if(input$LocSymbol==3) LocSymbol <- "d"
    if(input$LocSymbol==4) LocSymbol <- "t"
    
    temp <- data.frame(matrix(ncol=6,nrow = 0))
    colnames(temp) <- c("Name","Lat","Long","Sym","Col","Size")
    temp[1,1] <- input$LocName
    temp[1,2] <- input$LocLat
    temp[1,3] <- input$LocLong
    temp[1,4] <- LocSymbol
    temp[1,5] <- LocColor
    temp[1,6] <- input$LocSize
    
    localities$list <- rbind(localities$list,temp)
    
    if(nrow(localities$list)==0){localities$list <- NULL}
  })
  
  #send table to UI and make it editable cell by cell
  output$LocList <- DT::renderDataTable({
    req(localities$list)  
    datatable(localities$list, editable = TRUE,rownames = F,
              options = list(
                dom = 't',  # 't' mostra solo la tabella, senza l'intestazione e ricerca
                searching = T # Disabilita la ricerca
              ))%>%
      formatStyle(
        columns = c("Name","Lat","Long","Sym","Col","Size"), 
        color = 'black'   # Colore del testo per le celle editabili
      )
  })
  
  #modify locality table through UI
  observeEvent(input$LocList_cell_edit, {
    info <- input$LocList_cell_edit
    #current values in table
    modified_data <- localities$list  
    #update values (plus one otherwise in paste to the wrong clumn. Do not ask me why, it's like a chopper flying without spinning blades)
    modified_data[info$row, (info$col+1)] <- info$value 
    #update table
    localities$list <- modified_data
  })
  
  #delete Locality from list
  observeEvent(input$cutlocality,{
    if(!is.null(localities$list)){
      d <- input$LocList_rows_selected
      if(length(d)){localities$list <- localities$list[-d,]}
    }
    if(nrow(localities$list)==0){localities$list <- NULL}
  })
  
  #########################MODAL DIALOG WITH SMALL CIRCLE#########
  #create list file
  smallcircle <- reactiveValues(list=NULL)
  
  #open window
  observeEvent(input$smCircle,{
    # display a modal dialog with a header, textinput and action buttons
    showModal(jqui_draggable(modalDialog(
      tags$h2('Enter Small Circle details'),
      tags$h5('If center of circle is requred, please define it using the Add Locality window'),
      fluidRow(
        column(4,textInput(inputId = "SCname",label = "Name",value = "Circle")),
        column(4,numericInput(inputId = "SClat",label = "Latitude",value = 0,min = -90,max = 90)),
        column(4,numericInput(inputId = "SClong",label = "Longitude",value = 0))
      ),
      fluidRow(
        column(4,numericInput(inputId = "SCrad",label = "Radius",value = 0,min = 0,max = 180)),
        column(4,selectInput(inputId = "SCstyle",label = "Circle style",
                             choices = list("1-line"=1,"2-dashed"=2,"3-dotted"=3, "4-dotdash"=4),selected = 1)),
        column(4,selectInput("SCcolor", label= "Circle color",
                             choices= list("black"=1,"blue"=2,"green"=3,"pink"=4,"purple"=5,"brown"=6,"red"=7,"yellow"=8,"cyan"=9,"gray"=10), selected=1)),
      ),
      fluidRow(
        column(6, actionButton("addSC",label = "Add to list",width = "100%")),
        column(6, actionButton("cutSC",label = "Delete from list",width = "100%"))
      ),
      br(),
      fluidRow(
        column(12,DT::dataTableOutput("SCList"))
      ),
      easyClose = TRUE,
      footer=tagList(
        modalButton('close')
      )
    ),options = list(cancel = ".shiny-input-container")))
    
  })
  
  #add circle to list
  observeEvent(input$addSC,{
    #check if list already exists
    if(is.null(smallcircle$list)==T){
      smallcircle$list <- data.frame(matrix(ncol=6,nrow = 0))
      colnames(smallcircle$list) <- c("Name","Lat","Long","Radius","Col","Style")
    }
    #choose color of pole
    if(input$SCcolor==1) SCcolor <- "black"
    if(input$SCcolor==2) SCcolor <- "blue"
    if(input$SCcolor==3) SCcolor <- "green"
    if(input$SCcolor==4) SCcolor <- "pink"
    if(input$SCcolor==5) SCcolor <- "purple"
    if(input$SCcolor==6) SCcolor <- "brown"
    if(input$SCcolor==7) SCcolor <- "red"
    if(input$SCcolor==8) SCcolor <- "yellow"
    if(input$SCcolor==9) SCcolor <- "cyan"
    if(input$SCcolor==10) SCcolor <- "gray"
    
    
    temp <- data.frame(matrix(ncol=6,nrow = 0))
    colnames(temp) <- c("Name","Lat","Long","Radius","Col","Style")
    temp[1,1] <- input$SCname
    temp[1,2] <- input$SClat
    temp[1,3] <- input$SClong
    temp[1,4] <- input$SCrad
    temp[1,5] <- SCcolor
    temp[1,6] <- input$SCstyle
    
    smallcircle$list <- rbind(smallcircle$list,temp)
    
    if(nrow(smallcircle$list)==0){smallcircle$list <- NULL}
  })
  
  #send table to UI and make it editable cell by cell
  output$SCList <- DT::renderDataTable({
    req(smallcircle$list)  
    datatable(smallcircle$list, editable = TRUE,rownames = F,
              options = list(
                dom = 't',  # 't' mostra solo la tabella, senza l'intestazione e ricerca
                searching = T # Disabilita la ricerca
              ))%>%
      formatStyle(
        columns = c("Name","Lat","Long","Radius","Col","Style"), 
        color = 'black'   # Colore del testo per le celle editabili
      )
  })
  
  #Modify poles table throught UI               
  observeEvent(input$SCList_cell_edit, {         
    info <- input$SCList_cell_edit
    #current values in table
    modified_data <-  smallcircle$list  
    #update values (plus one otherwise in paste to the wrong column. Do not ask me why)
    modified_data[info$row, (info$col+1)] <- info$value 
    #update table
    smallcircle$list <- modified_data
  })
  
  #delete Circle from list
  observeEvent(input$cutSC,{
    if(!is.null(smallcircle$list)){
      d <- input$SCList_rows_selected
      if(length(d)){smallcircle$list <- smallcircle$list[-d,]}
    }
    if(nrow(smallcircle$list)==0){smallcircle$list <- NULL}
  })
  
  #########################MODAL DIALOG WITH EXTERNAL PMAG POLE#########
  
  ######open modal window
  observeEvent(input$add_PPole,{
    #reset input file or it stays there
    extpole$listfile <- NULL
    # display a modal dialog with a header, textinput and action buttons
    showModal(jqui_draggable(modalDialog(size = "m",
                                         tags$h2('External paleomagnetic poles'),
                                         tags$h5("Read only Magnetic-A exported (or same format) file"),
                                         fluidRow(
                                           column(4,fileInput("extrapolelist",label = "Load pole list"))
                                         ),
                                         fluidRow(
                                           column(4,numericInput("extrapolelong",label = "Pole long",value = 0,min = -360,max = 360)),
                                           column(4,numericInput("extrapolelat",label = "Pole lat",value = 0,min = -90,max = 90)),
                                           column(4,numericInput("extrapoleA95",label = "95% confidence",value =0,min = 0,max = 180))
                                         ),
                                         fluidRow(
                                           column(4,selectInput("extrapolecolor", label= "Pole color",
                                                                choices= list("black"=1,"blue"=2,"green"=3,"pink"=4,"purple"=5,"brown"=6,"red"=7,"yellow"=8,"cyan"=9,"gray"=10, "white"=11), selected=2)),
                                           column(4,selectInput("extrapolesimbol", label= "Pole symbol",
                                                                choices = list("circle"=1, "square"=2, "diamond"=3,"triangle"=4),selected=1)),
                                           column(4,textInput("extrapolename",label = " Manual pole name",value = "E-Pole"))
                                         ),
                                         br(),
                                         fluidRow(
                                           column(12, actionButton("addextrapole",label = "Add to list of loaded VGPs and Poles",width = "100%"))
                                         ),
                                         br(),
                                         easyClose = TRUE,
                                         footer=tagList(
                                           modalButton('close')
                                         )
    ),options = list(cancel = ".shiny-input-container")))
  })
  
  #create reactive tab file
  Added_poles <- reactiveValues(list=NULL)
  
  #creates reactive value for checking if file is uploaded and to append FISHER 
  extpole <- reactiveValues(listfile = NULL)
  #check for uploaded file
  observeEvent(input$extrapolelist,{extpole$listfile <- "uploaded"})
  #reset upload if requested
  #observeEvent(input$delextrapolelist,{extpole$listfile <- "reset"})
  #read file if present, reset if requested
  #read only magnetica exported files
  Extra_poles_list <- reactive({
    if(is.null(extpole$listfile)){
      return(NULL)
    }else if(extpole$listfile == "uploaded"){    
      temp <- read.csv(file = input$extrapolelist$datapath)
      colnames(temp) <- c("Loc.","Col","Sym","N","Lon","Lat","A95","B95","K")
      #remove N because there are not calculation, just copied values
      return(temp)
    }else if(extpole$listfile == "reset"){
      return(NULL)
    }
  })
  
  #add MANUAL pole to list if exists
  observeEvent(input$addextrapole,{
    #check if list already exists
    if(is.null(Added_poles$list)){
      Added_poles$list <- data.frame(matrix(ncol=9,nrow = 0))
      colnames(Added_poles$list) <- c("Loc.","Col","Sym","N","Lon","Lat","A95","B95","K")
    }
    #choose color of pole
    if(input$extrapolecolor==1) EPcol <- "black"
    if(input$extrapolecolor==2) EPcol <- "blue"
    if(input$extrapolecolor==3) EPcol <- "green"
    if(input$extrapolecolor==4) EPcol <- "pink"
    if(input$extrapolecolor==5) EPcol <- "purple"
    if(input$extrapolecolor==6) EPcol <- "brown"
    if(input$extrapolecolor==7) EPcol <- "red"
    if(input$extrapolecolor==8) EPcol <- "yellow"
    if(input$extrapolecolor==9) EPcol <- "cyan"
    if(input$extrapolecolor==10) EPcol <- "gray"
    if(input$extrapolecolor==11) EPcol <- "white"
    
    #select symbol of pole
    if(input$extrapolesimbol==1) EPsym <- "c"
    if(input$extrapolesimbol==2) EPsym <- "s"
    if(input$extrapolesimbol==3) EPsym <- "d"
    if(input$extrapolesimbol==4) EPsym <- "t"
    
    temp <- data.frame(matrix(ncol=9,nrow = 0))
    colnames(temp) <- c("Loc.","Col","Sym","N","Lon","Lat","A95","B95","K")
    temp[1,1] <- input$extrapolename
    temp[1,2] <- EPcol
    temp[1,3] <- EPsym
    temp[1,4] <- ""
    temp[1,5] <- input$extrapolelong
    temp[1,6] <- input$extrapolelat
    temp[1,7] <- input$extrapoleA95
    temp[1,8] <- ""
    temp[1,9] <- ""
    
    if(input$extrapolename!="" && !is.na(input$extrapolelong) && !is.na(input$extrapolelat) && !is.na(input$extrapoleA95)) {
      Added_poles$list <- rbind(Added_poles$list,temp)
    }
    
    if(!is.null(extpole$listfile)){
      temp_file <- Extra_poles_list()
      temp_file[,8] <- rep("")
      Added_poles$list <- rbind(Added_poles$list,temp_file)
    }
    
    #eliminates duplicates
    Added_poles$list <- dplyr::distinct(Added_poles$list)
    if(nrow(Added_poles$list)==0){Added_poles$list <- NULL}
    MVGP_list$vgps <- sites_list()
  })
  
  #########################MODAL DIALOG WITH FISHER A95 DETAILS##########
  #####open modal window
  observeEvent(input$add_A95,{
    # display a modal dialog with a header, text input and action buttons
    showModal(jqui_draggable(modalDialog(size = "m",
                                         tags$h3('Calculate A95 of selected poles'),
                                         br(),
                                         fluidRow(
                                           column(4,textInput(inputId = "name_A95",label = "Pole name",value = "M-A95")),
                                           column(4,selectInput("A95_sym", label = "Symbol",
                                                                choices = list("circle"=1, "square"=2, "diamond"=3,"triangle"=4),selected=1)),
                                           column(4,selectInput("A95_color", label = "Color",
                                                                choices= list("black"=1,"blue"=2,"green"=3,"pink"=4,"purple"=5,"brown"=6,"red"=7,"yellow"=8,"cyan"=9,"gray"=10,"white"=11), selected=7)),
                                         ),
                                         br(),
                                         fluidRow(
                                           column(12,actionButton("add_FISHER",label = "Add to list of loaded VGPs and Poles",width = "100%")),
                                         ),
                                         #result of statistic 
                                         h5(textOutput("Fisher_A95")),
                                         #makes window deeper
                                         br(),      
                                         easyClose = TRUE,
                                         footer=tagList(
                                           modalButton('close')
                                         ) 
    ),options = list(cancel = ".shiny-input-container")))  
  })
  
  #Calculate Fisher of selected poles, SENT TO UI within "all_poles_plotter"                
  extfisher <- reactive({             
    extlist <- data.frame(matrix(ncol=2,nrow=0))
    colnames(extlist) <- c("Lon","Lat")
    #selects entries
    s <- input$VGPs_List_rows_selected
    
    if(length(s)){
      lonlat_temp <- MVGP_list$vgps[s,5:6]
      lonlat_temp$Lon <- as.numeric(lonlat_temp$Lon)
      lonlat_temp$Lat <- as.numeric(lonlat_temp$Lat)
      extlist <- rbind(extlist,lonlat_temp)
    }
    
    #if something is selected operates
    if(nrow(extlist)>1){
      #calculate fisher of poles
      Fisher <- PmagDiR::fisher(extlist)
      Fisher <- Fisher[,-5]
      colnames(Fisher) <- c("Long","Lat","A95","N","K")
      extpole$Fisher <- Fisher
    }
    #if only one entry it makes it null, otherwise it stays memorised
    if(nrow(extlist)==1){extpole$Fisher <- NULL}
    #function return values used in all_poles_plotter
    return(extpole$Fisher)
  })
  
  #calculate fisher of poles and load it into list;
  observeEvent(input$add_FISHER,{  
    req(extfisher())
    #color and symbol of average
    if(input$A95_color==1) A95_col <- "black"
    if(input$A95_color==2) A95_col <- "blue"
    if(input$A95_color==3) A95_col <- "green"
    if(input$A95_color==4) A95_col <- "pink"
    if(input$A95_color==5) A95_col <- "purple"
    if(input$A95_color==6) A95_col <- "brown"
    if(input$A95_color==7) A95_col <- "red"
    if(input$A95_color==8) A95_col <- "yellow"
    if(input$A95_color==9) A95_col <- "cyan"
    if(input$A95_color==10) A95_col <- "gray"
    if(input$A95_color==11) A95_col <- "white"
    #select symbol of average
    if(input$A95_sym==1) A95_sym <- "c"
    if(input$A95_sym==2) A95_sym <- "s"
    if(input$A95_sym==3) A95_sym <- "d"
    if(input$A95_sym==4) A95_sym <- "t"
    #fisher
    extfisher()
    #temporary file for sendin fisher to table
    temp <- data.frame(matrix(ncol=9,nrow = 0))
    colnames(temp) <- c("Loc.","Col","Sym","N","Lon","Lat","A95","B95","K")
    temp[1,1] <- input$name_A95
    temp[1,2] <- A95_col
    temp[1,3] <- A95_sym
    temp[1,4] <- extpole$Fisher[1,4]
    temp[1,5] <- round(extpole$Fisher[1,1],digits = 1)
    temp[1,6] <- round(extpole$Fisher[1,2],digits=1)
    temp[1,7] <- round(extpole$Fisher[1,3],digits=1)
    temp[1,8] <- ""
    temp[1,9] <- ""
    
    Added_poles$list <- rbind(Added_poles$list,temp)
    #update UI table
    MVGP_list$vgps <- sites_list()
  })
  
  #send fisher to UI
  output$Fisher_A95<- renderText({ 
    #next is because otherwise it keeps old fisher in memory
    s <- input$VGPs_List_rows_selected
    if(length(s)){
      extfisher()
      #if(input$extrapolesfisher==2){
      extpolefish <- paste("Fisher Average result: N: ",extpole$Fisher[1,4],",",
                           " Long: ",round(as.numeric(extpole$Fisher[1,1]), digits = 1),",",
                           " Lat: ", round(as.numeric(extpole$Fisher[1,2]), digits = 1),",",
                           " A95: ",round(as.numeric(extpole$Fisher[1,3]),digits = 1),",",
                           " K: ",round(as.numeric(extpole$Fisher[1,5]), digits = 1),
                           sep = "")
      extpolefish
    }
  })
  
  #########################MODAL DIALOG WITH GREAT CIRCLES DETAILS##########                          
  #reactive file
  greatcircle <- reactiveValues(list= NULL)
  
  #######OPEN MODAL WINDOW
  observeEvent(input$add_GCircle,{
    # display a modal dialog with a header, text input and action buttons
    showModal(jqui_draggable(modalDialog(size = "m",
                                         tags$h3('Calculate pole of plane through selected poles'),
                                         br(),
                                         fluidRow(
                                           column(4,textInput(inputId = "name_GC",label = "Plane name",value = "GCircle")),
                                           column(4,selectInput("GC_sym", label = "Symbol",
                                                                choices = list("circle"=1, "square"=2, "diamond"=3,"triangle"=4),selected=1)),
                                           column(4,selectInput("GC_color", label = "Color",
                                                                choices= list("black"=1,"blue"=2,"green"=3,"pink"=4,"purple"=5,"brown"=6,"red"=7,"yellow"=8,"cyan"=9,"gray"=10), selected=7)),
                                         ),
                                         fluidRow(
                                           column(6,selectInput(inputId = "GC_poleYN",label = "Plot pole",choices = list("Yes"=1,"No"=2),selected = 1,width = "100%")),
                                           column(6,selectInput(inputId = "add_loc_to_plane",label = "Add locality to Plane",choices = list("No"=1,"Yes"=2),selected = 1,width = "100%"))          ######### #ancora non VA
                                         ),
                                         br(),
                                         fluidRow(
                                           column(6,actionButton("add_GCPole",label = "Add Plane to list",width = "100%")),
                                           column(6,actionButton(inputId = "cut_GCPole",label = "Delete Plane from list",width = "100%"))
                                         ),
                                         #result of statistic 
                                         h5(textOutput("GC_MAD")),
                                         #makes window deeper
                                         br(),
                                         fluidRow(
                                           column(12,DT::dataTableOutput("GCList"))
                                         ),      
                                         easyClose = TRUE,
                                         footer=tagList(
                                           modalButton('close')
                                         ) 
    ),options = list(cancel = ".shiny-input-container")))  
  })
  
  #Calculate Great Circle Through of selected poles, SENT TO UI within "all_poles_plotter"             
  extGCPole <- reactive({             
    extlist <- data.frame(matrix(ncol=2,nrow=0))
    colnames(extlist) <- c("Lon","Lat")
    #selects entries
    s <- input$VGPs_List_rows_selected
    l <- input$LocList_rows_selected
    
    if(length(s)){
      lonlat_temp <- MVGP_list$vgps[s,5:6]
      lonlat_temp$Lon <- as.numeric(lonlat_temp$Lon)
      lonlat_temp$Lat <- as.numeric(lonlat_temp$Lat)
      extlist <- rbind(extlist,lonlat_temp)
    }
    if(length(l)){
      if(input$add_loc_to_plane==2){
        lonlat_temp <- localities$list[l,3:2]
        colnames(lonlat_temp) <- c("Lon","Lat")
        extlist <- rbind(extlist,lonlat_temp)
      }
    }
    
    #if something is selected operates
    if(nrow(extlist)>1){
      #calculate fisher of poles
      #calculate great circles through poles and MAD following Kirschvink 1980        
      circle <- PmagDiR::circle_DI(extlist)
      #turn solution in data.frame
      circle <- data.frame(t(circle))
      colnames(circle) <- c("PoleLong","PoleLat","MAD")
      greatcircle$circle <- circle
    }
    #if only one entry it makes it null, otherwise it stays memorised
    if(nrow(extlist)==1){greatcircle$circle <- NULL}
    #function return values used in all_poles_plotter
    return(greatcircle$circle)
  })
  
  #calculate GC of poles and localities and load it into list                  
  observeEvent(input$add_GCPole,{
    req(extGCPole())
    #check if list already exists
    if(is.null(greatcircle$list)==T){
      greatcircle$list <- data.frame(matrix(ncol=6,nrow = 0))
      colnames(greatcircle$list) <- c("Name","Lat","Long","MAD","Color","Symbol")
    }
    #color and symbol of average
    if(input$GC_color==1) GC_col <- "black"
    if(input$GC_color==2) GC_col <- "blue"
    if(input$GC_color==3) GC_col <- "green"
    if(input$GC_color==4) GC_col <- "pink"
    if(input$GC_color==5) GC_col <- "purple"
    if(input$GC_color==6) GC_col <- "brown"
    if(input$GC_color==7) GC_col <- "red"
    if(input$GC_color==8) GC_col <- "yellow"
    if(input$GC_color==9) GC_col <- "cyan"
    if(input$GC_color==10) GC_col <- "gray"
    #select symbol of average
    if(input$GC_sym==1) GC_sym <- "c"
    if(input$GC_sym==2) GC_sym <- "s"
    if(input$GC_sym==3) GC_sym <- "d"
    if(input$GC_sym==4) GC_sym <- "t"
    #does GC calculation
    extGCPole()
    #temporary file for sending GCircle to table
    temp <- data.frame(matrix(ncol=6,nrow = 0))
    colnames(temp) <- c("Name","Lat","Long","MAD","Color","Symbol")
    temp[1,1] <- input$name_GC
    temp[1,2] <- round(greatcircle$circle[1,2],digits=1)
    temp[1,3] <- round(greatcircle$circle[1,1],digits=1)
    temp[1,4] <- round(greatcircle$circle[1,3],digits=1)
    temp[1,5] <- GC_col
    temp[1,6] <- GC_sym
    
    greatcircle$list <- rbind(greatcircle$list,temp)
    
    if(nrow(greatcircle$list)==0){greatcircle$list <- NULL}
  })
  
  #send table to UI and make it editable cell by cell
  output$GCList <- DT::renderDataTable({
    req(greatcircle$list)  
    datatable(greatcircle$list, editable = list(target="cell", disable= list(columns=c(1,2,3))),
              rownames = F,
              options = list(
                dom = 't',  # 't' mostra solo la tabella, senza l'intestazione e ricerca
                searching = T # Disabilita la ricerca
              ))%>%
      formatStyle(
        columns = c("Name","Lat","Long","MAD","Color","Symbol"), 
        color = 'black'   # Colore del testo per le celle editabili
      )
  })
  
  #Modify GCircles table throught UI               
  observeEvent(input$GCList_cell_edit, { 
    info <- input$GCList_cell_edit
    #current values in table
    modified_data <-  greatcircle$list  
    #update values (plus one otherwise in paste to the wrong column. Do not ask me why)
    modified_data[info$row, (info$col+1)] <- info$value 
    #update table
    greatcircle$list <- modified_data
  })
  
  #delete Circle from list
  observeEvent(input$cut_GCPole,{
    req(greatcircle$list)
    if(!is.null(greatcircle$list)){
      d <- input$GCList_rows_selected
      if(length(d)){greatcircle$list <- greatcircle$list[-d,]}
    }
    if(nrow(greatcircle$list)==0){greatcircle$list <- NULL}
  })
  
  ######## FUNCTIONS FOR PLOTTING MULTIPLE VGP PARTS #########
  #plot VGPs selected from list
  plot_selected_VGP <- function(s,lat0,lon0,Type=1){
    #set bootfile name
    for(i in s){
      #selects what to plot
      if(Type==1){                                             #QUESTA PARTE è MODIFICATA PER PRENDERE COL E SYM DALLA TABELLA!!!
        plot_VGP_S(VGP=VGP_saved$list[[MVGP_list$vgps[i,1]]][[1]][,1:2],
                   lat = lat0,
                   long = lon0,
                   auto_cent = F, on_plot = T,
                   col = MVGP_list$vgps[i,2],
                   symbol = MVGP_list$vgps[i,3])
      }else if(Type==2){
        #plot fisher means and not VGPs if requested
        PmagDiR::plot_PA95(lon = VGP_saved$list[[MVGP_list$vgps[i,1]]][[2]][[1]][1,2],
                           lat = VGP_saved$list[[MVGP_list$vgps[i,1]]][[2]][[1]][1,3],
                           A = VGP_saved$list[[MVGP_list$vgps[i,1]]][[2]][[1]][1,4],
                           lat0 = lat0,lon0 = lon0,
                           col_A = rgb(0,0,1,0.15),
                           symbol = MVGP_list$vgps[i,3],
                           col_f = VGP_saved$list[[MVGP_list$vgps[i,1]]][[1]][1,3],
                           on_plot = T)
      }else if(Type==3){
        plot_VGP_S(VGP=VGP_saved$list[[MVGP_list$vgps[i,1]]][[2]][[3]][,1:2],
                   lat = lat0,long = lon0,
                   auto_cent = F, on_plot = T,
                   col = rgb(1,0,0,0.1),col_sym_out = rgb(1,0,0,0.1),
                   symbol = "c")
        PmagDiR::plot_PA95(lon = VGP_saved$list[[MVGP_list$vgps[i,1]]][[2]][[1]][1,2],
                           lat = VGP_saved$list[[MVGP_list$vgps[i,1]]][[2]][[1]][1,3],
                           A = 0,
                           lon0 = lon0,lat0 = lat0,
                           col_f = MVGP_list$vgps[i,2],
                           symbol = MVGP_list$vgps[i,3],
                           size = 1.2,on_plot = T)
      }
      
    }
  }
  
  #create reactive file for merged VGPs color and symbol
  MrVGP <- reactiveValues(col_f=NULL)
  
  #function that creates plots for Multiple VGP analysis
  all_poles_plotter <- function(MVGP_ModalDialog=FALSE,Type=1){
    
    #select elements from VGP and Poles table
    s <- input$VGPs_List_rows_selected
    
    #replicate list internally to use data
    VGPs_List <- sites_list()
    
    #check if VGPs or Poles exist
    if(length(s) && !is.null(VGPs_List)){ #s stays, this avoid error if no list exists
      centerLat = ifelse(input$MVGP_center==1, as.numeric(VGPs_List[s,6]),input$MVGP_clat)
      centerLong = ifelse(input$MVGP_center==1, as.numeric(VGPs_List[s,5]),input$MVGP_clong)
    }else{
      centerLat <- input$MVGP_clat
      centerLong <- input$MVGP_clong
    }
    
    #plot empty spherical orthographic
    PmagDiR::sph_ortho(lat = centerLat,long = centerLong,
                       coast = ifelse(input$MVGP_coast==1,TRUE,FALSE),
                       grid = input$MultiVGPGrid)
    
    #NEXT IS only for main window, not Multi VGPs modal dialog window, add loaclities and APWP, and fisher Poles
    if(MVGP_ModalDialog==FALSE){
      #Add Localities
      #check if list exists
      if(!is.null(localities$list)){
        d <- input$LocList_rows_selected
        if(length(d)){
          for(i in d){
            PmagDiR::plot_PA95(lon = localities$list[i,3],
                               lat = localities$list[i,2],
                               lon0 = centerLong,lat0 = centerLat,A = NA,
                               symbol = localities$list[i,4],
                               col_f = localities$list[i,5],
                               size = localities$list[i,6],
                               on_plot = T)
            locname <- localities$list[i,1]
            text(x=PmagDiR::c2x(localities$list[i,3],localities$list[i,2],centLon = centerLong),
                 y=PmagDiR::c2y(localities$list[i,3],localities$list[i,2],centLon = centerLong,centLat = centerLat),
                 pos=3,cex=1.2,substitute(paste(bold(locname))))
          }
        }
      }
      #add APWP
      if(!is.null(input$APWP)){
        #add APWP
        #read custom apwp file avoiding warning if not existing
        customAPWP <- reactive({
          #avoid warning if file is not loaded
          if (is.null(input$customAPWP)) {
            return(NULL)
          }
          # actually read the file
          read.csv(file = input$customAPWP$datapath)
        })
        
        #select APWP, either Vaes+2023 or Torsvik+2012
        if(input$APWP==2) {
          apwp <- "V23"
          frame <- as.numeric(input$frameV23)
        }
        if(input$APWP==3) {
          apwp <- "T12"
          frame <- as.numeric(input$frameT12)
        }
        
        #round to 10 if T12 or V23 are selected, to avoid problems in plotting pole using PmagDiR::plot_APWP
        if(input$APWP==2 || input$APWP==3){
          Y <- round(input$apwp_Y,-1)
          O <- round(input$apwp_O,-1)
        }else{
          Y <- input$apwp_Y
          O <- input$apwp_O
        }
        
        #plot apwp either Vaes+2023 or Torsvik+2012
        if(input$APWP==2 || input$APWP==3){
          PmagDiR::plot_APWP(APWP = apwp,
                             lat0 = centerLat,
                             lon0 = centerLong,
                             frame = frame,Shiny = T,Y = Y,O = O,size = 1.2,
                             coast = ifelse(input$MVGP_coast==1,TRUE,FALSE),Age_size = 1.5,on_plot = TRUE)
        }
        
        #plot custom APWP,
        if(input$APWP==4){        
          lat0 <-  centerLat
          lon0 <-  centerLong
          req(customAPWP())
          c_apwp <- customAPWP()
          #select age interval depending on slide select
          c_apwp <- c_apwp[(c_apwp[,1]>=Y & c_apwp[,1]<=O),]
          #plot line connecting poles
          lin <- as.data.frame(PmagDiR::c2x(c_apwp[,2],c_apwp[,3],centLon = lon0))
          colnames(lin) <- "lx"
          lin$ly <- PmagDiR::c2y(c_apwp[,2],c_apwp[,3],centLon = lon0,centLat = lat0)
          lines(lin$lx,lin$ly,cex=1)
          #plot poles
          for(i in 1:nrow(c_apwp)){
            PmagDiR::plot_PA95(lon = c_apwp[i,2],lat = c_apwp[i,3],A = c_apwp[i,4],
                               lon0 = centerLong,lat0 = centerLat,
                               col_f ="gray",col_A = rgb(1,0.9,0,0.30),on_plot = TRUE)
          }
          #plot min and max age
          text1 <- paste(c_apwp[1,1],"Ma")
          text2 <- paste(c_apwp[nrow(c_apwp),1], "Ma")
          text(x=lin[1,1], y=lin[1,2],pos=4,substitute(paste(bold(text1))), cex= 1.5)
          text(x=lin[nrow(lin),1], y=lin[nrow(lin),2],pos=4,substitute(paste(bold(text2))), cex= 1.5)
        }
      }
      if(!is.null(greatcircle$list)){
        l <- input$GCList_rows_selected
        for(i in l){
          PmagDiR::plot_plane_sph(P_long = greatcircle$list[i,3],
                                  P_lat = greatcircle$list[i,2],
                                  lon0 = centerLong,
                                  lat0 = centerLat,
                                  plot_pole =  ifelse(input$GC_poleYN==1,TRUE,FALSE),
                                  on_plot = TRUE,
                                  col_f = greatcircle$list[i,5],
                                  symbol= greatcircle$list[i,6],
                                  lwd=1.2)
        }
      }
    }
    
    #if any vgp in table is selected it plots them, and the statistic,          
    if(length(s) && !is.null(VGPs_List)){
      #plot selected VGPS
      for(i in s){
        if(VGPs_List[i,8]!=""){
          plot_selected_VGP(s=i,lat0 = centerLat,lon0 = centerLong,Type = Type)
        }else{
          if(MVGP_ModalDialog==F){
            PmagDiR::plot_PA95(lon = as.numeric(VGPs_List[i,5]),
                               lat = as.numeric(VGPs_List[i,6]),
                               lon0 = centerLong, 
                               lat0 = centerLat,
                               A =as.numeric(VGPs_List[i,7]),
                               col_f = VGPs_List[i,2],
                               symbol = VGPs_List[i,3],on_plot = T)
          }
        }
        #plot names
        if(input$MVGP_names_YN==2 && MVGP_ModalDialog==F){
          #define coordinate for name of pole
          x <- PmagDiR::c2x(as.numeric(VGPs_List[i,5]),as.numeric(VGPs_List[i,6]),centLon = centerLong)
          y <- PmagDiR::c2y(as.numeric(VGPs_List[i,5]),as.numeric(VGPs_List[i,6]),centLon = centerLong,centLat = centerLat)
          name <- VGPs_List[i,1]
          #plot names
          text(x=x, y=y,pos=3,substitute(paste(bold(name))), cex= 1.2)
        }
      }
      
      #if this has to go to the Merge VGPS modal dialog, activate the statistic option
      if(MVGP_ModalDialog==TRUE){
        #Fisher on VGPS or Boots on VGPs algorithm
        # if(input$MVGP_Pole_Stat==2||input$MVGP_Pole_Stat==3){
        
        #choose color of average
        if(input$MVGP_aver_color==1) MrVGP$col_f <- "black"
        if(input$MVGP_aver_color==2) MrVGP$col_f <- "blue"
        if(input$MVGP_aver_color==3) MrVGP$col_f <- "green"
        if(input$MVGP_aver_color==4) MrVGP$col_f <- "pink"
        if(input$MVGP_aver_color==5) MrVGP$col_f <- "purple"
        if(input$MVGP_aver_color==6) MrVGP$col_f <- "brown"
        if(input$MVGP_aver_color==7) MrVGP$col_f <- "red"
        if(input$MVGP_aver_color==8) MrVGP$col_f <- "yellow"
        if(input$MVGP_aver_color==9) MrVGP$col_f <- "cyan"
        if(input$MVGP_aver_color==10) MrVGP$col_f <- "gray"
        if(input$MVGP_aver_color==11) MrVGP$col_f <- "white"
        
        #select symbol of average
        if(input$MVGP_aver_sym==1) MrVGP$MVGPaversym <- "c"
        if(input$MVGP_aver_sym==2) MrVGP$MVGPaversym <- "s"
        if(input$MVGP_aver_sym==3) MrVGP$MVGPaversym <- "d"
        if(input$MVGP_aver_sym==4) MrVGP$MVGPaversym <- "t"
        
        #calculate fisher or bootstrap of all VGPS
        VGP_LonLat <- data.frame(matrix(ncol=2,nrow=0))
        colnames(VGP_LonLat) <- c("Long","Lat")
        
        #set bootsfile name for not repeating statistic
        MVGP_name <- "MVGP_stat"
        for(i in s){
          if(VGPs_List[i,8]!=""){
            #fill the file name with all sites selected
            MVGP_name <- paste(MVGP_name,MVGP_list$vgps[i,1],sep="_")
            #combine all VGPs of selected sets
            LonLat_t <- VGP_saved$list[[MVGP_list$vgps[i,1]]][[1]][1:nrow(VGP_saved$list[[MVGP_list$vgps[i,1]]][[1]]),1:2]
            colnames(LonLat_t) <- c("Long","Lat")
            VGP_LonLat <- rbind(VGP_LonLat,LonLat_t)
          }
        }
        # #calculate fisher
        # if(input$MVGP_Pole_Stat==2){
        MVGP_Fish_on_VGP <- PmagDiR::fisher(VGP_LonLat)
        #rebuild results for table
        MVGP_ALL_Fisher <- data.frame(matrix(ncol=5, nrow = 1),row.names = input$fileN_MVGP)
        colnames(MVGP_ALL_Fisher) <- c("N","Long","Lat","A95","K")
        MVGP_ALL_Fisher[1,1] <- MVGP_Fish_on_VGP[1,4]
        MVGP_ALL_Fisher[1,2] <- MVGP_Fish_on_VGP[1,1]
        MVGP_ALL_Fisher[1,3] <- MVGP_Fish_on_VGP[1,2]
        MVGP_ALL_Fisher[1,4] <- MVGP_Fish_on_VGP[1,3]
        MVGP_ALL_Fisher[1,5] <- MVGP_Fish_on_VGP[1,6]
        #populate and plot CURRENT VGP pole statistic as text on top of sphere
        output$MVGP_ALLVGPS_stat <- renderText({
          MVGP_ALLVGPS_stat <- paste("Average pole: N: ",MVGP_ALL_Fisher[1,1],",",
                                     " Long: ",round(MVGP_ALL_Fisher[1,2], digits = 1),",",
                                     " Lat: ", round(MVGP_ALL_Fisher[1,3], digits = 1),",",
                                     " A95: ",round(MVGP_ALL_Fisher[1,4],digits = 1),",",
                                     " K: ",round(MVGP_ALL_Fisher[1,5], digits = 1),
                                     sep = "")
        })
      }
    }
    if(!is.null(smallcircle$list)){
      sc <- input$SCList_rows_selected
      for(i in sc){
        PmagDiR::plot_SCircle(lon = smallcircle$list[i,3],
                              lat = smallcircle$list[i,2],
                              radius = smallcircle$list[i,4],
                              lon0 = centerLong,
                              lat0 = centerLat,
                              col_l = smallcircle$list[i,5],
                              lty = as.integer(smallcircle$list[i,6]),
                              lwd=1.2,
                              on_plot = TRUE)
        
      }
    }
  }
  
  #next few lines make size of figure adjustable
  Stereosize <- reactiveValues(size=710)
  observeEvent(input$plusSize,{
    Stereosize$size <- Stereosize$size+15
  })
  observeEvent(input$minusSize,{
    Stereosize$size <- Stereosize$size-15
  })
  
  #############SEND PLOTS TO VGP ANALYSIS FIGURES
  output$MVGP_plot <- renderPlot({
    all_poles_plotter(Type = input$VGPsType)
    MVGP_plot <- recordPlot()
    
    #Export graphic
    output$VGPs_G <- downloadHandler(
      filename = function() {
        paste(input$fileN_VGP, Sys.Date(), ".pdf", sep="")
      },
      content = function(file) {
        pdf(file, onefile = TRUE,width = 11,height = 11)
        replayPlot(MVGP_plot)
        dev.off()
      }
    )
    #export Poles list and stat
    output$VGPs_table <- downloadHandler(
      filename = function() {
        paste(input$fileN_VGP, Sys.Date(), ".csv", sep="")
      },
      content = function(file) {
        write.csv(MVGP_list$vgps,file,row.names = F)
      }
    )
    
    #this download the selected VGPs files, zipped if more than one
    output$VGPs_Sets <- downloadHandler(
      filename = function() {
        s <- input$VGPs_List_rows_selected
        #name is of the file if only one, generic if more
        paste(ifelse(length(s)>1,input$fileN_VGP,paste(MVGP_list$vgps[s,1],"_",sep = "")), 
              Sys.Date(), 
              ifelse(length(s)>1,".zip",".csv"), sep="")
      },
      content = function(file){
        s <- input$VGPs_List_rows_selected
        # if more VGPs are selected, create a list and make a zip file with all list. Zipping strategy copied from online geek
        if(length(s)>1){
          to_download <- list()
          for(i in s){
            if(MVGP_list$vgps[i,8]!=""){
              VGPs_name <- MVGP_list$vgps[i,1]
              to_download[[VGPs_name]] <- round(VGP_saved$list[[MVGP_list$vgps[i,1]]][[1]][,1:2],digits = 2)
            }
          }
          #next is from online user
          temp_directory <- file.path(tempdir(), as.integer(Sys.time()))
          dir.create(temp_directory)
          
          as.list(to_download) %>%
            imap(function(x,y){
              if(!is.null(x)){
                file_name <- glue("{y}_data.csv")
                readr::write_csv(x, file.path(temp_directory, file_name))
              }
            })
          zip::zip(
            zipfile = file,
            files = dir(temp_directory),
            root = temp_directory
          )
        }else{
          if(MVGP_list$vgps[s,8]!=""){
            #if only one is selected just copies long and lat from file
            write.csv(round(VGP_saved$list[[MVGP_list$vgps[s,1]]][[1]][,1:2], digits = 2),file,row.names = F)
          }
        }
      },
      contentType = "application/zip"
    )
    
    output$VGPs_stats <- downloadHandler(
      filename = function() {
        paste(input$fileN_VGP, Sys.Date(), ".csv", sep="")
      },
      content = function(file){
        s <- input$VGPs_List_rows_selected
        statistics <- data.frame(matrix(nrow=length(s),ncol=7))
        colnames(statistics) <- c("Pole","N","Long","Lat","A95","K","B95")
        statistics[s,1] <- MVGP_list$vgps[s,1]
        statistics[s,2:5] <- MVGP_list$vgps[s,4:7]
        statistics[s,6] <- MVGP_list$vgps[s,9]
        statistics[s,7] <- MVGP_list$vgps[s,8]
        write.csv(statistics,file,row.names = F)
      }
    )
  },width = reactive({Stereosize$size}), height = reactive({Stereosize$size}))
  
  ############ END OF VIRTUAL GEOMAGNETIC POLES MODULE
  
  
  ############ MAGNETIC POLARITY MODULE##################
  #read file if present, reset if requested
  depthfile <- reactive({
    read.csv(file = input$depth_file$datapath)
  })
  
  
  #set reactive file for reversals position 
  Tab_normals <- reactiveValues(list=NULL)
  
  #main function and plot
  output$magstrat <- renderPlot({
    req(Dirs$dat)
    if(input$filetype!=4 || input$filetype!=5){
      #data are always tilt corrected
      DI <- fix_DI(Dirs$dat,coord = 2)
    }
    if(input$filetype==4 || input$filetype==5){
      DepFile <- depthfile()
      #IODP extended Spinner file has 45 columns, normal 36, so it select the type base on the columns number
      if(ncol(DepFile)==45 || ncol(DepFile)==36){
        DepFile_temp <- data.frame(matrix(ncol=2,nrow = nrow(DepFile)))
        colnames(DepFile_temp) <- c("sample","Depth")
        DepFile_temp[,1] <- paste(paste(DepFile[,1],paste(DepFile[,2],DepFile[,3],sep=""),
                                        paste(DepFile[,4],DepFile[,5],sep = ""),
                                        paste(DepFile[,6],DepFile[,7],sep = ""),sep="-"),
                                  paste(DepFile[,8],(DepFile[,8]+2),sep = "/"),sep = ";")
        #add 1 cm to the depth to be in the sample center
        DepFile_temp[,2] <- DepFile[,9]+0.01
        DepFile <- unique(DepFile_temp)
      }
      SampFile <- Dirs$dat
      DI_temp <- SampFile[,c(1,7:8)]
      DI_temp <- na.omit(DI_temp)
      DI_temp$depth <- rep(NA)
      for(i in 1:nrow(DI_temp)){
        for(l in 1:nrow(DepFile)){
          if(DepFile[l,1]==DI_temp[i,1]){DI_temp[i,4] <- DepFile[l,2]}
        }
      }
      if(sum(is.na(DI_temp[,4]))){output$warndepth <- renderText({"WARNING: Some depths are missing!"})}
      
      DI <- DI_temp[,-1]
    }
    
    #fix base and top
    if(!is.na(input$baseMS)){
      DI <- DI[(DI[,3]>=input$baseMS),]
    }
    if(!is.na(input$topMS)){
      DI <- DI[(DI[,3]<=input$topMS),]
    }
    #sort position upw
    DI <- DI[order(DI[,3]),]
    
    #plot magstrat
    if(input$colmgstr==1) colmgstr <- "black"
    if(input$colmgstr==2) colmgstr <- "blue"
    if(input$colmgstr==3) colmgstr <- "green"
    if(input$colmgstr==4) colmgstr <- "pink"
    if(input$colmgstr==5) colmgstr <- "purple"
    if(input$colmgstr==6) colmgstr <- "brown"
    if(input$colmgstr==7) colmgstr <- "red"
    if(input$colmgstr==8) colmgstr <- "yellow"
    if(input$colmgstr==9) colmgstr <- "cyan"
    if(input$colmgstr==10) colmgstr <- "gray"
    if(input$colmgstr==11) colmgstr <- "white"
    
    #if dec offset is empty then equal 0
    ifelse(is.na(input$Doffset)==T,decoffset <- 0,decoffset <- input$Doffset)
    
    Tab_normals$list <- PmagDiR::magstrat_DI(DIP = DI,lat = input$lat,long = input$long,offset=decoffset, plot_ext = F,POLE = F,E.A. = F,
                                             cex.main = 1.8,cex.lab = 1.6,cex.axis = 1.3,lwd.grid = 1.2,rev_depth = input$revdepth,unit = input$depthUnit,
                                             h_grid=input$hGrid,col = colmgstr,UseInc = input$VGP_inc,Shiny = T)
    #record plot
    mgstrPlot <- recordPlot()
    #Export graphic
    
    output$mgstr <- downloadHandler(
      filename = function() {
        paste(input$fileN_mgstr,"_mgstr_", Sys.Date(), ".pdf", sep="")
      },
      content = function(file) {
        pdf(file, onefile = TRUE,width = 12,height = 10)
        replayPlot(mgstrPlot)
        dev.off()
      }
    )
    #export tab with normal low and high level
    output$revTab <- downloadHandler(
      filename=function() {
        paste(input$fileN_mgstr,"_normal_zone_", Sys.Date(),".csv",sep="")
      },
      content=function(file){
        write.csv(Tab_normals$list,file, row.names = F)
      }
    )
  },width = 900,height = 800)
  ############ END OF MAGNETIC POLARITY MODULE
  
  ############ MAPPING MODULE#################
  #creates reactive value for checking if file is uploaded
  values <- reactiveValues(mapsites = NULL)
  #check for uploaded file
  observeEvent(input$sitefile,{values$mapsites <- "uploaded"})
  #reset upload if requested
  observeEvent(input$resetsitesfile,{values$mapsites <- "reset"})
  #read file if present, reset if requested
  sites_file <- reactive({
    if (is.null(values$mapsites)) {
      return(NULL)
    } else if (values$mapsites == 'uploaded') {
      read.csv(file = input$sitefile$datapath)
    } else if (values$mapsites == 'reset') {
      return(NULL)
    }
  })
  
  
  geo_point_plot <- eventReactive(input$mapgo,{
    
    #functions converting long & lat to xy in KavrayskiyVII projection
    c2x <- function(lon,lat) {((3*PmagDiR::d2r(lon))/2)*(sqrt((1/3)-((PmagDiR::d2r(lat)/pi)^2)))}
    c2y <- function(lat) {PmagDiR::d2r(lat)}
    
    #functions spherical (lon=x, lat=y) to Cartesian
    s2cx <- function(x,y) {cos(PmagDiR::d2r(x))*cos(PmagDiR::d2r(y))}
    s2cy <- function(x,y) {sin(PmagDiR::d2r(x))*cos(PmagDiR::d2r(y))}
    s2cz <- function(y) {sin(PmagDiR::d2r(y))}
    
    #define center meridian
    if(input$gridCent==1) {center <- 0}
    else if(input$gridCent==2){center <- 180}
    #define land color
    if(input$landCol==1){landcolor <- "black"}
    if(input$landCol==2){landcolor <- "gray"}
    if(input$landCol==3){landcolor <- "lightgray"}
    if(input$landCol==4){landcolor <- "green"}
    if(input$landCol==5){landcolor <- "darkgreen"}
    if(input$landCol==6){landcolor <- "orange"}
    if(input$landCol==7){landcolor <- "brown"}
    #define sea color
    if(input$seaCol==1){seacolor <- "cyan"}
    if(input$seaCol==2){seacolor <- "lightcyan"}
    if(input$seaCol==3){seacolor <- "lightgreen"}
    if(input$seaCol==4){seacolor <- "white"}
    if(input$seaCol==5){seacolor <- "lightgray"}
    #define grid color
    if(input$gridCol==1){gridcolor <- "black"}
    if(input$gridCol==2){gridcolor <- "gray"}
    if(input$gridCol==3){gridcolor <- "lightgray"}
    if(input$gridCol==4){gridcolor <- "blue"}
    if(input$gridCol==5){gridcolor <- "lightblue"}
    
    #Grid selection
    if(input$gridSpace==1) {grid <- 0}
    if(input$gridSpace==2) {grid <- 10}
    if(input$gridSpace==3) {grid <- 15}
    if(input$gridSpace==4) {grid <- 30}
    if(input$gridSpace==5) {grid <- 45}
    if(input$gridSpace==6) {grid <- 90}
    
    #draw map
    PmagDiR::Map_KVII(grid=grid,center=center,seaCol = seacolor,landCol = landcolor,gridCol = gridcolor)
    
    #plot point
    S_lon <- input$long
    S_lon <- S_lon-center
    S_lon <- ifelse(S_lon>180,
                    S_lon-360,S_lon)
    S_lon <- ifelse(S_lon<(-180),S_lon+360,S_lon)
    S_lat <- input$lat
    #select symbol
    if(input$siteSym==1) pch <- 21
    if(input$siteSym==2) pch <- 22
    if(input$siteSym==3) pch <- 23
    if(input$siteSym==4) pch <- 24
    
    #select color
    if(input$siteCol==1) col <- "black"
    if(input$siteCol==2) col <- "blue"
    if(input$siteCol==3) col <- "red"
    if(input$siteCol==4) col <- "darkgreen"
    if(input$siteCol==5) col <- "purple"
    
    site_x <- c2x(S_lon,S_lat)
    site_y <- c2y(S_lat)
    points(x=site_x,y=site_y,pch=pch, col="black",bg=col,cex=1.2)
    if(is.null(input$siteText)==FALSE){
      sitename <- input$siteText
      text(x=site_x, y=site_y,pos=3,substitute(paste(bold(sitename))), cex= 1.5)
    }
    if(!is.null(sites_file())){
      sites <- na.omit(sites_file())
      for(i in 1:nrow(sites)){
        lon <- sites[i,2]
        lon <- lon-center
        lon <- ifelse(lon>180,lon-360,lon)
        lon <- ifelse(lon<(-180),lon+360,lon)
        x <- c2x(lon,sites[i,3])
        y <- c2y(sites[i,3])
        if(sites[i,4]=="c") pch <- 21
        if(sites[i,4]=="s") pch <- 22
        if(sites[i,4]=="d") pch <- 23
        if(sites[i,4]=="t") pch <- 24
        points(x=x,y=y,pch=pch, col="black",bg=sites[i,5],cex=1.2)
        name <- sites[i,1]
        text(x=x, y=y,pos=3,substitute(paste(bold(name))), cex= 1.5)
      }
    }
    mapPlot <- recordPlot()
    output$mapG <- downloadHandler(
      filename = function() {
        paste("Map_", Sys.Date(), ".pdf", sep="")
      },
      content = function(file) {
        pdf(file, onefile = TRUE,width = 15,height = 10)
        replayPlot(mapPlot)
        dev.off()
      }
    )
    
  })
  output$geomap <- renderPlot({
    geo_point_plot()
  },height = 700)
  ############ END OF MAPPING MODULE
  
  ###################################END OF ALL SCRIPTS SO FAR
}
