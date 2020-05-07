library(data.table)
library(ensembleBMA)
library(ensembleMOS)
library(EBMAforecast)
library(keras)
library(neuralnet)
library(AMORE)
library(repmis)
library(ggplot2)
library(DEoptim)
library(lubridate)
library(keras)

  setwd("~/Plocha/DATA_EXTRAKCE")
srfp=data.table(readRDS("data_kalibrace.rds"))

#aux <- as.POSIXlt('1988-1-2 00:00:00', format = '%Y-%m-%d %H:%M:%S')
#format(aux, '%H:%M:%S')

srfp=srfp[format(ORIGIN, '%H:%M:%S')=='00:00:00',]
srfp=srfp[AHEAD==6]
srfp=srfp[ID==1060]
  #srfp=na.omit(srfp)

#row_sub = apply(srfp, 1, function(row) all(row !=0 ))
#srfp=srfp[row_sub,]

#### BMA ######
memberLabels<-names(srfp[,5:21])    
cas <- as.POSIXct(srfp$ORIGIN, format = '%Y-%m-%d %H:%M:%S')
srfp$date <- format(cas, format = '%Y%m%d%H')

srfpData<-ensembleData(forecasts=srfp[,5:21], dates=srfp$date, observations=srfp$RAINFALL,initializationTime = "00", forecastHour = 6)


#srfpData[,c(1:16,18)]=apply(srfpData[,c(1:16,18)],2,as.numeric)

srfpData=srfpData[500:610,]
srfpFit<- ensembleBMA(srfpData,trainingDays = 100, model="gamma0",control = controlBMAgamma0()) 
#srfpFit<- ensembleBMAgamma0(srfpData[500,],trainingDays = 100) 

#forecast  
srfpFit<-fitBMAgamma0(srfpData[1:100,])
srfpData[500,]

quantileForecast.ensembleBMAgamma0(srfpFit, srfpData[498:510,],quantiles=0.5)


### EMOS #####

srfpFitE<- ensembleMOScsg0(srfpData,trainingDays =30) 
srfpFitE<- fitMOS(srfpData, model = 'csg0')



    
### NEURALNET ##############################################################
kaldat=readRDS("data_kalibrace.rds")

kaldat=kaldat[AHEAD==6]
kaldat=kaldat[ID==1060]
kaldat=kaldat[,5:23]

############### v pripade ruznych jednotek je potreba Normalizace
normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x)))
}

maxmindf <- as.data.frame(lapply(kaldat, normalize))
trainset <- maxmindf[1:2000, ]
testset <- maxmindf[2001:length(kaldat$TIME), ]
###################################################################
trainset <- kaldat[1:2000, ]
testset <- kaldat[2001:length(kaldat$RAINFALL), ]

nn <- neuralnet(RAINFALL~ LAEF_1 + LAEF_2+ LAEF_3 + LAEF_4 + LAEF_5 + LAEF_6 + LAEF_7 + LAEF_8 + LAEF_9 + LAEF_10 +
                  LAEF_11 + LAEF_12 + LAEF_13 + LAEF_14 + LAEF_15 + LAEF_16, data=trainset, hidden=c(1), linear.output=FALSE, threshold=0.01)
nn$result.matrix
plot(nn)

#Test the resulting output
temp_test <- subset(testset, select = c("LAEF_1", "LAEF_2","LAEF_3", "LAEF_4", "LAEF_5", "LAEF_6", "LAEF_7", "LAEF_8", "LAEF_9", "LAEF_10", "LAEF_11", "LAEF_12", "LAEF_13", "LAEF_14", "LAEF_15", "LAEF_16"))
head(temp_test)
nn.results <- compute(nn, temp_test)
nn.results = predict(nn, temp_test)
  results <- data.frame(actual = testset$RAINFALL, prediction = nn.results$net.result)
    results  
    
    

##### AMORE ###########################################################
#    setwd("~/Plocha/DATA/GITHUB/calibration")
#   dta <- as.data.table(read.fwf("01048000.txt",  widths = c(8, 10, 10, 10, 10, 10)))
    
#   ames(dta) <- c('DTM', 'P', 'E', 'Q', 'Tmax', 'Tmin')
    
#    dta[, DTM := as.Date(gsub(' ','0', DTM), format = '%Y%m%d')]
    
 #   ggplot(dta) +
 #     geom_line(aes(x = DTM, y = Q), colour = 'steelblue4') +
 #     theme_bw() +
 #     labs(x = 'Time', y = 'Discharge', title = "id")
    setwd("~/Plocha/DATA_EXTRAKCE")
 #---
    kaldat=readRDS("data_kalibrace.rds")
    kaldat=kaldat[AHEAD==6]
    kaldat=kaldat[ID==1060]
    kaldat=kaldat[,5:24]    
    trainset <- kaldat[1:2000, ]
    testset <- kaldat[2001:length(kaldat$RAINFALL), ]
  #---
    
  #---
    kaldat=readRDS("COSMO_ALADIN_kalibrace.rds")
    kaldat=kaldat[AHEAD==6]
    kaldat=kaldat[ID==1060]
    kaldat=kaldat[,5:45]   
    kaldat[,c(17,18,19,20)]<-NULL  #odstrani cleny cosmo 17, 18, 19 a 20, kterej sou az v pozdejsich letech
    kaldat=kaldat[,c(1:33,36,37,34,35)] #uspordani sloupcu
    trainset <- kaldat[1:2000, ]
    testset <- kaldat[2001:length(kaldat$RAINFALL), ]
    
    #tab$LAEF_MEAN=apply(tab[,25:40], 1, mean)  #doplneni prumeru ensemblu
    #tab$COSMO_MEAN=apply(tab[,5:20], 1, mean)  #doplneni prumeru ensemblu
 ######     
    
     
    periods <- function(dta, n = 3, length = 10, start = NULL, safety.net = 100) {
      
      dta.subset <- dta[Q >= 0,]
      
      if(is.null(start)) year <- as.numeric(dta.subset[1,format(DTM, '%Y')]) + 1 else year <- start
      if(safety.net < n) safety.net <- n
      
      i <- 1
      x <- 0
      
      dec <- list()
      
      while (x < safety.net) {
        
        dec[[i]] <- dta.subset[DTM %between% c(as.Date(paste(year, '11-1', sep = '-')), as.Date(paste(year + length, '10-31', sep = '-')))]
        
        if(i == n) break
        if(!dim(dec[[i]])[1] < length*365) {
          
          i <- i + 1
          year <- year + length
        } else year <- year + 1
        
        x <- x + 1
      }
      
      rbindlist(dec, idcol = TRUE)
    }
    
    model.plot <- function(Qobs, Qsim, h, title = 'Model') {
      
      gg <- ggplot() +
        geom_line(aes(x = seq_along(Qobs[-(1:h)]), y = Qobs[-(1:h)]), colour = 'steelblue4') +
        geom_line(aes(x = seq_along(Qsim), y = Qsim), colour = 'red4') +
        geom_line(aes(x = seq_along(Qobs[-(1:h)] - Qsim), y = Qobs[-(1:h)] - Qsim), colour = 'orange', alpha = .5) +
        theme_bw() +
        labs(x = 'Time',
             y = 'Discharge', 
             title = title, 
             subtitle = paste('Residuals min =', 
                              round(min(Qobs[-(1:h)] - Qsim), 3),
                              '\nResiduals mean =',
                              round(mean(Qobs[-(1:h)] - Qsim), 3),
                              '\nResiduals max =',
                              round(max(Qobs[-(1:h)] - Qsim), 3)))
      
      return(gg)
    }
    
 #########   
    
    
    DTA <- periods(dta)   
  
    dta.matrix <- function(dta, h = rev(seq_along(dta))) {
      
      dta <- as.data.frame(dta)
      
      N <- dim(dta)[1]
      
      A <- list() 
      
      for (j in 1:dim(dta)[2]) {
        
        h.act <- h[j]
        dta.matrix <- matrix(NA, nrow = N - max(h), ncol = h.act)
        
        for (i in 1:h.act){
          dta.matrix[,i] <- dta[(max(h) + 1 - i):(N - i), j]
        }
        
        A[[j]] <- dta.matrix
      }
      
      do.call(cbind, A)
    }
    
    dta.cal <- DTA[.id %in% 2,]
   # dta.cal<-dta[1:10000]
    
    NN.dta <- dta.matrix(dta.cal[,.(Q, P)])
    
    
    
    
    trainset=as.matrix(trainset)
    sum(is.na(trainset))  #pocet NA v souboru
    trainset=na.omit(trainset) #odstraneni radku s NA
    
    net.start <- newff(n.neurons = c(37, 5, 3, 1),      
                       learning.rate.global = 1e-5,        
                       momentum.global = 0.08,              
                       error.criterium = 'LMS',           
                       Stao = NA,
                       hidden.layer = 'sigmoid',   
                       output.layer = 'purelin',           
                       method = 'ADAPTgdwm') 
    
    #NN.train <- train(net.start, NN.dta, dta.cal[-(1:(dim(dta.cal)[1] - dim(NN.dta)[1])), Q], error.criterium = 'LMS', report = TRUE, show.step = 100, n.shows = 5)
    NN.train <- train( net.start, trainset[,c(1:36)], trainset[,37], error.criterium = 'LMS', report = TRUE, show.step = 500, n.shows = 50)
    
    #NN.sim <- as.vector(sim(NN.train$net, NN.dta))
    NN.sim <- as.vector(sim(NN.train$net, kaldat[,c(1:36)]))
    
    #model.plot(dta.cal[-(1:(dim(dta.cal)[1] - dim(NN.dta)[1])), Q], NN.sim, h = dim(dta.cal)[1] - dim(NN.dta)[1])    
   # model.plot(kaldat[,19],NN.sim, h=dim(kaldat)[1]-dim(trainset)[1])  #h=kaldat[,19]-NN.sim
    
    res=NN.sim-kaldat[,37]
    
    plot(NN.sim, type="l", ylim=c(-1,max(kaldat[,37])), col="red")
    lines( kaldat[,37],col="blue")
    lines(res,type="l", col="orange")
    
    
   
   
    
  ###### KERAS #########
    
    
    
    