library(data.table)
library(ensembleBMA)
library(Metrics)
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
#srfp=srfp[1200:1400,]
#srfp=na.omit(srfp)

#row_sub = apply(srfp, 1, function(row) all(row !=0 ))
#srfp=srfp[row_sub,]

#### BMA ######
memberLabels<-names(srfp[,5:21])    
cas <- as.POSIXct(srfp$ORIGIN, format = '%Y-%m-%d %H:%M:%S')
srfp$date <- format(cas, format = '%Y%m%d%H')

srfpData<-ensembleData(forecasts=srfp[,5:21], dates=srfp$date, observations=srfp$RAINFALL,initializationTime = "00", forecastHour = 6)


#srfpData[,c(1:16,18)]=apply(srfpData[,c(1:16,18)],2,as.numeric)

#srfpData=srfpData[500:610,]
srfpFit<- ensembleBMA(srfpData,trainingDays = 50, model="gamma0",control = controlBMAgamma0()) 
#srfpFit<- ensembleBMAgamma0(srfpData[500,],trainingDays = 100) 

#forecast  
#srfpFit<-fitBMAgamma0(srfpData[1:100,])
#srfpData[500,]

qf=quantileForecast.ensembleBMAgamma0(srfpFit, srfpData, quantiles = c( 0.1,0.3,0.5,0.7, 0.9))

# prirazeni dates ke qf
dates=srfp[,1:4]
dates=dates[51:length(dates$ORIGIN),] #odecteni training days - 50
dates=cbind(dates,qf)
saveRDS(dates,"qf_06_1060")

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
 #---Data ALADIN ####
    kaldat=readRDS("data_kalibrace.rds")
    kaldat=kaldat[AHEAD==12]
    kaldat=kaldat[ID==1060]
    kaldat=kaldat[,5:24]    
    trainset <- kaldat[1:2000, ]
    testset <- kaldat[2001:length(kaldat$RAINFALL), ]
  #---
    
  #---Data ALADIN+COSMO #### 
    kaldat=readRDS("COSMO_ALADIN_kalibrace.rds")
    kaldat=kaldat[AHEAD==12]
    kaldat=kaldat[ID==1060]
    kaldat=kaldat[,5:45]   
    kaldat[,c(17,18,19,20)]<-NULL  #odstrani cleny cosmo 17, 18, 19 a 20, kterej sou az v pozdejsich letech
    kaldat=kaldat[,c(1:33,36,37,34,35)] #uspordani sloupcu
    sum(is.na(kaldat)) 
    kaldat=na.omit(kaldat)
    trainset <- kaldat[1:2000, ]
    testset <- kaldat[2001:length(kaldat$RAINFALL), ]
    
    #tab$LAEF_MEAN=apply(tab[,25:40], 1, mean)  #doplneni prumeru ensemblu
    #tab$COSMO_MEAN=apply(tab[,5:20], 1, mean)  #doplneni prumeru ensemblu
  #---   
    
  #--- Data ALADIN+COSMO t-1 t+1 #### 
    kaldat=readRDS("COSMO_ALADIN_kalibrace.rds")
    ta=kaldat[ID==1060]
    ta=kaldat[AHEAD==12]
    ta[,43]<-NULL
    ta$TIME=(ta$TIME)+21600
    #ta$ORIGIN=(ta$ORIGIN)+21600
   # tb=kaldat[,c(3,4,43)]
   # tb=tb[!duplicated(tb),]
   # setkey(ta, ID, TIME)
   # setkey(tb, ID, TIME)
    #tplus1=ta[tb]
    tplus1=ta
    setnames(tplus1,old = c("COSMO_01","COSMO_02", "COSMO_03","COSMO_04","COSMO_05","COSMO_06","COSMO_07","COSMO_08","COSMO_09","COSMO_10","COSMO_11","COSMO_12","COSMO_13","COSMO_14","COSMO_15","COSMO_16","COSMO_17","COSMO_18","COSMO_19","COSMO_20"), new = c("COSMO_01+1","COSMO_02+1","COSMO_03+1","COSMO_04+1","COSMO_05+1","COSMO_06+1","COSMO_07+1","COSMO_08+1","COSMO_09+1","COSMO_10+1","COSMO_11+1","COSMO_12+1","COSMO_13+1","COSMO_14+1","COSMO_15+1","COSMO_16+1","COSMO_17+1","COSMO_18+1","COSMO_19+1","COSMO_20+1"))
    setnames(tplus1,old = c( "LAEF_1", "LAEF_2","LAEF_3","LAEF_4","LAEF_5","LAEF_6","LAEF_7","LAEF_8","LAEF_9","LAEF_10","LAEF_11","LAEF_12","LAEF_13","LAEF_14","LAEF_15","LAEF_16","LAEF_CF","ALADIN-CZ","COSMO_MEAN","LAEF_MEAN"), new = c( "LAEF_1+1", "LAEF_2+1","LAEF_3+1","LAEF_4+1","LAEF_5+1","LAEF_6+1","LAEF_7+1","LAEF_8+1","LAEF_9+1","LAEF_10+1","LAEF_11+1","LAEF_12+1","LAEF_13+1","LAEF_14+1","LAEF_15+1","LAEF_16+1","LAEF_CF+1","ALADIN-CZ+1","COSMO_MEAN+1","LAEF_MEAN+1"))    
   
  #---  
    kaldat=readRDS("COSMO_ALADIN_kalibrace.rds")
    ta=kaldat[ID==1060]
    ta=kaldat[AHEAD==12]
    ta[,43]<-NULL
    ta$TIME=(ta$TIME)-21600
    #ta$AHEAD=ta$AHEAD-6
    #ta$ORIGIN=(ta$ORIGIN)-21600
    #tb=kaldat[,c(3,4,43)]
    #tb=tb[!duplicated(tb),]
   # setkey(ta, ID, TIME)
   # setkey(tb, ID, TIME)
    #tminus1=ta[tb]
    tminus1=ta
    setnames(tminus1,old = c("COSMO_01","COSMO_02", "COSMO_03","COSMO_04","COSMO_05","COSMO_06","COSMO_07","COSMO_08","COSMO_09","COSMO_10","COSMO_11","COSMO_12","COSMO_13","COSMO_14","COSMO_15","COSMO_16","COSMO_17","COSMO_18","COSMO_19","COSMO_20"), new = c("COSMO_01-1","COSMO_02-1","COSMO_03-1","COSMO_04-1","COSMO_05-1","COSMO_06-1","COSMO_07-1","COSMO_08-1","COSMO_09-1","COSMO_10-1","COSMO_11-1","COSMO_12-1","COSMO_13-1","COSMO_14-1","COSMO_15-1","COSMO_16-1","COSMO_17-1","COSMO_18-1","COSMO_19-1","COSMO_20-1"))
    setnames(tminus1,old = c( "LAEF_1", "LAEF_2","LAEF_3","LAEF_4","LAEF_5","LAEF_6","LAEF_7","LAEF_8","LAEF_9","LAEF_10","LAEF_11","LAEF_12","LAEF_13","LAEF_14","LAEF_15","LAEF_16","LAEF_CF","ALADIN-CZ","COSMO_MEAN","LAEF_MEAN"), new = c( "LAEF_1-1", "LAEF_2-1","LAEF_3-1","LAEF_4-1","LAEF_5-1","LAEF_6-1","LAEF_7-1","LAEF_8-1","LAEF_9-1","LAEF_10-1","LAEF_11-1","LAEF_12-1","LAEF_13-1","LAEF_14-1","LAEF_15-1","LAEF_16-1","LAEF_CF-1","ALADIN-CZ-1","COSMO_MEAN-1","LAEF_MEAN-1"))    
    
   #---
    
  setkey(tminus1, ID, AHEAD, TIME)
  setkey(tplus1, ID, AHEAD, TIME)
  setkey(kaldat, ID, AHEAD, TIME)
    
  tplusminus=kaldat[tminus1]
  tplusminus=tplusminus[tplus1]
  
  tplusminus=tplusminus[!is.na(tplusminus$ORIGIN), ]   #odstrani radky s NA ale zachova NA u COSMO 17-20

  r=tplusminus[,c("ORIGIN","AHEAD", "LAEF_2","LAEF_2-1","LAEF_2+1",  "RAINFALL","RAINFALL-1","RAINFALL+1")]
  
     
   # periods <- function(dta, n = 3, length = 10, start = NULL, safety.net = 100) {
      
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
   # model.plot <- function(Qobs, Qsim, h, title = 'Model') {
      
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
   # DTA <- periods(dta)   
   # dta.matrix <- function(dta, h = rev(seq_along(dta))) {
      
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
   # dta.cal <- DTA[.id %in% 2,]
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
    
  
    vertab=data.table(sim=NN.sim, obs=kaldat[,37])
    
    res=NN.sim-kaldat[,37]
    
    sum(is.na(NN.sim)) 
    sum(is.na(vertab$obs.RAINFALL)) 
   
    
  RMSE= rmse(as.vector(vertab$obs.RAINFALL), as.vector(NN.sim))
    vertab$RMSE=RMSE
    
  MAE=mae(as.vector(vertab$obs.RAINFALL), NN.sim)
  
  ME=mean(vertab$obs.RAINFALL-vertab$sim)
  
  data.table(RMSE,MAE, ME) 
  tab=data.table(RMSE,MAE, ME) 
  eval=rbind(eval,tab)
  
   
    
    plot(NN.sim, type="l", ylim=c(-1,max(kaldat[,37])), col="red")
    lines( kaldat[,37],col="blue")
    lines(res,type="l", col="orange")
    
    
   
   
    
  ###### KERAS #########
    
    
    
    