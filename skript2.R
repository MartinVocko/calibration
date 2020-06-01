lapply(installed.packages()[,1], library, character.only = TRUE)
require(raster)
require(rNOMADS)
require(rgdal)
require(data.table)
require(maptools)
require(ncdf)
require(ncdf4)
library(lattice)
library(chron)
require(reshape2)
require(plyr)
require(ggplot2)
require(scales)
require(lubridate)
require(zoo)
require(rasterVis)
require(RColorBrewer)
require(animation)
require(caTools)
require(verification)
require(ensembleBMA)
require(dataframes2xls)
require(pbapply)
require(SpecsVerification)
require(easyVerification)
require(matrixStats)
require(rgeos)
library(hydroGOF)
require(scoringRules)
require(plotrix)
require(gdata)
require(gridExtra)
require(cowplot)
require(ncdf)
require(svMisc)
require(parallel)
require(rgl)
require(devtools)
require(Rcpp)


install.packages("scoringRules")
install.packages("raster")
install.packages("rNOMADS")
install.packages("rgdal")
install.packages("data.table")
install.packages("maptools")
install.packages("ncdf")
install.packages("ncdf4")
install.packages("lattice")
install.packages("chron")
install.packages("reshape2")
install.packages("plyr")
install.packages("ggplot2")
install.packages("scales")
install.packages("lubridate")
install.packages("zoo")
install.packages("rasterVis")
install.packages("RColorBrewer")
install.packages("animation")
install.packages("caTools")
install.packages("verification")
install.packages("ensembleBMA")
install.packages("dataframes2xls")
install.packages("pbapply")
install.packages("SpecsVerification")
install.packages("easyVerification")
install.packages("matrixStats")
install.packages("rgeos")
install.packages("hydroGOF")
install.packages("plotrix")
install.packages("gdata")
install.packages("gridExtra")
install.packages("cowplot")
install.packages("svMisc")
install.packages('rgl')
install.packages('devtools')
install.packages('Rcpp')

plotwd = '~/Plocha/DATA/LAEF/2011'
wd='~/Plocha/DATA/LAEF/2012'

setwd(wd)
# sez = list.files(patt = 'grb')
# 
# # gsub
# outname = gsub('grb', 'nc', sez)
# for (i in sez)
# {
#   ex = paste('cd', wd, '; grib_to_netcdf -o', outname[i], sez[i])
#  system(ex) 
#  }
#   
#  
#  
r=brick('2013_6_6h.nc')
r@crs = CRS('+init=epsg:32633')
vp2 = spTransform(vp, CRS('+init=epsg:32633'))
b=brick('LAEF4hydro_pf_2012040100.nc', varname="tp", level=5)
#  b=brick('LAEF4hydro_cf_2011040100.nc', varname="tp")
#  b=brick('LAEF4hydro_cf_2011040100.nc')
#  plot(b)
#  
#  a=open.ncdf('LAEF4hydro_pf_2011040100.nc')
#  a=
#  print.ncdf(a)
#  names(a$var)
#  str(a$var$tp)
#  
 vp=readShapePoly("vstupol_all",repair)
# 
 
TAB = data.table()  
temp = list.files(pattern="*.nc")



# dta = stack(as.list(temp), varname = 'tp', level = 1)
# dta2=stack(as.list(temp), varname='tp',level=2)
 r = brick(temp[1], varname = 'tp', level = 1)
 r = brick(temp[1], varname = 'tp')
 b= brick(temp[1], varname='A_PCP_GDS3_HTGL')
 #r@crs = CRS('+proj=tmerc +lat_0=0 +lon_0=15 +k=1 +x_0=3500000 +y_0=0 +ellps=krass +units=m +no_defs')
# plot(r[[4]])
# plot(vp, add=TRUE,frame.plot=FALSE)

# extract(r, ShapeFile Oblasti)

exmat = extract(r[[1]],vp2, na.rm=TRUE, df = TRUE, cellnumbers = TRUE)
j = 1
for (j in 1:length(temp)){

time = as.POSIXct(gsub('_|\\.nc', '', temp[j]), format = '%Y%m%d%H')


for(i in 1:16){
  
  dta=brick(temp[j],varname='tp', level= i)
  #ex=extract(dta,vp,fun = mean, na.rm=TRUE, df = TRUE)
  ex = extract(dta, exmat$cell)
  ex = melt(data.table(OBL = exmat$ID, ex), id.vars = c('OBL'))
  tab = ex[, .(PR = mean(value)), by = .(OBL, variable)]
  tab[, PTIME:=time ]
  tim = as.POSIXct(rep(dta@z[[1]], each = 37))
  tab[, TIME:=tim ]
  #tab[, AHEAD:= TIME - PTIME]
  tab[, AHEAD:= difftime(TIME,PTIME,units="hours")]
#   tab[, PID:=as.double(gsub('Total.precipitation.', '', variable))]
#   tab[, AHEAD:= PID%%10 ]
#   tab[AHEAD==0, AHEAD:=10L]
#   tab[, IDX:=PID%/%10+1]
#   tab[AHEAD==10L, IDX:=IDX-1]
#   tab[, PTIME:=time[IDX]]
#   tab[, TIME:=PTIME + 3600 * 6 * (AHEAD - 1)]
  
  tab[, EID:= 1]
  TAB = rbind(TAB, tab[, .(OBL, EID, SID = 'cf', TIME, AHEAD, PR)])
  
  
}
}
x=factor(c(tab$AHEAD))
  revalue(TAB$AHEAD, c("0"="1","6"="7","12"="13","18"="19","24"="25","30"="31","36"="37","42"="43","48"="49","54"="55"))
  
saveRDS(TAB, 'aladin2015.RDS')


r@z # vytahni cas


#tabulka
dname <- "time"
tmp.array <- get.var.ncdf(a,dname)
m=1

tab <- data.frame(cbind(dname,as.vector(a)))



names(a) <- c("time",paste(dname,as.character(m), sep="_"))
head(na.omit(a), 10)

get.var.ncdf(a , varid="time") -> ti
str(ti)

#tabulka
ID = names(ex)
dID = data.table(ID)
dID[, BASE:=ymd(substr(gsub('\\.','-',gsub('X', '', ID)), 1, 10))]
dID[, LEAD:=as.integer(substr(gsub('\\.','-',gsub('X', '', ID)), 12, 13))]
d = readRDS('r')
setkey(d, BASE, LEAD) 
setkey(dID, BASE, LEAD)
d = d[dID]
