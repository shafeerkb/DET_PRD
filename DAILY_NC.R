rm(list = ls())
library(ncdf4)
library(dplyr)
library(weathermetrics)
library(raster)
library(fields)
library(rgdal)
library(gridExtra)
library(ggplot2)
library(ggpubr)
library(grid)
library(lubridate)
library(xlsx) 
library(akima) 
library(plyr)
library(png)
library(scales)
library(lattice)
library(gdata)
library(chron)


substrRight <- function(x, n){
  substr(x, nchar(x)-n+1, nchar(x))
}

ncfile=choose.files(caption = "Select files")

substr(substrRight(ncfile, 30),18,25)

DAYY=as.Date(substr(substrRight(ncfile, 30),18,25),format ="%Y%m%d")
TIME=paste(substr(substrRight(ncfile, 30),26,27),"_UTC",sep = "")
DIR=substr(substrRight(ncfile, 30),18,27)
script.dir <- dirname(sys.frame(1)$ofile)
setwd(script.dir)

source('R_script/NC2CSV_DLY.R')
source('R_script/NC2CSV_HRLY.R')
source('R_script/NC2CSV_DIST.R')
source('R_script/NC2_SUMMARY.R')
source('R_script/NC2_RIVER_BASIN.R')
source('R_script/meteogram_surface.R')
source('R_script/NC2_RF_AP.R')
source('R_script/NC2_RF_INDIA.R')
source('R_script/NC2_T2_AP.R')
source('R_script/NC2_T2_INDIA.R')

if(TIME=="12_UTC"){
  stat=c(5,13,21,29,37,45,51,55,59)
  end  =c(11,19,27,35,43,50,54,58,62)
  
  stat_rf=c(5,13,21,29,37,45,51,55,59)
  end_rf  =c(13,21,29,37,45,51,55,59,63)
  NDAYS=9
  
}else if(TIME=="00_UTC"){

  stat=c(1,9,17,25,33,41,49,53,57,61)
  end  =c(8,16,24,32,40,48,52,56,60,64)
  
  stat_rf=c(1,9,17,25,33,41,49,53,57,61)
  end_rf  =c(9,17,25,33,41,49,53,57,61,65)

  NDAYS=10
}

f1 <- nc_open(ncfile)

print(paste("file used:",ncfile,sep = ""))

nv_time=as.POSIXct(substr(f1$dim$time$units,13,25),"%Y-%m-%d %H",tz="UTC")+(60*60*5.5)+(60*60*f1$dim$time$vals)

dir.create(paste(script.dir,"/",DIR,sep =""), showWarnings = FALSE)

out.dir=paste(script.dir,"/",DIR,sep ="")

for (i in 1:NDAYS) {
  print(paste("DAY",i,"t2,rh,ws=",nv_time[stat[i]],"to",nv_time[end[i]], "rain=",nv_time[stat_rf[i]],"to",nv_time[end_rf[i]]))
}

NC_SUMMARY(out.dir,ncfile, DAYY,TIME,NDAYS,nv_time,stat,end,stat_rf,end_rf)
NC_RIVERBASIN(out.dir,ncfile, DAYY,TIME,NDAYS,nv_time,stat,end,stat_rf,end_rf)
NC_RF_AP(out.dir,ncfile, DAYY,TIME,NDAYS,nv_time,stat,end,stat_rf,end_rf)
NC_RF_INDIA(out.dir,ncfile, DAYY,TIME,NDAYS,nv_time,stat,end,stat_rf,end_rf)
NC_T2_INDIA(out.dir,ncfile, DAYY,TIME,NDAYS,nv_time,stat,end)
NC_T2_AP(out.dir,ncfile, DAYY,TIME,NDAYS,nv_time,stat,end)
METEOGRAM_DLY(out.dir,ncfile, DAYY,TIME,NDAYS,nv_time)
NC2CSV_DLY(out.dir,ncfile, DAYY,TIME,NDAYS,nv_time,stat,end,stat_rf,end_rf)
NC2CSV_HRLY(out.dir,ncfile, DAYY,TIME,NDAYS,nv_time)
NC2CSV_DIST(out.dir,ncfile, DAYY,TIME,NDAYS,nv_time,stat,end,stat_rf,end_rf)



