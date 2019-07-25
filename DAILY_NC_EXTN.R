rm(list = ls())
library(ncdf4)
library(dplyr)
library(weathermetrics)
library(raster)



substrRight <- function(x, n){
  substr(x, nchar(x)-n+1, nchar(x))
}

ncfile=choose.files(caption = "Select files")

substr(substrRight(ncfile, 30),18,25)

DAYY=as.Date(substr(substrRight(ncfile, 30),18,25),format ="%Y%m%d")
TIME=paste(substr(substrRight(ncfile, 30),26,27),"_UTC",sep = "")

#DAYY="2019-05-27"   #run date
#TIME="12_UTC"       #run time 12_UTC/00_UTC

#setwd("C:/Users/admin/Dropbox/DET_PRD")

script.dir <- dirname(sys.frame(1)$ofile)
setwd(script.dir)


aws_loc=read.csv("src_files/lat-long.csv",header = T)

if(TIME=="12_UTC"){
  x.Date <- format(as.Date(DAYY) + c(-0) , "%Y%m%d")
  Date= format(as.Date(DAYY) + c(1:9) , "D%Y%m%d")
  Date1= format(as.Date(DAYY) + c(1:9) , "%d-%m-%y")
  T2_day=mutate(aws_loc, DAY1=NA,DAY2=NA,DAY3=NA,DAY4=NA,DAY5=NA,DAY6=NA,DAY7=NA,DAY8=NA,DAY9=NA)
  HI_day=mutate(aws_loc, DAY1=NA,DAY2=NA,DAY3=NA,DAY4=NA,DAY5=NA,DAY6=NA,DAY7=NA,DAY8=NA,DAY9=NA)
  RF_day=mutate(aws_loc, DAY1=NA,DAY2=NA,DAY3=NA,DAY4=NA,DAY5=NA,DAY6=NA,DAY7=NA,DAY8=NA,DAY9=NA)
  WS_day=mutate(aws_loc, DAY1=NA,DAY2=NA,DAY3=NA,DAY4=NA,DAY5=NA,DAY6=NA,DAY7=NA,DAY8=NA,DAY9=NA)
  RH_day=mutate(aws_loc, DAY1=NA,DAY2=NA,DAY3=NA,DAY4=NA,DAY5=NA,DAY6=NA,DAY7=NA,DAY8=NA,DAY9=NA)
 
  stat=c(5,13,21,29,37,45,51,55,59)
  end  =c(11,19,27,35,43,50,54,58,62)
  
  stat_rf=c(5,13,21,29,37,45,51,55,59)
  end_rf  =c(13,21,29,37,45,51,55,59,63)
  NDAYS=9
  
}else if(TIME=="00_UTC"){
  x.Date <- format(as.Date(DAYY) + c(-0) , "%Y%m%d")
  Date= format(as.Date(DAYY) + c(0:9) , "D%Y%m%d")
  Date1= format(as.Date(DAYY) + c(0:9) , "%d-%m-%y")
  T2_day=mutate(aws_loc, DAY1=NA,DAY2=NA,DAY3=NA,DAY4=NA,DAY5=NA,DAY6=NA,DAY7=NA,DAY8=NA,DAY9=NA,DAY10=NA)
  HI_day=mutate(aws_loc, DAY1=NA,DAY2=NA,DAY3=NA,DAY4=NA,DAY5=NA,DAY6=NA,DAY7=NA,DAY8=NA,DAY9=NA,DAY10=NA)
  RF_day=mutate(aws_loc, DAY1=NA,DAY2=NA,DAY3=NA,DAY4=NA,DAY5=NA,DAY6=NA,DAY7=NA,DAY8=NA,DAY9=NA,DAY10=NA)
  WS_day=mutate(aws_loc, DAY1=NA,DAY2=NA,DAY3=NA,DAY4=NA,DAY5=NA,DAY6=NA,DAY7=NA,DAY8=NA,DAY9=NA,DAY10=NA)
  RH_day=mutate(aws_loc, DAY1=NA,DAY2=NA,DAY3=NA,DAY4=NA,DAY5=NA,DAY6=NA,DAY7=NA,DAY8=NA,DAY9=NA,DAY10=NA)

  stat=c(1,9,17,25,33,41,49,53,57,61)
  end  =c(8,16,24,32,40,48,52,56,60,64)
  
  stat_rf=c(1,9,17,25,33,41,49,53,57,61)
  end_rf  =c(9,17,25,33,41,49,53,57,61,65)
  
  NDAYS=10
}

#ncfile=paste("..\\..\\Downloads\\DET_5N37N_50E95E_",x.Date,substr(TIME,1,2),".nc",sep = "")

f1 <- nc_open(ncfile)

print(paste("file used:",ncfile,sep = ""))

#f1 <- nc_open(paste(choose.files(),x.Date,"12.nc",sep = ""))

nv_time=as.POSIXct(substr(f1$dim$time$units,13,25),"%Y-%m-%d %H",tz="UTC")+(60*60*5.5)+(60*60*f1$dim$time$vals)


