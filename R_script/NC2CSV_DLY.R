NC2CSV_DLY <- function(out.dir,ncfile, DAYY,TIME,NDAYS,nv_time,stat,end,stat_rf,end_rf){

  setwd(out.dir)
  dir.create("NC2GEOTIFF", showWarnings = FALSE)
  dir.create("NC2CSV", showWarnings = FALSE)
  
aws_loc=read.csv("../src_files/lat-long.csv",header = T)

if(TIME=="12_UTC"){
  x.Date <- format(as.Date(DAYY) + c(-0) , "%Y%m%d")
  Date= format(as.Date(DAYY) + c(1:9) , "D%Y%m%d")
  Date1= format(as.Date(DAYY) + c(1:9) , "%d-%m-%y")
  T2_day=mutate(aws_loc, DAY1=NA,DAY2=NA,DAY3=NA,DAY4=NA,DAY5=NA,DAY6=NA,DAY7=NA,DAY8=NA,DAY9=NA)
  HI_day=mutate(aws_loc, DAY1=NA,DAY2=NA,DAY3=NA,DAY4=NA,DAY5=NA,DAY6=NA,DAY7=NA,DAY8=NA,DAY9=NA)
  RF_day=mutate(aws_loc, DAY1=NA,DAY2=NA,DAY3=NA,DAY4=NA,DAY5=NA,DAY6=NA,DAY7=NA,DAY8=NA,DAY9=NA)
  WS_day=mutate(aws_loc, DAY1=NA,DAY2=NA,DAY3=NA,DAY4=NA,DAY5=NA,DAY6=NA,DAY7=NA,DAY8=NA,DAY9=NA)
  RH_day=mutate(aws_loc, DAY1=NA,DAY2=NA,DAY3=NA,DAY4=NA,DAY5=NA,DAY6=NA,DAY7=NA,DAY8=NA,DAY9=NA)

}else if(TIME=="00_UTC"){
  x.Date <- format(as.Date(DAYY) + c(-0) , "%Y%m%d")
  Date= format(as.Date(DAYY) + c(0:9) , "D%Y%m%d")
  Date1= format(as.Date(DAYY) + c(0:9) , "%d-%m-%y")
  T2_day=mutate(aws_loc, DAY1=NA,DAY2=NA,DAY3=NA,DAY4=NA,DAY5=NA,DAY6=NA,DAY7=NA,DAY8=NA,DAY9=NA,DAY10=NA)
  HI_day=mutate(aws_loc, DAY1=NA,DAY2=NA,DAY3=NA,DAY4=NA,DAY5=NA,DAY6=NA,DAY7=NA,DAY8=NA,DAY9=NA,DAY10=NA)
  RF_day=mutate(aws_loc, DAY1=NA,DAY2=NA,DAY3=NA,DAY4=NA,DAY5=NA,DAY6=NA,DAY7=NA,DAY8=NA,DAY9=NA,DAY10=NA)
  WS_day=mutate(aws_loc, DAY1=NA,DAY2=NA,DAY3=NA,DAY4=NA,DAY5=NA,DAY6=NA,DAY7=NA,DAY8=NA,DAY9=NA,DAY10=NA)
  RH_day=mutate(aws_loc, DAY1=NA,DAY2=NA,DAY3=NA,DAY4=NA,DAY5=NA,DAY6=NA,DAY7=NA,DAY8=NA,DAY9=NA,DAY10=NA)
}

f1 <- nc_open(ncfile)

dlat=ncvar_get(f1,"latitude")
dlon=ncvar_get(f1,"longitude")
t2 <- ncvar_get( f1,"2m_temperature")         %>% round(digits = 2) +1 
rh <- ncvar_get( f1,"2m_relative_humidity")   %>% round(digits = 2)
pr <- ncvar_get( f1,"6hr_accumulated_precip") %>% round(digits = 2)
ws <- ncvar_get( f1,"wind_magnitude")*3.6     %>% round(digits = 2)

rh[rh<0]=0
rh[rh>100]=100


####################GEOTIFF#######################

rf_raster <- raster(xmn = min(dlon), xmx = max(dlon), ymn = min(dlat), ymx = max(dlat), nrows = length(dlat), ncols = length(dlon))
for (dd in 1:NDAYS) {
  RF_rstr=pr[,,end_rf[dd]]-pr[,,stat_rf[dd]]
  RF_rstr[RF_rstr<0]=0
  
values(rf_raster) <- matrix(RF_rstr,length(dlat),length(dlon),byrow = T)
writeRaster(rf_raster,filename=paste("NC2GEOTIFF/",x.Date,TIME,"_",Date[dd],".tif",sep = ""), format="GTiff", overwrite=TRUE)
}
###########################################

for (i in 1:NROW(aws_loc)){ 
  cat("\r",i,NROW(aws_loc),round(i/NROW(aws_loc),digits = 2)*100,"%")
  ilat= which.min(abs(aws_loc$LAT[i]-dlat))
  ilon= which.min(abs(aws_loc$LON[i]-dlon)) 
       for (dd in 1:NDAYS) {
         T2_day[i,(7+dd)]=max(t2[ilon,ilat,(stat[dd]:end[dd])])
         WS_day[i,(7+dd)]=max(ws[ilon,ilat,(stat[dd]:end[dd])])
         RH_day[i,(7+dd)]=mean(rh[ilon,ilat,(stat[dd]:end[dd])])
         HI=heat.index(t = t2[ilon,ilat,(stat[dd]:end[dd])], rh = rh[ilon,ilat,(stat[dd]:end[dd])], temperature.metric = "celsius", output.metric = "celsius", round = 2)
         HI_day[i,(7+dd)]=max(HI)
         RF_day[i,(7+dd)]=pr[ilon,ilat,end_rf[dd]]-pr[ilon,ilat,stat_rf[dd]]
         
         if(RF_day[i,(7+dd)]<0){RF_day[i,(7+dd)]=0}
         
         if(dd>6){
           T2_day[i,(7+dd)]=T2_day[i,(7+dd)]+1
           HI_day[i,(7+dd)]=HI_day[i,(7+dd)]+1
         }
       }
}



colnames(T2_day)=c("Sno","SP_CODE","DISTRICT","MANDAL","VILLAGE","LON","LAT",Date1)
colnames(HI_day)=c("Sno","SP_CODE","DISTRICT","MANDAL","VILLAGE","LON","LAT",Date1)
colnames(RF_day)=c("Sno","SP_CODE","DISTRICT","MANDAL","VILLAGE","LON","LAT",Date1)
colnames(WS_day)=c("Sno","SP_CODE","DISTRICT","MANDAL","VILLAGE","LON","LAT",Date1)
colnames(RH_day)=c("Sno","SP_CODE","DISTRICT","MANDAL","VILLAGE","LON","LAT",Date1)

write.csv(file = paste("NC2CSV/T2_day",x.Date,TIME,".csv",sep = ""),T2_day,row.names = F)
write.csv(file = paste("NC2CSV/HI_day",x.Date,TIME,".csv",sep = ""),HI_day,row.names = F)
write.csv(file = paste("NC2CSV/RF_day",x.Date,TIME,".csv",sep = ""),RF_day,row.names = F)
write.csv(file = paste("NC2CSV/WS_day",x.Date,TIME,".csv",sep = ""),WS_day,row.names = F)
write.csv(file = paste("NC2CSV/RH_day",x.Date,TIME,".csv",sep = ""),RH_day,row.names = F)

T2_day_mndal=group_by(T2_day, DISTRICT,MANDAL)%>%summarise_at(vars(Date1), max, na.rm = TRUE)
HI_day_mndal=group_by(HI_day, DISTRICT,MANDAL)%>%summarise_at(vars(Date1), max, na.rm = TRUE)
RF_day_mndal=group_by(RF_day, DISTRICT,MANDAL)%>%summarise_at(vars(Date1), max, na.rm = TRUE)
WS_day_mndal=group_by(WS_day, DISTRICT,MANDAL)%>%summarise_at(vars(Date1), max, na.rm = TRUE)
RH_day_mndal=group_by(RH_day, DISTRICT,MANDAL)%>%summarise_at(vars(Date1), mean, na.rm = TRUE)

write.csv(file = paste("NC2CSV/T2_day_mndal",x.Date,TIME,".csv",sep = ""),T2_day_mndal,row.names = F)
write.csv(file = paste("NC2CSV/HI_day_mndal",x.Date,TIME,".csv",sep = ""),HI_day_mndal,row.names = F)
write.csv(file = paste("NC2CSV/RF_day_mndal",x.Date,TIME,".csv",sep = ""),RF_day_mndal,row.names = F)
write.csv(file = paste("NC2CSV/WS_day_mndal",x.Date,TIME,".csv",sep = ""),WS_day_mndal,row.names = F)
write.csv(file = paste("NC2CSV/RH_day_mndal",x.Date,TIME,".csv",sep = ""),RH_day_mndal,row.names = F)

}

