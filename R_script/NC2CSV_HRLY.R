NC2CSV_HRLY <- function(out.dir,ncfile, DAYY,TIME,NDAYS,nv_time){

setwd(out.dir)


aws_loc=read.csv("../src_files/lat-long.csv",header = T)
f1 <- nc_open(ncfile)

df <- data.frame(matrix(ncol = length(nv_time), nrow = NROW(aws_loc)))
colnames(df) <- nv_time

T2_hrly=cbind(aws_loc,df)
RH_hrly=cbind(aws_loc,df)
RF_hrly=cbind(aws_loc,df)
WS_hrly=cbind(aws_loc,df)
HI_hrly=cbind(aws_loc,df)


dlat=ncvar_get(f1,"latitude")
dlon=ncvar_get(f1,"longitude")
t2 <- ncvar_get( f1,"2m_temperature")         %>% round(digits = 2) 
rh <- ncvar_get( f1,"2m_relative_humidity")   %>% round(digits = 2)
pr <- ncvar_get( f1,"6hr_accumulated_precip") %>% round(digits = 2)
ws <- ncvar_get( f1,"wind_magnitude")         %>% round(digits = 2)*3.6  #m/s to km/hr

rh[rh<0]=0
rh[rh>100]=100

rf=pr*0
for (i in 2:length(pr[1,1,])) {
  rf[,,i]=pr[,,i]-pr[,,(i-1)]
}

N=NCOL(T2_hrly)
M=NCOL(aws_loc)+1
for (i in 1:NROW(aws_loc)){
  cat("\r",i,NROW(aws_loc),round(i/NROW(aws_loc),digits = 2)*100,"%")
  
  
  ilat= which.min(abs(aws_loc$LAT[i]-dlat))
  ilon= which.min(abs(aws_loc$LON[i]-dlon)) 
  
  T2_hrly[i,M:N]=t2[ilon,ilat,]
  RF_hrly[i,M:N]=rf[ilon,ilat,]
  RH_hrly[i,M:N]=rh[ilon,ilat,] 
  WS_hrly[i,M:N]=ws[ilon,ilat,]
  HI_hrly[i,M:N]= heat.index(t = t2[ilon,ilat,], rh = rh[ilon,ilat,], temperature.metric = "celsius", output.metric = "celsius", round = 2)
  }


write.csv(file = "NC2CSV/RF_hrly.csv",RF_hrly,row.names = F)
write.csv(file = "NC2CSV/T2_hrly.csv",T2_hrly,row.names = F)
write.csv(file = "NC2CSV/RH_hrly.csv",RH_hrly,row.names = F)
write.csv(file = "NC2CSV/WS_hrly.csv",WS_hrly,row.names = F)
write.csv(file = "NC2CSV/HI_hrly.csv",HI_hrly,row.names = F)

RF_hrly_mndal=group_by(RF_hrly, DISTRICT,MANDAL)%>%summarise_at(vars(colnames(df)), max, na.rm = TRUE)
T2_hrly_mndal=group_by(T2_hrly, DISTRICT,MANDAL)%>%summarise_at(vars(colnames(df)), max, na.rm = TRUE)
RH_hrly_mndal=group_by(RH_hrly, DISTRICT,MANDAL)%>%summarise_at(vars(colnames(df)), mean, na.rm = TRUE)
WS_hrly_mndal=group_by(WS_hrly, DISTRICT,MANDAL)%>%summarise_at(vars(colnames(df)), max, na.rm = TRUE)
HI_hrly_mndal=group_by(HI_hrly, DISTRICT,MANDAL)%>%summarise_at(vars(colnames(df)), max, na.rm = TRUE)

write.csv(file = "NC2CSV/RF_hrly_mndal.csv",RF_hrly_mndal,row.names = F)
write.csv(file = "NC2CSV/T2_hrly_mndal.csv",T2_hrly_mndal,row.names = F)
write.csv(file = "NC2CSV/RH_hrly_mndal.csv",RH_hrly_mndal,row.names = F)
write.csv(file = "NC2CSV/WS_hrly_mndal.csv",WS_hrly_mndal,row.names = F)
write.csv(file = "NC2CSV/HI_hrly_mndal.csv",HI_hrly_mndal,row.names = F)

}









