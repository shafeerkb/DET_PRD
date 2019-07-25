NC2CSV_DIST <- function(out.dir,ncfile, DAYY,TIME,NDAYS,nv_time,stat,end,stat_rf,end_rf){
  
setwd(out.dir)

DIS=c("Srikakulam","Vizianagaram","Visakhapatnam","East Godavari","West Godavari","Krishna","Guntur","Prakasam","Kurnool","Anantapur","Kadapa","Chittoor","Nellore")


if(TIME=="12_UTC"){
  x.Date <- format(as.Date(DAYY) + c(-0) , "%Y%m%d")
  Date= format(as.Date(DAYY) + c(1:9) , "D%Y%m%d")
  
  T2_day=data.frame(DIST=DIS, DAY1=NA,DAY2=NA,DAY3=NA,DAY4=NA,DAY5=NA,DAY6=NA,DAY7=NA,DAY8=NA,DAY9=NA)
  HI_day=data.frame(DIST=DIS, DAY1=NA,DAY2=NA,DAY3=NA,DAY4=NA,DAY5=NA,DAY6=NA,DAY7=NA,DAY8=NA,DAY9=NA)
  RF_day=data.frame(DIST=DIS, DAY1=NA,DAY2=NA,DAY3=NA,DAY4=NA,DAY5=NA,DAY6=NA,DAY7=NA,DAY8=NA,DAY9=NA)
  WS_day=data.frame(DIST=DIS, DAY1=NA,DAY2=NA,DAY3=NA,DAY4=NA,DAY5=NA,DAY6=NA,DAY7=NA,DAY8=NA,DAY9=NA)
  RH_day=data.frame(DIST=DIS, DAY1=NA,DAY2=NA,DAY3=NA,DAY4=NA,DAY5=NA,DAY6=NA,DAY7=NA,DAY8=NA,DAY9=NA)

}else if(TIME=="00_UTC"){
  x.Date <- format(as.Date(DAYY) + c(-0) , "%Y%m%d")
  Date= format(as.Date(DAYY) + c(0:9) , "D%Y%m%d")
  T2_day=data.frame(DIST=DIS, DAY1=NA,DAY2=NA,DAY3=NA,DAY4=NA,DAY5=NA,DAY6=NA,DAY7=NA,DAY8=NA,DAY9=NA,DAY10=NA)
# HI_day=data.frame(DIST=DIS, DAY1=NA,DAY2=NA,DAY3=NA,DAY4=NA,DAY5=NA,DAY6=NA,DAY7=NA,DAY8=NA,DAY9=NA,DAY10=NA)
  RF_day=data.frame(DIST=DIS, DAY1=NA,DAY2=NA,DAY3=NA,DAY4=NA,DAY5=NA,DAY6=NA,DAY7=NA,DAY8=NA,DAY9=NA,DAY10=NA)
  WS_day=data.frame(DIST=DIS, DAY1=NA,DAY2=NA,DAY3=NA,DAY4=NA,DAY5=NA,DAY6=NA,DAY7=NA,DAY8=NA,DAY9=NA,DAY10=NA)
  RH_day=data.frame(DIST=DIS, DAY1=NA,DAY2=NA,DAY3=NA,DAY4=NA,DAY5=NA,DAY6=NA,DAY7=NA,DAY8=NA,DAY9=NA,DAY10=NA)

}

f1 <- nc_open(ncfile)

dlat=ncvar_get(f1,"latitude")
dlon=ncvar_get(f1,"longitude")
t2 <- ncvar_get( f1,"2m_temperature")          +1 
rh <- ncvar_get( f1,"2m_relative_humidity")   
pr <- ncvar_get( f1,"6hr_accumulated_precip") 
ws <- ncvar_get( f1,"wind_magnitude")*3.6     

nc <- nc_open("../src_files/ap_ds_5k.nc")
lat <- ncvar_get( nc, "lat" )
lon <- ncvar_get( nc, "lon" )
msk <-  ncvar_get(nc,"dist")
grid.list<- list( x= lon, y=lat)

       for (dd in 1:NDAYS) {
         print(dd)
         
         T2AP=t2[,,stat[dd]:end[dd]]
         RHAP=rh[,,stat[dd]:end[dd]]
         WSAP=ws[,,stat[dd]:end[dd]]
         
         RFAP=pr[,,end_rf[dd]]-pr[,,stat_rf[dd]]
         T2AP1=apply(T2AP, c(1,2), max)
         RHAP1=apply(RHAP, c(1,2), max)
         WSAP1=apply(WSAP, c(1,2), mean)
         
         
         if(i>6){T2AP1=T2AP1+1}
         

         for (dis in 1:13) {
           
           obj<- list( x=dlon , y=dlat, z= RFAP)
           intrep_RFAP=interp.surface.grid( obj, grid.list)$z*(msk/msk)
           
           obj<- list( x=dlon , y=dlat, z= T2AP1)
           intrep_T2AP1=interp.surface.grid( obj, grid.list)$z*(msk/msk)
           
           obj<- list( x=dlon , y=dlat, z= RHAP1)
           intrep_RHAP1=interp.surface.grid( obj, grid.list)$z*(msk/msk)
           
           obj<- list( x=dlon , y=dlat, z= WSAP1)
           intrep_WSAP1=interp.surface.grid( obj, grid.list)$z*(msk/msk)
           

           intrep_RFAP[msk!=dis]=NA
           intrep_T2AP1[msk!=dis]=NA
           intrep_RHAP1[msk!=dis]=NA
           intrep_WSAP1[msk!=dis]=NA
           RF_day[dis,(dd+1)]=mean(intrep_RFAP,na.rm = T)  %>% round(digits = 2)
           T2_day[dis,(dd+1)]=mean(intrep_T2AP1,na.rm = T) %>% round(digits = 2)
           RH_day[dis,(dd+1)]=mean(intrep_RHAP1,na.rm = T) %>% round(digits = 2)
           WS_day[dis,(dd+1)]=mean(intrep_WSAP1,na.rm = T) %>% round(digits = 2)
           
#           image.plot(intrep_T2AP1[,144:1],x=lon,y=rev(lat))
         }
         
         # image.plot(intrep_RF[,144:1],x=lon,y=rev(lat))
         # plot(s3,add=TRUE,lwd=1)
         # plot(s2,add=TRUE,cex.axis=4.5,lwd=.25)
         # text(paste(x.Datem[i]),x=79.3,y=18,cex = 4.5,font = 2)
         
       }

       
colnames(T2_day)=c("DISTRICT",Date)
#colnames(HI_day)=c("DISTRICT",Date)
colnames(RF_day)=c("DISTRICT",Date)
colnames(WS_day)=c("DISTRICT",Date)
colnames(RH_day)=c("DISTRICT",Date)
# 
write.csv(file = paste("NC2CSV/T2_day_DIST",x.Date,TIME,".csv",sep = ""),T2_day,row.names = F)
#write.csv(file = paste("NC2CSV/HI_day_DIST",x.Date,TIME,".csv",sep = ""),HI_day,row.names = F)
write.csv(file = paste("NC2CSV/RF_day_DIST",x.Date,TIME,".csv",sep = ""),RF_day,row.names = F)
write.csv(file = paste("NC2CSV/WS_day_DIST",x.Date,TIME,".csv",sep = ""),WS_day,row.names = F)
write.csv(file = paste("NC2CSV/RH_day_DIST",x.Date,TIME,".csv",sep = ""),RH_day,row.names = F)
# 
# T2_day_mndal=group_by(T2_day, DISTRICT,MANDAL)%>%summarise_at(vars(Date), max, na.rm = TRUE)
# HI_day_mndal=group_by(HI_day, DISTRICT,MANDAL)%>%summarise_at(vars(Date), max, na.rm = TRUE)
# RF_day_mndal=group_by(RF_day, DISTRICT,MANDAL)%>%summarise_at(vars(Date), max, na.rm = TRUE)
# WS_day_mndal=group_by(WS_day, DISTRICT,MANDAL)%>%summarise_at(vars(Date), max, na.rm = TRUE)
# RH_day_mndal=group_by(RH_day, DISTRICT,MANDAL)%>%summarise_at(vars(Date), mean, na.rm = TRUE)
# 
# write.csv(file = paste("T2_day_mndal",x.Date,TIME,".csv",sep = ""),T2_day_mndal,row.names = F)
# write.csv(file = paste("HI_day_mndal",x.Date,TIME,".csv",sep = ""),HI_day_mndal,row.names = F)
# write.csv(file = paste("RF_day_mndal",x.Date,TIME,".csv",sep = ""),RF_day_mndal,row.names = F)
# write.csv(file = paste("WS_day_mndal",x.Date,TIME,".csv",sep = ""),WS_day_mndal,row.names = F)
# write.csv(file = paste("RH_day_mndal",x.Date,TIME,".csv",sep = ""),RH_day_mndal,row.names = F)
# 

}
