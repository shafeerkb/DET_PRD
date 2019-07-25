NC_SUMMARY <- function(out.dir,ncfile, DAYY,TIME,NDAYS,nv_time,stat,end,stat_rf,end_rf){
  setwd(out.dir)
  dir.create("NC2PDF", showWarnings = FALSE)

if(TIME=="12_UTC"){
  x.Date <- format(as.Date(DAYY) + c(-0) , "%Y%m%d")
  Date= format(as.Date(DAYY) + c(1:9) , "D%Y%m%d")
  Date1= format(as.Date(DAYY) + c(1:9) , "%d-%m-%y")
    
  DAYY_TM=as.Date(substr(substrRight(ncfile, 30),18,25),format ="%Y%m%d")+1

}else if(TIME=="00_UTC"){
  x.Date <- format(as.Date(DAYY) + c(-0) , "%Y%m%d")
  Date= format(as.Date(DAYY) + c(0:9) , "D%Y%m%d")
  Date1= format(as.Date(DAYY) + c(0:9) , "%d-%m-%y")
  DAYY_TM=as.Date(substr(substrRight(ncfile, 30),18,25),format ="%Y%m%d")
}


s2 <- readOGR("../src_files/AP_Dis", "AP_DISTRICT_BND")
s3 <- readOGR("../src_files/AP_state", "AP_state_gcs1")
DIS=c("Srikakulam","Vizianagaram","Visakhapatnam","East Godavari","West Godavari","Krishna","Guntur","Prakasam","Kurnool","Anantapur","Kadapa","Chittoor","Nellore")



nc <- nc_open("../src_files/ap_ds_5k.nc")
lat <- ncvar_get( nc, "lat" )
lon <- ncvar_get( nc, "lon" )
msk <-  ncvar_get(nc,"dist")+1

f1 <- nc_open(ncfile)


dlat=ncvar_get(f1,"latitude")
dlon=ncvar_get(f1,"longitude")
t2 <- ncvar_get( f1,"2m_temperature")         %>% round(digits = 2) +1 
rh <- ncvar_get( f1,"2m_relative_humidity")   %>% round(digits = 2)
pr <- ncvar_get( f1,"6hr_accumulated_precip") %>% round(digits = 2)
ws <- ncvar_get( f1,"wind_magnitude")*3.6     %>% round(digits = 2)

rh[rh<0]=0
rh[rh>100]=100

###########################################
  for (dd in 1:NDAYS) {
    print(dd)
#  for (dd in 1:3) {
  RF_rstr=pr[,,end_rf[dd]]-pr[,,stat_rf[dd]]
  RF_rstr[RF_rstr<0]=0
  
   bias=0
  if(dd>6){
   bias=1
  }
  
  grid.list<- list( x= lon, y=lat)
  obj<- list( x=dlon , y=dlat, z= RF_rstr)
  intrep_rf=interp.surface.grid( obj, grid.list)
  RF=intrep_rf$z*(msk/msk)

  obj<- list( x=dlon , y=dlat, z= apply(t2[,,end[dd]:stat[dd]]+bias, c(1,2), max))
  intrep_t2=interp.surface.grid( obj, grid.list)
  T2M=intrep_t2$z*(msk/msk)
  


#  lvlsmn=c(23,24,25,26,27,28,29,30,31,32,33,34,35,36,37,38,39,40,41,42,43,44,45,46,47,48,49,50)-2
  lvlsmn=c(0,10,15,20,25,28,29,30,31,32,33,34,35,36,37,38,39,40,41,42,43,44,45,46,47,48,49,50)
  collmn=c('#F0F0F0','#DEDEF2','#B4D7FF','#75BAFF','#359AFF','#0482FF','#0069D2','#00367F','#148F1B','#1ACF05','#63ED07','#FFF42B','#E8DC00','#F06000','#FF7F27','#FFA66A','#F84E78','#F71E54','#BF0000','#880000','#64007F','#C200FB','#DD66FF','#EBA6FF','#F9E6FF','#D4D4D4','#969696')
  
  


PR=T2M  #interp(new$Longitude,new$Latitude,new$Tmax,lon,lat,duplicate = "mean",linear = T)$z*(msk/msk)

png("T2.png", width = 8, height =6.5, units = 'in', res = 300)
par(mar=c(0,0,0,0))
image(PR[,144:1],x=lon,y=rev(lat),breaks = lvlsmn,main="", col = collmn,axes=F,xlab = "",ylab = "")
plot(s3,add=TRUE,lwd=1,axes=F)
plot(s2,add=TRUE,cex.axis=4.5,lwd=.25,axes=F)
#text(format(as.Date(DAYY)-1, "%Y_%b_%d"),x=78.5,y=18.5,cex = 1,font = 2)
#text(paste("Temperature (?C)"),x=78.5,y=17.9,cex = 2.5,font = 2)

dev.off()


z=is.na(msk)==FALSE

df=data.frame(matrix( nrow=length(z[z == TRUE]), ncol=1))
colnames(df)=c("No")
#df1=data.frame(DIST=as.character(),Temp=as.numeric())
k=0
for (i in 1:NROW(msk)) {
  for (j in 1:NCOL(msk)) {
    if(!is.na(PR[i,j]))
    {
      k=k+1
      df$No=k
      df$DIST[k]=DIS[(msk[i,j]-1)]
      df$Temp[k]=as.numeric(PR[i,j])
      df$T_MAX[k]=paste(lvlsmn[which.min(df$Temp[k]>=lvlsmn)-1],"-",lvlsmn[which.min(df$Temp[k]>=lvlsmn)]-0.1,sep = "")
    }
}}

df$DIST <- factor(df$DIST,levels=rev(DIS))

#df$T_MAX=  as.character(paste(floor(df$Temp),"-",floor(df$Temp)+.9))


summ=dplyr::group_by(df,DIST,T_MAX)%>% dplyr::summarise(N =n())%>% dplyr::mutate(perc = 100 * N/sum(N))

min(df$Temp)
df=df[complete.cases(df), ]
summ <- ddply(summ, .(DIST),
                     transform, pos =100- cumsum(perc) + (0.5 * perc) )

summ$pos[summ$perc<5] =NA


p4 <- ggplot() + geom_bar(aes(y=perc, x = DIST , fill = T_MAX), data = summ,stat="identity")+
  geom_text(data=summ, aes(x = DIST, y = pos, label = paste(round(perc,digits = 0),"%")),size=4,fontface = "bold")+
  coord_flip()+
#  scale_fill_manual(values=collmn[which(lvlsmn==floor(min(df$Temp))):(which(lvlsmn==floor(min(df$Temp)))+length(unique(df$T_MAX)))])+
  scale_fill_manual(values=collmn[(which.min(lvlsmn<=floor(min(df$Temp)))-1):(which.min(lvlsmn<=floor(min(df$Temp)))-1+length(unique(df$T_MAX)))])+  
  theme(strip.text.x = element_text(face = "bold",size = 12.55, colour = "Black"))+
  theme(axis.text.x = element_text(face = "bold", color = "black", size = 12.55))+
  theme(axis.text.y = element_text(face = "bold", color = "black", size = 12.55))+
  xlab("") + ylab("% of Area")  +
  theme (legend.position=c(.8,5.7),legend.direction = "vertical")+
  theme(axis.title.x = element_text(color="black", size=13, face="bold"))+
  guides(colour = guide_legend(override.aes = list(size=3)))+
  theme(legend.text = element_text(size = 12.55, colour = "black",face="bold"),legend.spacing.x = unit(.08, 'cm'))+
  theme(legend.title=element_blank())+
  scale_y_continuous(position = "left")+
  scale_x_discrete(position = "left")+
  theme(legend.position="left")

#  +ggtitle("Maximum observed Temperature (?C) District wise area (%) 30-May-2019 "  )    

# p4

png("per.png", width = 8, height = 5, units = 'in', res = 300)
par(bg=NA) 
print(p4)
dev.off()



collmn=c("#FFFFFF","#C3FDCA","#01FF04","#048500","#FDC0CB","#FC0300","#610301")
lvlsmn=c(0,.1,2.5,15.6,64.5,115.6,204.5,800)
lvlsmn_cat=c("No Rain","Very light Rain","Light Rain","Moderate Rain","Heavy Rain","Very Heavy Rain","Extremely Heavy Rain")


PR=  RF#interp(new$Longitude,new$Latitude,new$Rain,lon,lat,duplicate = "mean",linear = T)$z*(msk/msk)
png("RF.png", width = 8, height = 6.5, units = 'in', res = 300)
par(mar=c(0,0,0,0))
image(PR[,144:1],x=lon,y=rev(lat),breaks = lvlsmn,main="", col = collmn,axes=F,xlab = "",ylab = "")
plot(s3,add=TRUE,lwd=1,axes=F)
plot(s2,add=TRUE,cex.axis=4.5,lwd=.25,axes=F)
#text(format(as.Date(DAYY)-1, "%Y_%b_%d"),x=78.5,y=18.5,cex = 1,font = 2)
#text(paste("Rainfall"),x=78.5,y=17.9,cex = 2.5,font = 2 )

dev.off()


z=is.na(msk)==FALSE

df=data.frame(matrix( nrow=length(z[z == TRUE]), ncol=1))
colnames(df)=c("No")
#df1=data.frame(DIST=as.character(),Temp=as.numeric())
k=0

for (i in 1:NROW(msk)) {
  for (j in 1:NCOL(msk)) {
    if(!is.na(PR[i,j]))
    {
      k=k+1
      df$No=k
      df$DIST[k]=DIS[(msk[i,j]-1)]
      df$Temp[k]=as.numeric(PR[i,j])
      if(k==1){df$Temp[k]=as.numeric(PR[i,j])*0}
      if(k==10){df$Temp[k]=2}
      if(k==100){df$Temp[k]=7}
      if(k==200){df$Temp[k]=20}
      if(k==300){df$Temp[k]=100}
      if(k==400){df$Temp[k]=150}
      df$T_MAX[k]=lvlsmn_cat[max(which(df$Temp[k]>lvlsmn))]
      if(is.na(df$T_MAX[k])){df$T_MAX[k]=lvlsmn_cat[1]}
    }
  }}

df=df[complete.cases(df), ]
df$DIST <- factor(df$DIST,levels=rev(DIS))

#df$T_MAX=  as.character(paste(floor(df$Temp),"-",floor(df$Temp)+.9))
df$T_MAX <- factor(df$T_MAX, levels = lvlsmn_cat)

summ=dplyr::group_by(df,DIST,T_MAX)%>% dplyr::summarise(N =n())%>% dplyr::mutate(perc = 100 * N/sum(N))

min(df$Temp)


summ <- ddply(summ, .(DIST),
              transform, pos =100- cumsum(perc) + (0.5 * perc) )


summ$pos[summ$perc<5] =NA
p5 <- ggplot(data=summ,order=gclass) + geom_bar(aes(y=perc, x = DIST , fill = T_MAX), data = summ,stat="identity")+
  geom_text(data=summ, aes(x = DIST, y = pos, label = paste(round(perc,digits = 0),"%")),size=4,fontface = "bold")+
  coord_flip()+
  scale_fill_manual(values=collmn)+
  theme(strip.text.x = element_text(face = "bold",size = 18, colour = "Black"))+
  theme(axis.text.x = element_text(face = "bold", color = "black", size = 12.55))+
  theme(axis.text.y = element_text(face = "bold", color = "black", size = 12.55))+
  xlab("") + ylab("% of Area")  +
  theme (legend.position=c(.8,5.7),legend.direction = "vertical")+
  guides(colour = guide_legend(override.aes = list(size=3)))+
  theme(axis.title.x = element_text(color="black", size=13, face="bold"))+
  theme(legend.text = element_text(size = 12.55, colour = "black",face="bold"),legend.spacing.x = unit(.08, 'cm'))+
  theme(legend.title=element_blank())+
  scale_y_continuous(position = "left")+
  scale_x_discrete(position = "left")+
  theme(legend.position="left")

#  +ggtitle("Maximum observed Temperature (?C) District wise area (%) 30-May-2019 "  )    

# p4a

png("perrf.png", width = 8, height = 5, units = 'in', res = 300)
par(bg=NA) 
print(p5)
dev.off()

full=ggarrange(p4,p5,
               # labels = c("A", "B","c","d"),
               ncol = 1, nrow = 2,heights=c(1,1),vjust=1,
               align = c( "v"))


png(paste("teat.png",sep = ""), width = 10, height = 10, units = 'in', res = 300)
print(full)
dev.off()



pdf(paste("NC2PDF/Forecast_summary",format(as.Date(DAYY_TM)+dd-1, "%Y_%b_%d"),".pdf",sep = ""), width = 10 , height =7 ,onefile=T,bg="white")
RTG <- readPNG("../src_files/RTGC_English_Telugu_12x1.png")
grid.raster(RTG[,1:700,],x=0.1,y=.96, width=unit(.17, "npc"), interp=FALSE)
grid.raster(RTG[,1700:2400,],x=0.9,y=.96, width=unit(.17, "npc"), interp=FALSE)

img1 <- readPNG("teat.png")
img2 <- readPNG("T2.png")
img4 <- readPNG("RF.png")

VASAR <- readPNG("../src_files/vasarlab_scale_rf.png")
Category <- readPNG("../src_files/Category_rf.png")


grid.raster(img2,x=0.22,y=.68, width=unit(0.35, "npc"), interp=FALSE)
grid.raster(img1,x=0.70,y=.475, width=unit(0.56, "npc"), interp=FALSE)

grid.raster(img4,x=0.22,y=.27, width=unit(0.35, "npc"), interp=FALSE)
#grid.raster(img3,x=0.72,y=.28, width=unit(0.6, "npc"), interp=FALSE)
grid.raster(VASAR,x=0.32,y=.17, width=unit(0.2, "npc"), interp=FALSE)
grid.raster(Category,x=0.33,y=.57, width=unit(0.17, "npc"), interp=FALSE)

grid.text("Forecasted Rainfall and Maximum Temperature", x=.5, .96, gp=gpar(fontsize=18,fontface = "bold") , check=TRUE)
grid.text(paste("Date: ",format(as.Date(DAYY_TM)+dd-1, "%Y_%b_%d"),sep = ""), x=.5, .915, gp=gpar(fontsize=13,fontface = "bold") , check=TRUE)
grid.rect(width = .9, height = 0.001,x=.5,y=.89,  gp=gpar(col=NA, fill=rgb(0, 0,0,alpha=0.2)))
grid.rect(width = .9, height = 0.001,x=.5,y=.06,  gp=gpar(col=NA, fill=rgb(0, 0,0,alpha=0.2)))
grid.rect(width = .9, height = 0.001,x=.5,y=.473,  gp=gpar(col=NA, fill=rgb(0, 0,0,alpha=0.2)))

#grid.text("Maximum Temperature (?C)", x=.05, .87, gp=gpar(fontsize=13,fontface = "bold") , check=TRUE,just = "left")
#grid.text(paste("Maximum Recorded ",tmax,"(?C)",sep = ""), x=.05, .83, gp=gpar(fontsize=11) , check=TRUE,just = "left") 
#grid.text(paste(tmax_loc,sep = ""), x=.05, .80, gp=gpar(fontsize=11) , check=TRUE,just = "left") 
#grid.text(paste("(",tmax_dis,")",sep = ""), x=.05, .77, gp=gpar(fontsize=11) , check=TRUE,just = "left") 

# if(rfmax>0){
#   grid.text("Rainfall ", x=.05, .45, gp=gpar(fontsize=13,fontface = "bold") , check=TRUE,just = "left")
#   grid.text(paste("Maximum Recorded ",rfmax," mm" ,sep = ""), x=.05, .41, gp=gpar(fontsize=11) , check=TRUE,just = "left") 
#   grid.text(paste(rfmax_loc,sep = ""), x=.05, .38, gp=gpar(fontsize=11) , check=TRUE,just = "left") 
#   grid.text(paste("(",rfmax_dis,")",sep = ""), x=.05, .35, gp=gpar(fontsize=11) , check=TRUE,just = "left") 
# }
grid.text(paste("*Accumulated rainfall Forecast from 05:30 AM to 05:30 AM (next day) IST",sep = ""), x=.05, y=.04, gp=gpar(fontsize=8) , check=TRUE,just = "left")
grid.text(paste(" Source: Experimental Weather predictions from ISRO-SDSC SHAR based on  ECMWF "," Prepared by: RTG-AWARE",sep = ""), x=.05, y=.02, gp=gpar(fontsize=8) , check=TRUE,just = "left")
grid.text(paste("Forecast based on ",format(as.Date(DAYY), "%Y_%b_%d "),TIME,sep = ""),x=.05, y=.90, gp=gpar(fontsize=10,fontface = "bold") , check=TRUE,just = "left")
# 
# grid.text(paste("Summary of Automatic Weather Stations from 08:30AM of",format(as.Date(DAYY), "%Y_%b_%d"), "to 08:30AM of " ,format(as.Date(DAYY)+1, "%Y_%b_%d"),sep = " "), x=.05, y=.04, gp=gpar(fontsize=10) , check=TRUE,just = "left")
# grid.text(paste("Source: APSDPS, Prepared by: RTG-AWARE",sep = ""), x=.05, y=.02, gp=gpar(fontsize=10) , check=TRUE,just = "left")


dev.off()

file.remove("teat.png")
file.remove("T2.png")
file.remove("RF.png")
file.remove("perrf.png")
file.remove("per.png")
file.remove("temp.csv")


}

}
