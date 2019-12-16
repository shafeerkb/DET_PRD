NC_T2_INDIA_DIST <- function(out.dir,ncfile, DAYY,TIME,NDAYS,nv_time,stat,end,stat_rf,end_rf){

  setwd(out.dir)
  dir.create("NC2PDF", showWarnings = FALSE)


if(TIME=="12_UTC"){
  x.Date <- format(as.Date(DAYY) + c(-0) , "%Y%m%d")
  Date= format(as.Date(DAYY) + c(1:9) , "D%Y%m%d")
  Date1= format(as.Date(DAYY) + c(1:9) , "%d-%m-%y")

}else if(TIME=="00_UTC"){
  x.Date <- format(as.Date(DAYY) + c(-0) , "%Y%m%d")
  Date= format(as.Date(DAYY) + c(0:9) , "D%Y%m%d")
  Date1= format(as.Date(DAYY) + c(0:9) , "%d-%m-%y")

}
f1 <- nc_open(ncfile)

dlat=ncvar_get(f1,"latitude")
dlon=ncvar_get(f1,"longitude")

pr <- ncvar_get( f1,"2m_temperature") %>% round(digits = 2)


nc <- nc_open("../src_files/ap_ds_5k.nc")
lat <- ncvar_get( nc, "lat" )
lon <- ncvar_get( nc, "lon" )
msk <-  ncvar_get(nc,"dist")+1



s2 <- readOGR("../src_files/IND_dist", "india_final__geo")
s3 <- readOGR("../src_files/IMD_shp", "imdDissolve_Copy")


for (i in 1:NDAYS) {
  print(paste("DAY",i,"t2 minimum from",nv_time[stat[i]],"to",nv_time[end[i]], "rain=",nv_time[stat_rf[i]],"to",nv_time[end_rf[i]]))
}

###########################################
neg=colorRampPalette(c("#BA81EA","#42E8F6","#DEF6F8"))( 8) 
pos=colorRampPalette(c("#12830A","#71B61D","#E1EE18","#DF9905","#DA4C08","#F86659"))(16) 

#collmn=colorRampPalette(c('#F0F0F0','#DEDEF2','#B4D7FF','#75BAFF','#359AFF','#0482FF','#0069D2','#63ED07','#FFA66A','#F71E54','#BF0000','#880000','#64007F','#DD66FF','#EBA6FF','#D4D4D4','#969696'))(27) 
collmn=c(neg,pos)
lvlsmn=c(-40,-20,-15,-10,-8,-6,-4,-2,0,2,4,6,8,10,12,14,16,18,20,22,24,26,28,30,35)                                


png(file="TEMP.png",  width = 1000*6,height = 675*6)
par(mfrow=c(3,4))
par(mar=c(.5,.4,.3,.3))  
for (dd in 1:NDAYS) {
#for (dd in 1:2) {
print(dd)

    RF_rstr=pr[,,stat[dd]:end[dd]]
    RF_rstr=apply(RF_rstr, c(1,2), min)  

  
  # grid.list<- list( x= lon, y=lat)
  # obj<- list( x=dlon , y=dlat, z= RF_rstr)
  # intrep_rf=interp.surface.grid( obj, grid.list)
  # RF=intrep_rf$z*(msk/msk)
  
  image(RF_rstr[,(ncol(RF_rstr):1)],x=dlon,y=rev(dlat),breaks = lvlsmn,main="", col = collmn,axes=F,xlab = "",ylab = "",xlim=c(62,95))
  box()
  text(paste(format(nv_time[stat[dd]],"%b_%d")),x=87,y=33,cex = 14.5,font = 2)
  plot(s3,add=TRUE,border= rgb(0, 0, 0,alpha=0.8),lwd=1)
  plot(s2,add=TRUE,border= rgb(0, 0, 0,alpha=0.3),cex.axis=.5,lwd=.5)
  }
dev.off()
###########################################

pdf(paste("NC2PDF/T2_MIN_INDIA_DIST_",format(as.Date(DAYY), "%Y_%b_%d "),TIME,".pdf",sep = ""), width = 10 , height =7.5 ,onefile=T,bg="white")
# RTG <- readPNG("../src_files/RTGC_English_Telugu_12x1.png")
# grid.raster(RTG[,1:700,],x=0.1,y=.96, width=unit(.17, "npc"), interp=FALSE)
# grid.raster(RTG[,1700:2400,],x=0.9,y=.96, width=unit(.17, "npc"), interp=FALSE)

RF <- readPNG("TEMP.png")
#VASAR <- readPNG("../src_files/vasarlab_scale_rf.png")


#grid.raster(RF,x=0.5,y=.47, width=unit(0.9, "npc"), interp=FALSE)
grid.raster(RF,x=0.47,y=.47, width=unit(0.9, "npc"), interp=FALSE)
#grid.raster(VASAR,x=0.8,y=.18, width=unit(0.32, "npc"), interp=FALSE)


grid.text("Minimum Temperature forecast ", x=.5, .96, gp=gpar(fontsize=18,fontface = "bold") , check=TRUE)
grid.text(paste("Date: ",format(nv_time[stat[1]],"%Y_%b_%d"),sep = ""), x=.5, .915, gp=gpar(fontsize=13,fontface = "bold") , check=TRUE)
grid.rect(width = .9, height = 0.001,x=.5,y=.89,  gp=gpar(col=NA, fill=rgb(0, 0,0,alpha=0.2)))

grid.rect(width = .9, height = 0.001,x=.5,y=.06,  gp=gpar(col=NA, fill=rgb(0, 0,0,alpha=0.2)))

#grid.text(paste("*Minimum Temperature forecast:",sep = ""), x=.05, y=.04, gp=gpar(fontsize=8) , check=TRUE,just = "left")
grid.text(paste("Source: Experimental predictions from ISRO-SDSC SHAR based on European Centre for Medium Range Weather forecast System (ECMWF)",sep = ""), x=.05, y=.03, gp=gpar(fontsize=8) , check=TRUE,just = "left")
grid.text(paste("Forecast based on ",format(as.Date(DAYY), "%Y_%b_%d "),TIME,sep = ""),x=.05, y=.90, gp=gpar(fontsize=10,fontface = "bold") , check=TRUE,just = "left")

grid.text(expression(~degree~C), x=.96, .82, gp=gpar(fontsize=12,fontface = "bold") , check=TRUE)
for (i in 1:length(collmn)){
  grid.rect(width = .025, height = 0.035,x=.945,y=.06+(i*.03),  gp=gpar(col=NA, fill=collmn[i]))
  if(i!=length(collmn)){grid.text(lvlsmn[(i+1)], x=.97,y= .07+(i*.03), gp=gpar(fontsize=11,fontface = "bold",just = "left") , check=TRUE)}
}

dev.off()

png(paste("NC2PDF/T2_MIN_INDIA_",format(as.Date(DAYY), "%Y_%b_%d "),TIME,".png",sep = ""), width = 10000 , height =7500)
# RTG <- readPNG("../src_files/RTGC_English_Telugu_12x1.png")
# grid.raster(RTG[,1:700,],x=0.1,y=.96, width=unit(.17, "npc"), interp=FALSE)
# grid.raster(RTG[,1700:2400,],x=0.9,y=.96, width=unit(.17, "npc"), interp=FALSE)

RF <- readPNG("TEMP.png")
#VASAR <- readPNG("../src_files/vasarlab_scale_rf.png")


grid.raster(RF,x=0.5,y=.47, width=unit(0.9, "npc"), interp=FALSE)
#grid.raster(VASAR,x=0.8,y=.18, width=unit(0.32, "npc"), interp=FALSE)


grid.text("Minimum Temperature forecast ", x=.5, .96, gp=gpar(fontsize=220,fontface = "bold") , check=TRUE)
grid.text(paste("Date: ",format(nv_time[stat[1]],"%Y_%b_%d"),sep = ""), x=.5, .915, gp=gpar(fontsize=160,fontface = "bold") , check=TRUE)
grid.rect(width = .9, height = 0.001,x=.5,y=.89,  gp=gpar(col=NA, fill=rgb(0, 0,0,alpha=0.2)))

grid.rect(width = .9, height = 0.001,x=.5,y=.06,  gp=gpar(col=NA, fill=rgb(0, 0,0,alpha=0.2)))

#grid.text(paste("*Rainfall forecast: Accumulated rainfall from 05:30 AM to 05:30 AM (next day) IST",sep = ""), x=.05, y=.04, gp=gpar(fontsize=120) , check=TRUE,just = "left")
grid.text(paste("Source: Experimental predictions from ISRO-SDSC SHAR based on European Centre for Medium Range Weather forecast System (ECMWF)",sep = ""), x=.05, y=.03, gp=gpar(fontsize=120) , check=TRUE,just = "left")
grid.text(paste("Forecast based on ",format(as.Date(DAYY), "%Y_%b_%d "),TIME,sep = ""),x=.05, y=.90, gp=gpar(fontsize=140,fontface = "bold") , check=TRUE,just = "left")

grid.text(expression(~degree~C), x=.98, .82, gp=gpar(fontsize=120,fontface = "bold") , check=TRUE)
for (i in 1:length(collmn)){
  grid.rect(width = .025, height = 0.035,x=.965,y=.06+(i*.03),  gp=gpar(col=NA, fill=collmn[i]))
  if(i!=length(collmn)){grid.text(lvlsmn[(i+1)], x=.99,y= .07+(i*.03), gp=gpar(fontsize=120,fontface = "bold",just = "left") , check=TRUE)}
}

dev.off()



file.remove("TEMP.png")

}
