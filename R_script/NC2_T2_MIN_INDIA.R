NC_T2_MIN_INDIA <- function(out.dir,ncfile, DAYY,TIME,NDAYS,nv_time,stat,end){

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

s2 <- readOGR("../src_files/IMD_shp", "imdDissolve_Copy")
s3 <- readOGR("../src_files/IMD_shp", "imdDissolve_Copy")


for (i in 1:NDAYS) {
  print(paste("DAY",i,"t2,rh,ws=",nv_time[stat[i]],"to",nv_time[end[i]]))
}

###########################################

collmn=colorRampPalette(c('#F0F0F0','#DEDEF2','#B4D7FF','#75BAFF','#359AFF','#0482FF','#0069D2','#63ED07','#FFA66A','#F71E54','#BF0000','#880000','#64007F','#DD66FF','#EBA6FF','#D4D4D4','#969696'))(27) 
#lvlsmn=c(-10,-5,0,5,10,15,20,23,26,28,30,32,34,36,38,39,40,41,42,43,44,45,46,47,48,49,50,51)
lvlsmn=c(-10,-5,0,1,2,3,4,5,6,7,8,9,11,12,13,14,15,16,17,18,19,20,22,24,26,28,30,35)                                

png(file="TEMP.png",   width = 800*2,height = 675*2)
par(mfrow=c(3,3))
par(mar=c(.5,.4,.3,.3))  
#for (dd in 1:NDAYS) {
for (dd in 1:9) {
print(dd)

  DET1=pr[,,stat[dd]:end[dd]]
  DET1=apply(DET1, c(1,2), min)

  if(dd>6){DET1=DET1}
  
  # grid.list<- list( x= lon, y=lat)
  # obj<- list( x=dlon , y=dlat, z= RF_rstr)
  # intrep_rf=interp.surface.grid( obj, grid.list)
  # RF=intrep_rf$z*(msk/msk)
  
  image(DET1[,(ncol(DET1):1)],x=dlon,y=rev(dlat),breaks = lvlsmn,main="", col = collmn,axes=F,xlab = "",ylab = "",xlim=c(62,95))
  box()
  text(paste(format(nv_time[stat[dd]],"%b_%d")),x=87,y=33,cex = 4.5,font = 2)
  plot(s3,add=TRUE,border= rgb(0, 0, 0,alpha=0.2),lwd=.5)
  plot(s2,add=TRUE,border= rgb(0, 0, 0,alpha=0.5),cex.axis=.5,lwd=.2)
  }
dev.off()
###########################################

pdf(paste("NC2PDF/T2_MIN_INDIA_",format(as.Date(DAYY), "%Y_%b_%d "),TIME,".pdf",sep = ""), width = 10 , height =7.5 ,onefile=T,bg="white")
RTG <- readPNG("../src_files/RTGC_English_Telugu_12x1.png")
grid.raster(RTG[,1:700,],x=0.1,y=.96, width=unit(.17, "npc"), interp=FALSE)
grid.raster(RTG[,1700:2400,],x=0.9,y=.96, width=unit(.17, "npc"), interp=FALSE)

RF <- readPNG("TEMP.png")
VASAR <- readPNG("../src_files/vasarlab_scale_rf.png")


grid.raster(RF,x=0.45,y=.46, width=unit(0.75, "npc"), interp=FALSE)



grid.text("Daily Minimum Temperature (2m) ", x=.5, .96, gp=gpar(fontsize=18,fontface = "bold") , check=TRUE)
grid.text(paste("Date: ",format(nv_time[stat[1]],"%Y_%b_%d"),sep = ""), x=.5, .915, gp=gpar(fontsize=13,fontface = "bold") , check=TRUE)
grid.rect(width = .9, height = 0.001,x=.5,y=.89,  gp=gpar(col=NA, fill=rgb(0, 0,0,alpha=0.2)))

grid.rect(width = .9, height = 0.001,x=.5,y=.037,  gp=gpar(col=NA, fill=rgb(0, 0,0,alpha=0.2)))

#grid.text(paste("*ISRO SDSC-SHAR Rainfall forecast: Accumulated rainfall from 05:30 AM to 05:30 AM (next day) IST",sep = ""), x=.05, y=.04, gp=gpar(fontsize=8) , check=TRUE,just = "left")
grid.text(paste(" Source: Experimental predictions from ISRO-SDSC SHAR based on European Centre for Medium Range Weather forecast System (ECMWF)",sep = ""), x=.05, y=.02, gp=gpar(fontsize=8) , check=TRUE,just = "left")
grid.text(paste("*Forecast based on ",format(as.Date(DAYY), "%Y_%b_%d "),TIME,sep = ""),x=.05, y=.90, gp=gpar(fontsize=10,fontface = "bold") , check=TRUE,just = "left")

grid.text(expression(~degree~C), x=.88, .88, gp=gpar(fontsize=15,fontface = "bold") , check=TRUE)
for (i in 1:length(collmn)){
  grid.rect(width = .025, height = 0.035,x=.85,y=.047+(i*.03),  gp=gpar(col=NA, fill=collmn[i]))
  if(i!=length(collmn)){grid.text(lvlsmn[(i+1)], x=.88,y= .057+(i*.03), gp=gpar(fontsize=13,fontface = "bold",just = "left") , check=TRUE)}
}


dev.off()

file.remove("TEMP.png")

}
