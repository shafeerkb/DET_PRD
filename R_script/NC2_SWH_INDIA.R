NC_SWH_INDIA <- function(out.dir,ncfile, DAYY,TIME,NDAYS,nv_time,stat_swh,end_swh,stat_rf,end_rf){

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

dlat=ncvar_get(f1,"wave_latitude")
dlon=ncvar_get(f1,"wave_longitude")

pr <- ncvar_get( f1,"significant_wave_height") 


nc <- nc_open("../src_files/ap_ds_5k.nc")
lat <- ncvar_get( nc, "lat" )
lon <- ncvar_get( nc, "lon" )
msk <-  ncvar_get(nc,"dist")+1



s2 <- readOGR("../src_files/IMD_shp", "imdDissolve_Copy")
s3 <- readOGR("../src_files/IMD_shp", "imdDissolve_Copy")

for (i in 1:NDAYS) {
  print(paste("DAY",i,"t2,rh,ws=",nv_time[stat_swh[i]],"to",nv_time[end_swh[i]]))
}

###########################################

collmn=c("slategray","skyblue","lightgreen","pink3","firebrick","firebrick4")
lvlsmn=c(0,0.5,1.25,2.5,4,6,9)

png(file="TEMP.png",  width = 1000*2,height = 675*2)
par(mfrow=c(3,4))
par(mar=c(.5,.4,.3,.3))  
for (dd in 1:NDAYS) {
#  for (dd in 1:9) {
print(dd)

  DET1=pr[,,stat_swh[dd]:end_swh[dd]]
  DET1=apply(DET1, c(1,2), max)

  # grid.list<- list( x= lon, y=lat)
  # obj<- list( x=dlon , y=dlat, z= RF_rstr)
  # intrep_rf=interp.surface.grid( obj, grid.list)
  # RF=intrep_rf$z*(msk/msk)
  
  image(DET1[,(ncol(DET1):1)],x=dlon,y=rev(dlat),breaks = lvlsmn,main="", col = collmn,axes=F,xlab = "",ylab = "",xlim=c(76,95),ylim=c(5,25))
  box()
  text(paste(format(nv_time[stat[dd]],"%b_%d")),x=79,y=22.5,cex = 4.5,font = 2,col="blue")
  plot(s3,add=TRUE,border= rgb(0, 0, 0,alpha=0.2),lwd=.2)
  plot(s2,add=TRUE,border= rgb(0, 0, 0,alpha=0.2),cex.axis=.5,lwd=.2)
  lines(c(84.2844,94.2612),c(18.5,18.5),type="l", lty=2, lwd=2)
  lines(c(80.2862,92.835),c(13,13),type="l", lty=2, lwd=2)
  lines(c(92.835,94.7),c(13,16),type="l", lty=2, lwd=2)
  lines(c(90,90),c(18.5,21.9807),type="l", lty=2, lwd=2)
  lines(c(88.4,88.4),c(13,18.5),type="l", lty=2, lwd=2)
  
  lines(c(86,86),c(5,13),type="l", lty=2, lwd=2)
  lines(c(92.835,92.835),c(5,13),type="l", lty=2, lwd=2)
  
  text("NW BOB",x=88,y=20,cex = 2,font = 2,col="blue")
  text("NE BOB",x=92,y=20,cex = 2,font = 2,col="blue")
  text("WC BOB",x=85,y=16,cex = 2,font = 2,col="blue")
  text("EC BOB",x=92,y=16,cex = 2,font = 2,col="blue")
  text("SW BOB",x=82,y=10,cex = 2,font = 2,col="blue")
  text("SE BOB",x=89,y=10,cex = 2,font = 2,col="blue")
  
  }
dev.off()
###########################################

pdf(paste("NC2PDF/SWH_INDIA_",format(as.Date(DAYY), "%Y_%b_%d "),TIME,".pdf",sep = ""), width = 10 , height =7.5 ,onefile=T,bg="white")
RTG <- readPNG("../src_files/RTGC_English_Telugu_12x1.png")
grid.raster(RTG[,1:700,],x=0.1,y=.96, width=unit(.17, "npc"), interp=FALSE)
grid.raster(RTG[,1700:2400,],x=0.9,y=.96, width=unit(.17, "npc"), interp=FALSE)

RF <- readPNG("TEMP.png")
VASAR <- readPNG("../src_files/seastate_scale_BOB.png")


grid.raster(RF,x=0.5,y=.47, width=unit(0.9, "npc"), interp=FALSE)
grid.raster(VASAR,x=0.73,y=.18, width=unit(0.45, "npc"), interp=FALSE)

grid.text("Sea State Forecast", x=.5, .96, gp=gpar(fontsize=18,fontface = "bold") , check=TRUE)

grid.text("State of the Sea (Significant Wave Height)", x=.65, y=0.31, gp=gpar(fontsize=10,fontface = "bold") , check=TRUE)
grid.text(paste("Date: ",format(nv_time[stat_swh[1]],"%Y_%b_%d"),sep = ""), x=.5, .915, gp=gpar(fontsize=13,fontface = "bold") , check=TRUE)
grid.rect(width = .9, height = 0.001,x=.5,y=.89,  gp=gpar(col=NA, fill=rgb(0, 0,0,alpha=0.2)))

grid.rect(width = .9, height = 0.001,x=.5,y=.06,  gp=gpar(col=NA, fill=rgb(0, 0,0,alpha=0.2)))

grid.text(paste("*Maximum Significant Wave Height from (02:30,05:30,08:30,11:30,14:30,17:30,20:30,23:30 IST) time steps",sep = ""), x=.05, y=.04, gp=gpar(fontsize=8) , check=TRUE,just = "left")
grid.text(paste(" Source: Experimental predictions from ISRO-SDSC SHAR based on European Centre for Medium Range Weather forecast System (ECMWF)",sep = ""), x=.05, y=.02, gp=gpar(fontsize=8) , check=TRUE,just = "left")
grid.text(paste("*Forecast based on ",format(as.Date(DAYY), "%Y_%b_%d "),TIME,sep = ""),x=.05, y=.90, gp=gpar(fontsize=10,fontface = "bold") , check=TRUE,just = "left")


dev.off()

#file.remove("TEMP.png")

}
