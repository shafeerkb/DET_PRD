NC_RIVERBASIN <- function(out.dir,ncfile, DAYY,TIME,NDAYS,nv_time,stat,end,stat_rf,end_rf){

  setwd(out.dir)
  dir.create("RIVER_BASIN", showWarnings = FALSE)


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

pr <- ncvar_get( f1,"6hr_accumulated_precip") %>% round(digits = 2)


nc <- nc_open("../src_files/river1.nc")
lat <- ncvar_get( nc, "lat" )
lon <- ncvar_get( nc, "lon" )
msk <-  ncvar_get(nc,"msk")

kr <- readOGR("../src_files/shp_basin", "KSubBasins_proj")
gd <- readOGR("../src_files/shp_basin", "GSubBasins_proj")

kr1 <- readOGR("../src_files/shp_basin", "Krishna_Basin")
gd1 <- readOGR("../src_files/shp_basin", "Godavari_Basin")

s2 <- readOGR("../src_files/AP_Dis", "AP_DISTRICT_BND")
s3 <- readOGR("../src_files/shp_basin", "IND_adm1")


for (i in 1:NDAYS) {
  print(paste("DAY",i,"t2,rh,ws=",nv_time[stat[i]],"to",nv_time[end[i]], "rain=",nv_time[stat_rf[i]],"to",nv_time[end_rf[i]]))
}

###########################################

collmn=c("#FFFFFF","#C3FDCA","#01FF04","#048500","#FDC0CB","#FC0300","#610301")
lvlsmn=c(0,.1,2.5,15.6,64.5,115.6,204.5,800)

png(file="TEMP.png",  width = 1000*2,height = 675*2)
par(mfrow=c(3,4))
par(mar=c(.5,.4,.3,.3))  
#for (dd in 1:NDAYS) {
  for (dd in 1:9) {
print(dd)

  RF_rstr=pr[,,end_rf[dd]]-pr[,,stat_rf[dd]]
  RF_rstr[RF_rstr<0]=0
  
  grid.list<- list( x= lon, y=lat)
  obj<- list( x=dlon , y=dlat, z= RF_rstr)
  intrep_rf=interp.surface.grid( obj, grid.list)
  RF=intrep_rf$z*(msk/msk)
  
  image(RF[,213:1],x=lon,y=rev(lat),breaks = lvlsmn,main="", col = collmn,axes=F,xlab = "",ylab = "")
  box()
  text(paste(format(nv_time[stat[dd]],"%b_%d")),x=75.5,y=22,cex = 4.5,font = 2)
  plot(s3,add=TRUE,border= rgb(0, 0, 0,alpha=0.2),lwd=.5)
  plot(s2,add=TRUE,border= rgb(0, 0, 0,alpha=0.5),cex.axis=.5,lwd=.2)
  
  
  plot(kr,add=TRUE,lwd=1.5,col= rgb(1, 0, 0,alpha=0.0),border=rgb(1, .6, 0,alpha=0.8))
  plot(gd,add=TRUE,lwd=1.5,col= rgb(0, 0, 1,alpha=0.0),border=rgb(0, 0, 1,alpha=0.8))
 
  plot(kr1,add=TRUE,lwd=3.5,col= rgb(1, 0, 0,alpha=0.0),border=rgb(1, .6, 0,alpha=1))
  plot(gd1,add=TRUE,lwd=3.5,col= rgb(0, 0, 1,alpha=0.0),border=rgb(0, 0, 1,alpha=1)) 
  
  text(74.137,17.068,"1.",cex = 2,font = 2,col=rgb(1, 0, 0,alpha=0.7))
  text(75.656,16.539,"2.",cex = 2,font = 2,col=rgb(1, 0, 0,alpha=0.7))
  text(74.515,16.078,"3.",cex = 2,font = 2,col=rgb(1, 0, 0,alpha=0.7))
  text(75.354,15.682,"4.",cex = 2,font = 2,col=rgb(1, 0, 0,alpha=0.7))
  text(74.76,18.32,"5.",cex = 2,font = 2,col=rgb(1, 0, 0,alpha=0.7))
  text(76.68,17.17,"6.",cex = 2,font = 2,col=rgb(1, 0, 0,alpha=0.7))
  text(78.38,16.44,"7.",cex = 2,font = 2,col=rgb(1, 0, 0,alpha=0.7))
  text(75.68,14.55,"8.",cex = 2,font = 2,col=rgb(1, 0, 0,alpha=0.7))
  text(76.63,14.28,"9.",cex = 2,font = 2,col=rgb(1, 0, 0,alpha=0.7))          
  text(78.89,17.44,"10.",cex = 2,font = 2,col=rgb(1, 0, 0,alpha=0.7))
  text(79.740,17.257,"11",cex = 1.5,font = 2,col=rgb(1, 0, 0,alpha=0.7))
  text(80.15,17.436,"12.",cex = 2,font = 2,col=rgb(1, 0, 0,alpha=0.7))
  
  text(75.54,19.21,"1.",cex = 2,font = 2,col=rgb(0, 0, 1,alpha=0.7))
  text(74.43,19.39,"2.",cex = 2,font = 2,col=rgb(0, 0, 1,alpha=0.7))
  text(76.09,19.83,"3.",cex = 2,font = 2,col=rgb(0, 0, 1,alpha=0.7))
  text(77.37,18.19,"4.",cex = 2,font = 2,col=rgb(0, 0, 1,alpha=0.7))
  text(78.51,19.03,"5.",cex = 2,font = 2,col=rgb(0, 0, 1,alpha=0.7))
  text(79.19,18.29,"6.",cex = 2,font = 2,col=rgb(0, 0, 1,alpha=0.7))
  text(77.83,19.91,"7.",cex = 2,font = 2,col=rgb(0, 0, 1,alpha=0.7))
  text(78.66,20.7,"8.",cex = 2,font = 2,col=rgb(0, 0, 1,alpha=0.7))
  text(79.91,20.54,"9.",cex = 2,font = 2,col=rgb(0, 0, 1,alpha=0.7))
  text(80.64,18.04,"10.",cex = 2,font = 2,col=rgb(0, 0, 1,alpha=0.7))
  text(81.08,19.4,"11.",cex = 2,font = 2,col=rgb(0, 0, 1,alpha=0.7))
  text(82.02,18.38,"12.",cex = 2,font = 2,col=rgb(0, 0, 1,alpha=0.7))
  
}
dev.off()
###########################################







pdf(paste("RIVER_BASIN/RIVER_BASIN_",format(as.Date(DAYY), "%Y_%b_%d "),TIME,".pdf",sep = ""), width = 10 , height =7.5 ,onefile=T,bg="white")
RTG <- readPNG("../src_files/RTGC_English_Telugu_12x1.png")
grid.raster(RTG[,1:700,],x=0.1,y=.96, width=unit(.17, "npc"), interp=FALSE)
grid.raster(RTG[,1700:2400,],x=0.9,y=.96, width=unit(.17, "npc"), interp=FALSE)

RF <- readPNG("TEMP.png")
VASAR <- readPNG("../src_files/vasarlab_scale_rf.png")
subbasib <- readPNG("../src_files/subbasin_name.png")

grid.raster(RF,x=0.5,y=.47, width=unit(0.9, "npc"), interp=FALSE)
grid.raster(VASAR,x=0.8,y=.18, width=unit(0.32, "npc"), interp=FALSE)
grid.raster(subbasib,x=0.45,y=.19, width=unit(0.32, "npc"), interp=FALSE)

grid.text("Krishna & Godavari basin Rainfall forecast ", x=.5, .96, gp=gpar(fontsize=18,fontface = "bold") , check=TRUE)
grid.text(paste("Date: ",format(nv_time[stat[1]],"%Y_%b_%d"),sep = ""), x=.5, .915, gp=gpar(fontsize=13,fontface = "bold") , check=TRUE)
grid.rect(width = .9, height = 0.001,x=.5,y=.89,  gp=gpar(col=NA, fill=rgb(0, 0,0,alpha=0.2)))

grid.rect(width = .9, height = 0.001,x=.5,y=.06,  gp=gpar(col=NA, fill=rgb(0, 0,0,alpha=0.2)))

grid.text(paste("*ISRO SDSC-SHAR Rainfall forecast: Accumulated rainfall from 05:30 AM to 05:30 AM (next day) IST",sep = ""), x=.05, y=.04, gp=gpar(fontsize=8) , check=TRUE,just = "left")
grid.text(paste(" Source: Experimental predictions from ISRO-SDSC SHAR based on European Centre for Medium Range Weather forecast System (ECMWF)",sep = ""), x=.05, y=.02, gp=gpar(fontsize=8) , check=TRUE,just = "left")
grid.text(paste("*Rainfall forecast based on ",format(as.Date(DAYY), "%Y_%b_%d "),TIME,sep = ""),x=.05, y=.90, gp=gpar(fontsize=10,fontface = "bold") , check=TRUE,just = "left")


dev.off()

file.remove("TEMP.png")
}
