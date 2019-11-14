METEOGRAM_DLY <- function(out.dir,ncfile, DAYY,TIME,NDAYS,nv_time){
  
  setwd(out.dir)
  dir.create("METEOGRAM", showWarnings = FALSE)


aws_loc=read.csv("../src_files/locations_meteogram.csv")



f1 <- nc_open(ncfile)


if(TIME=="12_UTC"){
 stat= 4
 end= 43
}else if(TIME=="00_UTC"){
 stat= 1
 end= 39 
}

dlat=ncvar_get(f1,"latitude")
dlon=ncvar_get(f1,"longitude")
t2 <- ncvar_get( f1,"2m_temperature")   +1      %>% round(digits = 2) 
rh <- ncvar_get( f1,"2m_relative_humidity")   %>% round(digits = 2)
pr <- ncvar_get( f1,"6hr_accumulated_precip") %>% round(digits = 2)
ws <- (ncvar_get( f1,"wind_magnitude")+1.5 )*3.6     %>% round(digits = 2)  #m/s to km/hr
wd <- ncvar_get( f1,"wind_direction")         %>% round(digits = 2)
mlp<- ncvar_get( f1,"mean_sea_level_pressure")%>% round(digits = 2)

rf=pr*0
for (i in 2:length(pr[1,1,])) {
  rf[,,i]=pr[,,i]-pr[,,(i-1)]
}

for (i in 1:NROW(aws_loc)){
 # print(i)
  df <- data.frame(matrix(ncol = 8, nrow = length(nv_time)))
  colnames(df) <- c("Time","t2","HI","rh","rf","msp","wspd","wdir")
  df$Time=nv_time
  
#  for (i in 1:1){  
  cat("\r",i,NROW(aws_loc),round(i/NROW(aws_loc),digits = 2)*100,"%")

  ilat= which.min(abs(aws_loc$LAT[i]-dlat))
  ilon= which.min(abs(aws_loc$LON[i]-dlon)) 
  
  df$t2=t2[ilon,ilat,]
  df$rf=rf[ilon,ilat,]
  df$rh=rh[ilon,ilat,] 
  df$wspd=ws[ilon,ilat,]
  df$wdir=wd[ilon,ilat,]
  df$msp=mlp[ilon,ilat,]
  df$HI= heat.index(t = t2[ilon,ilat,], rh = rh[ilon,ilat,], temperature.metric = "celsius", output.metric = "celsius", round = 2)

#df=df[4:43,]
  
df_wrt=df
df_wrt$msp=df_wrt$msp/100
colnames(df_wrt)=c("Time","2m_temperature(C)","Heat_index(C)","2m_relative_humidity(%)","Rainfall(mm)","mean_sea_level_pressure(mb)","Wind_speed(km/h)","wind_direction")
write.csv(file= paste("METEOGRAM/",aws_loc$VILLAGE[i],"_",round(aws_loc$LON[i],3),"E",round(aws_loc$LAT[i],3),"N",".csv",sep = ""),df_wrt)
  
df=df[stat:end,]


# par(mfrow=c(3,1))
# 
# bp=t(df[,c(1,5)])
# barplot(df$rf, main="Car Distribution", xlab="Number of Gears")
# axis(1,at=df$Time)

# 
# plot( df$Time,df$t2)


df$msp=df$msp/100

ddd=(max(df$msp)-min(df$msp))/(max(df$wspd)-min(df$wspd))
wsmlp=min(df$wspd)+(df$msp-min(df$msp))/ddd
mlpws=((wsmlp-min(df$wspd))*ddd)+min(df$msp)


p <- ggplot(df, aes(x = Time))+
  #   theme_linedraw()+
  theme_gray()+
     geom_line(aes(y = wspd),colour = "darkmagenta",alpha=1,size=1)+
     geom_bar(aes(x=Time, y=rf*0+min(df$wspd)), width = 8500,stat="identity")+
#     scale_y_continuous(limits = c(10, 40))+
     geom_line(aes(x=Time, y=wsmlp),colour = "chocolate1",alpha=1,size=.6)+
     scale_y_continuous(sec.axis = sec_axis(~.*ddd-min(df$wspd)*ddd+min(df$msp), name = "SLP(mb)"),limits = c(min(df$wspd), max(df$wspd)))+
#    scale_y_continuous(sec.axis = sec_axis(~.*dd-min(df$t2  )*dd+min(df$rh), name = " 2m RH(%)"),limits = c(min(df$t2), max(df$HI)))+
  
#     scale_y_continuous(pri.axis(~.*2000, name = "Rainfall(mm)"))+
#     scale_colour_manual(values = c("blue", "red"))+
     labs(y = "Wind speed(Km/hr)", x = "Date and time")+
     theme(axis.title.x = element_blank())+
  scale_x_datetime(date_breaks = "1 day", labels = date_format("%b %d"),position = "top")+
  theme(axis.title.y = element_text(color="darkmagenta", size=12, face="bold"),
        axis.title.y.right = element_text(color="chocolate1", size=12, face="bold"),
        axis.text.y = element_text(color="black", size=11, face="bold"),
        axis.text.x.top =  element_text(color="black", size=11, face="bold"))+
#        axis.text.x.  = element_text(color="black", size=11, face="bold"))+
#   scale_colour_manual(values = c("blue", "green"))+
#  geom_segment(df=d, mapping=aes(x=Time, y=20, xend=x+vx, yend=y+vy), arrow=arrow(), size=2, color="blue") + 
  geom_text(aes(x=Time, y=min(df$wspd)+1,angle=-(wdir)-90 ),size=8,color="darkmagenta", label="\u2192")+
  theme(plot.margin = unit(c(0,0,0.2,.0), "lines"))
#p

#  theme(legend.position = c(.9, .2))


#    theme(legend.position = c(0.5, 0.9))+
#    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())


rf1 <- ggplot(data=df, aes(x=Time, y=rf)) +
  #   theme_linedraw()+
  theme_gray()+
      geom_bar(stat="identity", width = 8500,fill = "royalblue1", colour = "white",alpha=1)+
#      geom_text(aes(label=round(rf,digits = 1)), position=position_dodge(width=0.9), vjust=-0.25)+
      xlab("Date&Time (IST) ") + ylab("Rainfall(mm)") +
#      scale_fill_brewer(palette="Greens") +
      geom_line(aes(x=Time, y=wspd*0),alpha=.2)+
 # scale_y_continuous(limits = c(10, 40))
 # scale_y_continuous(sec.axis = sec_axis(~.*50, name = "Wind speed"))+
      theme(plot.margin = unit(c(0,0,.0,.0), "lines"))+
      scale_x_datetime(date_breaks = "1 day", labels = date_format("%b %d"))+
      theme(axis.title.x = element_text(color="black", size=12, face="bold"),
        axis.title.y = element_text(color="royalblue1", size=12, face="bold"),
        axis.text.x = element_text(color="black", size=11, face="bold"),
        axis.text.y = element_text(color="black", size=11, face="bold"))
       
#rf

msp <- ggplot(data=df, aes(x=Time, y=msp)) +
  theme_linedraw()+
  geom_line()+ geom_bar(aes(x=Time, y=90000),stat="identity")+
  scale_y_continuous(limits = c(90000, 110000))+
  
  theme(axis.text.x = element_blank(),
        axis.title.x = element_blank())+
  theme(plot.margin = unit(c(0,0,.0,.0), "lines"))


dd=(100-min(df$rh))/(max(df$HI)-min(df$t2))
rht2=min(df$t2)+(df$rh-min(df$rh))/dd
t2rh2=((rht2-min(df$t2))*dd)+min(df$rh)

t2m <- ggplot(data=df, aes(x=Time, y=t2)) + #geom_line(aes(x=Time, y=HI))+
  #   theme_linedraw()+
  theme_gray()+
  geom_bar(aes(x=Time, y=min(df$t2)),width = 8500,stat="identity")+
  ylab("2m Temperature (°C)") +
  geom_line(colour = "red",alpha=1,size=1)+
  geom_line(aes(x=Time, y=HI),colour = "maroon1",alpha=1,size=.8,linetype = "dashed")+
  geom_line(aes(x=Time, y=rht2),colour = "forestgreen",alpha=1,size=1)+
  scale_y_continuous(sec.axis = sec_axis(~.*dd-min(df$t2)*dd+min(df$rh), name = " 2m RH(%)"),limits = c(min(df$t2), max(df$HI)))+
  theme(axis.text.x = element_blank(),axis.title.x = element_blank())+
#  scale_y_continuous(limits = c(min(df$t2), max(df$t2)))+
  scale_x_datetime(date_breaks = "1 day", labels = date_format("%b %d"))+
 # labs(y = "2m Temperature (°C)", x = "Date and time", colour = "")+
 # theme(legend.position = c(.9, .8))+
  theme(plot.margin = unit(c(0,0,.0,.0), "lines"))+
  theme(axis.title.y = element_text(color="red", size=12, face="bold"),
        axis.title.y.right =  element_text(color="forestgreen", size=12, face="bold"),
  axis.text.y = element_text(color="black", size=11, face="bold"))

#t2
full=ggarrange(p,t2m,rf1,
          # labels = c("A", "B","c","d"),
          ncol = 1, nrow = 3,heights=c(1,1,1),vjust=1,
          align = c( "v"))


png(paste("teat.png",sep = ""), width = 10, height = 6.5, units = 'in', res = 300)
print(full)
dev.off()



pdf(paste("METEOGRAM/",aws_loc$VILLAGE[i],"_Meteogram.pdf",sep = ""), width = 10 , height =7.9 ,onefile=T,bg="white")
IMG <- readPNG("teat.png")
grid.raster(IMG,x=0.51,y=.45, width=unit(.95, "npc"), interp=FALSE)
grid.text(paste("Forecast based on ",DAYY," ",TIME,sep = ""), x=.05, y=.04, gp=gpar(fontsize=10) , check=TRUE,just = "left")
grid.text(paste(" Source: Experimental predictions from ISRO-SDSC SHAR based on ECMWF, Prepared by: RTG-AWARE",sep = ""), x=.05, y=.02, gp=gpar(fontsize=10) , check=TRUE,just = "left")

grid.rect(width = .9, height = 0.001,x=.5,y=.85,  gp=gpar(col=NA, fill=rgb(0, 0,0,alpha=0.2)))
grid.rect(width = .9, height = 0.001,x=.5,y=.06,  gp=gpar(col=NA, fill=rgb(0, 0,0,alpha=0.2)))

grid.rect(width = .05, height = 0.007,x=.7,y=.88,  gp=gpar(col=NA, fill="royalblue1"))
grid.rect(width = .05, height = 0.007,x=.7,y=.92,  gp=gpar(col=NA, fill="darkmagenta"))
grid.rect(width = .05, height = 0.007,x=.7,y=.96,  gp=gpar(col=NA, fill="chocolate1"))

grid.text("Rainfall (mm)", x=.73, .88, gp=gpar(fontsize=12,fontface = "bold") , check=TRUE,just = "left")
grid.text("Wind Speed (km/h)", x=.73, .92, gp=gpar(fontsize=12,fontface = "bold") , check=TRUE,just = "left")
grid.text("mean sea level pressure (mb)", x=.73, .96, gp=gpar(fontsize=12,fontface = "bold") , check=TRUE,just = "left")

grid.rect(width = .05, height = 0.007,x=.47,y=.88,  gp=gpar(col=NA, fill="red"))
grid.rect(width = .05, height = 0.007,x=.47,y=.92,  gp=gpar(col=NA, fill="maroon1"))
grid.rect(width = .05, height = 0.007,x=.47,y=.96,  gp=gpar(col=NA, fill="forestgreen"))

grid.text("2m Temperature (°C)", x=.5, .88, gp=gpar(fontsize=12,fontface = "bold") , check=TRUE,just = "left")
grid.text("Heat Index (°C)", x=.5, .92, gp=gpar(fontsize=12,fontface = "bold") , check=TRUE,just = "left")
grid.text("2m RH (%)", x=.5, .96, gp=gpar(fontsize=12,fontface = "bold") , check=TRUE,just = "left")

grid.text("Heat Index (°C)", x=.02, .38,rot = 90, gp=gpar(fontsize=12,fontface = "bold",col="maroon1") , check=TRUE,just = "left")
grid.text(paste(aws_loc$VILLAGE[i]," (",round(aws_loc$LON[i],3),"E,",round(aws_loc$LAT[i],3),"N)",sep = ""), x=.025, .90, gp=gpar(fontsize=16,fontface = "bold") , check=TRUE,just = "left")
grid.text("5 day 3-hourly Forecast Meteogram", x=.025, .95, gp=gpar(fontsize=16,fontface = "bold") , check=TRUE,just = "left")


dev.off()
}

file.remove("teat.png")
}

