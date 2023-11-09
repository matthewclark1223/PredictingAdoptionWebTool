library(tidyverse)
library(sf)

aoi<-read_sf("./Data/ExampleBoundary/CoastalForestsEastAfrica.shp")
st_crs(aoi)<-"+proj=longlat +datum=WGS84 +no_defs" 

ggplot(aoi)+geom_sf()


check_overlap<-function(raster,poly){
  rastbbox<-st_bbox(raster)
  polybbox<-st_bbox(aoi)
  
  if(polybbox[[1]] > rastbbox[[3]] | polybbox[[3]] < rastbbox[[1]] |
      polybbox[[2]] > rastbbox[[4]] | polybbox[[4]] < rastbbox[[2]]){
    return(FALSE)}
  
  else{return(TRUE)}
  
}

tifs<-list.files("./Data/LandCover",pattern=".tif")
tifs<-paste0("./Data/LandCover/",tifs)


tifs<-data.frame(grid=tifs,load=rep(NA,length(tifs)))

for(i in 1:nrow(tifs)){
  tif1<-raster::raster(tifs[i,]$grid)
  tifs[i,]$load<-check_overlap(tif1,aoi)
}
tifs<-tifs%>%filter(load==TRUE)
rasts<-lapply(c(tifs$grid), raster::raster)

rastsClip<- lapply(rasts,terra::crop, aoi)

#maybe do this one after the mosaic
rastsClip<- lapply(rasts,raster::mask, aoi)
##

ic<-sprc(lapply(rastsClip, rast))
r <- mosaic(ic)
plot(r)


  
  
