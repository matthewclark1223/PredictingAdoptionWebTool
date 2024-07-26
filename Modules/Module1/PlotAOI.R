library(tidyverse)
library(sf)

#aoi<-read_sf("./Data/ExampleBoundary/CoastalForestsEastAfrica.shp")
#aoi<-read_sf("~/Pemba_Project/PembaShapeFile.shp")
aoi<-read_sf("./Data/ExampleBoundary/Kruger2Canyons/K2C.shp")
#aoi<-read_sf("./Data/ExampleBoundary/GADM_SouthAfrica/gadm41_ZAF_3.shp")
#aoi<-read_sf("./Data/ExampleBoundary/gadm41_MOZ_shp/gadm41_MOZ_0.shp")


st_crs(aoi)<-"+proj=longlat +datum=WGS84 +no_defs" 

#ggplot(aoi)+geom_sf()

#Make custom function to check if a given raster and polygon bounding box overlap

check_overlap<-function(raster,poly){
  rastbbox<-st_bbox(raster)
  polybbox<-st_bbox(aoi)
  
  if(polybbox[[1]] > rastbbox[[3]] | polybbox[[3]] < rastbbox[[1]] |
      polybbox[[2]] > rastbbox[[4]] | polybbox[[4]] < rastbbox[[2]]){
    return(FALSE)}
  
  else{return(TRUE)}
  
}


#list out all of the tifs for subsaharan africa
tifs<-list.files("./Data/LandCover/ClassifiedRasts",pattern=".tif" ) 
#get the full file paths
tifs<-paste0("./Data/LandCover/ClassifiedRasts/",tifs)

#put them into a df for easy looping
tifs<-data.frame(grid=tifs,load=rep(NA,length(tifs)))

#loop through each 
for(i in 1:nrow(tifs)){
  tif1<-raster::raster(tifs[i,]$grid)
  tifs[i,]$load<-check_overlap(tif1,aoi) #check overlap with the polygon(s)
}
tifs<-tifs%>%filter(load==TRUE) #filter out ones without overlap
rasts<-lapply(c(tifs$grid), raster::raster)  #load the overlapping ones into a list


rastsClip<- lapply(rasts,terra::crop, aoi)  #crop the rasters to the bounding box of the polygon

#maybe do this one after the mosaic#############
#rastsClip<- lapply(rasts,raster::mask, aoi)    #OPTIONAL mask out the raster data outside the polygon
#######################################

ic<-terra::sprc(lapply(rastsClip, terra::rast)) #turn each raster::raster into a terra::raster then combine
r <- terra::mosaic(ic)

levels(r) = data.frame(ID=c(1:11), desc=c(  #set the levels
  "Closed forest","Open forest",
  "Shrubs","Herbaceous vegetation",
  "Herbaceous wetland", "Bare","Snow/Ice","Agriculture",
  "Urban","Waterbody", "Sea"
))


terra::coltab(r) <- data.frame(ID=c(1:11), 
                               cols=c(
  
  alpha("#00441b",0.3), #1 = closed forest
  alpha("#006d2c",0.3), #2 = open forest
  alpha("#addd8e",0.3),#3 = shrubs
  alpha( "#fee391",0.3),#4 = herbaceous veg
  alpha("#7bccc4",0.3),#5 = herbaceous wetland
  alpha( "#ffffe5",0.3),#6 = bare
  alpha( "#deebf7",0.3),#7 = snow/ice
  alpha( "#b30000",0.3),#8 = agriculture
  alpha( "#54278f",0.3),#9 = urban
  alpha( "#3690c0",0.3),#10 = waterbody
  alpha("#045a8d",0.3)#11 = sea
  
)) #set the colors

terra::plot(r,legend=FALSE,mar=c(2.5,2,2,9),xlab="Longitude",ylab="Latitude")
Inside <- raster::mask(r, aoi)


terra::coltab(Inside) <- data.frame(ID=c(1:11), 
                               cols=c(
                                 
 "#00441b", #1 = closed forest
 "#006d2c", #2 = open forest
 "#addd8e",#3 = shrubs
 "#fee391",#4 = herbaceous veg
 "#7bccc4",#5 = herbaceous wetland
 "#ffffe5",#6 = bare
 "#deebf7",#7 = snow/ice
 "#b30000",#8 = agriculture
 "#54278f",#9 = urban
 "#3690c0",#10 = waterbody
 "#045a8d"#11 = sea
 )) #set the colors


raster::plot(Inside,add=T, legend = TRUE)



#plot(aoi, col=alpha("grey",0.001), border="#c51b8a", lwd=1, add=TRUE)
#plot(aoi, col=alpha("#c51b8a",0.15), border="black", lwd=2.75, add=TRUE)

#plot(aoi, col=alpha("yellow",0.35), border="black", lwd=2.75, add=TRUE)

