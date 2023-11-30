tifs<-list.files("./Data/LandCover/ClassifiedRasts",pattern=".tif" ) 
#get the full file paths
tifs<-paste0("./Data/LandCover/ClassifiedRasts/",tifs)

#put them into a df for easy looping
tifs<-data.frame(grid=tifs,load=rep(NA,length(tifs)))



rasts<-lapply(c(tifs$grid), raster::raster)  #load the overlapping ones into a list


#######################################

ic<-terra::sprc(lapply(rasts, terra::rast)) #turn each raster::raster into a terra::raster then combine
r <- terra::mosaic(ic)

levels(r) = data.frame(ID=c(1:11), desc=c(  #set the levels
  "Closed forest","Open forest",
  "Shrubs","Herbaceous vegetation",
  "Herbaceous wetland", "Bare","Snow/Ice","Agriculture",
  "Urban","Waterbody", "Sea"
))


terra::coltab(r) <- data.frame(ID=c(1:11), 
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

raster::plot(r)
