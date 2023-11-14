tifs<-list.files("./Data/LandCover",pattern=".tif") #https://lcviewer.vito.be/download
tifs<-paste0("./Data/LandCover/",tifs) #paste full file paths
tifs<-data.frame(grid=tifs) #add each path to a data frame for us to loop through easily
rasts<-lapply(c(tifs$grid), raster::raster)  #load each as a raster

#create a df for reclassifying the classes....weird syntax for the raster package
reclass_df <- c(0, 0, 0,   # NA
                111, 116, 1,  #"ClosedForest"
                121, 126, 2,    #OpenForest
                19,20, 3,    #  Shrubs
                29,30,4,   #HerbaceousVeg
                89,90, 5,    #HerbaceousWetland
                59,60, 6,   #Bare
                69,70, 7,   #Snow/Ice
                99,100, 6,   #Bare
                39,40, 8,   #Agriculture
                49,50, 9,   #Urban
                79,80,10,   #WaterBody
                199,200,11)   #Sea

#the package needs to read this as a matrix
reclass_m <- matrix(reclass_df,
                    ncol = 3,
                    byrow = TRUE)


for(r in 1:length(rasts)){
  
  test<-rasts[[r]]  #seperate the focal raster
  test_classified <- raster::reclassify(test,
                                        reclass_m)  #reclasify
  
  test = terra::as.factor(test_classified) #Make it a factor
  
  test<-terra::rast(test) #transform it to a terra object
  
  r[r == 0] <- NA # this 0 value for NA was causing some issues in testing. just make actual NA
  
  levels(test) = data.frame(ID=c(1:11), desc=c(  #set the levels
    "closedForest","OpenForest",
    "Shrubs","HerbaceousVeg",
    "HerbaceousWetland", "Bare","Snow/Ice","Agriculture",
    "Urban","Waterbody", "Sea"
  ))
  
  #rewrite the raster
  terra::writeRaster(test,paste0("./Data/LandCover/ClassifiedRasts/reclassified",r,".tif"),overwrite=TRUE  )
  
}




#




















