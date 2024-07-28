BUFFER_KM<-10 #~10km
#Load the landscan gridded population data from 2022
Pop<-terra::rast("./Data/SettlementsPopulation/landscan-global-2022.tif")

#Crop the bounding box to the project area
Pop<-terra::crop(Pop,aoi)

#Make all values outside the project area NA
Pop<-terra::mask(Pop,aoi)

#simple plot of population data
#terra::plot(Pop,colNA="#252525")

#Get the total number of individuals living in the project area
PotentialAdopters<-sum(Pop[],na.rm=T)


##### If buffering by distance to resource#####
# Define the land cover class of interest (for example, grassland)
land_cover_class <- 4 #grassland

# Reclassify the land cover raster to binary (1 for the class of interest, NA for others)
land_cover_binary <-r #new raster
land_cover_binary[]<-ifelse(r[]==land_cover_class,1,NA) #reclassify

#This is removing pixels of land cover that aren't connected at 100< others.
#We're essentially just reducing noise here
land_cover_binary <- terra::sieve(land_cover_binary, 100,directions=8) 

#Reclassify again. The sieving creates 0's instead of NAs
land_cover_binary[]<-ifelse(land_cover_binary[]==1,1,NA)

#Plot results
#terra::plot(land_cover_binary)
#patch<-terra::patches(land_cover_binary)


#Turn the land cover into polygon data fo rbetter buffering
patchpoly<-terra::as.polygons(land_cover_binary)

#Make it an sf vector object
patchpoly<-sf::st_as_sf(patchpoly)

# Latitude conversion (constant) function
km_to_degrees_lat <- function(km) {
  return(km / 111)
}
# Convert assigned buffer distance to degrees
degrees_lat <- km_to_degrees_lat(BUFFER_KM)

#Make buffer with calculated distance in degrees
buf<-st_buffer(patchpoly, degrees_lat) 
st_crs(buf)<-"+proj=longlat +datum=WGS84 +no_defs" 

#plot buffer
#p<-ggplot(patchpoly)+geom_sf(fill="green")+
 # geom_sf(data=buf,fill="red",alpha=0.4)

#Extract the population counta inside the buffer
extract <- terra::extract(x = Pop,              # Raster layer
                          y = buf,    # SpatialPoints* object   
                          na.rm = TRUE,       # Remove NAs
                          fun = sum   ) 
names(extract)[2]<-"pop" #change the column name

patchpoly$pop<-as.vector(extract$pop)#Make sure it's a vector not a df



PotentialAdopters<-sum(patchpoly$pop,na.rm=T)#Get the sum



