
BUFFER_KM<-10 #~10km in SA
BUFFER_PERC<-0.1 #e.g., 10% of pixels need to be grassland within 10 km buffer
BUILDING_COUNT<-c("101-250","251-1000","1001 and up")#"1-50","51-100","101-250","251-1000","1001 and up"

Pop<-terra::rast("./Data/SettlementsPopulation/landscan-global-2022.tif")

Pop<-terra::crop(Pop,aoi)
Pop<-terra::mask(Pop,aoi)

terra::plot(Pop,colNA="#252525")

PotentialAdopters<-sum(Pop[],na.rm=T)


##### If buffering by distance to resource#####
# Define the land cover class of interest (for example, grassland)
land_cover_class <- 4 #grassland

# Reclassify the land cover raster to binary (1 for the class of interest, NA for others)
land_cover_binary <-r
land_cover_binary[]<-ifelse(r[]==land_cover_class,1,NA)


land_cover_binary <- terra::sieve(land_cover_binary, 100,directions=8) #This is removing pixels of land cover that aren't connected at 100< others.


land_cover_binary[]<-ifelse(land_cover_binary[]==1,1,NA)
terra::plot(land_cover_binary)


patch<-terra::patches(land_cover_binary)

buf<-terra::buffer(patch, 0.1)


patchpoly<-terra::as.polygons(patch)
terra::plot(patch)
terra::plot(patchpoly)

patchpoly<-sf::st_as_sf(patchpoly)
sf::st_as_sf(st_cast(patchpoly, "POLYGON"))

ggplot(patchpoly)+geom_sf(fill="green")

extract <- terra::extract(x = Pop,              # Raster layer
                          y = patchpoly,    # SpatialPoints* object   
                          na.rm = TRUE,       # Remove NAs
                          fun = sum   ) 
names(extract)[2]<-"pop"

patchpoly$pop<-as.vector(extract$pop)



PotentialAdopters<-sum(patchpoly$pop,na.rm=T)



ggplot(patchpoly)+
  geom_sf(aes(fill=pop))

