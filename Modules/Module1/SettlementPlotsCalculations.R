
COUNTRIES<-c("South Africa")
BUFFER_KM<-10 #~10km in SA
BUFFER_PERC<-0.1 #e.g., 10% of pixels need to be grassland within 10 km buffer
BUILDING_COUNT<-c("101-250","251-1000","1001 and up")#"1-50","51-100","101-250","251-1000","1001 and up"

files<-list()
for(i in 1:length(COUNTRIES)){
  country<-COUNTRIES[i]
  countryfile<-paste0("./Data/SettlementsPopulation/SettlementsCentroid/",country,"/",
                      list.files(paste0("./Data/SettlementsPopulation/SettlementsCentroid/",
                                        country),pattern=".shp"))
  files<-c(files,countryfile)
  }


settlements<-lapply(c(files), sf::read_sf)  #load the country settlement files into a list

sf_use_s2(FALSE) #don#t use spherical geometry
settlementsClip<- lapply(settlements,sf::st_intersection, aoi )  #crop the settlements to the AOI

settlements<-bind_rows(settlementsClip) # get rid of the list format. bind all rows


settlements<-settlements%>%filter(is_fp==0)%>%filter(prob_fp<=0.25)%>% #remove known and suspected false positives
  filter(bld_count%in% BUILDING_COUNT) #only include settlements with specified number of buildings

raster::plot(r)

#plot the aoi
raster::plot(Inside,add=T, legend = TRUE)



plot(settlements, col=alpha("#dd3497",0.65), fill=alpha("black",0.65),lwd=0.9 , add=TRUE,pch=20,cex=3)

PotentialAdopters<-nrow(settlements)



##### If buffering by distance to resource#####
land_cover_class<-4 #example grassland



check_for_grassland <- function(values,...) {
  values <- na.omit(values)
  return(length(which(values%in%land_cover_class))>=BUFFER_PERC*length(values))  #This could be modified to be more than one. That way spurious single pixels don't mess up the buffering
}



# Latitude conversion (constant)
km_to_degrees_lat <- function(km) {
  return(km / 111)
}


# Convert 10 km to degrees
km <- BUFFER_KM
degrees_lat <- km_to_degrees_lat(km)




# Use the modified extract function

buffer<-st_buffer(settlements, degrees_lat) # Circular buffer size, units depend on CRS (utm = meters)
#plot(buffer, col=alpha("#dd3497",0.65), fill=alpha("black",0.65),lwd=0.9 , add=TRUE,pch=20,cex=3)




extract <- terra::extract(x = r,              # Raster layer
                           y = buffer,    # SpatialPoints* object   
                           na.rm = TRUE,       # Remove NAs
                           fun = check_for_grassland,  # Function to check for 'grassland'
                           df = TRUE,          # Return a dataframe?
                           small = TRUE   )     # Include small buffers?

settlementsBuf<-cbind(settlements,extract)%>%filter(desc==TRUE)

raster::plot(r)
raster::plot(Inside,add=T, legend = TRUE)

plot(settlementsBuf, col=alpha("#dd3497",0.65), fill=alpha("black",0.65),lwd=0.9 , add=TRUE,pch=20,cex=3)



PotentialAdopters<-nrow(settlementsBuf)






