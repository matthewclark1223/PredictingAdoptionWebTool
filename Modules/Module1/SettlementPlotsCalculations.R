
#vector of countries the project area is in
COUNTRIES<-c("South Africa")

#Distance in km settlemets should be from resource type of interest
#e.g., grassland
BUFFER_KM<-10 #~10km

#e.g., 10% of pixels need to be resource type (e.g., grassland)
#within 10 km buffer
BUFFER_PERC<-0.1

#Sizes of settlements considered
BUILDING_COUNT<-c("101-250","251-1000","1001 and up")#"1-50","51-100","101-250","251-1000","1001 and up"

#Identify relevent settlements files
files<-list()
for(i in 1:length(COUNTRIES)){
  country<-COUNTRIES[i]
  countryfile<-paste0("./Data/SettlementsPopulation/SettlementsCentroid/",country,"/",
                      list.files(paste0("./Data/SettlementsPopulation/SettlementsCentroid/",
                                        country),pattern=".shp"))
  files<-c(files,countryfile)
  }

#load settlements files
settlements<-lapply(c(files), sf::read_sf)  #load the country settlement files into a list

sf_use_s2(FALSE) #don#t use spherical geometry
settlementsClip<- lapply(settlements,sf::st_intersection, aoi )  #crop the settlements to the AOI

settlements<-bind_rows(settlementsClip) # get rid of the list format. bind all rows


settlements<-settlements%>%filter(is_fp==0)%>%filter(prob_fp<=0.25)%>% #remove known and suspected false positives
  filter(bld_count%in% BUILDING_COUNT) #only include settlements with specified number of buildings

#Plot bounding box
raster::plot(r)

#plot the aoi
raster::plot(Inside,add=T, legend = TRUE)


#Plot settlements
plot(settlements, col=alpha("#dd3497",0.65), fill=alpha("black",0.65),lwd=0.9 , add=TRUE,pch=20,cex=3)

#Identify # of settlments inside project area
PotentialAdopters<-nrow(settlements)



##### If buffering by distance to resource#####
land_cover_class<-4 #example grassland


#Function to apply within the buffer around each settlement
check_for_grassland <- function(values,...) {
  values <- na.omit(values)
  return(length(which(values%in%land_cover_class))>=BUFFER_PERC*length(values))  #This could be modified to be more than one. That way spurious single pixels don't mess up the buffering
}


#Function to get the buffer distance specified above on the correct scale
# Latitude conversion (constant)
km_to_degrees_lat <- function(km) {
  return(km / 111)
}

#Apply function
# Convert dist km to degrees
km <- BUFFER_KM
degrees_lat <- km_to_degrees_lat(km)






buffer<-st_buffer(settlements, degrees_lat) # Circular buffer size, units depend on CRS (utm = meters)
#plot(buffer, col=alpha("#dd3497",0.65), fill=alpha("black",0.65),lwd=0.9 , add=TRUE,pch=20,cex=3)



# Use the modified extract function
extract <- terra::extract(x = r,              # Raster layer
                           y = buffer,    # SpatialPoints* object   
                           na.rm = TRUE,       # Remove NAs
                           fun = check_for_grassland,  # Function to check for 'grassland'
                           df = TRUE,          # Return a dataframe?
                           small = TRUE   )     # Include small buffers?

#bind the T/F statement to the data and filter by T
#If within the buffer from the resource type
settlementsBuf<-cbind(settlements,extract)%>%filter(desc==TRUE)

#Plot the settlements that are counted
raster::plot(r)
raster::plot(Inside,add=T, legend = TRUE)
plot(settlementsBuf, col=alpha("#dd3497",0.65), fill=alpha("black",0.65),lwd=0.9 , add=TRUE,pch=20,cex=3)


#Sum the potential adopters (settlements nearby resource of interest)
PotentialAdopters<-nrow(settlementsBuf)






