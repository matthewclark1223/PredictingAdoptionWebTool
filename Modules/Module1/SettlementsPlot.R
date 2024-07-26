COUNTRIES<-c("South Africa")
BUFFER_KM<-10
BUILDING_COUNT<-c("101-250","251-1000","1001 and up")#"1-50","51-100","101-250","251-1000","1001 and up"

files<-list()
for(i in 1:length(COUNTRIES)){
  country<-COUNTRIES[i]
  countryfile<-paste0("./Data/SettlementsPopulation/Settlements/",country,"/",
                      list.files(paste0("./Data/SettlementsPopulation/Settlements/",
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
plot(aoi, col=alpha("#c51b8a",0.3), border="black", lwd=0.5, add=TRUE)



plot(settlements, col=alpha("black",0.99), border=alpha("white",0.001),lwd=0.1 , add=TRUE)


#Settlement centroids
Cents<-settlements%>%
  st_centroid()

plot(aoi, col=alpha("yellow",0.35), border="black", lwd=2.75, add=TRUE)
plot(Cents, col=alpha("black",0.6), fill=alpha("black",0.6),lwd=0.9 , add=TRUE,pch=20,cex=3)

#now do buffering by resource type