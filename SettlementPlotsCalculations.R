
COUNTRIES<-c("Tanzania","Kenya")
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
settlementsClip<- lapply(settlements,sf::st_crop, aoi)  #crop the settlements to the AOI

settlements<-bind_rows(settlementsClip) # get rid of the list format. bind all rows


settlements<-settlements%>%filter(is_fp==0)%>%filter(prob_fp<=0.25)%>% #remove known and suspected false positives
  filter(bld_count%in% BUILDING_COUNT) #only include settlements with specified number of buildings

raster::plot(r)
plot(settlements, col=alpha("pink",0.99), border=alpha("pink",0.001),lwd=0.1 , add=TRUE)


#now do buffering by resource type




