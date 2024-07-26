
Countries<-list.files("./Data/SettlementsPopulation/Settlements/")


for(i in 1:length(Countries)){
  country<-Countries[i]
  countryfile<-paste0("./Data/SettlementsPopulation/Settlements/",country,"/",
                      list.files(paste0("./Data/SettlementsPopulation/Settlements/",
                                        country),pattern=".shp"))
  settlements<-sf::read_sf(countryfile)
  
  if("is_fp" %in%names(settlements)==TRUE){
  Cents<-settlements%>%
    filter(is_fp==0)%>%filter(prob_fp<=0.25)%>% #remove known and suspected false positives
    st_centroid()}
  
  
  if("is_fp" %in%names(settlements)==FALSE){
    Cents<-settlements%>%
      #filter(is_fp==0)%>%filter(prob_fp<=0.25)%>% #remove known and suspected false positives
      st_centroid()}
  
  dir.create(paste0("./Data/SettlementsPopulation/SettlementsCentroid/",country))
  
  sf::st_write(Cents,paste0("./Data/SettlementsPopulation/SettlementsCentroid/",country,"/",country,".shp"))
  
}



