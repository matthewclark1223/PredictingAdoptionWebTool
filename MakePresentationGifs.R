library(deSolve)
library(tidyverse)
library(gganimate)
I0 = 0.02    # initial fraction infected
S0 = 1 - I0 # initial fraction susceptible
R0 = 0

# Assign transmission and pathogen-induced death/recovery rates:
beta = 0.2 #rate of infection
gamma = 0.00 #rate of recovery

# We will use the package deSolve to integrate, which requires certain data structures.
# Store parameters and initial values
# Parameters must be stored in a named list.
params <- list(beta = beta,
               gamma = gamma)

# Initial conditions are stored in a vector
inits <- c(S0, I0, R0) #0 denotes that it is an initial condition

# Create a time series over which to integrate.
# Here we have an epidemic that is observed over t_max number of days (or weeks or etc).
t_min = 0
t_max = 50
times = t_min:t_max

# We must create a function for the system of ODEs.
# See the 'ode' function documentation for further insights.
SIR <- function(t, y, params) {
  with(as.list(c(params, y)), {
    
    dS = - beta * y[1] * y[2] #y[1] susceptible and y[2] is infected
    
    dI = beta * y[1] * y[2] - gamma * y[2] #susceptibles and recovered
    
    dR = gamma * y[2] #rate of recovery and how many infected there are
    
    res <- c(dS,dI,dR)
    list(res)
  })
}

# Run the integration:
out <- ode(inits, times, SIR, params, method="rk")
#
# Store the output in a data frame:
out <- data.frame(out)
colnames(out) <- c("time", "S", "I", "R")

out<-as.data.frame(out)

ggplot(out,aes(x=time,y=I))+geom_point(color="black")+xlim(0,50)+theme_classic()+
  xlab("Time")+ylab("Proportion adopted")+
  theme(axis.title = element_text(colour = "black",size=16),
        axis.text=element_text(color="black",size=14))+
  transition_states(
    time,
    transition_length = 1,
    state_length = 1
  )+ shadow_mark(color = "red") 


##3Now demo the code for the raster animations
library(raster)
library(sf)
Pem2018LC<-raster::raster("~/Pemba_Project/HCRI_Grant/ProjectFiles/LandCoverLayers/Rd/pemmyLC2018rd_Unprocessed.tif")
Pemba<-sf::read_sf("~/Pemba_Project/PembaShapeFile.shp")

Pem2018LC_df<-raster::as.data.frame(Pem2018LC, xy = TRUE) 
Pem2018LC_df<-na.omit(Pem2018LC_df)
names(Pem2018LC_df)[3]<-"layer"
Pem2018LC_df<-Pem2018LC_df%>%filter(layer!=7)%>%filter(layer!=0)


ShehiaCents<-Pemba%>%
  st_centroid()%>%
  mutate(time=sample(1:50,size=nrow(Pemba),replace=TRUE))

#devtools::install_github("thomasp85/transformr")

ggplot(data = Pemba)+
  geom_tile(data = Pem2018LC_df , 
            aes(x = x, y = y,fill=as.character(layer)))+
  geom_sf(data=ShehiaCents)+
  theme_bw()+
  transition_states(
    time,
    transition_length = 1,
    state_length = 1
  )+ shadow_mark(color = "red")







