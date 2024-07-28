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

p<-ggplot(out,aes(x=time,y=I))+geom_point(color="black")+xlim(0,50)+theme_classic()+
  xlab("Time")+ylab("Proportion adopted")+
  theme(axis.title = element_text(colour = "black",size=16),
        axis.text=element_text(color="black",size=14))+
  transition_states(
    time,
    transition_length = 1,
    state_length = 1
  )+ shadow_mark(color = "black") 

anim<-animate(p, renderer = gifski_renderer())
anim_save("./TalkFigs/sigmoidPoints.gif", anim)
####
#Do moving sigmoids



# Assign transmission and pathogen-induced death/recovery rates:
beta = 0.2 #rate of infection
gamma = 0.00 #rate of recovery
params <- list(beta = beta,
               gamma = gamma)
inits <- c(S0, I0, R0) #0 denotes that it is an initial condition
out <- ode(inits, times, SIR, params, method="rk")
out <- data.frame(out)
colnames(out) <- c("time", "S", "I", "R")
out1<-as.data.frame(out)

beta = 0.2 #rate of infection
gamma = 0.10 #rate of recovery
params <- list(beta = beta,
               gamma = gamma)
inits <- c(S0, I0, R0) #0 denotes that it is an initial condition
out <- ode(inits, times, SIR, params, method="rk")
out <- data.frame(out)
colnames(out) <- c("time", "S", "I", "R")
out2<-as.data.frame(out)

beta = 0.3 #rate of infection
gamma = 0.00 #rate of recovery
params <- list(beta = beta,
               gamma = gamma)
inits <- c(S0, I0, R0) #0 denotes that it is an initial condition
out <- ode(inits, times, SIR, params, method="rk")
out <- data.frame(out)
colnames(out) <- c("time", "S", "I", "R")
out3<-as.data.frame(out)

out1$Time<-rep(1,nrow(out1))
out2$Time<-rep(2,nrow(out2))
out3$Time<-rep(3,nrow(out3))
out<-rbind(out1,out2,out3)
p2<-ggplot(out,aes(x=time,y=I))+geom_line(color="black")+xlim(0,50)+theme_classic()+
  xlab("Time")+ylab("Predicted adoption")+
  theme(axis.title = element_text(colour = "black",size=16),
        axis.text=element_text(color="black",size=14))+
  transition_states(
    Time,
    transition_length = 1,
    state_length = 1
  )
anim<-animate(p2, renderer = gifski_renderer())

anim_save("./TalkFigs/sigmoidlines.gif", anim)

##3Now demo the code for the raster animations
library(raster)
library(sf)


r_df<-raster::as.data.frame(r, xy = TRUE) 
r_df<-na.omit(r_df)
names(r_df)[3]<-"layer"


Cents<-settlements%>%
  st_centroid()%>%
  mutate(time=sample(1:50,size=nrow(settlements),replace=TRUE))

#devtools::install_github("thomasp85/transformr")

cols <- c("Agriculture" = "#b30000", "Bare" = "#ffffe5", "Closed forest" = "#00441b",
          "Herbaceous vegetation"= "#fee391",
          "Herbaceous wetland"="#7bccc4","Open forest"="#006d2c","Shrubs"="#addd8e",
          "Urban"= "#54278f","Waterbody"="#3690c0")



library(gganimate)

p3<-ggplot(data = Cents)+
 # geom_tile(data = r_df , 
  #          aes(x = x, y = y,fill=as.character(layer)))+
  geom_sf(data=aoi,color="black",fill=alpha("#c51b8a",0.3),size=2)+
  geom_sf()+
#  scale_fill_manual(values=cols, name="Land cover")+
  theme_bw()+xlab("Longitude")+ylab("Latitude")+
  theme(axis.title = element_text(color="black",size=16),axis.text = element_text(color="black",size=12))+
  transition_states(
    time,
    transition_length = 1,
    state_length = 1
  )+ shadow_mark(color = "black")


anim<-animate(p3, renderer = gifski_renderer())

anim_save("./TalkFigs/MapPointsPlain.gif", anim)

p4<-ggplot(data = Cents)+
   geom_tile(data = r_df , 
            aes(x = x, y = y,fill=as.character(layer)))+
  geom_sf(data=aoi,color="black",fill=alpha("#c51b8a",0.3),size=2)+
  geom_sf()+
    scale_fill_manual(values=cols, name="Land cover")+
  theme_bw()+xlab("Longitude")+ylab("Latitude")+
  theme(axis.title = element_text(color="black",size=16),axis.text = element_text(color="black",size=12))+
  transition_states(
    time,
    transition_length = 1,
    state_length = 1
  )+ shadow_mark(color = "black")


anim<-animate(p4, renderer = gifski_renderer())

anim_save("./TalkFigs/MapPointsLC.gif", anim)
