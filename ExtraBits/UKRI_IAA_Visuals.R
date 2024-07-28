

library(tidyverse)
library(deSolve)
I0 = 0.02    # initial fraction adopted, seed
S0 = 1 - I0 # initial fraction available to adopt


# Assign rates of spread, drop-out, and independent uptake:
beta = 0.3 #rate of spread 0.4
gamma = 0.2 #rate of drop-out 0.15
alpha = 0.000 #independent rate of adoption 0.002

# We will use the package deSolve to integrate, which requires certain data structures.
# Store parameters and initial values
# Parameters must be stored in a named list.
params <- list(beta = beta,
               gamma = gamma,
               alpha=alpha)

# Initial conditions are stored in a vector
inits <- c(S0, I0) #0 denotes that it is an initial condition

# Create a time series over which to integrate.

t_min = 0
t_max = 100
times = t_min:t_max

# We must create a function for the system of ODEs.
# See the 'ode' function documentation for further insights.
SISa <- function(t, y, params) {
  with(as.list(c(params, y)), {
    
    dS = - beta * y[1] * y[2] +gamma * y[2] - alpha*y[1]#y[1] non-adopters (S) and y[2] is adopters (I)
    
    dI = beta * y[1] * y[2] - gamma * y[2] + alpha*y[1]
    
    res <- c(dS,dI)
    list(res)
  })
}

# Run the integration:
out <- ode(inits, times, SISa, params, method="rk")
#
# Store the output in a data frame:
out <- data.frame(out)
colnames(out) <- c("time", "S", "I")


p<-ggplot(out,aes(x=time,y=I))+xlim(0,t_max)+theme_classic()+
  xlab("Time")+ylab("Adopters")+
  scale_x_continuous(breaks=c(0,30,100),labels=c("Project\nstart","2024","Project\nend"))+
  scale_y_continuous(breaks=c(0.0,0.1,0.2,0.3,0.4),labels=c(0,30,60,90,120))+
  #annotate("rect", xmin = 0, xmax = 33, ymin = -0, ymax = 0.4,
     #      alpha = .13,fill = "#4daf4a")+
 # annotate("rect", xmin = 33, xmax = 66, ymin = -0, ymax = 0.4,
  #         alpha = .13,fill = "#377eb8")+
 # annotate("rect", xmin = 66, xmax = 100, ymin = -0, ymax = 0.4,
  #         alpha = .13,fill = "#e41a1c")+
  #annotate("text", x = 17, y = 0.39, label = "Early",color="#4daf4a",size=8,alpha=0.9)+
  #annotate("text", x = 50, y = 0.39, label = "Middle",color="#377eb8",size=8,alpha=0.9)+
  #annotate("text", x = 83, y = 0.39, label = "Late",color="#e41a1c",size=8,alpha=0.9)+
  #annotate("text", x = 83, y = 0.34, label = "Optimal scaling",color="#525252",size=6,alpha=0.75)+
  geom_line(color="white",size=2,linetype="dashed" )+
  theme(axis.title = element_text(colour = "black",size=16),
        axis.text=element_text(color="black",size=14))


# now make some data

I0 = 0.02    # initial fraction adopted, seed
S0 = 1 - I0 # initial fraction available to adopt


# Assign rates of spread, drop-out, and independent uptake:
beta = 0.3 #rate of spread 0.4
gamma = 0.23 #rate of drop-out 0.15
alpha = 0.000 #independent rate of adoption 0.002

# We will use the package deSolve to integrate, which requires certain data structures.
# Store parameters and initial values
# Parameters must be stored in a named list.
params <- list(beta = beta,
               gamma = gamma,
               alpha=alpha)

# Initial conditions are stored in a vector
inits <- c(S0, I0) #0 denotes that it is an initial condition

# Create a time series over which to integrate.

t_min = 0
t_max = 100
times = t_min:t_max

# We must create a function for the system of ODEs.
# See the 'ode' function documentation for further insights.
SISa <- function(t, y, params) {
  with(as.list(c(params, y)), {
    
    dS = - beta * y[1] * y[2] +gamma * y[2] - alpha*y[1]#y[1] non-adopters (S) and y[2] is adopters (I)
    
    dI = beta * y[1] * y[2] - gamma * y[2] + alpha*y[1]
    
    res <- c(dS,dI)
    list(res)
  })
}

# Run the integration:
out2 <- ode(inits, times, SISa, params, method="rk")
#
# Store the output in a data frame:
out2 <- data.frame(out2)
colnames(out2) <- c("time", "S", "I")

out2<-out2%>%filter(time<=30)

out2<-out2[c(25, 30, 21 ,13 ,0, 3, 27, 18, 7, 1, 16, 19),]
p<-p+geom_point(data=out2,color="black",size=4,alpha=0.7,shape=21,fill="#7f0000")



#Now add predictions

I0 = 0.10117    # initial fraction infected
S0 = 1 - I0 # initial fraction susceptible


# Assign social spread, drop out and independent adoption rates:

beta = rnorm(1000,0.3,0.02) #rate of social spread
gamma = rnorm(1000,0.23,0.02) #rate of drop out
alpha =rep(0,1000)#independent rate of adoption

# We will use the package deSolve to integrate, which requires certain data structures.
# Store parameters and initial values
# Parameters must be stored in a named list.
params <- list(beta = beta,
               gamma = gamma,
               alpha=alpha)

# Initial conditions are stored in a vector
inits <- c(S0, I0) #0 denotes that it is an initial condition

# Create a time series over which to integrate.
t_min = 31
t_max = 100
times = t_min:t_max

# We must create a function for the system of ODEs.
# See the 'ode' function documentation for further insights.
SISa <- function(t, y, params) {
  with(as.list(c(params, y)), {
    
    dS = - beta * y[1] * y[2] +gamma * y[2] - alpha*y[1]#y[1] available (S) and y[2] is adopted (I)
    
    dI = beta * y[1] * y[2] - gamma * y[2] + alpha*y[1]
    
    res <- c(dS,dI)
    list(res)
  })
}

# Run the integration:

out3<-ode(inits, times, SISa, sapply(params,"[[",1), method="rk")
out3 <- data.frame(out3)
out3$Sample<-1
colnames(out3) <- c("time", "1", "2","Sample")


for(i in 2:length(beta)){
  
  out4<-ode(inits, times, SISa, sapply(params,"[[",i), method="rk")
  out4<-as.data.frame(out4)
  out4$Sample<-i
  out3<-rbind(out3,out4)
}

colnames(out3) <- c("time", "S", "I","Sample")

out5<-out3#%>%filter(time>30)


#out5<-out5%>%group_by(time)%>%filter(I<quantile(out5$I,0.75)&I>quantile(out5$I,0.25))
p+
  tidybayes::stat_lineribbon(data=out5,aes(fill_ramp = after_stat(.width)), 
                             .width = c(0.2,0.5,0.7), fill = "#252525",alpha=0.75) +#4d004b
  annotate("point", x = 100, y = 0.31,
             colour = "#7f0000", size = 6)+
  annotate("text", x = 87, y = 0.31,label="Adoption\ntarget",
           colour = "#7f0000", size = 6)+
  ggdist::scale_fill_ramp_continuous(range = c(1, 0),name="Credibility interval")+
  ggthemes::theme_clean()+theme(legend.position = "none",
                                axis.title = element_text(colour = "black",size=16),
                                axis.text=element_text(color="black",size=14))









