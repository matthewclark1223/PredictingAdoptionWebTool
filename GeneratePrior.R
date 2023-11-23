library(deSolve)
I0 = 0.02    # initial fraction infected
S0 = 1 - I0 # initial fraction susceptible


# Assign social spread, drop out and independent adoption rates:

beta = rnorm(1000,0.3,0.02) #rate of social spread
gamma = rnorm(1000,0.15,0.02) #rate of drop out
alpha =rnorm(1000,0.02,0.002)#independent rate of adoption

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
t_max = 50
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

out<-ode(inits, times, SISa, sapply(params,"[[",1), method="rk")
out <- data.frame(out)
out$Sample<-1
colnames(out) <- c("time", "1", "2","Sample")


for(i in 2:length(beta)){
  
  out2<-ode(inits, times, SISa, sapply(params,"[[",i), method="rk")
  out2<-as.data.frame(out2)
  out2$Sample<-i
  out<-rbind(out,out2)
}

colnames(out) <- c("time", "S", "I","Sample")

#library(ggplot2)
ggplot(out,aes(x=time,y=I))+
  #geom_line(aes(group=Sample),alpha=0.01)+
  tidybayes::stat_lineribbon(aes(fill_ramp = after_stat(.width)), .width = ppoints(10), fill = "#045a8d") +#4d004b
  ggdist::scale_fill_ramp_continuous(range = c(1, 0),name="Credibility interval")+
  ggthemes::theme_clean()+xlab("Time")+ylab("Predicted adoption")+
  theme(axis.title = element_text(colour = "black",size=16),
        axis.text=element_text(color="black",size=14))






