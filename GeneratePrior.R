library(deSolve)
I0 = 0.02    # initial fraction infected
S0 = 1 - I0 # initial fraction susceptible


# Assign transmission and pathogen-induced death/recovery rates:

beta = rnorm(1000,0.4,0.02) #rate of infection
gamma = rnorm(1000,0.15,0.02) #rate of recovery
alpha =rnorm(1000,0.005,0.0002)#independent rate of infection

# We will use the package deSolve to integrate, which requires certain data structures.
# Store parameters and initial values
# Parameters must be stored in a named list.
params <- list(beta = beta,
               gamma = gamma,
               alpha=alpha)

# Initial conditions are stored in a vector
inits <- c(S0, I0) #0 denotes that it is an initial condition

# Create a time series over which to integrate.
# Here we have an epidemic that is observed over t_max number of days (or weeks or etc).
t_min = 0
t_max = 50
times = t_min:t_max

# We must create a function for the system of ODEs.
# See the 'ode' function documentation for further insights.
SISa <- function(t, y, params) {
  with(as.list(c(params, y)), {
    
    dS = - beta * y[1] * y[2] +gamma * y[2] - alpha*y[1]#y[1] susceptible and y[2] is infected
    
    dI = beta * y[1] * y[2] - gamma * y[2] + alpha*y[1]#susceptibles and recovered
    
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
  geom_line(aes(group=Sample),alpha=0.01)+theme_classic()





