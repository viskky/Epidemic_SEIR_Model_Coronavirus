library(deSolve)
############# ODE function #########
my_ode<-function(t,state,parms){
  with(as.list(state),{
    dndt=rep(0,length(state))
    #-------My Equations----------------
    dndt[1]= -(b*S*I)/N # dS/dt
    dndt[2]= (b*S*I)/N - d*E # dC/dt
    dndt[3]= d*E - v*I
    dndt[4]= v*I
    #-------------------------------------
    return(list(dndt)) # Return
  })
}
############ END of function ##########
latent= c(1/4,1/2)
force_of_inf= c(0.01,0.02)
infection =c(1/2,1)
d=mean(latent)  # inverse of latent period
b=mean(force_of_inf) # force of infection
v=mean(infection)   # inverse of infection period
N=1000 # Total number of individuals in the population
S=999 # Initial number of susceptible
I=0    # Initial number of infected
R=0	 # Initial number of removed individual
E=1    # Initial number of exposed individual
t=seq(0,100,0.2) # Run for 100 days
init=c(S=S,E=E,I=I,R=R) # Vector with initial values

out <- ode(y = init, times = t, func = my_ode, parms = NULL)
Suscept<-out[,2]
Exposed<-out[,3]
Infected<-out[,4]
Removed<-out[,5]
time<-out[,1]
plot(time,Suscept,type="l",main= 'SEIR model with worst case scenario',xlab="Time (days)",ylab="Number of S/E/I/R patients",ylim = c(0,1100),col='purple')
lines(time,Exposed,col="red")
lines(time,Infected,col="darkgreen")
lines(time,Removed,col="turquoise")
legend("right",title='state',c("E","I","R","S"),col=c("red","darkgreen","turquoise","purple"),lty=1)
