##this code was written by Madeleine Armer on 11/16/21 
##I am simulating different types of SIR models to asnwer different questions about epidemics in populations 


#Visualizing an SIR MODEL
#keep in mind this SIR model does not track different individuals in the population, only calculates the sums of what S I Rs in th population
## intial states

state <- list()
N <- 1000
state$S <- N
state$I <-
  c(1)##how do make this for every I when  I hits a certain # of days it triggers
state$R <- 0

i <-  1

alpha <- 0.0001 ##rate of infectivity - this rate is an average rate
beta <- 0.01 ##rate of recovery


while (state$R[i] < N) {
  state$S[i + 1] <-
    rbinom(1, state$S[i], (1 - alpha) ^ (state$I[i])) ##1 is for people still susceptible
  
  
  state$R[i + 1] <-
    state$R[i] + rbinom(1, state$I[i], beta) #going from infected to recovered
  
  state$I[i + 1] <-
    N - state$S[i + 1] - state$R[i + 1] #infected is N - S -R
  
  i <- i + 1
}

state <- as.data.frame(state)


plot(
  state$S,
  col = "deepskyblue",
  type = "l",
  main = "SIR model, alpha = 0.01, beta = 0.05",
  xlab = "Index - Time points",
  ylab = "Number of people infected",
  lwd = 2
)
points(state$I,
       col = "forestgreen",
       type = "l",
       lwd = 2)
points(state$R,
       col = "firebrick2",
       type = "l" ,
       lwd = 2)
legend(
  "bottomright",
  legend = c("S", "I", "R"),
  col = c("deepskyblue", "forestgreen", "firebrick2"),
  lty = 1,
  cex = 0.8
)

