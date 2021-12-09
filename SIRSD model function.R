##This code was written by Madeleine Armer on 11/17
#This function will run a simulation of an infectious disease running through a population
#and provide you with population metrics at IQR time points




SIRSD <- function(alpha, N, days, avg_days_sick, immunity_length) {
  mat <- matrix(NA, nrow = N, ncol = days)
  mat[, 1] <- c(rep("S", N - 1), "I")
  i <- 1
  t <- 1
  
  for (t in 1:(days - 1)) {
    for (i in 1:N) {
      if (mat[i, t] == "S") {
        alph <- rbinom(1, 1, (1 - alpha) ^ sum(mat[, t] == "I"))
        mat[i, t + 1] <- ifelse(alph == 1, "S", "I")
      }
      
      if (mat[i, t] == "I") {
        It <-
          sum(mat[i, 1:(t - 1)] == "I") #sum of days where person is infected
        
        pR <-
          pbeta(It / (avg_days_sick*2), 1, 1)  #prob of being recovered after # days
        bR <- rbinom(1, 1, pR) #prob that you will recover
        mat[i, t + 1] <- ifelse(bR == 1, "R", "I")
        
        
        
        if (mat[i, t + 1] == "I" ) {
          It <-  sum(mat[i, 1:t] == "I")
          pD  <-
            plnorm(It / (avg_days_sick*7) , 1, 1) #prob of being recovered after # days
          bD <- rbinom(1, 1, pD) #prob that you will die
          mat[i, t + 1] <- ifelse(bD == 1, "D", "I")
        }
        
      }
      if (mat[i, t] == "R") {
        mat[i, t + 1] <-
          "R" # if you are recovered you have a chance of going to be reinfected
        
        Rt <-  sum(mat[i, 1:t] == "R")
        if (Rt > immunity_length) {
          Ss <- rbinom(1, 1, 0.5) #prob that you will become supsceptible agian
          mat[i, t + 1] <- ifelse(Ss == 1, "S", "R")
        }
      }
      
      
      if (mat[i, t] == "D") {
        ## THE DEATH LOOPPPPP
        mat[i, t + 1] <- "D"
      }
      
    }
  }
  #for population metrics
  q_pS <- (sum(mat[, (days / 4)] == "S") / N) * 100
  q_pI <- (sum(mat[, (days / 4)] == "I") / N) * 100
  q_pR <- (sum(mat[, (days / 4)] == "R") / N) * 100
  q_pD <- (sum(mat[, (days / 4)] == "D") / N) * 100
  q <- rbind(q_pS, q_pI, q_pR, q_pD)
  
  h_pS <- (sum(mat[, (days / 2)] == "S") / N) * 100
  h_pI <- (sum(mat[, (days / 2)] == "I") / N) * 100
  h_pR <- (sum(mat[, (days / 2)] == "R") / N) * 100
  h_pD <- (sum(mat[, (days / 2)] == "D") / N) * 100
  h <- rbind(h_pS, h_pI, h_pR, h_pD)
  
  q3_pS <- (sum(mat[, (days *0.75)] == "S") / N) * 100
  q3_pI <- (sum(mat[, (days *0.75)] == "I") / N) * 100
  q3_pR <- (sum(mat[, (days *0.75)] == "R") / N) * 100
  q3_pD <- (sum(mat[, (days *0.75)] == "D") / N) * 100
  q3 <- rbind(q3_pS, q3_pI, q3_pR, q3_pD)
  
  f_pS <- (sum(mat[, days] == "S") / N) * 100
  f_pI <- (sum(mat[, days] == "I") / N) * 100
  f_pR <- (sum(mat[, days] == "R") / N) * 100
  f_pD <- (sum(mat[, days] == "D") / N) * 100
  f <- rbind(f_pS, f_pI, f_pR, f_pD)
  
  pop_metrics <- cbind(q, h, q3, f)
  dimnames(pop_metrics) <- list(c("S", "I", "R", "D"), c("FirstQuarter","Halfway","ThirdQuarter","Final"))

  
  return(pop_metrics)
}


###RUNNING SIMULATION 

sim_SIRDS <- function(nsim, SIRmodel){ #plug in the number of simulations and 

  S.q1 <- rep(NA, nsim)
  S.q2 <-rep(NA, nsim)
  S.q3 <- rep(NA, nsim)
  S.q4 <- rep(NA, nsim)
  
  I.q1 <- rep(NA, nsim)
  I.q2 <-rep(NA, nsim)
  I.q3 <- rep(NA, nsim)
  I.q4 <- rep(NA, nsim)
  
  R.q1 <- rep(NA, nsim)
  R.q2 <- rep(NA, nsim)
  R.q3 <-rep(NA, nsim)
  R.q4 <- rep(NA, nsim)
  
  D.q1 <- rep(NA, nsim)
  D.q2 <- rep(NA, nsim)
  D.q3 <- rep(NA, nsim)
  D.q4 <- rep(NA, nsim)


for(i in 1:nsim){
  sim <- as.data.frame( SIRmodel)
  S.q1[i] <- sim[1,1]
  S.q2[i] <- sim[1,2] 
  S.q3[i] <- sim[1,3]
  S.q4[i] <- sim[1,2] 
  
  I.q1[i] <- sim[2,1]
  I.q2[i] <- sim[2,2] 
  I.q3[i] <- sim[2,3]
  I.q4[i] <- sim[2,2] 
  
  R.q1[i] <- sim[3,1]
  R.q2[i] <- sim[3,2] 
  R.q3[i] <- sim[3,3]
  R.q4[i] <- sim[3,2] 
  
  D.q1[i] <- sim[4,1]
  D.q2[i] <- sim[4,2] 
  D.q3[i] <- sim[4,3]
  D.q4[i] <- sim[4,2] 
}


sim_metrics <- rbind(
S = cbind(SQ1 = mean(S.q1), SQ2 = mean(S.q2), SQ3 = mean(S.q3), SQ4 = mean(S.q4)),
I  = cbind(IQ1 = mean(I.q1), IQ2 = mean(I.q2), IQ3 = mean(I.q3), IQ4 = mean(I.q4)),
R = cbind(RQ1 = mean(R.q1), RQ2 = mean(R.q2), RQ3 = mean(R.q3), RQ4 = mean(R.q4)),
D = cbind(DQ1 = mean(D.q1), DQ2 = mean(D.q2), DQ3 = mean(D.q3), DQ4 = mean(D.q4))
)
dimnames(sim_metrics) <- list(c("S", "I", "R", "D"), c("FirstQuarter","Halfway","ThirdQuarter","Final"))

return(sim_metrics)
}

start <- Sys.time()
sim_mets.001 <- sim_SIRDS(10000, SIRSD(0.001, 10000, 365, 10, 90))
end <- Sys.time()
end - start
sim_mets.001

sim_mets.0001 <- sim_SIRDS(1000, SIRSD(0.0001, 1000, 90, 10, 90))

sim_mets.01 <- sim_SIRDS(1000, SIRSD(0.01, 1000, 90, 10, 90))

sim_mets.1 <- sim_SIRDS(1000, SIRSD(0.1, 1000, 90, 10, 90))



sim_mets.001.15 <- sim_SIRDS(1000, SIRSD(0.001, 1000, 365, 15, 90)) #low infection, low recovery
sim_mets.001.15.half <- sim_SIRDS(100, SIRSD(0.001, 1000, 180, 15, 90)) #low infection, low recovery #half year
sim_mets.001.15.quarter <- sim_SIRDS(100, SIRSD(0.001, 1000, 90, 15, 90)) #low infection, low recovery #quarter year

sim_mets.01.5 <- sim_SIRDS(1000, SIRSD(0.01, 1000, 365, 5, 90)) #high infection high recovery 
sim_mets.01.5.half <- sim_SIRDS(1000, SIRSD(0.01, 1000, 180, 5, 90)) #high infection high recovery #half year
sim_mets.01.5.quarter <- sim_SIRDS(1000, SIRSD(0.01, 1000, 90, 5, 90)) #high infection high recovery #quarter year

sim_mets.01.15 <- sim_SIRDS(1000, SIRSD(0.01, 1000, 365, 15, 90)) #high infection low recovery 
sim_mets.01.15.half <- sim_SIRDS(1000, SIRSD(0.01, 1000, 180, 15, 90)) #high infection low recovery #half year
sim_mets.01.15.quarter <- sim_SIRDS(1000, SIRSD(0.01, 1000, 90, 15, 90)) #high infection low recovery #quarter year

sim_mets.001.5 <- sim_SIRDS(1000, SIRSD(0.001, 1000, 365, 5, 90)) #low infection, high recovery
sim_mets.001.5.half <- sim_SIRDS(1000, SIRSD(0.001, 1000, 180, 5, 90)) #low infection, high recovery #half year
sim_mets.001.5.quarter <- sim_SIRDS(1000, SIRSD(0.001, 1000, 90, 5, 90)) #low infection, high recovery #quarter year






