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



##Running simultions 

(sim1 <-  SIRSD(0.05, 100, 365, 10, 90))

nsim <- 10
sims <- replicate(nsim,SIRSD(0.001, 1000, 365, 10, 90))













