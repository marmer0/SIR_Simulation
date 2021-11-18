
##This code was written by Madeleine Armer on 11/17 
#This runs a simulation of an infectious disease running through a population
#and provide you with population metrics at IQR time points 
#I ran this for 2 years with a common alpha rate over 100 subjects 

alpha <- 0.02 ##rate of infectiousness

mat <- matrix(NA, nrow = 100, ncol = 730)
mat[, 1] <- c(rep("S", 99), "I")



for (t in 1:729) {
  for (i in 1:100) {
    if (mat[i, t] == "S") {
      alph <- rbinom(1, 1, (1 - alpha) ^ sum(mat[, t] == "I"))
      mat[i, t + 1] <- ifelse(alph == 1, "S", "I")
    }
    
    if (mat[i, t] == "I") {
      It <-  sum(mat[i, 1:(t - 1)] == "I")
      
      pR <-
        pbeta(It / 14, 1, 1)  #prob of being recovered after # days
      bR <- rbinom(1, 1, pR) #prob that you will recover
      mat[i, t + 1] <- ifelse(bR == 1, "R", "I")
      
      It <-  sum(mat[i, 1:(t - 1)] == "I")
      
      if (mat[i, t + 1] == "I") {
        It <-  sum(mat[i, 1:t] == "I")
        pD  <-
          plnorm(It / 52 , 1, 1) #prob of being recovered after # days
        bD <- rbinom(1, 1, pD) #prob that you will die
        mat[i, t + 1] <- ifelse(bD == 1, "D", "I")
      }
      
    }
    if (mat[i, t] == "R") {
      mat[i, t + 1] <-
        "R" # if you are recovered you stay recovered until you become supsecptible again
      
      Rt <-  sum(mat[i, 1:t] == "R")
      if (Rt > 540) {
        Ss <- rbinom(1, 1, 0.5) #prob that you will become  supsceptible again rather than recovered 
        mat[i, t + 1] <- ifelse(Ss == 1, "S", "R")
      }
    }
    
    
    if (mat[i, t] == "D") {
      ## THE DEATH LOOPPPPP
      mat[i, t + 1] <- "D"
    }
    
  }
}



##at half a year
a<- (sum(mat[, 180] == "S") / 100) * 100
b <- (sum(mat[, 180] == "I") / 100) * 100
(sum(mat[, 180] == "R") / 100) * 100
(sum(mat[, 180] == "D") / 100) * 100

##at a year
c <- (sum(mat[, 365] == "S") / 100) * 100
d <- (sum(mat[, 365] == "I") / 100) * 100
(sum(mat[, 365] == "R") / 100) * 100
(sum(mat[, 365] == "D") / 100) * 100

##at 1.5 years
(sum(mat[, 547] == "S") / 100) * 100
(sum(mat[, 547] == "I") / 100) * 100
(sum(mat[, 547] == "R") / 100) * 100
(sum(mat[, 547] == "D") / 100) * 100

##at 2 years
(sum(mat[, 730] == "S") / 100) * 100
(sum(mat[, 730] == "I") / 100) * 100
(sum(mat[, 730] == "R") / 100) * 100
(sum(mat[, 730] == "D") / 100) * 100
