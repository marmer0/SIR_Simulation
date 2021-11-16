##this code was written by Madeleine Armer on 11/16/21 
##I am simulating different types of SIR models to asnwer different questions about epidemics in populations 


##SIR model - tracking individuals in the population through a matrix: each row is one person and the column is the time point, the intersection of row and col is the persons state at the time 

alpha <- 0.1 ##rate of infectivity  
beta <- 0.05 #rate of recovery 

mat <- matrix(NA, nrow=100, ncol = 100) #initialize the matrix

mat[,1] <- c(rep("S",99),"I")  #adding the first intial states of the population adding S, I and Rs


for (t in 1:99) {
  for (i in 1:100) {
    if (mat[i, t] == "S") {
      alph <- rbinom(1, 1, (1 - alpha) ^ sum(mat[, t] == "I"))
      mat[i, t + 1] <- ifelse(alph == 1, "S", "I") #looping through first column and using rbinom prob to calculate if next time point will be S or I 
    }
    if (mat[i, t] == "I") { #now if the idv is infected, you can be come recovered after a certain number of days 
      It <-  sum(mat[i, 1:(t-1) ] == "I")

      pR <- pbeta(It / 10, 1, 1)  #prob of being recovered after # of days
      bR <- rbinom(1, 1, pR) #prob that you will recover
      
      mat[i, t + 1] <- ifelse(bR == 1, "R", "I")
      
      
    }
    if(mat[i,t] == "R"){
      mat[i, t+1] <- "R" # if you are recovered you stay recovered 
     
    }
  }
}
mat

#now that we have this matrix, we can answer questions of how long a person was not sick, sick and recovered 

