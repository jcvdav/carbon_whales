birthProb <- 0.325
matAge <- 4
carCap <- 500
deathProb <- 0.16
envSd <- 0.04
simLen <- 100
nsim <- 10

pop <- sample (0:60,40, replace=T)

#Grow function
growPop <- function(x,b,a,k)          
{      
  if (length(x)<k)
  {
    fert <- sum(x>=a)
    randb <- runif(fert)      
    newborn <- sum(randb<b)    
    x <- c(rep(0,newborn),x)   
  }
  return(x)               
}

#Death function
deathPop <- function(x,d,sd)         
{     
  randd <- runif(length(x))        
  deaths <- which(randd<(d+rnorm(1,0,sd)))        
  if(length(deaths)>0) {
    x <- x[-deaths]
    }                 
  return(x)                     
}

#Aging function
agePop <- function(x)      
{      
  x <- x+1                  
  return(x)
}

#The dynamics for one year in the life of the whales
popDyn <- function(x,b,a,k,d,sd)
{
  browser()
  x <- growPop(x,b,a,k)
  x <- deathPop(x,d,sd)
  x <- agePop(x)
  return(x)
}
#The function for the life of the whales until time n in age structure
popLife.S <- function(x,b,a,k,d,sd,n)
{
  Spop <- x
  for(i in 1:n)
    Spop=popDyn(Spop,b,a,k,d,sd)
  return(Spop)
}
#100 year simulation
S.100 <- popLife.S(pop,birthProb,matAge,carCap,deathProb,envSd,simLen)
#plotting
hist(S.100)
#The function for the number of individuals until time t
popLife.N <- function(x,b,a,k,d,sd,n)
{
  Npop=NA
  Spop=x
  for(i in 1:n) 
  {
    Spop=popDyn(Spop,b,a,k,d,sd)
    Npop[i]=sum(Spop>0)
  }    
  return(Npop)
}

#Function for extinction count and popoulation tracking
extPop <- function(x,b,a,k,d,sd,n,s) {
  simresults <- matrix(NA,n,s)
  for (j in 1:s)
  {simresults[,j] <- popLife.N(x,b,a,k,d,sd,n)}
  extprob <- sum(simresults[n,]==0)/s
  return(extprob)
}

#Extinction probability
extPop(pop,birthProb,matAge,carCap,deathProb,envSd,simLen,nsim)

#Updating the function for the matrix
numPop <- function(x,b,a,k,d,sd,n,s)
{
  simresults=matrix(NA,n,s)
  for (j in 1:s)
  {simresults[,j]=popLife.N(x,b,a,k,d,sd,n)}
  return(simresults)
}

#Creating the matrix of simulations
popMatrix <- numPop(pop,birthProb,matAge,carCap,deathProb,envSd,simLen,nsim)
matplot(popMatrix,type="l",col="grey")
