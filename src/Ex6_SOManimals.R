#Author: Thomas Hollis
#Subject: Bachelor Thesis

#1. Data Import
data <- read.csv("R/AnimalsSOMnolab.csv")

#2. Global variables


t <- 1
d <- matrix(0, 10, 10)
pattern_size <- 13
map <- matrix(list(), 10, 10)
positionx <- integer(16)
positiony <- integer(16)
SOM <- matrix(0,2,16)

colnames(SOM) <- c("dove", "hen", "duck", "goose", "owl", "hawk", "eagle", "fox", "dog", "wolf", "cat", "tiger", "lion", "horse", "zebra", "cow")
rownames(SOM) <- c("X", "Y")

#3. Set random map weights
for (i in 1:10)
{
  for (j in 1:10)
  {
    map[[i,j]] <- runif(13, 0, 1)
  }
}

#4. Pick animal at random
for (t in 1:10000)
{
  r <- round(runif(1, 1, 16))
  x <- data[r,]
  X <- t(x)
  
  for (i in 1:10)
  {
    for (j in 1:10)
    {
      w <- map[[i,j]]
      W <- t(w)
      d[i,j] <- sqrt((as.numeric(x)-as.numeric(W))%*%(as.numeric(X)-as.numeric(w)));
    }
  }
  distance <- d[1,1]
  
  for (i in 1:10)
  {
    for (j in 1:10)
    {
      if (d[i,j] < distance)
      {
        distance <- d[i,j]
        u <- i
        v <- j  
      }
    }
  }
  #setting learning parameters
  theta <- 1;
  alpha <- 100/(200+t);
  for (i in 1:10)
  {
    for (j in 1:10)
    {  
      eta <- exp(-(((i-u)^2)+(j-v)^2)/2*theta^2) 
      map[[i,j]] <- map[[i,j]] + alpha*eta*(X-map[[i,j]])
    }
  }
}


#TESTING PHASE
for (test in 1:16)
{
  y <- data[test,]
  Y <- t(y)
  for (i in 1:10)
  {
    for (j in 1:10)
    {
      w <- map[[i,j]]
      W <- t(w)
      d[i,j] <- sqrt((as.numeric(y)-as.numeric(W))%*%(as.numeric(Y)-as.numeric(w)))
    }
  }
  distance <- d[1,1]
  for (i in 1:10)
  {
    for (j in 1:10)
    {
      if (d[i,j] < distance)
      {
        distance <- d[i,j] 
        positionx[test] <- i 
        positiony[test] <- j 
      }
    }
  }
}

#Mapping each animal
for(i in 1:16)
{
  SOM[1,i] <- positionx[i]
  SOM[2,i] <- positiony[i]
}


plot(t(SOM))
text(t(SOM), labels = colnames(SOM), pos = 3)
