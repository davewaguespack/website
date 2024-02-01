rm(list=ls())
gc(reset=TRUE)
library(data.table)
library(stargazer)
library(ggplot2)



# results table
coeffs <- data.table(
  run=double(),
  rate=double(),
  capital=double()
)


for (i in 1:10){
  coeffs[i, run := i]

  
}




for (i in 1:10){
  simdata <-  data.table(rate= rnorm(20, mean = 8, sd = 6 ))
 # simdata[1,return := 10000]
  simdata[, return := 10000+ shift(return,1)]
  simdata
  
  
  simdata[, return := (1+rate/100)*(10000+ shift(return,1))]
  simdata[,temp := (1+rate/100)*10000]

  simdata
  
  
  simdata[, temp:= cumsum(rate)]
  simdata[,    temp := 10000+ shift(temp,1)]
  
  simdata[2:30, return := (1+rate/100)*(10000+ shift(return,1))]
  simdata
  
  
  simdata[,X1 :=  rnorm(obs )]
  simdata[,Y1 := h1_effect*X1 + rnorm(obs, mean = 1, sd = .2 )]
  m1 <- lm(Y1 ~ X1, data = simdata)
  #summary(m1)$coefficients
  temp <-  data.table(beta = summary(m1)$coefficients[2,1],
                      se = summary(m1)$coefficients[2,2],
                      t = summary(m1)$coefficients[2,3]
  )
  coeffs <- rbind(coeffs,temp)
}

# plot results
# betas
ggplot(coeffs, aes(beta)) + 
  geom_density() +
  geom_vline(xintercept = h1_effect, color="red")  
# standard errors
ggplot(coeffs, aes(se)) + 
  geom_density() 
# t-values: how many false positive results?
ggplot(coeffs, aes(t)) + 
  geom_density() +
  geom_vline(xintercept = h1_effect, color="red") +
  geom_vline(xintercept = -1.96, color="blue") +
  geom_vline(xintercept = 1.96, color="blue") 
# how many significant results?
coeffs[,mean(t >= 1.96 | t <= -1.96)]




