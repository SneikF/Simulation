set.seed(1)
X=runif(10000,7,12)
H=exp(-X^(2))
mean(H)
#Desviación estandar 
sd(H)
Int=(5)*mean(H)
print(sd(H))