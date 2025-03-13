set.seed(1)
X=runif(10,7,12)
X
H=exp(-X^(2))
H
mean(H)
Int=(5)*mean(H)
print(Int)

set.seed(1)
X=runif(10,1,10)
X
H=((sin(X))/X)
H
mean(H)
Int=(9)*mean(H)
print(Int)

set.seed(1)
X=runif(10,5,10)
X
H=((log(3*X)*sinh(X))/(X^2+X+1))
H
mean(H)
Int=(5)*mean(H)
print(Int)
