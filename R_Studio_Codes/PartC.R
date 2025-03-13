##  Theta=7

##  (1) 
##  Función de densidad
f_<-function(x){
  if(x>=7 & x<=32){
    return(1/25)
  } 
  else{0
    }
}
##  Función de distribución
F_<-function(x){
  if(x<7){
    return(0)
  } 
  else{
    if(x>=7 & x<32){
      return((x-7)/25)
    }
    else{1}
  }
}

## Simluaciones

dens<-NULL
Dist<-NULL
for (i in seq(6,33,by=0.001)){
  dens<-c(dens,f_(i))
  Dist<-c(Dist,F_(i))
}

plot(seq(6,33,by=0.001),dens,type="l")


plot(seq(6,33,by=0.001),Dist,type="l")

## Algoritmo para simular

Sim_<-function(u){
  return(25*u+7)
}

###Simulaci?n de 10000 realizaciones
vector_u<-runif(10000)
vector_x<-NULL

for (j in 1:10000){
  vector_x[j]<-Sim_(vector_u[j])
}

hist(vector_x, prob=TRUE, main = "Histograma/Densidad", col = "aquamarine4")
lines(seq(6,33, by=0.001), dens, type = "l", col = "Red",lwd=3)


##  (2) 
##  Función de densidad
f_<-function(x){
  if(x>=0 & x<20){
    return(x/400)
  } 
  else{
    if(x>=20 & x<30){
      return(1/20)
    }
    else{0}
  }
}

##Función de Distribución
F_<-function(x){
  if(x<0){
    return(0)
  }
  else{
    if(x>=0 & x<20){
      return((x^2)/800)
    }
    else{
      if(x>=20 & x<30){
        return(-(1/2)+x/20)
      }
      else{1}
    }
    
  }
  
}
## Simluaciones

dens<-NULL
Dist<-NULL
for (i in seq(-2,31,by=0.001)){
  dens<-c(dens,f_(i))
  Dist<-c(Dist,F_(i))
}

plot(seq(-2,31,by=0.001),dens,type="l")
plot(seq(-2,31,by=0.001),Dist,type="l")

## Algoritmo para simular

Sim_<-function(u){
  if(u<0.5){
    return(20*sqrt(2*u))
  }
  else{20*u+10}
}

###Simulaci?n de 10000 realizaciones
vector_u<-runif(10000)
vector_x<-NULL

for (j in 1:10000){
  vector_x[j]<-Sim_(vector_u[j])
}

hist(vector_x, prob=TRUE, main = "Histograma/Densidad", col = "coral2")
lines(seq(-2,31,by=0.001),dens,type="l",col="blue", lwd=3)

##  (3) 
##  Función de densidad
f_<-function(x){
  if(x>=-10 & x<0){
    return(-x/100)
  } 
  else{
    if(x>=0 & x<10){
      return(x/100)
    }
    else{0}
  }
}

##Función de Distribución
F_<-function(x){
  if(x< -10){
    return(0)
  }
  else{
    if(x>=-10 & x<0){
      return(1/2-(x^2)/200)
    }
    else{
      if(x>=0 & x<10){
        return(1/2+(x^2)/200)
      }
      else{1}
    }
    
  }
  
}
## Simluaciones

dens<-NULL
Dist<-NULL
for (i in seq(-11,11,by=0.001)){
  dens<-c(dens,f_(i))
  Dist<-c(Dist,F_(i))
}

plot(seq(-11,11,by=0.001),dens,type="l")
plot(seq(-11,11,by=0.001),Dist,type="l")


## Algoritmo para simular

Sim_<-function(u){
  if(u>0 & u<0.5){
    return(-10*sqrt(1-2*u))
  }
  else{
    if(u>0.5 & u<1){
      return(10*sqrt(2*u-1))
    }
    else{0}
  }
}

###Simulaci?n de 10000 realizaciones
vector_u<-runif(10000)
vector_x<-NULL

for (j in 1:10000){
  vector_x[j]<-Sim_(vector_u[j])
}

hist(vector_x, prob=TRUE, main = "Histograma/Densidad", col = "cadetblue3")
lines(seq(-11,11,by=0.001),dens,type="l",col="green",lwd=3)


