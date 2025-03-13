# Universidad Nacional de Colombia
# Proyecto de Probabilidad - PARTE D
#
## Simulaci贸n de momentos de una funci贸n de una variable aleatoria ##

## Ejercicio 1 ##

## Dada una variable aleatoria X~N(0.3, sqrt(0.5)). Se define la
## variable aleatoria Y= e^x

##Generen 10000 simulaciones de X mediante una funci贸n ya implementada
## para la distribuci贸n de X.
#
# NOTA: Se recomienda cargar todo el c骴igo antes de usar cualquier funcion



vectorX <- function(x){return(rnorm(x,mean=0.3,sd=sqrt(0.5)))}

simulacionX <- vectorX(10000)

hist(simulacionX, prob=TRUE)
lines(seq(-2,3,by=0.01),dnorm(x=seq(-2,3,by=0.01),mean=0.3,sd=sqrt(0.5)),
      type="l",col="blue")

## Transformen cada una de dichas realizaciones aplicando la funci贸n
## g y realicen un histograma de los valores transformados.

vectorY<-function(simulacionX){
  simulacionY <- replicate(length(simulacionX), 0)
  for (i in simulacionX) {
    simulacionY[i] <- exp(simulacionX)
  }
  return(simulacionY)
}
simulacionY <- vectorY(simulacionX)
hist(simulacionY, 
     main = "Densidad de Y",
     ylim = c(0, 0.6),
     xlab = "x",
     ylab =  "Frecuencia relativa",
     col  = "palevioletred3",
     prob = TRUE,
     breaks = 200
)

lines(seq(0, 10, 0.1),dlnorm(seq(0, 10, 0.1),
      meanlog =0.3, 
      sdlog =sqrt(0.5)), col = "black")


## Calcule de manera te贸rica (m茅todo 1) los siguientes momentos de Y:
## E[sqrt(y)], E[Y], E[Y^3]


PuntoB <- function(){
  Momentos <- replicate(3, 0)
  Momentos[1] <- exp(((1/2)*0.3)+((((1/2)^2)*0.5)/(2)))
  Momentos[2] <- exp((1*0.3)+(((1^2)*0.5)/(2)))
  Momentos[3] <- exp((3*0.3)+(((3^2)*0.5)/(2)))
  
  return(Momentos)
}

## Calcule (usando el m茅todo 2) los mismos momentos de Y

PuntoC <- function(){
  
  MomentosY <- replicate(3, 0)
  
  momento1X <- 0
  t<-0
  f=expression(exp((0.3*t)+(0.5*(t^2)*0.5)))
  # Derivada
  df<-D(f,'t')
  
  momento1X <- eval(df)
  
  MomentosY[1] <- exp(momento1X^(1/2)) + ((exp(momento1X^(1/2)))*0.5*0.5) 
  MomentosY[2] <- exp(momento1X^1) + ((exp(momento1X^1))*0.5*0.5)  
  MomentosY[3] <- exp(momento1X^3) + ((exp(momento1X^3))*0.5*0.5) 
  
  return(MomentosY)  
}

## Implemente el m茅todo 3 para diferentes valores de 
## r(=10,100,1000,10000)


PuntoD <- function(){
  Total <- matrix(0, nrow = 4, ncol = 3)
  
  simu1 <- vectorX(10)
  simu2 <- vectorX(100)
  simu3 <- vectorX(1000)
  simu4 <- vectorX(10000)
  
  #r=10 
  for (i in simu1) {
    Total[1,1] <- Total[1,1] + exp((1/2)*i)
    Total[1,2] <- Total[1,2] + exp(1*i)
    Total[1,3] <- Total[1,3] + exp(3*i)
  }
  
  Total[1,1] <- Total[1,1] / length(simu1)
  Total[1,2] <- Total[1,2] / length(simu1)
  Total[1,3] <- Total[1,3] / length(simu1)
  
  # r=100 
  for (i in simu2) {
    Total[2,1] <- Total[2,1] + exp((1/2)*i)
    Total[2,2] <- Total[2,2] + exp(1*i)
    Total[2,3] <- Total[2,3] + exp(3*i)
  }
  
  Total[2,1] <- Total[2,1] / length(simu2)
  Total[2,2] <- Total[2,2] / length(simu2)
  Total[2,3] <- Total[2,3] / length(simu2)
  
  
  # r=1000 
  for (i in simu3) {
    Total[3,1] <- Total[3,1] + exp((1/2)*i)
    Total[3,2] <- Total[3,2] + exp(1*i)
    Total[3,3] <- Total[3,3] + exp(3*i)
  }
  
  Total[3,1] <- Total[3,1] / length(simu3)
  Total[3,2] <- Total[3,2] / length(simu3)
  Total[3,3] <- Total[3,3] / length(simu3)
  
  # r=10000 
  for (i in simu4) {
    Total[4,1] <- Total[4,1] + exp((1/2)*i)
    Total[4,2] <- Total[4,2] + exp(1*i)
    Total[4,3] <- Total[4,3] + exp(3*i)
  }
  
  Total[4,1] <- Total[4,1] / length(simu4)
  Total[4,2] <- Total[4,2] / length(simu4)
  Total[4,3] <- Total[4,3] / length(simu4)
  
  return(Total)
  
}


## Ejercicio 2 ##

#funcion de masa de probabilidad
p_X2<-function(x){
  stopifnot(is.numeric(x))
  if (x %in% c(0:100)){
    (0.0787762*sqrt(x))/(8+x)
  }
  else{
    0
  }
}

#grafica de funcion de masa de probabilidad
mass2<-function(x,r){
  stopifnot(is.numeric(x))
  stopifnot(is.numeric(r))
  stopifnot(r>=x)
  u<- x:r
  mass<-NULL
  for (i in u){
    mass<-c(mass,p_X2(i))
  }
  
  plot(u,mass,
       type = 'h',
       main = 'Distribuci髇 demanda de empanadas',
       ylab = 'Probabilidad',
       xlab = 'x',
       lwd  = 2,
       col  = '#D9B26F'
  )
  lines(u,mass,
        type = 'p',
        pch  = 16,
        col  = '#A69658'
  )
}

#funcion de distribucion acumulada
F_X2<-function(x){
  stopifnot(is.numeric(x))
  a <- as.numeric(cut(x, c(-Inf,1:100,Inf),right=FALSE))
  n<-0
  if (x>=1){
    if (x>=100){
      1
    }
    else{
      for(i in 1:x){
        n<-n+p_X2(i)
      }
      return(n)
    }
  }
  else
    0
}
#Grafica de funcion de distribucion acumulada
Dist2<-function(x,r){
  stopifnot(is.numeric(x))
  stopifnot(is.numeric(r))
  stopifnot(r>=x)
  u<- x:r
  dist<-NULL
  dist1<-NULL
  dist2<-NULL
  p<- 0
  for (i in u){
    dist<-c(dist,F_X2(i))
    if (i %in% c(1:100)){
      dist2<-c(dist2,F_X2(i-1))
      dist1<-c(dist1,F_X2(i))
      p<-p+1
    }
  }
  if(x>=1){
    i<-x:(x+p-1)
  }
  else{
    i<-1:(1+p-1)
  }
  plot(u,dist,
       type = 's',
       main ='Funcion de distribuci髇 acumulada, demanda de empanadas',
       ylab ='F_X',
       xlab = 'x',
       lwd  =  2,
       col  = '#7A9E7E'
  )
  lines(i,dist1,
        type = 'p',
        pch  = 16,
        col  = '#31493C'
  )
  lines(i,dist2,
        type = 'p',
        pch  = 21,
        col  = '#31493C'
  )
}

#Ingresa una probabilidad y retorna un valor de la funcion de distribucion
#acumulada
num2<-function(x){
  stopifnot(is.numeric(x))
  for (i in 1:100){
    if (x>F_X2(i-1) & x<=F_X2(i)){
      return(i)
    }
  }
}

#Diagrama de barra para verificar con x numero aleatorios puede
#representar la funci髇 de masa de probabilidad
Rept2<-function(x){
  stopifnot(is.numeric(x))
  stopifnot(x>=1)
  system.time(vector_u<-runif(x))
  vector_x<-NULL
  
  for (j in 1:x){
    vector_x[j]<-num2(vector_u[j])
  }
  
  barplot(table(vector_x)/x,
          main ='Diagrama de barras, demanda de empanadas',
          ylab ='Frecuencia relativa',
          xlab = 'x',
          col  = '#9DC4B5'
  )
}

#Grafica de ganancia, con x numeros aleatorios y "y" numero de empanadas hechas
Venta<-function(x,y){
  stopifnot(is.numeric(x))
  stopifnot(is.numeric(y))
  stopifnot(x>=1)
  maxvalor<-NULL
  vector_u<-NULL
  ganancia<-NULL
  system.time(vector_u<-runif(x))
  for (j in 1:x){
    ganancia[j]<-G_X2(num2(vector_u[j]),y)
  }
  barplot(table(ganancia)/x,
          main ='Diagrama de barras, ganancia de empanadas',
          ylab ='Frecuencia relativa',
          xlab = 'x',
          col  = '#70163C',
          log  = 'y',
  )
  return(mean(ganancia))
}

#Ganancia m醲ima con x numeros aleatorios y "y" iteraciones.
Vmax<-function(x,y){
  stopifnot(is.numeric(x))
  stopifnot(is.numeric(y))
  stopifnot(x>=1)
  stopifnot(y>=1)
  maxvalor<-NULL
  l<-1:y
  for(k in 1:y){
    vector_u<-NULL
    ganancia<-NULL
    system.time(vector_u<-runif(x))
    for (j in 1:x){
      ganancia[j]<-G_X2(num2(vector_u[j]),k)
    }
    maxvalor[k]<-mean(ganancia)
  }
  barplot(rbind(maxvalor,l),
        main ='Diagrama de barras, ganancia m醲ima',
        ylab ='Ganancia esperada',
        xlab = 'q',
        col  = '#F9CB40',
        names= 1:y,
    )
  return(which.max(maxvalor))
}



#Calculo de ganancias, funci髇 G_X2(x,q)


MinE<-function(x,y){
  stopifnot(is.numeric(x))
  stopifnot(is.numeric(y))
  if(x>=y){return(y)}
  else{return(x)}
}
Cost<-function(x,y){
  stopifnot(is.numeric(x))
  stopifnot(is.numeric(y))
  if(y>x){return(y-x)}
  else{return(0)}
}

G_X2<-function(x,y){
  stopifnot(is.numeric(x))
  stopifnot(is.numeric(y))
  1500*MinE(x,y)+600*Cost(x,y)-900*y
}

##