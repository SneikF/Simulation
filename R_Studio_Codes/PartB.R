# Universidad Nacional de Colombia
# Proyecto de Probabilidad - PARTE B
#
# Las funciones que se presentan son de la forma convencional vista en el curso
# De manera que para los tres ejercicios, se puede calcular
# p_X"i"(x) y F_X"i"(x), para un i=1,2,3
#
#
# De igual forma se tienen las graficas de masa de probabilidad y
# de distribucion acumulada como
# mass"i"(min,max), Dist"i"(min, max), para un i=1,2,3
# siendo (min, max) el intervalo de x que se desea graficar
#
#
# Tambien se encuentra la funcion de
# Rept"i"(n), para un i=1,2,3
# siendo n el numero de repeticiones del programa con un n>0,n\in\mathbb{N}^{+}
#
#
# NOTA: Se recomienda cargar todo el código antes de usar cualquier funcion


# Función de masa y distribucion de probabilidad
#Ejercicio 1

p_X1<-function(x){
  stopifnot(is.numeric(x))
  if (x %in% c(6:15)){
    1/10
  }
  else{
    0
  }
}

mass1<-function(x,r){
  stopifnot(is.numeric(x))
  stopifnot(is.numeric(r))
  stopifnot(r>=x)
  u<- x:r
  mass<-NULL
  for (i in u){
    mass<-c(mass,p_X1(i))
  }
  
  plot(u,mass,
       type = 'h',
       main ='Distribución unforme, discreta',
       ylab ='Probabilidad',
       xlab = 'x',
       lwd  =  2,
       col  = '#52B788'
       )
  lines(u,mass,
       type = 'p',
       pch  = 16,
       col  = '#498467'
       )
}

F_X1<-function(x){
  stopifnot(is.numeric(x))
  a <- as.numeric(cut(x, c(-Inf,6,7,8,9,10,11,12,13,14,15,Inf),right=FALSE))
  switch (a,0,1/10,2/10,3/10,4/10,5/10,6/10,7/10,8/10,9/10,1)
}



Dist1<-function(x,r){
  stopifnot(is.numeric(x))
  stopifnot(is.numeric(r))
  stopifnot(r>=x)
  u<- x:r
  dist<-NULL
  dist1<-NULL
  dist2<-NULL
  p<- 0
  for (i in u){
    dist<-c(dist,F_X1(i))
    if (i %in% c(6:15)){
      dist2<-c(dist2,F_X1(i-1))
      dist1<-c(dist1,F_X1(i))
      p<-p+1
    }
  }
  if(x>=6){
    i<-x:(x+p-1)
  }
  else{
    i<-6:(6+p-1)
  }
  plot(u,dist,
       type = 's',
       main ='Funcion de distribución acumulada, uniforme',
       ylab ='F_X',
       xlab = 'x',
       lwd  =  2,
       col  = '#607466'
    )
  lines(i,dist1,
        type = 'p',
        pch  = 16,
        col  = '#343E3D'
  )
  lines(i,dist2,
        type = 'p',
        pch  = 21,
        col  = '#343E3D'
  )
}


################ Ejercicio 2 ###########################

p_X2<-function(x){
  stopifnot(is.numeric(x))
  if (x %in% c(1:7)){
    (1/28)*x
  }
  else{
    0
  }
}

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
       main = 'Distribución Particular',
       ylab = 'Probabilidad',
       xlab = 'x',
       lwd  = 2,
       col  = '#8B1E3F'
  )
  lines(u,mass,
        type = 'p',
        pch  = 16,
        col  = '#3C153B'
  )
}

F_X2<-function(x){
  stopifnot(is.numeric(x))
  a <- as.numeric(cut(x, c(-Inf,1,2,3,4,5,6,7,Inf),right=FALSE))
  switch (a,0,1/28,3/28,6/28,10/28,15/28,21/28,1)
}


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
    if (i %in% c(1:7)){
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
       main ='Funcion de distribución acumulada, particular',
       ylab ='F_X',
       xlab = 'x',
       lwd  =  2,
       col  = '#747578'
  )
  lines(i,dist1,
        type = 'p',
        pch  = 16,
        col  = '#554348'
  )
  lines(i,dist2,
        type = 'p',
        pch  = 21,
        col  = '#554348'
  )
}



###################### Ejercicio 3 ########################
#Funcion de factorial
Fact<-function(x){
  stopifnot(is.numeric(x))
  if (x==0){
    x<-1
  }
  else{
    x<- x * Fact(x-1)
  }
  return (x)
}

#Funcion de coeficiente binomial
Combi<-function(n,r){
  stopifnot(is.numeric(r))
  stopifnot(is.numeric(n))
  stopifnot(r<=n)
  return (((Fact(n))/(Fact(n-r)*Fact(r))))
}

p_X3<-function(x){
  stopifnot(is.numeric(x))
  if (x %in% c(1:10)){
    Combi(10,x)*(7/16)^(x)*(9/16)^(10-x)
  }
  else{
    0
  }
}


mass3<-function(x,r){
  stopifnot(is.numeric(x))
  stopifnot(is.numeric(r))
  stopifnot(r>=x)
  u<- x:r
  mass<-NULL
  for (i in u){
    mass<-c(mass,p_X3(i))
  }
  
  plot(u,mass,
       type = 'h',
       main = 'Distribución Binomial (n=10, p=7/16)',
       ylab = 'Probabilidad',
       xlab = 'x',
       lwd  = 2,
       col  = '#376996'
      )
  lines(u,mass,
       type = 'p',
       pch  = 16,
       col  = '#1D3461'
      )
}

F_X3<-function(x){
  stopifnot(is.numeric(x))
  a <- as.numeric(cut(x, c(-Inf,1,2,3,4,5,6,7,8,9,10,Inf),right=FALSE))
  n<-0
  if (x>=1){
   if (x>=10){
     1
   }
    else{
      for(i in 1:x){
        n<-n+p_X3(i)
      }
      return(n)
    }
  }
  else
    0
}


Dist3<-function(x,r){
  stopifnot(is.numeric(x))
  stopifnot(is.numeric(r))
  stopifnot(r>=x)
  u<- x:r
  dist<-NULL
  dist1<-NULL
  dist2<-NULL
  p<- 0
  for (i in u){
    dist<-c(dist,F_X3(i))
    if (i %in% c(1:10)){
      dist2<-c(dist2,F_X3(i-1))
      dist1<-c(dist1,F_X3(i))
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
       main ='Funcion de distribución acumulada, Binomial (n=10, p=7/16)',
       ylab ='F_X',
       xlab = 'x',
       lwd  =  2,
       col  = '#DD1155'
  )
  lines(i,dist1,
        type = 'p',
        pch  = 16,
        col  = '#880044'
  )
  lines(i,dist2,
        type = 'p',
        pch  = 21,
        col  = '#880044'
  )
}

############################################

#Replicas con n repeticiones
num1<-function(x){
  stopifnot(is.numeric(x))
  for (i in 6:15){
    if (x>F_X1(i-1) & x<=F_X1(i)){
      return(i)
    }
  }
}
num2<-function(x){
  stopifnot(is.numeric(x))
  for (i in 1:7){
    if (x>F_X2(i-1) & x<=F_X2(i)){
      return(i)
    }
  }
}
num3<-function(x){
  stopifnot(is.numeric(x))
  for (i in 1:10){
    if (x>F_X3(i-1) & x<=F_X3(i)){
      return(i)
    }
  }
}
Rept1<-function(x){
  stopifnot(is.numeric(x))
  stopifnot(x>=1)
  system.time(vector_u<-runif(x))
  vector_x<-NULL
  
  for (j in 1:x){
    vector_x[j]<-num1(vector_u[j])
  }
  
  barplot(table(vector_x)/x,
      main ='Diagrama de barras, distribución uniforme',
      ylab ='Frecuencia relativa',
      xlab = 'x',
      col  = '#D8973C'
    )
}
Rept2<-function(x){
  stopifnot(is.numeric(x))
  stopifnot(x>=1)
  system.time(vector_u<-runif(x))
  vector_x<-NULL
  
  for (j in 1:x){
    vector_x[j]<-num2(vector_u[j])
  }
  
  barplot(table(vector_x)/x,
          main ='Diagrama de barras, distribución particular',
          ylab ='Frecuencia relativa',
          xlab = 'x',
          col  = '#A4243B'
  )
}
Rept3<-function(x){
  stopifnot(is.numeric(x))
  stopifnot(x>=1)
  system.time(vector_u<-runif(x))
  vector_x<-NULL
  
  for (j in 1:x){
    vector_x[j]<-num3(vector_u[j])
  }
  
  barplot(table(vector_x)/x,
          main ='Diagrama de barras, distribución binomial',
          ylab ='Frecuencia relativa',
          xlab = 'x',
          col  = '#4DCCBD'
  )
}