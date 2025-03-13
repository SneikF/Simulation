# Universidad Nacional de Colombia
# Proyecto de Probabilidad - PARTE A
#
# El codigo de la propuesta de numero aleatorio
# fue tomado de la siguiente página web:
# https://rubenfcasal.github.io/simbook/gen-cong.html
#
# Se usan las funciones graph"i"(n), para un i=1,2
# donde n son el numero de numeros alatorios que se ingresan en el histograma
#
#
# NOTA: Se recomienda cargar todo el código antes de usar cualquier funcion 



# --------------------------------------------------
# Generador congruencial de números pseudoaleatorios
# --------------------------------------------------

# initRANDC(semilla,a,c,m)
# -----------------------
#   Selecciona el generador congruencial
#   Por defecto RANDU de IBM con semilla del reloj
#   OJO: No se hace ninguna verificación de los parámetros
initRANDC <- function(semilla=as.numeric(Sys.time()), a=2^16+3, c=0, m=2^31) {
  .semilla <<- as.double(semilla) %% m  #Cálculos en doble precisión
  .a <<- a
  .c <<- c
  .m <<- m
  return(invisible(list(semilla=.semilla,a=.a,c=.c,m=.m))) #print(initRANDC())
}

# RANDC()
# -----------------------
#   Genera un valor pseudoaleatorio con el generador congruencial
#   Actualiza la semilla (si no existe llama a initRANDC)
RANDC <- function() {
  if (!exists(".semilla", envir=globalenv())) initRANDC()
  .semilla <<- (.a * .semilla + .c) %% .m
  return(.semilla/.m)
}

# RANDCN(n)
# -----------------------
#   Genera un vector de valores pseudoaleatorios con el generador congruencial
#   (por defecto de dimensión 1000)
#   Actualiza la semilla (si no existe llama a initRANDC)
RANDCN <- function(n=1000) {
  x <- numeric(n)
  for(i in 1:n) x[i]<-RANDC()
  return(x)
  # return(replicate(n,RANDC()))  # Alternativa más rápida    
}

initRANDC(543210) 

graph1<-function(x){
  stopifnot(is.numeric(x))
  stopifnot(x>=1)
  system.time(u <- RANDCN(x))  # Generar
  hist(u, freq = FALSE,
       main ='Histograma, función propuesta',
       ylab ='Frecuencia relativa',
       xlab = 'x',
       col  = '#698F3F',
       prob = TRUE
    )
  abline(h = 1, lty = 'dashed') 
}

graph2<-function(x){
  stopifnot(is.numeric(x))
  stopifnot(x>=1)
  system.time(u <- runif(x))
  hist(u, freq = FALSE,
       main ='Histograma, función runif',
       ylab ='Frecuencia relativa',
       xlab = 'x',
       col  = '#A79AB2',
       prob = TRUE
  )
  abline(h = 1, lty = 'dashed')
}