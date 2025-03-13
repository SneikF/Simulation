#En primer lugar fijamos una semilla para reproducir
#los mismos valores de la muestra siempre:
set.seed(1)
#Luego imponemos cuantos valores de la muestra se tomaran,
#en este caso en la primera casilla son 10 correspondiente
#a r=10, y el intervalo en el que se encuentran estos valores,
#que en este caso es de 7 a 12 por lo tanto en la segunda y 
#tercera casilla se encuentran estos limites de integracion:
X=runif(10000,1,10)
#Posteriormente se define la funcion h(x)
H=exp(-X^(2))
#Luego tenemos el valor esperado de esta funcion E[h(x)]
mean(H)
#Y se multiplica por la diferencia entre los limites 
#de integracion, y se denota este valor con Int
Int=(5)*mean(H)
#Por ultimo nos mostrara el valor de esta integral
print(Int)