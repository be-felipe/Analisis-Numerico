###################
#Horner problema 1
###################
coeficientes=c(2,0,-3,3,-4)  #Lo llenamos
resultado=0                  #Resultado 
x=-2
n=1

#Coienza el ciclo del while mientras que el contador sea menor
#a la cantidad de coeficientes
while(n<6){
  resultado=resultado*x+coeficientes[n]
  n=n+1;
}
print(resultado)