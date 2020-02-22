#########################
#Horner reto 1.1
#########################

#Variables 
n=4                          #Numero de grados

coeficientes=c(5,4,3,1)      #Coeficientes del polinomio     
coeficientesDerivada=c()     #Coeficientes derivando el polinomio
resultado=c()
res=0
j=n-1
x=3 ##coeficientes derivada



#####
##derivar##
i1=1;
while (i1<n){
  coeficientesDerivada[i1]=coeficientes[i1]*(j)
  i1=i1+1
  j=j-1
}
#####
##Horner##

j=1
while(j<n){
  res=res*x+coeficientesDerivada[j]
  resultado[j]= res;
  j=j+1;
}


TablaHorner = data.frame(resultado ) 

#Fin

