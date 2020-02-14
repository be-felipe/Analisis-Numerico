##################
#Taylor
#################
n=0;
Experimental=0;
tolerancia=0.000001;
teorico=1.648721271;


#Hacemos un ciclo
repeat{
  Experimental=Experimental+(0.5^n)/factorial(n);
  n=n+1;
  if((abs(Experimental-teorico))<tolerancia) break;
}
#Funcion Evaluada en el punto -2.
#Problema 2
f=function(a) (2*a^4)-(3*a^2)+(3*a)-4
f(-2)

#Tabla De taylor 
tabla = data.frame(Experimental,teorico,tolerancia)
tabla