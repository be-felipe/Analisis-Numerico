#################
#Horner Problemas
#################
#Horner punto 1  en la documentacion

#Horner punto 2
c=c(2,0,-3,3,-4)
res=0;
resultado = c(0)
x=-2
n=1
while(n<6){
  res=res*x+c[n]
  resultado[n]= res;
  n=n+1;
}
cat ("El resultado es  = ",res);
cat ("El resultado por cada iteracion es :")
Tablita = data.frame(resultado)
print(Tablita)



#Horner punto 3
resultadoIterativo = 0;
i = 0;
x = 1.0001;
#Inicio del Ciclo
repeat{
  
  resultadoIterativo=resultadoIterativo+x^i;
  i=i+1;
  if(i>50) break;
}

#function Q
q=function(b) ((b^51)-1)/(b-1)
resultadoDirecto=q(x);

#error absoluto
errorA=abs(resultadoDirecto-resultadoIterativo);

#error relativo
errorR=((resultadoIterativo/(resultadoIterativo+resultadoDirecto))*(errorA/resultadoIterativo))+((resultadoDirecto/(resultadoIterativo+resultadoDirecto))*(errorA/resultadoDirecto));

#Tabla 

Datos = data.frame(i,errorA,errorR,resultadoIterativo,resultadoDirecto)
print(Datos);