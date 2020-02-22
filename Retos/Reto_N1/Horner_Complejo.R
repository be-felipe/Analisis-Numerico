
#########################
##Horner reto punto 2
#########################

res=0
coeficientesI=c(1,10,8,0)
res=0;
resultado = c(complex())
x=complex(real=2,imaginary=1)
n=1
while(n<5){
  res=res*x+coeficientesI[n]
  resultado[n]= res;
  n=n+1;
}
Tablita = data.frame(resultado)