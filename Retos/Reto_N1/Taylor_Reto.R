#######################
##Taylor Aproximaciones
#######################


#############
#Funciones
#############

f0=function(x) sin(x)
f1=function (x) cos(x)
f2=function(x) -sin(x)
f3=function(x) -cos(x)


############
#Variables
############

n=1
resultado=0
tolerancia=0.000001
teorico=sin(pi/64)
valor=-pi/64
resultados=c()
errorAbsoluto=c()
errorRelativo=c()


#Comienzo del while

i=1
while(abs(teorico-resultado)>tolerancia){
  resultados[i]=resultado
  errorAbsoluto[i]=abs(teorico-resultado)
  errorRelativo[i]=(errorAbsoluto[i]/resultado)*100
  if(n%%4==3)
    resultado=resultado+f1(0)*(valor)^n/factorial(n)
  if(n%%4==2)
    resultado=resultado+f2(0)*(valor)^n/factorial(n)
  if(n%%4==1)
    resultado=resultado+f3(0)*(valor)^n/factorial(n)
  if(n%%4==0)
    resultado=resultado+f0(0)*(valor)^n/factorial(n)
  i=i+1
  n=n+1
}

###############
#Errores
###############

resultados[i]=resultado
errorAbsoluto[i]=abs(teorico-resultado)
errorRelativo[i]=(errorAbsoluto[i]/resultado)*100
errorRelativo[1]=100

###########
#Tabla
###########

tablita=data.frame(resultados,teorico,errorAbsoluto,errorRelativo)

cat("Los resultados de todo el procedimiento se mostrara en la siguiente tabla")
cat("Datos Aproximaciones :  ")
print(tablita)


################
#Graficas
################

plot(main='Sin(x)',xlab='x',ylab='y',f0,xlim=c(0,5),ylim=c(0,1))
par(new = TRUE)
plot(main='Sin(x)',xlab='x',ylab='y',resultados,col='salmon',type='l',xlim=c(0,5),ylim=c(0,1))

#Fin