#########
#Quiz
#########

#Maquina y su precición
options(scripen = 999)
options(digits = 9)
#Se coloca la libreria polynomf 
install.packages("PolynomF")#instalar paquete 
help(PolynomF)
library(PolynomF)

#2punto
#Variables
n = 3 #Grados deL polinomio
x = c(0,1,2)
y = c(10,15,5)

Tangente = tan
plot (x,y,main = "Polinomio",xlim=c(0,2),ylim=c(0,16),col = "blue",ylab = "Y",xlab = "X")
resu=poly_calc(x,y) 
par(new=TRUE)
plot(resu,xlim=c(0,2),ylim=c(0,16),col = "red",ylab = "Y",xlab = "X",main = "Polinomio")
resu;
legend("bottomleft",legend=c("Interpolante","punto"), lty=c(2:1),bty="b")




