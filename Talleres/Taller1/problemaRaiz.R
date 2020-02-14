# Problema Raiz de 7
###########################
#Variables

n = 7         # valor de la Raiz
E = 0.000001  #ERROR PERMITIDO
x = 2.6      #Valor inicial
y = 0         #Valor exacto con el error permitido
contador = 0  #Contador que tendra la cantidad de iteraciones 
errorAbs = 0
errores = c(0);
arreglosy = c(0);


y = 0.5*(x+(n/x))
while (abs(x-y) > E)
{
  contador = contador +1  # Se aumenta el contador 
  errorAbs = abs(x-y) 
  errores[contador]= errorAbs # se guarda el error por cada iteracion 
  arreglosy [contador]= y # se guarda el valor de y por cada iteracion que se hace
  x = y
  y = 0.5*(x+(n/x))

}
x=round(x,6);
y=round (y,6);
cont = seq (1, contador)
cat ( "Resultado =" ,y," Iteraciones = ", contador );
cat (" Comprabacion =" ,y*y)
Datos= data.frame(cont, errores,arreglosy); #Los datos que va contener la tabla
print(Datos);


