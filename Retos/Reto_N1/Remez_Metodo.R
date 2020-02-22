###################
#Método de Remez
###################


#Variables remez

f =function(x) sin(x)
g= function(x) cos(x)
gradospoli = 3
ndatos = gradospoli 

# Grafica del seno
plot(g,xlim = c(-2,2), ylim=c(-1,1),ylab = "Y" , col ="red", main ="Funcion cos(x)")
plot(f,xlim = c(-2,2), ylim=c(-1,1),ylab = "Y" )


#Grafica del coseno

############################


#Arreglo de los valores en x y en Y
x = c()
y = c()
suma = (2*(pi/64))/(ndatos-1)

# Hallar las particiones de X y  de Y
i =1
while (length(x)<ndatos)
{
  if (length(x) == 0 )
  {
    x[i]= -pi/64;
    i = i +1
  }
  
  x[i] = (x[i-1]+ (suma))
  
  i = i+1
  
}
j = 1
while (j < ndatos+1 )
  
{
  
  y[j]= f(x[j])
  j = j+1
}

#Tabla de los valores de X y de Y

tablota = data.frame (x,y)


y[j]=g(pi/128)

#Hallar los coeficientes de nuestro sistema de ecuaciones

n = rbind(c(1,x[1],(x[1])^2,(x[1])^3),
          c(1,x[2],(x[2])^2,(x[2])^3),
          c(1,x[3],(x[3])^2,(x[3])^3),
          c(0,1,2*(pi/128),3*(pi/128)^2)
  
  
)

#Variable que tiene en cada posicion el valor de los coeficientes
z=solve(n,y)

print(z)

#Se Hace  el polinimio de grado 3 con los coeficientes obtenidos

h = function(x) z[1]+(z[2]*x)+(z[3]*x^2)+(z[4]*x^3)

#Graficamos la funcion del seno y la del polinimio generado 
par(new = TRUE)
plot(h,xlim = c(-2,2),col= "blue",ylim=c(-1,1), main = "APROXIMACIÓN REMEZ",ylab = "Y")

#Variables Para calcular el error
ErroAbsoluto = 0
ErrorRelativo = 0
ValorXpunto = pi/256


#############
#Errores 
#############

ErroAbsoluto = abs((f(ValorXpunto)- h(ValorXpunto))*10^-6)
ErrorRelativo  = ((ErroAbsoluto / h(ValorXpunto))*100)*10^-6


####################
#Tabla con los datos
####################
RemezTablita = data.frame(ErrorRelativo, ErroAbsoluto,ValorXpunto)
#Se imprime la Tabla

cat("Dado el punto  ",ValorXpunto)
cat("El Error Relativo es de :" ,ErrorRelativo)
cat("El Error Absoluto es de :" ,ErroAbsoluto)
