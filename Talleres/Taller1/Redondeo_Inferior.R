#########################
#Redondeo inferior
#########################

#Variables
n = 4; #cifras que solo puede almacenar
x =  536.78 ; #Valor inicial
x = x * 10^-3;
print(x);
xAproximado = 0.5367*10^3

E =0.00008*10^3 
print(E)
E = abs(E*(10^(3-n)))
print(E)


Tabla = data.frame(E,x,xAproximado)
print(Tabla)