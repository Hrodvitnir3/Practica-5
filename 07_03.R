#Ejercicio 1
numArtefactos<-c("17", "54", "10", "34", "90", "33", "49", "82", "12", "23", "56", "78", "44", "102", "10", "53", "4", "28", "37", "95")
is.integer(numArtefactos)
#Los datos se guardan como double
numArtefactos_int<-as.integer(numArtefactos)
is.integer(numArtefactos_int)

#2
MediaNAI<-mean(numArtefactos_int)

#3
MedianaNAI<-median(numArtefactos_int)
#Mediana hace referencia al valor que divide al grupo de factores en dos grupos, teniendo el valor de la mediana la posición central.

#4
moda<-function(numArtefactos_int) { 
  u<-unique(numArtefactos_int) 
  tab<-tabulate(match(numArtefactos_int,u))
  u[tab==max(tab)]
}
Moda<-moda(numArtefactos_int)
#Para calcular la moda en numArtefactos_int, primero se emplea la funcio unique, que crea los valores unicos, es decir, hace que solo aparezca una vez cada numero. 
#Tras esto, usamos la funcion tabulate, para que nos indice la frecuencia de cada numero. 



#5
frecuencias <- table(numArtefactos_int) #1.Creamos una tabla con los vectores numericos
frecuencias.ordenada <- frecuencias[order(frecuencias, decreasing = TRUE)]#2.Ordenamos la tabla en orden decrecente
frecuencias.ordenada
moda <- frecuencias.ordenada[1] #Dado que el 10 es la moda, elegimos que solo nos muestre el primer valor.
moda  


#6
Cuartiles<-quantile(numArtefactos_int)
Cuartiles2<-as.numeric(Cuartiles)
#Los cuartiles es una medida de posicion que nos dividen los vectores en grupos de porcentajes.

#7
RI<-IQR(numArtefactos_int)
RI
# El rango intercuartilico es la resta entre el primer y el tercer cuartil. Esto nos indica donde se agrupan el 50% de los datos, aproximadamente. 

#8
range(numArtefactos_int)
#El rango es el valor maximo y minimo de nuestros datos. 
rango_artefactos<-range(numArtefactos_int)

#9
Varianza<-var(numArtefactos_int) #Forma 1
sd(numArtefactos_int)^2 #Forma 2. Ya que la varianza no es mas que la desviacion al cuadrado, lo podemos calcular asi tambien

#10
sqrt(var(numArtefactos_int)) #Forma 1
sd(numArtefactos_int) #Forma 2


#11
#La desviacion estandar es la raiz cuadrada (sqrt) de la varianza (var)


#12
plot(numArtefactos_int,numArtefactos_int)

#13
vector3<-c("21", "45", "33", "98", "34", "90", "67", "87", "45", "11", "73", "38", "28", "15", "50", "57", "12", "87", "29", "1")

#14
coef_var(numArtefactos_int)

vector3<-as.numeric(vector3)
sd(vector3)/mean(vector3)*100
#Obervamos que la desviacion de vector3 es mucho mayor que numArtefactos_int

#15
Tabla1<-table(MediaNAI,MedianaNAI,Moda,DesviacionEstandar,Varianza,RI)
View(Tabla1)   

#16
skewness(vector3)
#Esta funcion nos da informacion sobre la simetria en cuanto a la distribucion de los datos. En este caso, es una asimetrica positiva, pues el resultado es positivo
hist(vector3)
#Con ls funcion hist podemos comprobar que es positiva, ya que los mayores resultados se concentran a la izquierda del centro. 
#La diferencia entre asimetría positiva, negativa y simétrica, es la posición de la curva respecto al centro. En el caso de la asimetría positiva, la curva quedaria a la izquierda del centro; 
#en la asimetria negativa,la curva quedaría a la derecha del centro. En la simétrica, el pico álgido de la curva, coincidiria con el centro del eje x. 
#Un ejemplo de asimétrica positiva, serían los resultados de una prueba muy dificil, donde la mayoria de los resultados se concrentren en los valores mas bajos del eje x.
#Un ejemplo de asimétrica negativa, serían los resultados de una prueba muy facil, donde la mayoria de los resultados se concrentren en los valores mas altos del eje x.
#Un ejemplo de asimétrica negativa, serían los resultados de una prueba muy facil, donde la mayoria de los resultados se concrentren en los valores mas altos del eje x.
##Un ejemplo de simétrica, serían las alturas de un equipo de baloncesto, donde la mayoria de los resultados se concrentren en torno al punto central (la media)


#17
kurtosis(vector3)
#Dado que el resulado es menor de 3, nuestros datos tienden a producir menos valores atípicos y menos extremos que en una distribución normal. A este curtois se le conoce como playkurtic 
