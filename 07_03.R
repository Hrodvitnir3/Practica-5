#Ejercicio 1
numArtefactos<-c("17", "54", "10", "34", "90", "33", "49", "82", "12", "23", "56", "78", "44", "102", "10", "53", "4", "28", "37", "95")


numArtefactos_int<-as.integer(numArtefactos)
is.integer(numArtefactos_int)
#2
mean(numArtefactos_int)

#3
median(numArtefactos_int)

#4
mfv(numArtefactos_int)
moda<-function(numArtefactos_int) {
  u<-unique(numArtefactos_int) 
  tab<-tabulate(match(numArtefactos_int,u))
  u[tab==max(tab)]
}
moda(numArtefactos_int)
#Moda: numero que se repite
#u: valores unicos, solo sola 1 vez los numeros
#tabular

#5
frecuencias <- table(numArtefactos_int)
frecuencias.ordenada <- frecuencias[order(frecuencias, decreasing = TRUE)]
frecuencias.ordenada
moda <- frecuencias.ordenada[1]
moda        

#6
quantile(numArtefactos_int)
#cuartiles: medida de posicion, dividir la frecuencia en partes iguales

#7
IQR(numArtefactos_int)

#8
range(numArtefactos_int)

#9
var(numArtefactos_int)

#10
sd(numArtefactos_int)

#11
#12

#13
vector3<-c("21", "45", "33", "98", "34", "90", "67", "87", "45", "11", "73", "38", "28", "15", "50", "57", "12", "87", "29", "1")
