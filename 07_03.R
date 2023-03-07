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

#7
IQR(numArtefactos_int)

#8
range(numArtefactos_int)

#9
