install.packages("data.table")
library(data.table)

#1. Importar el dataset, guardarlo en un objeto bidimensional (puede ser un data.frame, data.table, tibble, etc.)
UFCData = fread(file.choose(), fill = T, header = T, sep = ",")


#2. Dividir el dataset original en dos datasets distintos, uno con toda la informacion referente al participante
# de la esquina roja y la informacion en comun del encuentro y otro con la informacion referente al
# participante de la esquina azul y la informacion en común del encuentro.

#Extraigo informacion del peleador rojo
esRedFighter = UFCData[,!grepl( "^B_" , names(UFCData))] #el ^ ayuda a seleccionar solo las columnas que EMPIEZAN con R_

#Extraigo informacion del peleador azul
esBlueFighter = UFCData[,!grepl( "^R_" , names(UFCData))]

redFighter <- data.table()
blueFighter <- data.table()
for (i in 1:length(esRedFighter)){
  if (esRedFighter[i]){
   redFighter <- cbind(redFighter, UFCData[,..i])
  }
  if (esBlueFighter[i]) {
    blueFighter <- cbind(blueFighter, UFCData[,..i])
  }
}
