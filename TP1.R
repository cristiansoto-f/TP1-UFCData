install.packages("data.table")
library(data.table)

#1. Importar el dataset, guardarlo en un objeto bidimensional (puede ser un data.frame, data.table, tibble, etc.)
UFCData = fread(file.choose(), fill = T, header = T, sep = ",")
UFCData[1:3]
str(UFCData) #Muestra las distintas variables

#2. Dividir el dataset original en dos datasets distintos, uno con toda la información referente al participante
# de la esquina roja y la información en común del encuentro y otro con la información referente al
# participante de la esquina azul y la información en común del encuentro. 
#Para ejecutar la función grepl es necesario pausar el paquete data.table
#Extraigo informacion del peleador rojo
RedFighter = UFCData[,grepl( "^R_" , names(UFCData))] #el ^ ayuda a seleccionar solo las columnas que EMPIEZAN con R_
#Extraigo informacion del peleador azul
BlueFighter = UFCData[,grepl( "^B_" , names(UFCData))]
#Extraigo informacion en comun excluyendo mendiante !grepl 
CommonData = UFCData[,!grepl( "^B_" , names(UFCData)) & !grepl( "^R_" , names(UFCData))]

#A continuación uno los data table
RF  = cbind(RedFighter, CommonData)
BF  = cbind(BlueFighter, CommonData)
