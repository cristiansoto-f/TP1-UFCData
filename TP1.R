install.packages("data.table")
library(data.table)

#1. Importar el dataset, guardarlo en un objeto bidimensional (puede ser un data.frame, data.table, tibble, etc.)
UFCData = fread(file.choose(), fill = T, header = T, sep = ",")
UFCData[1:3]
str(UFCData) #Muestra las distintas variables

#2. Dividir el dataset original en dos datasets distintos, uno con toda la información referente al participante
# de la esquina roja y la información en común del encuentro y otro con la información referente al
# participante de la esquina azul y la información en común del encuentro. 