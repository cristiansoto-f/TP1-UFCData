install.packages("data.table")
library(data.table)
UFCData = fread(file.choose(), fill = T, header = T, sep = ",")
UFCData[1:3]
str(UFCData) #Muestra las distintas variables