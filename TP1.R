#install.packages("data.table")
#install.packages("stringr")
library(data.table)
library(stringr)

# 1. Importar el dataset, guardarlo en un objeto bidimensional (puede ser un data.frame, data.table, tibble, etc.)
dataUFC = fread(file.choose(), fill = T, header = T, sep = ",")

# 2. Dividir el dataset original en dos datasets distintos, uno con toda la informacion referente al participante
# de la esquina roja y la informacion en comun del encuentro y otro con la informacion referente al
# participante de la esquina azul y la informacion en comÃºn del encuentro.

# Extraigo informacion del peleador rojo y comun
isRedFighter = dataUFC[,!grepl( "^B_" , names(dataUFC))] #el ^ ayuda a seleccionar solo las columnas que NO EMPIEZAN con B_

# Extraigo informacion del peleador azul y comun
isBlueFighter = dataUFC[,!grepl( "^R_" , names(dataUFC))]

redFighter <- data.table()
blueFighter <- data.table()

for (i in 1:ncol(dataUFC)){
  if (isRedFighter[i]){
   redFighter <- cbind(redFighter, dataUFC[,..i])
  }
  if (isBlueFighter[i]) {
    blueFighter <- cbind(blueFighter, dataUFC[,..i])
  }
}

# 3. Para los dos datasets obtenidos en el item anterior
# a) Reemplazar el preﬁjo que indica el color de la esquina en los nombres de las columnas (R_ o B_) por
# un ” (un campo vacío). Por ejemplo, el nombre de la columna “R_ﬁghter” tiene que pasar a llamarse
# “ﬁghter”. Ambos datasets tendrian que tener nombres de columnas identicos al ﬁnalizar.

colnames(blueFighter)<-str_remove_all(colnames(blueFighter),"^B_")
colnames(redFighter)<-str_remove_all(colnames(redFighter),"^R_")

# b) Crear una variable (o recodear la variable “Winner”) que indique si el participante ganó la pelea (1) o
# no (0). Por ejemplo si se está trabajando con los datos del participante de la esquina azul y la variable
# Winner toma el valor de Red, entonces la nueva variable tendria que tomar el valor 0.

for(i in 1:nrow(redFighter)){
  redFighter[i,"Winner"] <- (dataUFC[i,"Winner"] == "Red")
  blueFighter[i,"Winner"] <- (dataUFC[i,"Winner"] == "Blue")
}

# c) Crear una variable que haga referencia al color de la esquina de cada dataset.
color <- vector(mode = "character", nrow(redFighter))

for (i in 1:length(color)) {color[i]<-"Red"}
redFighter <- cbind(color, redFighter)

for (i in 1:length(color)) {color[i]<-"Blue"}
blueFighter <- cbind(color, blueFighter)

rm(color)

#4. Unir las ﬁlas de ambos datasets en uno solo.
dataUFCFinal<- rbind(blueFighter, redFighter) 

#5. Reordenar las columnas del dataset obtenido en el punto anterior de forma tal que la primer columna
# sea la que se calculó en el ítem 3.b (la cual indica si el participante ganó o no la pelea), manteniendo el
# orden de las demás
dataUFCFinal <- setcolorder(dataUFCFinal, "Winner")


#6.
#No todas las variables son continuas y faltan datos para calcular
for(i in 1:ncol(dataUFCFinal)){
  if(is.numeric(dataUFCFinal[[i]]))
  {
    print("------------------------°--------------------------------")
    print(sprintf("Variable: %s", colnames(dataUFCFinal)[i]))
    print(sprintf("Media: %f", mean(dataUFCFinal[[i]], na.rm = T)))
    print(sprintf("Mediana: %f", median(dataUFCFinal[[i]], na.rm = T)))
    print(sprintf("MAD: %f", mad(dataUFCFinal[[i]], na.rm = T)))
    print(sprintf("Varianza: %f", var(dataUFCFinal[[i]], na.rm = T)))
    print(sprintf("Desviación estándar: %f", sd(dataUFCFinal[[i]], na.rm = T)))
    #print(colnames(dataUFCFinal[[i]]))
    print("------------------------°--------------------------------")
  }
}
