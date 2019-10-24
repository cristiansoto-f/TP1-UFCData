#install.packages("data.table")
#install.packages("stringr")
library(data.table)
library(stringr)

# 1. Importar el dataset, guardarlo en un objeto bidimensional (puede ser un data.frame, data.table, tibble, etc.)
dataUFC = fread(file.choose(), fill = T, header = T, sep = ",")

# 2. Dividir el dataset original en dos datasets distintos, uno con toda la informacion referente al participante
# de la esquina roja y la informacion en comun del encuentro y otro con la informacion referente al
# participante de la esquina azul y la informacion en comÃƒÂºn del encuentro.

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
# a) Reemplazar el preï¬jo que indica el color de la esquina en los nombres de las columnas (R_ o B_) por
# un â€ (un campo vacÃ­o). Por ejemplo, el nombre de la columna â€œR_ï¬ghterâ€ tiene que pasar a llamarse
# â€œï¬ghterâ€. Ambos datasets tendrian que tener nombres de columnas identicos al ï¬nalizar.

colnames(blueFighter)<-str_remove_all(colnames(blueFighter),"^B_")
colnames(redFighter)<-str_remove_all(colnames(redFighter),"^R_")

# b) Crear una variable (o recodear la variable â€œWinnerâ€) que indique si el participante ganÃ³ la pelea (1) o
# no (0). Por ejemplo si se estÃ¡ trabajando con los datos del participante de la esquina azul y la variable
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

#4. Unir las ï¬las de ambos datasets en uno solo.
dataUFCFinal<- rbind(blueFighter, redFighter) 

#5. Reordenar las columnas del dataset obtenido en el punto anterior de forma tal que la primer columna
# sea la que se calculÃ³ en el Ã­tem 3.b (la cual indica si el participante ganÃ³ o no la pelea), manteniendo el
# orden de las demÃ¡s
dataUFCFinal <- setcolorder(dataUFCFinal, "Winner")


# 6. Obtener estadísticas descriptivas básicas para todas las variables continuas: media, desvío, varianza,
# número de observaciones, máximo y mínimo, cuartiles, etc.
for(i in 1:ncol(dataUFCFinal)){
  if(is.numeric(dataUFCFinal[[i]]))
  {
    print("------------------------Â°--------------------------------")
    print(sprintf("Variable: %s", colnames(dataUFCFinal)[i]))
    print(sprintf("Media: %f", mean(dataUFCFinal[[i]], na.rm = T)))
    print(sprintf("Mediana: %f", median(dataUFCFinal[[i]], na.rm = T)))
    print(sprintf("MAD: %f", mad(dataUFCFinal[[i]], na.rm = T)))
    print(sprintf("Varianza: %f", var(dataUFCFinal[[i]], na.rm = T)))
    print(sprintf("DesviaciÃ³n estÃ¡ndar: %f", sd(dataUFCFinal[[i]], na.rm = T)))
    #print(colnames(dataUFCFinal[[i]]))
    print("------------------------Â°--------------------------------")
  }
}

# 7. Para cada variable numérica gra???car el histograma de la misma a efectos de poder visualizar la
# distribución de la misma. Utilizar por default 10 intervalos, aunque se puede varíar el número de los
# mismos si se considerase necesario.
# 8. Gra???car el número de encuentros por año, para cada una de las categorías de peso (weight_class).
# 9. Crear una lista de data.frames (u otro tipo de array de datos) donde cada elemento de la lista sea
# un subset del los datos el cual contenga la info relacionada a cada una de las distintas categorías de
# peso. Elegir una de las categorías de peso y crear un nuevo dataset el cual solo contenga los datos
# pertenecientes a dicha categoría. Estos datos van a ser la base a partir de la cual se va a trabajar en los
# siguientes puntos.