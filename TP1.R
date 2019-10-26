#install.packages("data.table")
#install.packages("stringr")
#install.packages("moments")
library(data.table)
library(stringr)
library(moments)

# 1. Importar el dataset, guardarlo en un objeto bidimensional (puede ser un data.frame, data.table, tibble, etc.)
dataUFC = fread(file.choose(), fill = T, header = T, sep = ",")

# 2. Dividir el dataset original en dos datasets distintos, uno con toda la informacion referente al participante
# de la esquina roja y la informacion en comun del encuentro y otro con la informacion referente al
# participante de la esquina azul y la informacion en común del encuentro.

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
# a) Reemplazar el prefijo que indica el color de la esquina en los nombres de las columnas (R_ o B_) por
# un ” (un campo vacío). Por ejemplo, el nombre de la columna “R_fighter” tiene que pasar a llamarse
# “fighter”. Ambos datasets tendrian que tener nombres de columnas identicos al finalizar.


colnames(blueFighter)<-str_remove_all(colnames(blueFighter),"^B_")
colnames(redFighter)<-str_remove_all(colnames(redFighter),"^R_")

# b) Crear una variable (o recodear la variable “Winner”) que indique si el participante ganó la pelea (1) o
# no (0). Por ejemplo si se está trabajando con los datos del participante de la esquina azul y la variable
# Winner toma el valor de Red, entonces la nueva variable tendria que tomar el valor 0

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

#4. Unir las filas de ambos datasets en uno solo.
dataUFCFinal<- rbind(blueFighter, redFighter) 

#5. Reordenar las columnas del dataset obtenido en el punto anterior de forma tal que la primer columna
# sea la que se calculó en el ítem 3.b (la cual indica si el participante ganó o no la pelea), manteniendo el
# orden de las demás
dataUFCFinal <- setcolorder(dataUFCFinal, "Winner")


# 6.Obtener estadísticas descriptivas básicas para todas las variables continuas: media, desvío, varianza,
# número de observaciones, máximo y mínimo, cuartiles, etc.
statsUFC <- data.frame()
for(i in 1:ncol(dataUFCFinal)){
  if(is.numeric(dataUFCFinal[[i]]) && !is.integer(dataUFCFinal[[i]]))
  {
    statsUFC["media",ncol(statsUFC)+1] <- mean(dataUFCFinal[[i]], na.rm = T)

    statsUFC["mediana", ncol(statsUFC)] <- median(dataUFCFinal[[i]], na.rm = T)
    
    statsUFC["mad", ncol(statsUFC)] <- mad(dataUFCFinal[[i]], na.rm = T)
    
    statsUFC["var", ncol(statsUFC)] <- var(dataUFCFinal[[i]], na.rm = T)
    
    statsUFC["std dev", ncol(statsUFC)] <- sd(dataUFCFinal[[i]], na.rm=T)
    
    statsUFC["kurt", ncol(statsUFC)] <- kurtosis(dataUFCFinal[[i]], na.rm = T)

    statsUFC["Asim", ncol(statsUFC)] <- skewness(dataUFCFinal[[i]], na.rm = T)
    
    statsUFC["min", ncol(statsUFC)] <- min(dataUFCFinal[[i]], na.rm=T)
    statsUFC["1er Q", ncol(statsUFC)] <- quantile(dataUFCFinal[[i]], probs = 0.25, na.rm = T)
    statsUFC["2do Q", ncol(statsUFC)] <- quantile(dataUFCFinal[[i]], probs = 0.5, na.rm = T)
    statsUFC["3er Q", ncol(statsUFC)] <- quantile(dataUFCFinal[[i]], probs = 0.75, na.rm = T)
    statsUFC["max", ncol(statsUFC)] <- max(dataUFCFinal[[i]], na.rm=T)
    
    statsUFC["obs", ncol(statsUFC)] <- length(which(!is.na(dataUFCFinal[[i]])))
    #print(statsUFC["observaciones", ncol(statsUFC)])
    
    colnames(statsUFC)[ncol(statsUFC)] <- colnames(dataUFCFinal)[i]
  }
}


# 7.Para cada variable numérica graficar el histograma de la misma a efectos de poder visualizar la
# distribución de la misma. Utilizar por default 10 intervalos, aunque se puede varíar el número de los
# mismos si se considerase necesario.
histograma<-function(x){
  for (i in 1:ncol(dataUFCFinal)) {
    if(is.numeric(x[[i]])) {
      hist((x),col="red", main="Histograma" ,ylab="Frecuencia" ,xlab="Valores",breaks = 10)  
    }else{
      print("No es Numerico")
    }
  }
}
histograma(dataUFCFinal$Winner)
histograma(dataUFCFinal$avg_CLINCH_landed)

# 8. Graficar el número de encuentros por año, para cada una de las categorías de peso (weight_class).

# 9. Crear una lista de data.frames (u otro tipo de array de datos) donde cada elemento de la lista sea
# un subset del los datos el cual contenga la info relacionada a cada una de las distintas categorías de
# peso. Elegir una de las categorías de peso y crear un nuevo dataset el cual solo contenga los datos
# pertenecientes a dicha categoría. Estos datos van a ser la base a partir de la cual se va a trabajar en los
# siguientes puntos.

# 10. Graficar la distribución, separando los casos que ganaron de los que perdieron (puede ser en 2 gráficos
# separados o dentro del mismo gráfico utilizando colores distintos, o de cualquier forma en la que se
# pueda discriminar los casos que ganaron de los que no) de un mínimo de 4 las siguientes variables :

# 11. Discretizar las variables countinuas del punto anterior, el criterio para definir los intervalos es libre.

# 12. Crear un nuevo dataset el cual va a estar compuesto por la variable que indica si se gano o no el
# encuentro y las variables del punto anterior.

# 13. Transformar las variables del dataset del punto anterior, exepto la que indica si se ganó o perdió, en
# variables dummy (también conocido como one-hot-encoding) en el que para cada nivel de la variable
# se genera una columna la cual indica fila por fila si la variable toma un valor perteneciente a esa
# subcategoría o nivel.

# 14. Con estos nuevos datos (previamente dividiéndolos en una población de entrenamiento y una poblción
# de validación), estimar la probabilidad de ganar el encuentro. Se sugiere utilizar una regresión logística,
# pero se puede utilizar otro tipo de modelos siempre y cuando se comente el motivo detrás de su elección.
# Aclaración: el número de variables regresoras a utlizar es de libre criterio, y si se desease utilizar
# variables que no se encuentren dentro de las listadas, se puede hacer.

# 15. Analizar y comentar sobre los resultados obtenidos en el punto 14.
