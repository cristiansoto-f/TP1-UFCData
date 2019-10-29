#install.packages("data.table")
#install.packages("stringr")
#install.packages("moments")
#install.packages("ggplot2")
#install.packages("dplyr")
library(data.table)
library(stringr)
library(moments)
library(ggplot2)
library(dplyr)

# 1. Importar el dataset, guardarlo en un objeto bidimensional (puede ser un data.frame, data.table, tibble, etc.)
dataUFC = fread(file.choose(), fill = T, header = T, sep = ",")

# 2. Dividir el dataset original en dos datasets distintos, uno con toda la informacion referente al participante
# de la esquina roja y la informacion en comun del encuentro y otro con la informacion referente al
# participante de la esquina azul y la informacion en comÃºn del encuentro.

redFighter <- data.table()
blueFighter <- data.table()

for (i in 1:ncol(dataUFC)){
  if (!str_detect(names(dataUFC[,..i]), "^B_")){
    redFighter <- cbind(redFighter, dataUFC[,..i])
  }
  if (!str_detect(names(dataUFC[,..i]), "^R_")) {
    blueFighter <- cbind(blueFighter, dataUFC[,..i])
  }
}

# 3. Para los dos datasets obtenidos en el item anterior
# a) Reemplazar el prefijo que indica el color de la esquina en los nombres de las columnas (R_ o B_) por
# un '' (un campo vacío). Por ejemplo, el nombre de la columna "R_fighter" tiene que pasar a llamarse
# "fighter". Ambos datasets tendrian que tener nombres de columnas identicos al finalizar.

colnames(blueFighter) <- str_remove_all(colnames(blueFighter),"^B_")
colnames(redFighter) <- str_remove_all(colnames(redFighter),"^R_")

# b) Crear una variable (o recodear la variable “Winner”) que indique si el participante ganó la pelea (1) o
# no (0). Por ejemplo si se está trabajando con los datos del participante de la esquina azul y la variable
# Winner toma el valor de Red, entonces la nueva variable tendria que tomar el valor 0

for(i in 1:nrow(redFighter)){
  redFighter[i,"Winner"] <- (dataUFC[i,"Winner"] == "Red")
  blueFighter[i,"Winner"] <- (dataUFC[i,"Winner"] == "Blue")
}

# c) Crear una variable que haga referencia al color de la esquina de cada dataset.
for (i in 1:nrow(redFighter)) {redFighter[i,"color"]<-"Red"}
for (i in 1:nrow(blueFighter)) {blueFighter[i,"color"]<-"Blue"}

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
    
    colnames(statsUFC)[ncol(statsUFC)] <- colnames(dataUFCFinal)[i]
  }
}


# 7.Para cada variable numérica graficar el histograma de la misma a efectos de poder visualizar la
# distribución de la misma. Utilizar por default 10 intervalos, aunque se puede varíar el número de los
# mismos si se considerase necesario.

# Dada la magnitud de los histogramas a calcular, se mostrarán los que se consideran
# más interesantes aparte  y luego mediante un loop todos, en un PDF.

# Histogramas más importantes 

#Edad
#ggplot(data=dataUFCFinal, aes(dataUFCFinal[[76]])) + 
#  geom_histogram(color = "black", fill = "palegreen1", bins = 33, na.rm = T) +
#  labs(x = "Edad", y="Cantidad", title = "Histograma edad") +
#  xlim(c(18,51))

ggplot(data=dataUFCFinal, aes(dataUFCFinal[[76]])) + 
  geom_histogram(bins = 33, na.rm = T, aes(fill=..count..)) +
  labs(x = "Edad", y="Cantidad", title = "Histograma edad") +
  xlim(c(18,51)) +
  scale_fill_gradient("Cantidad", low = "green", high = "red")

#Peso (en libras)
#El siguiente grafico no refleja algunos outliers de luchadores pesados
ggplot(data=dataUFCFinal, aes(dataUFCFinal[[75]])) + 
  geom_histogram(bins = 30, na.rm = T, aes(fill=..count..)) +
  labs(x = "Peso", y="Cantidad", title = "Histograma peso (en libras)") +
  xlim(c(115,350)) +
  scale_fill_gradient("Cantidad", low = "green", high = "red")

#Altura
ggplot(data=dataUFCFinal, aes(dataUFCFinal[[73]])) + 
  geom_histogram(color = "black", fill = "palegreen1", bins = 25, na.rm = T) +
  labs(x = "Altura (cm)", y="Cantidad", title = "Histograma altura") +
  xlim(c(150,211))

# Tiempo de pelea
#muchas parecen terminar en un momento X, por qué?
ggplot(data=dataUFCFinal, aes(dataUFCFinal[[63]])) + 
  geom_histogram(color = "black", fill = "palegreen1", bins = 200, na.rm = T) +
  labs(x = "Tiempo(seg)", y="Cantidad", title = "Histograma tiempo total de pelea") +
  #xlim(c(150,211))

# Todos los histogramas

histogram_list = list() #Se generarán 420mb de datos
for(i in 1:ncol(dataUFCFinal)){
  if(is.numeric(dataUFCFinal[[i]]))
  {
    h = (ggplot(data=dataUFCFinal, aes(dataUFCFinal[[i]])) + 
      geom_histogram(color = "black", fill = "palegreen1", bins = 10, na.rm = T) +
      labs(x = sprintf("%s", colnames(dataUFCFinal)[i]), y="Cantidad"))
    histogram_list[[i]] = h
  }
}

pdf("histogramas.pdf")
for(i in 1:ncol(dataUFCFinal)){
  if(is.numeric(dataUFCFinal[[i]]))
  {
    print(histogram_list[[i]])
  }
}
dev.off()
rm(h)
rm(histogram_list)

# 8. Graficar el número de encuentros por año, para cada una de las categorías de peso (weight_class).
# Es necesario trabajar con dataUFC dado que dataUFCFinal contabiliza dos veces los datos en común
# por el hecho de haber separado a los peleadores y unirlos en filas (punto 4)
categorias<-data.frame(dataUFC$weight_class,year(dataUFC$date))
names(categorias)=c("peso","año")
ggplot(data=categorias, aes(x=peso,fill= as.factor(año)))  + geom_bar() 

#PDF con el numero de encuentros por año para cada categoría
cat = c("Women's Strawweight", "Women's Flyweight", "Women's Featherweight", "Women's Bantamweight", "Welterweight", "Open Weight", "Middleweight", "Lightweight", "Light Heavyweight", "Heavyweight", "Flyweight", "Featherweight", "Catch Weight", "Bantamweight")
count_list = list()
for(i in 1:length(cat))
{
  weigh.class = dataUFC[, .(weight_class == cat[i], year(dataUFC$date))]
  tab_sum = weigh.class %>% group_by(V2) %>%
    filter(V1) %>%
    summarise(trues = n())
  
  p = ggplot(tab_sum, aes(V2, trues)) + 
    geom_line() +
    geom_point(size = 2, color = "red") +
    labs(x = "Año", y="Cantidad de Peleas", title = sprintf("Categoria: %s", cat[i]))
  count_list[[i]] = p
}

pdf("cantidadpeleas.pdf")
for(i in 1:length(cat)){
    print(count_list[[i]])
}
dev.off()
rm(p)
rm(count_list)
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
