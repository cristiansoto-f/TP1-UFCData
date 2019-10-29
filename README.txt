Actualizaciones TP.
29/10
	Simplificado el código del ejercicio 8 y completo.
	Agregado el ejercicio 9 y elegida la categoria de peso medio, ya que es la que dispone de más información.

	Agrego librería para probar un par de cosas y simplificar.
	Cambio un poco el acceso al data table para los histogramas (en vez de el número de columna, busca el nombre de variable).
	Remplazo el ingreso manual de categorías por una función de extracción.	
	Elimino una variable redundante.
	Renombro un par de variables para claridad.
	Agrego gráfico de Stance. Faltan detalles, pero está la base.
	Más detalles, quedan solo arreglos estéticos.
	Agrego gráfico de age.
	Modifico los gráficos para ser más acordes a la consigna.
	Agregados gráficos de wins y losses, por lo que el punto 10 estaría terminado. Hay arreglos estéticos que se pueden hacer más tarde.
	Agregadas en comentario las variables continuas a discretizar para el 11.
	Agrego librería y función para mostrar más de 1 gráfico.

26/10
	Agregué ejercicio 7.
	Consideraciones: El histograma del tiempo de pelea muestra un valor sumamente alto en cierto punto, que todavía no 
	establecí y no tengo idea de por qué.
	Agregué gráficos por categorias punto 8, para esto fue necesaria el paquete dplyr, no agregué todos, debe haber alguna 		forma de mostrar los distintos graficos con un loop. No he encontrado la forma de contabilizar cada categoría en cada 		año de una sola vez.
	Si alguno encuentra la forma de hacerlo y generar un pdf por ejemplo, estaría buenisimo, de lo contrario más adelante se 	le agregarán cada categoria "manualmente"

 26/10
	Agregue el ejercicio 8, fijenese si está bien o quieren modificar algo  

26/10
	Simplifiqué un poco el cálculo de cuartiles.
	Agregué la cantidad de observaciones. Por la diferencia entre magnitudes, la tabla en r-studio muestra los valores en notación científica, pero están bien.
	Simplifiqué el 3c. 
	Simplifiqué el 2.

 25/10 
 	-Corregí escritura del ejercicio, utilizar utf-8 para poder visualizar las tildes correctamente y guardarlo bajo ese 		formato.
	-Agregué un par de medidas de descripcion y eliminé los printf.
	
 
 25/10 
	Agregue al ejercicio 6 la kurtosis y la asimetria, es necesario cargar el paquete moments
Para el ejercicio 7 hice una funcion donde solo pones la variable que queres ya que no se como hacer para que te tire todos los histogramas juntos
 
 25/10
	Agrego un data frame para guardar las estadísticas. Incluyo cuartiles y max y min. Falta el numero de observaciones. Agrego una excepción para evitar campos de enteros (la consigna dice "continuas"), pero, por lo visto, campos como "longest_win_streak", están guardados como continuos.


 24/10
	Arreglé el tema de la columna extra en el ejercicio 4.
 
  23/10
  	El ejercicio 6 se encuentra incompleto dado que no todas las variables son continuas, no pude encontrar una función que me   indique si los valores de las columnas son float. Intenté con la libreria float y la función is.float pero no hubo resultado.
Faltan más medidas de descripción.



