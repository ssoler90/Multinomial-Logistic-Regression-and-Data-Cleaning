rm(list = ls())

library(nnet)

# Cargamos el archivo CSV
datos <- read.csv("mobile_train.csv")

# Mostramos las primeras filas del conjunto de datos
head(datos)

# Calculamos y mostramos la matriz de correlaciones
matriz_correlaciones <- cor(datos)
print(matriz_correlaciones)

# Calculamos y mostramos la frecuencia de cada valor en 'price_range'
frecuencia_price_range <- table(datos$price_range)
print(frecuencia_price_range)

# Fijamos una semilla para reproducibilidad
set.seed(123)

# Creamos un vector con los índices de las filas
indices <- 1:nrow(datos)

# fijamos el tamaño del conjunto de entrenamiento al 80%
tamaño_entrenamiento <- round(0.8 * nrow(datos))

# Seleccionamos aleatoriamente los índices para el conjunto de entrenamiento
indices_entrenamiento <- sample(indices, tamaño_entrenamiento, replace = FALSE)

# Obtenemos los índices para el conjunto de prueba
indices_prueba <- setdiff(indices, indices_entrenamiento)

# Creamos conjuntos de entrenamiento y prueba usando los índices
datos_entrenamiento <- datos[indices_entrenamiento, ]
datos_prueba <- datos[indices_prueba, ]


# Ajustamos el modelo de regresión logística multinomial
modelo <- multinom(price_range ~ ., data = datos_entrenamiento)

# Mostramos el resumen del modelo
summary(modelo)

# Realizamos predicciones en el conjunto de entrenamiento
predicciones_entrenamiento <- predict(modelo, datos_entrenamiento, type = "class")

# Calculamos la precisión de los datos de entrenamiento (accuracy)
precision_entrenamiento <- mean(predicciones_entrenamiento == datos_entrenamiento$price_range)  
print(precision_entrenamiento)

# Realizamos predicciones en el conjunto de datos de prueba
predicciones_pruebas <- predict(modelo, newdata = datos_prueba, type = "class")

# Calcular la precisión (accuracy)
precision_pruebas <- mean(predicciones_pruebas == datos_prueba$price_range)  
print(precision_pruebas)

#### MODELO 2 (Solo con la variable ram) #####

modelo2 <- multinom(price_range ~ ram, data = datos_entrenamiento)

# Mostramos el resumen del modelo
summary(modelo2)

# Realizamos predicciones en el conjunto de entrenamiento
predicciones_entrenamiento2 <- predict(modelo2, datos_entrenamiento, type = "class")

# Calculamos la precisión de los datos de entrenamiento (accuracy)
precision_entrenamiento2 <- mean(predicciones_entrenamiento2 == datos_entrenamiento$price_range)  
print(precision_entrenamiento2)

# Realizamos predicciones en el conjunto de datos de prueba
predicciones_pruebas2 <- predict(modelo2, newdata = datos_prueba, type = "class")

# Calcular la precisión (accuracy)
precision_pruebas2 <- mean(predicciones_pruebas2 == datos_prueba$price_range)  
print(precision_pruebas2)

###### MODELO 3 ("battery_power", "pc", "px_height", "px_width", "ram") #####

# Seleccionamos las columnas de interés
selected_data <- datos_prueba[, c("battery_power", "pc", "px_height", "px_width", "ram", "price_range")]

# Verificar la estructura de los datos seleccionados
str(selected_data)

# Modelado usando solo esas variables
modelo3 <- multinom(price_range ~ ., data = selected_data)

# Resumen del modelo
summary(modelo3)

# Realizamos predicciones en el conjunto de entrenamiento
predicciones_entrenamiento3 <- predict(modelo3, datos_entrenamiento, type = "class")

# Calculamos la precisión de los datos de entrenamiento (accuracy)
precision_entrenamiento3 <- mean(predicciones_entrenamiento3 == datos_entrenamiento$price_range)  
print(precision_entrenamiento3)

# Realizamos predicciones en el conjunto de datos de prueba
predicciones_pruebas3 <- predict(modelo3, newdata = datos_prueba, type = "class")

# Calcular la precisión (accuracy)
precision_pruebas3 <- mean(predicciones_pruebas3 == datos_prueba$price_range)  
print(precision_pruebas3)