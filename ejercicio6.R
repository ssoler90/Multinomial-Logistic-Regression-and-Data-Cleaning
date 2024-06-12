rm(list=ls())

install.packages("titanic")
install.packages("mice")
install.packages("caret")

library(titanic)
data("titanic_train")

# En busca de valores nulos
colSums(is.na(titanic_train))

# Obtenemos 177 valores nulos en la columna, la primera alternativa que tendríamos es 
# deshacernos de ellos, sin embargo intituivamente sabemos que la edad pudo jugar un papel
# fundamental a la hora de sobrevivir, así que en lugar de borrarlos vamos a tratar de 
# reemplazarlos por valores que se adapten lo mejor posible a las cualidades del pasajero.

#Visualizamos los valores de Age que son na
View(titanic_train[is.na(titanic_train$Age),])

set.seed(40)
# Creamos una copia con los datos nu nulos y pasamos la columna Sex a numérica
titanic_train_1 <- titanic_train
titanic_train_1 <- titanic_train_1[!is.na(titanic_train_1$Age),]
titanic_train_1$Sex_numeric <- ifelse(titanic_train_1$Sex == "female", 0, 1)


cor(titanic_train_1[,c("Age","Pclass","SibSp","Parch","Fare", "Sex_numeric")])

library(mice)

# Creamos un nuevo conjunto que contiene solo las columnas "Age", "Pclass" y "SibSp"
simple <- titanic_train[c("Age","Pclass","SibSp")]

# Utilizamos la función mice() para realizar la imputación múltiple en el conjunto de datos "simple"
imp_mice <- mice(simple,seed=144,m=5,maxit=10)

# Muestra los valores imputados para la columna "Age" del conjunto de datos imputados.
imp_mice$imp$Age

imputed <- complete(imp_mice,1)
imputed

# Reemplazamos la columna "Age" en el conjunto de datos original "titanic_train" con los valores imputados obtenidos del paso anterior.
titanic_train$Age <- imputed$Age

#####DUPLICADOS#####
any(duplicated(titanic_train))
#NO HAY DUPLICADOS

####COHERENTES#####
colSums(titanic_train=='')

#La cantidad de nulos de Cabin hace que borremos esa columna
#Investigamos los datos de Embarked

str(titanic_train$Cabin)

# Eliminamos la columna Cabin
titanic_train <- titanic_train[, -which(names(titanic_train) == 'Cabin')]

# Miramos la cantidad de datos diferentes de Embarked
table(titanic_train$Embarked)

#Eliminamos los datos vacíos de Embarked
titanic_train <- titanic_train[titanic_train$Embarked != '', ]

#ESTUDIO COLUMNA POR COLUMNA

#-------- PassengerId ------------------- 
# Eliminamos la columna PassengerId por no aportar información
titanic_train <- titanic_train[, -which(names(titanic_train) == 'PassengerId')]

#-------- Survived -------------------
table(titanic_train$Survived)
# Todo en orden

#--------- PClass ---------------------
table(titanic_train$Pclass)
# Todo en orden

#---------Name-------------------
head(titanic_train$Name, 100)
# Es posible hacer Feature Enginering para poder extraer titulos y de ahí ver si influye en la supervivencia
# Por nuestra parte no vamos a trabajar con la columna Name.
titanic_train <- titanic_train[, -which(names(titanic_train) == 'Name')]

#-------Sex--------------------- 
table(titanic_train$Sex)
# Todo en orden
# Convertimos la Columna Sex en variable numérica, 1 para female y 0 para male 
titanic_train$Sex <- ifelse(titanic_train$Sex == 'female', 1, 0)

#------- Age ------------------
hist(titanic_train$Age, breaks = 25, col = "skyblue", xlab = "Age", ylab = "Frecuencia", main = "Histograma de Age")
# Todo en Orden

#------- SibSp -------------------
table(titanic_train$SibSp)
hist(titanic_train$SibSp, breaks = 25, col = "skyblue", xlab = "SibSp", ylab = "Frecuencia", main = "Histograma de SibSp")
# Todo en orden

#-------- Parch ------------------
table(titanic_train$Parch)
hist(titanic_train$Parch, breaks = 25, col = "skyblue", xlab = "Parch", ylab = "Frecuencia", main = "Histograma de Parch")
# Todo en orden

#-------- Ticket -----------------
head(titanic_train$Ticket, 20)
# Habría que investigar más que relación tienen los codigos de números y letras, sin embargo decidimos prescindir de la columna
titanic_train <- titanic_train[, -which(names(titanic_train) == 'Ticket')]

#-------- Fare -------------------
boxplot(titanic_train$log_Fare)
# vemos la presencia de outliers

# Histograma de la columna 'Fare'
hist(titanic_train$Fare, breaks = 25, col = "skyblue", xlab = "Fare", ylab = "Frecuencia", main = "Histograma de Fare")

# Histograma logarítmico de la columna 'Fare'
hist(log(titanic_train$Fare + 1), breaks = 20, col = "skyblue", xlab = "log(Fare + 1)", ylab = "Frecuencia", main = "Histograma logaritmico de Fare")

# Al aplicar logaritmo la presencia de outliers se "suaviza" así que sustituimos la columna Fare por su equivalente logaritmico
titanic_train$log_Fare <- log1p(titanic_train$Fare)

# Borramos la columna Fare
titanic_train <- titanic_train[, -which(names(titanic_train) == 'Fare')]


#----------- Embarked ------------
unique(titanic_train$Embarked)

# Convertimos la columna 'Embarked' en variables dummy
dummies_embarked <- model.matrix(~ Embarked - 1, data = titanic_train)

# Unimos las variables dummy al conjunto de datos
titanic_train <- cbind(titanic_train, dummies_embarked)

# Borramos la columna Embarked
titanic_train <- titanic_train[, -which(names(titanic_train) == 'Embarked')]

#----------------SCALING--------------------------
library(caret)

# Definimos el preprocesamiento con el método de escalado Min-Max
preprocess <- preProcess(titanic_train, method = "range")

# Aplicamos el preprocesamiento a los datos
scaled_data <- predict(preprocess, titanic_train)


# Ajustamos el modelo de regresión logística multinomial
model <- multinom(Survived ~ ., data = scaled_data)

# Ver el resumen del modelo
summary(model)

# Obtenemos las predicciones del modelo
predictions <- predict(model, scaled_data, type = "class")

# Comparamos las predicciones con los valores reales
correct_predictions <- sum(predictions == scaled_data$Survived)

# Calculamos la precisión
accuracy <- correct_predictions / nrow(scaled_data)