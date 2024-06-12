rm(list=ls())

install.packages("titanic")

library(titanic)
data("titanic_train")
data("titanic_test")

df <- data.frame(titanic_train)
df_test <- data.frame(titanic_test)
summary(df)
str(df)
colSums(is.na(df ))
colSums(df=='')
unique(df$Sex)
table(df$Sex)

df$Sexf <- factor(df$Sex)
df$Sexfn = as.numeric(df$Sexf)

df$Sexfn1 <- (df$Sexfn==1)*1
df$Sexfn2 <- (df$Sexfn==2)*1

summary(glm(Survived~Sexfn1, family=binomial, data=df))
summary(glm(Survived~Sexfn2, family=binomial, data=df))


modelo <- glm(Survived ~ Sexfn1, family = binomial, data = df)

# Obtenemos las predicciones del modelo
predicciones <- predict(modelo, type = "response")

# Convertimos las probabilidades en etiquetas binarias (0 o 1)
predicciones_binarias <- ifelse(predicciones > 0.5, 1, 0)

# Calculamos la precisión (accuracy)
precision <- mean(predicciones_binarias == df$Survived)
precision

sigmoid <- function(z){1/(1+ exp(-z))}

# Cálculo de la probabilidad de sobrevivir sabiendo que es hombre
prob_sobrevivir <- mean(df$Survived == 1)
prob_hombre <- mean(df$Sexfn2 == 1)
prob_hombre_dado_sobrevivio <- mean(df$Sexfn2[df$Survived == 1] == 1)
prob_sobrevivir_dado_hombre <- (prob_hombre_dado_sobrevivio * prob_sobrevivir) / prob_hombre

# Cálculo de la probabilidad de sobrevivir sabiendo que es mujer
prob_sobrevivir <- mean(df$Survived == 1)
prob_mujer <- mean(df$Sexfn1 == 1)
prob_mujer_dado_sobrevivio <- mean(df$Sexfn1[df$Survived == 1] == 1)
prob_sobrevivir_dado_mujer <- (prob_mujer_dado_sobrevivio * prob_sobrevivir) / prob_mujer

# Presición del modelo
prob_mujer*prob_sobrevivir_dado_mujer + (1 - prob_mujer)*(1 - prob_sobrevivir_dado_hombre)