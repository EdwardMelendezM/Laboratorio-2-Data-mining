# LIMPIEZA DE DATOS DE TITANIC
library(caret)
# Ruta del archivo CSV
path <- "data/raw/train.csv"
dataset <- read.csv(path)

# Mostrar las primeras 5 filas del dataset
head(dataset, n = 5)

#-----------------------------------------------------------------
# VERIFICAR DATOS FALTANTES

# Verificar si hay valores faltantes en cada columna
colSums(is.na(dataset))

# Contar el número total de valores faltantes
sum(is.na(dataset))

# Eliminar filas con valores faltantes
dataset <- dataset[complete.cases(dataset), ]

# Imputar valores faltantes con la media de la variable "Age"
dataset$Age <- ifelse(is.na(dataset$Age), mean(dataset$Age, na.rm = TRUE), dataset$Age)

# Eliminar las columnas irrelevantes
dataset <- subset(dataset, select = -c(PassengerId, Name, Ticket, Cabin))

#-----------------------------------------------------------------

# IDENTIFICACION DE DATOS ATIPICOS
# Identificar valores atípicos en la variable "Fare" utilizando el método de los límites Tukey
Q1 <- quantile(dataset$Fare, 0.25)
Q3 <- quantile(dataset$Fare, 0.75)
IQR <- Q3 - Q1
upper_bound <- Q3 + 1.5 * IQR
lower_bound <- Q1 - 1.5 * IQR

# Eliminar valores atípicos en la variable "Fare"
dataset <- dataset[dataset$Fare < upper_bound & dataset$Fare > lower_bound, ]

# Reemplazar valores atípicos en la variable "Fare" con el límite superior
dataset$Fare <- ifelse(dataset$Fare > upper_bound, upper_bound, dataset$Fare)

# Reemplazar valores atípicos en la variable "Fare" con el límite inferior
dataset$Fare <- ifelse(dataset$Fare < lower_bound, lower_bound, dataset$Fare)

#-----------------------------------------------------------------
# Normalizar los datos
normalized_data <- as.data.frame(scale(dataset[, c("Age", "Fare")]))

# Agregar las columnas normalizadas al dataset original
dataset[, c("Age", "Fare")] <- normalized_data

## Realizar la normalización min-max
# preprocessed_data <- preProcess(dataset[, c("Age", "Fare")], method = c("range"))
# normalized_data <- predict(preprocessed_data, newdata = dataset[, c("Age", "Fare")])4

#-----------------------------------------------------------------

#CONVERTIR DATOS A BINARIOS (CATEGORICOS)
# Binarizar la columna "Sex"
dataset$Sex <- ifelse(dataset$Sex == "male", 1, 0)

# Convertir valores de la columna "Embarked" en numéricos
dataset$Embarked <- as.numeric(factor(dataset$Embarked, levels = c("S", "C", "Q")))

# Verificar que los valores se hayan convertido correctamente
unique(dataset$Embarked)

#-----------------------------------------------------------------
# Guardar el dataset procesado
pathClean <- "data/proceseed/dataset_clean.csv"
write.csv(dataset, file = pathClean, row.names = FALSE)


