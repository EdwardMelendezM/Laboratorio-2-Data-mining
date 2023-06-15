#Librerias
library(cluster)
library(ggplot2)
library(factoextra)


# Ruta del archivo CSV
path <- "data/raw/train.csv"
dataset <- read.csv(path)

# Mostrar las primeras 5 filas del dataset
primeras_filas <- head(dataset, n = 5)
primeras_filas

# PassengerId: Identificación del pasajero.
# Survived: Indicador de si el pasajero sobrevivió (1) o no (0).
# Pclass: Clase en la que viajaba el pasajero (1, 2 o 3).
# Name: Nombre del pasajero.
# Sex: Género del pasajero (male o female).
# Age: Edad del pasajero.
# SibSp: Número de hermanos o cónyuges a bordo.
# Parch: Número de padres o hijos a bordo.
# Ticket: Número de ticket.
# Fare: Tarifa pagada por el pasajero.
# Cabin: Número de cabina.
# Embarked: Puerto de embarque (C = Cherbourg, Q = Queenstown, S = Southampton).


# Tamaño del dataset
dim(dataset)

# Nombres de las columnas
colnames(dataset)

# Tipos de variables
str(dataset)

# Estadísticas descriptivas
summary(dataset)

# ANALISIS DE DATOS

# Analisis univariables
# ----------------------------------------------------------
# Variables numéricas
hist(dataset$Age)
boxplot(dataset$Fare)

# Variables categóricas
table(dataset$Survived)
barplot(table(dataset$Survived))


# Análisis bivariable
# ----------------------------------------------------------
# Variables numéricas vs. numéricas
plot(dataset$Age, dataset$Fare)
cor(dataset$Age, dataset$Fare)


# Variables categóricas vs. categóricas
table(dataset$Survived, dataset$Sex)
chisq.test(dataset$Survived, dataset$Sex)


# Variables numéricas vs. categóricas
boxplot(dataset$Age ~ dataset$Survived)
t.test(dataset$Fare ~ dataset$Survived)


# Análisis multivariable
# ----------------------------------------------------------
# Análisis de correlación múltiple
correlation_matrix <- cor(dataset[, c("Age", "Fare", "SibSp", "Parch")])
heatmap(correlation_matrix)


# Gráficos de dispersión múltiple
pairs(dataset[, c("Age", "Fare", "SibSp", "Parch")])


# Análisis de agrupamiento
# Eliminar filas con valores faltantes
kmeans_data <- na.omit(kmeans_data)

# Verificar y eliminar valores infinitos si es necesario
kmeans_data <- kmeans_data[is.finite(rowSums(kmeans_data)), ]

# Realizar análisis de agrupamiento k-means
kmeans_result <- kmeans(kmeans_data, centers = 3)

# Visualizar resultados
plot_kmeans <- fviz_cluster(kmeans_result, data = kmeans_data, geom = "point")
plot_kmeans

# Guardar la imagen del plot en formato PNG
png("results/plot_kmeans.png")
print(plot_kmeans)
dev.off()


# Visualización de datos

# Gráfico de barras
grafico_barras <- ggplot(dataset, aes(x = Survived)) +
  geom_bar()
# Guardar gráfico de barras
ggsave("results/grafico_barras_survived.png", plot = grafico_barras)


# Gráfico de líneas
grafico_lineas <- ggplot(dataset, aes(x = Age, y = Fare)) +
  geom_line()
# Guardar gráfico de líneas
ggsave("results/grafico_lineas_age_fare.png", plot = grafico_lineas)


# Gráfico de dispersión
grafico_dispersión <- ggplot(dataset, aes(x = Age, y = Fare)) +
  geom_point()
# Guardar gráfico de dispersión
ggsave("results/grafico_dispersión_age_fare.png", plot = grafico_dispersión)

# Análisis temporal (suponiendo que tienes una columna "Fecha")
#library(lubridate)
#dataset$Fecha <- ymd(dataset$Fecha)
#ggplot(dataset, aes(x = Fecha, y = Fare)) +
#  geom_line()


# Pruebas estadísticas (ejemplo con t-test)
t.test(dataset$Age ~ dataset$Survived)
