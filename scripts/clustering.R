# LIBRERIAS
library(tidyverse)
library(caret)



#Importar data
path <- "data/proceseed/dataset_clean_train.csv"
dataset <- read.csv(path)

dataset <- as_tibble(dataset)
dataset

# Los datos de prueba no se utilizan en el proceso de creación del modelo y deben
# reservarse exclusivamente para probar el modelo una vez que se haya creado por completo.
# Aquí uso el 80% para entrenar.
inTrain <- createDataPartition(y = dataset$Survived, p = .8, list = FALSE)
dataset_train <- dataset %>% slice(inTrain)
dataset_test <- dataset %>% slice(-inTrain)

train_index <- createFolds(dataset_train$Survived, k = 10)
# -------------------------------------------------------------------------
# Árbol de inferencia condicional (Árbol de decisión)
ctreeFit <- dataset_train %>% train(Survived ~ .,
                                    method = "ctree",
                                    data = .,
                                    tuneLength = 5,
                                    trControl = trainControl(method = "cv", indexOut = train_index))
ctreeFit
plot(ctreeFit$finalModel)

# Guardar como imagen PNG
png("results/modelo_arbol_de_decision.png")
plot(ctreeFit$finalModel)
dev.off()

# -------------------------------------------------------------------------
# 1.2.2. - C 4.5 Decision Tree

# Cargar el paquete RWeka
if (!require(RWeka)) {
  install.packages("RWeka")
  library(RWeka)
}

# Ajustar el modelo J48 en el nuevo conjunto de datos "dataset"
C45Fit <- dataset_train %>% train(Survived ~ .,
                                  method = "J48",
                                  data = .,
                                  tuneLength = 5,
                                  trControl = trainControl(method = "cv", indexOut = train_index))

C45Fit

# --------------------------------------------------------------------
# 1.2.3. - K-vecinos más cercano 
knnFit <- dataset_train %>% train(Survived ~ .,
                                  method = "knn",
                                  data = .,
                                  preProcess = "scale",
                                  tuneLength = 5,
                                  tuneGrid = data.frame(k = 1:10),
                                  trControl = trainControl(method = "cv", indexOut = train_index))

knnFit
knnFit$finalModel

# --------------------------------------------------------------------
# 1.2.5. - Linear Support Vector Machines
svmFit <- dataset_train %>% train(Survived ~.,
                              method = "svmLinear",
                              data = .,
                              tuneLength = 5,
                              trControl = trainControl(method = "cv", indexOut = train_index))
svmFit
svmFit$finalModel


# --------------------------------------------------------------------
# 1.2.6. - Random Forest
randomForestFit <- dataset_train %>% train(Survived ~ .,
                                       method = "rf",
                                       data = .,
                                       tuneLength = 5,
                                       trControl = trainControl(method = "cv", indexOut = train_index))
randomForestFit
randomForestFit$finalModel

# --------------------------------------------------------------------
# 1.2.7. - Gradient Boosted Decision Trees (xgboost)
xgboostFit <- dataset_train %>% train(Survived ~ .,
                                  method = "xgbTree",
                                  data = .,
                                  tuneLength = 5,
                                  trControl = trainControl(method = "cv", indexOut = train_index),
                                  tuneGrid = expand.grid(
                                    nrounds = 20,
                                    max_depth = 3,
                                    colsample_bytree = .6,
                                    eta = 0.1,
                                    gamma=0,
                                    min_child_weight = 1,
                                    subsample = .5
                                  ))
xgboostFit

xgboostFit$finalModel

# --------------------------------------------------------------------
# 1.2.8. - Artificial Neural Network

nnetFit <- dataset_train %>% train(Survived ~ .,
                               method = "nnet",
                               data = .,
                               tuneLength = 5,
                               trControl = trainControl(method = "cv", indexOut = train_index),
                               trace = FALSE)
nnetFit
nnetFit$finalModel



# --------------------------------------------------------------------
#                COMPARACION DE MODELOS
# --------------------------------------------------------------------

resamps <- resamples(list(
  ctree = ctreeFit,
  SVM = svmFit,
  KNN = knnFit,
  randomForest = randomForestFit,
  xgboost = xgboostFit,
  NeuralNet = nnetFit
))

# --------------------------------------------------------------------
# Resumen de resultados

summary(resamps)

# Graficar modelos
library(lattice)
bwplot(resamps, layout = c(3, 1))


# Hallar diferencias
difs <- diff(resamps)
difs
summary(difs)

# --------------------------------------------------------------------
# 1.4. - Aplicación del modelo elegido a los datos de prueba


# Convertir la variable objetivo en factor con los mismos niveles
dataset_test$Survived <- factor(dataset_test$Survived)
pr <- factor(pr, levels = levels(dataset_test$Survived))

# Ver resultados de la matriz de confusión
confusionMatrix(pr, reference = dataset_test$Survived)



