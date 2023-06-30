library(keras)


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

# ---------------------------------------------------------------
X <- dataset_train %>% select(!Survived) %>% 
  mutate(across(everything(), as.integer)) %>% as.matrix()
head(X)

library(caret)

# Convertir la variable objetivo en codificación de variables dummy
formula <- as.formula("Survived ~ .")
y <- dummyVars(formula, data = dataset_train) %>%
  predict(dataset_train)

head(y)

 #------------------------------------------------------------
# Convertir las variables predictoras en una matriz de datos
X_test <- dataset %>%
  select(-Survived) %>%
  mutate(across(everything(), as.integer)) %>%
  as.matrix()

# Convertir la variable objetivo en codificación de variables dummy
y_test <- model.matrix(Survived ~ . - 1, data = dataset)

head(X_test)
head(y_test)

library(keras)

model <- keras_model_sequential() %>%
  layer_dense(units = 10, activation = 'relu', input_shape = c(ncol(X_test)),
              kernel_regularizer = regularizer_l2(l = 0.01)) %>%
  layer_dense(units = ncol(y_test), activation = 'softmax')

model %>% compile(loss = 'categorical_crossentropy', optimizer = 'adam', metrics = 'accuracy')


