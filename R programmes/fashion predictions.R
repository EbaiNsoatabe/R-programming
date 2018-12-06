data <- dataset_fashion_mnist()
summary(data)

str(data$train)
summary(data$test)

train <- data$train$x
outputtrain <- data$train$y
test <- data$test$x
outputtest <- data$test$y

train <- array_reshape(train, c(nrow(train), 784))
test <- array_reshape(test, c(nrow(test), 784))

outputtrain <- to_categorical(outputtrain, 10)
outputtest <- to_categorical(outputtest, 10)

neural_network <- keras_model_sequential()

neural_network %>% 
  layer_dense(units = 784, activation = 'sigmoid', input_shape = c(784)) %>% 
  layer_dropout(rate = 0.4) %>% 
  layer_dense(units = 392, activation = 'sigmoid') %>%
  layer_dropout(rate = 0.3) %>%
  layer_dense(units = 196, activation = 'sigmoid') %>%
  layer_dropout(rate = 0.2) %>%
  layer_dense(units = 98, activation = 'sigmoid') %>%
  layer_dropout(rate = 0.1) %>%
  layer_dense(units = 10, activation = 'softmax')

neural_network %>% compile(
  loss = 'categorical_crossentropy',
  optimizer = optimizer_rmsprop(),
  metrics = c('accuracy')
)

history <- neural_network %>% fit(
  train, outputtrain,
  epochs = 30, batch_size = 128, 
  validation_split = 0.2
)

prediction <- neural_network %>% predict_classes(test)
prediction <- to_categorical(prediction, 10)
prediction[prediction==outputtest] <- 1
prediction[prediction!=outputtest] <- 0
print(c("Correct out of 10000:", sum(prediction)))
