setwd("C:\\Users\\Admin\\R programmes/")
uncleantraining <- read.csv("titanic.csv")
uncleantest <- read.csv("titanictest.csv")

uncleantraining$Survival <- uncleantraining$Survived
uncleantraining$Survived <- NULL
uncleantraining$Survived <- uncleantraining$Survival
uncleantraining$Survival <- NULL

uncleantest$Survived <- 0 

data <- rbind(uncleantraining, uncleantest)

data$PassengerId=NULL
data$Ticket=NULL
data$Fare=NULL
data$Cabin=NULL
data$Name=NULL
data$Embarked=NULL

age <- as.data.frame(data$Age)
age <- na.omit(age)
age$Aged <- age$`data$Age`

meanAge <- mean(age[, "Aged"])

data$Age[is.na(data$Age)] <- meanAge  

FeatureScaling <- function(x) { ((x - min(x)) / (max(x) - min(x))) }
Normalised <- function(data){FeatureScaling(data)}

data$Pclass <- Normalised(data$Pclass)
data$Age <- Normalised(data$Age)
data$SibSp <- Normalised(data$SibSp)
data$Parch <- Normalised(data$Parch)
data$Sex <- gsub("female", 1, data$Sex)
data$Sex <- gsub("male", 0, data$Sex)

traindata <- data[1:891,]
testdata <- data[892:1309,]

outputtrain <- traindata[, 6]
traindata$Survived <- NULL
traindata <- as.matrix(traindata)
testdata$Survived <- NULL

outputtrain <- to_categorical(outputtrain, 2) 

neural_network <- keras_model_sequential()

neural_network %>% 
  layer_dense(units = 36, activation = 'relu', input_shape = c(5)) %>% 
  layer_dropout(rate = 0.3) %>% 
  layer_dense(units = 22, activation = 'relu') %>%
  layer_dropout(rate = 0.2) %>%
  layer_dense(units = 15, activation = 'relu') %>%
  layer_dropout(rate = 0.1) %>%
  layer_dense(units = 9, activation = 'relu') %>%
  layer_dropout(rate = 0.05) %>%
  layer_dense(units = 2, activation = 'softmax')

neural_network %>% compile(
  loss = 'categorical_crossentropy',
  optimizer = optimizer_rmsprop(),
  metrics = c('accuracy')
)

history <- neural_network %>% fit(
  traindata, outputtrain,
  epochs = 20, batch_size = 128, 
  validation_split = 0.2
)
