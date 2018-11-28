install.packages("class")
library(class)

BreastCancerRaw <- read.csv("C:\\Users\\Admin\\R programmes/BreastCancerData.csv", header = FALSE)

names(BreastCancerRaw) <- c("ID","diagnosis","radius_mean","texture_mean","perimeter_mean","area_mean","smoothness_mean",
                            "compactness_mean","concavity_mean","concave points_mean","symmetry_mean","fractal_dimensions_mean",
                            "radius_se","texture_se","perimeter_se","area_se","smoothness_se","compactness_se",
                            "concavity_se","concave points_se","symmetry_se","fractal_dimensions_se","radius_worst",
                            "texture_worst","perimeter_worst","area_worst","smoothness_worst","compactness_worst",
                            "concavity_worst","concave points_worst","symmetry_worse","fractal_dimensions_worse")

Data_NoID <- BreastCancerRaw[,-1]
Data <- Data_NoID[,-1]

normalising <- function(x){
  ((x - min(x)) / (max(x) - min(x)))
}

NormalData <- as.data.frame(lapply(Data, normalising))

TrainingData <- NormalData[1:400,]
TestData <- NormalData[401:569,]

for(x in c(1:100)){
  kval <- x
  Predictions <- knn(TrainingData, TestData, BreastCancerRaw[1:400, 2], kval)
  Reference <- BreastCancerRaw[401:569, 2]
  print(kval)
  print(table(Predictions, Reference))
}

install.packages("ggplot2")
library(ggplot2)

big_errors <- c()

kValues <- c()

for(i in c(1:100)) {
  
  next_error <-  table(knn(TrainingData,TestData,Data_NoID[1:400,1],k=i),Reference)[1,2] +
    table(knn(TrainingData,TestData,Data_NoID[1:400,1],k=i),Reference)[2,1]
  
  big_errors <- c(big_errors, next_error)
  
  kValues <- c(kValues, i)
}

big_errors_df <- data.frame(kValues,big_errors)
names(big_errors_df) <- c()

ggplot(big_errors_df, aes(x = kValues, y = big_errors)) +
  geom_point() +
  geom_smooth(method = "loess",colour = "blue", size = 1) + 
  ggtitle("Error vs k-Value for Breast Cancer Data") +
  xlab("k-Values") +
  ylab("Error") +
  theme(axis.text.x=element_text(angle=-45, vjust=0.5)) +
  theme(axis.text.y=element_text(angle=-45, hjust=-0.1, vjust=-0.5)) +
  scale_colour_manual(values = c("red","blue"))

