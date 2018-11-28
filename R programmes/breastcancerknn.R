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

for(x in c(1:50)){
  kval <- x
  Predictions <- knn(TrainingData, TestData, BreastCancerRaw[1:400, 2], kval)
  Reference <- BreastCancerRaw[401:569, 2]
  print(kval)
  print(table(Predictions, Reference))
}