#!/usr/bin/Rscript

trainingData = read.csv("train.csv")
novelData = read.csv("test.csv")

# The following function will train my neural network with the testData
trainer <- function(testData) {
	weights = runif(n = 10, min = 0, max = 1)
	return (weights)
}

# Giving this function a trained network (represented by weights) as well as
# a datum to analyze, it returns a 1 or 0 representing Survived or Died 
# respectively
classifier <- function(datum, weights) {
	return (1)
}

weights = trainer(trainingData)

output <- data.frame(PassengerId = integer(), Survived = integer())

# Construct the output data:
for (x in 1:(nrow(novelData))) {
	output <- rbind(output, c(
    novelData[x,"PassengerId"],
    classifier(novelData[x,])
  ))
}


print (output)


