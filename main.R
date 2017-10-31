#!/usr/bin/Rscript

trainingData = read.csv("train.csv")
novelData = read.csv("test.csv")

trainer <- function(testData) {
	weights = runif(n = 10, min = 0, max = 1)
	return (weights)
}

classifier <- function(datum, weights) {
	return (1)
}

weights = trainer(trainingData)

for (x in 1:(nrow(novelData))) {
	print(classifier(datum, weights))
}


