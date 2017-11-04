#!/usr/bin/Rscript

trainingData = read.csv("train.csv", header=TRUE, stringsAsFactors=FALSE)
novelData = read.csv("test.csv", header=TRUE, stringsAsFactors=FALSE)
numWeights <- 6

# The following function takes the novelData and "cleans" it, including 
# engineering new features. For now, I simply throw away non-numerical data
#
# Preconditions: 
#
# Postconditions: cleanData will have 7 columns. 
cleaner <- function(dataset) {
  cleanData = dataset

  cleanData[,"Name"] <- NULL

  sex = cleanData[,"Sex"]
  sex <- replace(sex, sex=="male", 1)
  sex <- replace(sex, sex=="female", 0)
  cleanData[,"Sex"] <- sex

  # What to do with blank ages? For now, call them 0. Worry about guessing
  # ages later.
  cleanData[,"Age"][is.na(cleanData[,"Age"])] <- 0

  numPeople <- cleanData[,"SibSp"] + cleanData[,"Parch"]
  cleanData[,"numPeople"] <- numPeople
  cleanData[,"SibSp"] <- NULL
  cleanData[,"Parch"] <- NULL

  cleanData[,"Ticket"] <- NULL
  cleanData[,"Cabin"] <- NULL

  # I might think of a clever way to arrange these in a way that makes sense;
  # i.e. in a way that each port corresponds to the order they were stopped at
  # or corresponding each port to geographical coordinates or something. Maybe
  # I could also do some research to think of a sensible way to measure distance
  # between these.
  embarked <- cleanData[,"Embarked"]
  embarked <- replace(embarked, embarked == "Q", 1)
  embarked <- replace(embarked, embarked == "S", 2)
  embarked <- replace(embarked, embarked == "C", 3)
  cleanData[,"Embarked"] <- embarked

  return (cleanData)
}

# The following function will train my neural network with the testData
trainer <- function(testData) {
  # Clean testData, and separate out the Survived column. 
  testData <- cleaner(testData)
  survived <- testData[,"Survived"]
  testData[,"Survived"] <- NULL

  # The magic number numWeights is dependent on the number of columns that cleaner()
  # returns
  weights = runif(n = numWeights, min = 0, max = 1)

  # Backpropagation:
  for (row in 1:nrow(testData)) {
    class <- classifier(as.numeric(as.vector(testData[row,])),weights)
    roundClass <- (class < .5 && 0 || 1)
    if (roundClass != survived[row]) {
      # Send the error backwards proportional to the weights
      error <- survived[row] - class 
      error <- error / numWeights
      errorVector <- error * weights
      weights <- weights + errorVector
      #weights <- sapply(weights, sigmoid)
    }
  }
  
  return (weights)
}

sigmoid <- function (x) {
  return (1/(1+exp(-x)))
}

# Giving this function a trained network (represented by weights) as well as
# a datum to analyze, it returns a 1 or 0 representing Survived or Died 
# respectively
#
# Precondition: datum and weights must be vectors of the same length.
classifier <- function(datum, weights) {
  if (length(weights) != length(datum) - 1) {
    print ("Error. The size of data in classifier doesn't match the weights
            table")
    print (paste("length of weights: ", length(weights)))
    print (paste("length of datum:", length(datum)))
  }

  # Get rid of PassengerId
  datum <- datum[-1]

  # Normalize datum
  datum <- sapply(datum, sigmoid)

  # For now, implement only a one layered network
  class <- sum(datum * weights)
  class <- sigmoid (class)

  # Round to 0 or 1. TODO: Check to see whether you'd expect 1 or 0 to appear
  # more often. Were there more survivors or more unsurvivors? Take a burden
  # off of the training algorithm maybe by choosing to return 1 or 0. For now,
  # just round. Keep it simple, stupid.
  #
  # EDIT: actually, just have this return a number between 0 and 1. Make user
  # do rounding. This allows training.
  # return (class < .5 && 0 || 1)
  return (class)
}

# Construct output data, print result, and exit
weights = trainer(trainingData)
output <- data.frame(PassengerId = integer(), Survived = integer())
cleanData <- cleaner(novelData)

for (row in 1:nrow(cleanData)) {
  # classify
  class <- classifier( as.numeric(as.vector(cleanData[row,])), weights)

  # append to output
  output[row,"PassengerId"] <- cleanData[row,"PassengerId"]
  output[row,"Survived"] <- class
}

#for (x in 1:(nrow(novelData))) {
#  output <- rbind(output, c(
#    novelData[x,"PassengerId"],
#    classifier(novelData[x,], weights)
#  ))
#}
print (output)

