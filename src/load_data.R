setwd("~/projects/predmachlearn-011")
pml_training <- read.csv("data/pml-training.csv")
summary(pml_training)

# summary() show that there are empty values in fields as "min_yaw_belt" and
# "#DIV/0!" values in fields as "min_yaw_forearm" that make that these fields
# are taken as factor. So, this values has to be read as NA

pml_training <- read.csv("data/pml-training.csv", na.strings=c("NA","#DIV/0!",""))
summary(pml_training) # no more incorrect factor fields

# Some columns had a lot of missing values and the others none
# So take only columns without missing values

library(ggplot2)
qplot(1:ncol(pml_training), colSums(is.na(pml_training)), xlab = "col", ylab = "# of NAs")

predictors <- colnames(pml_training)
predictors <- predictors[colSums(is.na(pml_training)) == 0] # columns without missing values

# First seven columns are description, time, window data, not relevant
predictors <- predictors[-(1:7)]

# From 160 to 53 columns

# All the remaining predictors are not near zero variance
nearZeroVar(pml_training[, predictors], saveMetrics = TRUE)

pml_training <- pml_training[, predictors]

pml_testing <- read.csv("data/pml-testing.csv", na.strings=c("NA","#DIV/0!",""))
pml_testing <- pml_testing[, predictors[1:52]]
