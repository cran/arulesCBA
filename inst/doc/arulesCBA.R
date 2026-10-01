## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(collapse = TRUE, comment = "#>")
library(arulesCBA)
set.seed(1234)

## ----install, eval=FALSE------------------------------------------------------
# install.packages("arulesCBA")

## ----load-package-------------------------------------------------------------
library(arulesCBA)

## ----split-data---------------------------------------------------------------
train_id <- sample(seq_len(nrow(iris)), 100)
iris_train <- iris[train_id, ]
iris_test <- iris[-train_id, ]

table(iris_train$Species)
table(iris_test$Species)

## ----train-model--------------------------------------------------------------
classifier <- CBA(Species ~ ., data = iris_train)
classifier

## ----inspect-rules------------------------------------------------------------
inspect(head(classifier$rules, 5))

## ----rule-quality-------------------------------------------------------------
summary(quality(classifier$rules)[, c("support", "confidence", "lift")])

## ----predict------------------------------------------------------------------
prediction <- predict(classifier, iris_test)
head(prediction)

## ----evaluate-----------------------------------------------------------------
table(predicted = prediction, observed = iris_test$Species)
accuracy(prediction, iris_test$Species)

## ----tune-model---------------------------------------------------------------
classifier_tuned <- CBA(
  Species ~ .,
  data = iris_train,
  support = 0.05,
  confidence = 0.9,
  maxlen = 4
)
classifier_tuned

## ----parameter-list, eval=FALSE-----------------------------------------------
# classifier <- CBA(
#   Species ~ .,
#   data = iris_train,
#   parameter = list(support = 0.05, confidence = 0.9, maxlen = 4)
# )

## ----balanced-support, eval=FALSE---------------------------------------------
# classifier_balanced <- CBA(
#   class ~ .,
#   data = training_data,
#   support = 0.1,
#   confidence = 0.8,
#   balanceSupport = TRUE
# )

## ----prepare-transactions-----------------------------------------------------
iris_transactions <- prepareTransactions(Species ~ ., iris_train)
iris_transactions
inspect(head(iris_transactions, 3))

## ----mine-cars----------------------------------------------------------------
cars <- mineCARs(
  Species ~ .,
  iris_transactions,
  support = 0.1,
  confidence = 0.8,
  maxlen = 4,
  verbose = FALSE
)
cars
inspect(head(cars, 5))

## ----help, eval=FALSE---------------------------------------------------------
# help(package = "arulesCBA")
# ?CBA
# ?mineCARs
# ?CBA_ruleset

