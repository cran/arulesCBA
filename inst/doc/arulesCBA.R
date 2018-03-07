### R code from vignette source 'arulesCBA.rnw'

###################################################
### code chunk number 1: arulesCBA.rnw:129-130
###################################################
library(arulesCBA)


###################################################
### code chunk number 2: arulesCBA.rnw:139-140
###################################################
data(iris)


###################################################
### code chunk number 3: arulesCBA.rnw:146-147
###################################################
classifier <- CBA(Species ~ ., iris, supp = 0.05, conf=0.9)


###################################################
### code chunk number 4: arulesCBA.rnw:154-155
###################################################
print(classifier)


###################################################
### code chunk number 5: arulesCBA.rnw:163-164
###################################################
rules(classifier)


###################################################
### code chunk number 6: arulesCBA.rnw:169-170
###################################################
inspect(rules(classifier))


###################################################
### code chunk number 7: arulesCBA.rnw:176-177
###################################################
classes <- predict(classifier, iris)


###################################################
### code chunk number 8: arulesCBA.rnw:182-184
###################################################
head(classes)
table(classes)


###################################################
### code chunk number 9: arulesCBA.rnw:192-195
###################################################
library(gmodels)
CrossTable(classes, iris$Species,
           prop.chisq = FALSE, prop.r = FALSE, prop.c = FALSE)


