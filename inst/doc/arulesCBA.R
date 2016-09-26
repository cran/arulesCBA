### R code from vignette source 'arulesCBA.rnw'

###################################################
### code chunk number 1: arulesCBA.rnw:129-130
###################################################
library(arulesCBA)


###################################################
### code chunk number 2: arulesCBA.rnw:142-144
###################################################
data(iris)
iris.disc <- as.data.frame(lapply(iris[1:4], function(x) discretize(x, categories=9)))


###################################################
### code chunk number 3: arulesCBA.rnw:149-150
###################################################
iris.disc$Species <- iris$Species


###################################################
### code chunk number 4: arulesCBA.rnw:155-156
###################################################
head(iris.disc)


###################################################
### code chunk number 5: arulesCBA.rnw:162-163
###################################################
classifier <- CBA(iris.disc, "Species", supp = 0.05, conf=0.9)


###################################################
### code chunk number 6: arulesCBA.rnw:170-171
###################################################
print(classifier)


###################################################
### code chunk number 7: arulesCBA.rnw:179-180
###################################################
rules(classifier)


###################################################
### code chunk number 8: arulesCBA.rnw:185-186
###################################################
inspect(rules(classifier))


###################################################
### code chunk number 9: arulesCBA.rnw:192-193
###################################################
classes <- predict(classifier, iris.disc)


###################################################
### code chunk number 10: arulesCBA.rnw:198-200
###################################################
head(classes)
table(classes)


###################################################
### code chunk number 11: arulesCBA.rnw:208-211
###################################################
library(gmodels)
CrossTable(classes, iris.disc$Species,           
           prop.chisq = FALSE, prop.r = FALSE, prop.c = FALSE)


