#Classification Based on Association Rules

[![Travis-CI Build Status](https://api.travis-ci.org/ianjjohnson/arulesCBA.svg?branch=master)](https://travis-ci.org/ianjjohnson/arulesCBA)
[![CRAN RStudio mirror downloads](http://cranlogs.r-pkg.org/badges/arulesCBA)](https://cran.r-project.org/package=arulesCBA)
[![CRAN version](http://www.r-pkg.org/badges/version/arulesCBA)](https://cran.r-project.org/package=arulesCBA)

This R package performs association rule-based classification.

It includes two classification algorithms.

The first is the CBA algorithm described in Liu, et al. 1998. (CBA)
The second is a weighted majority-vote based algorithm which is currently being designed and tested. (bCBA)

Both algorithms create classifiers based on association rules and can then use those classifiers to classify incoming datasets.

The algorithms are implemented in C, and data is formatted and passed to the C implementations via an R interface accessible through this package.
