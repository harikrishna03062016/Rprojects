---
title: "R Notebook"
output:
  pdf_document: default
  html_notebook: default
  html_document: default
---

installing required packages


```r
#install.packages("rpart")
#install.packages("rpart.plot")
```

loading required packages

```r
library(rpart)
library(rpart.plot)
```

loading data

```r
heart = read.csv('heart.csv')
heart$heart.disease = factor(heart$heart.disease)
head(heart)
```

```
##   age sex pain.type  BP cholestrol fbs resting.ecg max.heart.rate
## 1  70   1         4 130        322   0           2            109
## 2  67   0         3 115        564   0           2            160
## 3  57   1         2 124        261   0           0            141
## 4  64   1         4 128        263   0           0            105
## 5  74   0         2 120        269   0           2            121
## 6  65   1         4 120        177   0           0            140
##   exercise.angina ST.depression ST.slope flouroscopy.coloured thal
## 1               0           2.4        2                    3    3
## 2               0           1.6        2                    0    7
## 3               0           0.3        1                    0    7
## 4               1           0.2        2                    1    7
## 5               1           0.2        1                    1    3
## 6               0           0.4        1                    0    7
##   heart.disease  X
## 1             2 NA
## 2             1 NA
## 3             2 NA
## 4             1 NA
## 5             1 NA
## 6             1 NA
```

constructing a decision tree

```r
tree = rpart(heart.disease~., data=heart)
prp(tree)
```

![](heartDiseaseDecisionTree_files/figure-latex/unnamed-chunk-4-1.pdf)<!-- --> 

