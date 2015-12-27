---
title: "Classifying the Properness of Weight Lifting Acitivity"
author: "A Coursera Student"
date: "December 27, 2015"
output: html_document
---

<br><br>
<h4><b><u>BACKGROUND</u></b></h4>

<br><br>
<h4><b><u>Loading the Training Dataset</u></b></h4>

```{r}
library(caret)
rawData <- read.csv(file='./data/pml-training.csv', na.strings=c(""," ","NA"))
dim(rawData)
```
