---
title: "Project"
author: "Derek Garcia"
date: "2023-04-20"
output: html_document
---

```{r setup, include=TRUE}
knitr::opts_chunk$set(echo = TRUE)

library(MASS)
library(tidyverse)
library(ROCR)
library(class)

```

```{r}
mlb_pitchers <- read.csv(file = ".csv", sep = ",", header = TRUE, fileEncoding = "LATIN1")

#View(mlb_pitchers)

mlb_pitchers$Cy.Young <- as.factor(mlb_pitchers$Cy.Young)

mlb_pitchers$Cy.Young.Finalist <- as.factor(mlb_pitchers$Cy.Young.Finalist)

mlb_pitchers <- na.omit(mlb_pitchers)

summary(mlb_pitchers)

```

#### Setting train/test sets for mlb pitchers ####

```{r}
train_set = mlb_pitchers |> dplyr::filter(mlb_pitchers$Year < 2018)
test_set = mlb_pitchers |> dplyr::filter(mlb_pitchers$Year >= 2018 & mlb_pitchers$Year!=2020)


```


#### Fitting LDA

```{r}
lda.fit = lda(Cy.Young ~ IP + Lgs + QS + BQR + sDR + lDR + Pit.GS, data = train_set)

lda.fit

lda.pred = predict(lda.fit, train_set)

lda.class = lda.pred$class

table(lda.class, Cy_Young_Winner = train_set$Cy.Young)

mean(lda.class == train_set$Cy.Young)

table(train_set$Cy.Young)/nrow(train_set)

lda_pred = predict(lda.fit, train_set)
lda_pred_post = lda_pred$posterior[,2]
pred = prediction(lda_pred_post, train_set$Cy.Young)
perf = performance(pred, "tpr", "fpr")
plot(perf, main = "ROC Curve for LDA")
abline(0, 1, lty = 3)
```

- This is a very unbalanced data set because 2% of pitchers in the training set having a chance at winning the Cy Young over 98% of pitchers not having a chance at winning



#### Fitting QDA 

```{r}
qda.fit = qda(Cy.Young ~ IP + Lgs + QS + BQR + sDR + lDR + Pit.GS, data = train_set)

qda.fit

qda.pred = predict(qda.fit)

qda.class = qda.pred$class

table(qda.class, train_set$Cy.Young)

mean(qda.class == train_set$Cy.Young)

qda_pred = predict(qda.fit, train_set)
qda_pred_post = qda_pred$posterior[,2]
pred = prediction(qda_pred_post, train_set$Cy.Young)
perf = performance(pred, "tpr", "fpr")
plot(perf, main = "ROC Curve for QDA")
abline(0, 1, lty = 3)

```



