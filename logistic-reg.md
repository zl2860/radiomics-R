prediction\_model
================
Zongchao Liu
4/11/2020

# Import data

``` r
data = read_csv('./outcome_2020_4.csv') %>%
  mutate(PCR = factor(PCR),
         cTNM = factor(cTNM))
levels(data$PCR) = c("N", "Y")
```

# A glance at the difference in radscores (Y = 1, N = 0)

``` r
data %>%
  ggplot(aes(x = radscore, fill = PCR)) +
  geom_histogram() +
  facet_grid(PCR ~ .) +
  ggsci::scale_fill_jama() +
  theme_bw()
```

    ## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.

![](logistic-reg_files/figure-gfm/unnamed-chunk-2-1.png)<!-- -->

# The following blocks are for data partition & modeling using logistic reg

``` r
set.seed(1)
ctrl = trainControl(method = "repeatedcv", number = 5, repeats = 5,
                    classProbs = TRUE,
                    summaryFunction = twoClassSummary)

data = read_csv('./outcome_2020_4.csv') %>%
  mutate(PCR = factor(PCR),
         cTNM = factor(cTNM))
levels(data$PCR) = c("N", "Y")

rowTrain = createDataPartition(y = data$PCR,
                               p = 0.9, # 划分训练测试集
                               list = F)

log.fit = train(x = data[rowTrain,c(4:6,8:14,16)], #选定变量，除去了姓名，性别，cTNM
                y = data$PCR[rowTrain],
                method = "glm",
                metric = "ROC",
                trControl = ctrl
                )


test.pred.prob = predict(log.fit, newdata = data[-rowTrain,], type = "prob")[,2]
test.pred = rep("N", length(test.pred.prob ))
test.pred[test.pred.prob > 0.5] = "Y"

roc.glm = roc(data$PCR[-rowTrain], test.pred.prob)
caret::confusionMatrix(data = factor(test.pred,levels = c("N","Y")),
                       reference = data$PCR[-rowTrain],
                       positive = "Y")
```

    ## Confusion Matrix and Statistics
    ## 
    ##           Reference
    ## Prediction N Y
    ##          N 7 1
    ##          Y 0 1
    ##                                           
    ##                Accuracy : 0.8889          
    ##                  95% CI : (0.5175, 0.9972)
    ##     No Information Rate : 0.7778          
    ##     P-Value [Acc > NIR] : 0.372           
    ##                                           
    ##                   Kappa : 0.6087          
    ##                                           
    ##  Mcnemar's Test P-Value : 1.000           
    ##                                           
    ##             Sensitivity : 0.5000          
    ##             Specificity : 1.0000          
    ##          Pos Pred Value : 1.0000          
    ##          Neg Pred Value : 0.8750          
    ##              Prevalence : 0.2222          
    ##          Detection Rate : 0.1111          
    ##    Detection Prevalence : 0.1111          
    ##       Balanced Accuracy : 0.7500          
    ##                                           
    ##        'Positive' Class : Y               
    ## 

``` r
roc.glm = roc(data$PCR[-rowTrain],test.pred.prob)
plot(roc.glm,
     legacy.axes = TRUE,
     print.auc = T)
```

![](logistic-reg_files/figure-gfm/unnamed-chunk-3-1.png)<!-- -->
