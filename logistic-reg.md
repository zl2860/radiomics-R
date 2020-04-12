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
set.seed(886)
ctrl = trainControl(method = "repeatedcv", number = 5, repeats = 5,
                    classProbs = TRUE,
                    summaryFunction = twoClassSummary)

data = read_csv('./outcome_2020_4.csv') %>%
  mutate(PCR = factor(PCR),
         cTNM = factor(cTNM))
levels(data$PCR) = c("N", "Y")

rowTrain = createDataPartition(y = data$PCR,
                               p = 0.85, # 划分训练测试集
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
    ##          N 9 0
    ##          Y 2 3
    ##                                           
    ##                Accuracy : 0.8571          
    ##                  95% CI : (0.5719, 0.9822)
    ##     No Information Rate : 0.7857          
    ##     P-Value [Acc > NIR] : 0.3960          
    ##                                           
    ##                   Kappa : 0.6585          
    ##                                           
    ##  Mcnemar's Test P-Value : 0.4795          
    ##                                           
    ##             Sensitivity : 1.0000          
    ##             Specificity : 0.8182          
    ##          Pos Pred Value : 0.6000          
    ##          Neg Pred Value : 1.0000          
    ##              Prevalence : 0.2143          
    ##          Detection Rate : 0.2143          
    ##    Detection Prevalence : 0.3571          
    ##       Balanced Accuracy : 0.9091          
    ##                                           
    ##        'Positive' Class : Y               
    ## 

## ROC curve

``` r
roc.glm = roc(data$PCR[-rowTrain],test.pred.prob)
```

    ## Setting levels: control = N, case = Y

    ## Setting direction: controls < cases

``` r
plot(roc.glm,
     legacy.axes = TRUE,
     print.auc = T)
```

![](logistic-reg_files/figure-gfm/unnamed-chunk-4-1.png)<!-- -->
