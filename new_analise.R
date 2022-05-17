---
  title: "R Notebook"
output: html_notebook
---
  
  This is an [R Markdown](http://rmarkdown.rstudio.com) Notebook. When you execute code within the notebook, the results appear beneath the code. 

Try executing this chunk by clicking the *Run* button within the chunk or by placing your cursor inside it and pressing *Ctrl+Shift+Enter*. 

```{r}
#plot(cars)
rm(dataset)
```

Add a new chunk by clicking the *Insert Chunk* button on the toolbar or by pressing *Ctrl+Alt+I*.

When you save the notebook, an HTML file containing the code and output will be saved alongside it (click the *Preview* button or press *Ctrl+Shift+K* to preview the HTML file).

The preview shows you a rendered HTML copy of the contents of the editor. Consequently, unlike *Knit*, *Preview* does not run any R code chunks. Instead, the output of the chunk when it was last run in the editor is displayed.


Sumário dos dados

```{r}
library(ggplot2)
library(tidyverse)
#library(ggpubr)
library(dplyr)
library(tidymodels) # metapackage for ML 
library(stacks) # stack ML models for better perfomance

theme_set(theme_dark())
#doParallel::registerDoParallel(cores = 4) # parallel computations

# alternatively, this also loads %>%
#theme_set(theme_pubr())
```

```{r}
library(rpart) # para arvores de regressão
#library(randomForest) # para Random Forests
library(rattle)
library(dplyr)
library(rpart.plot) # para arvores de regressão
library(modelr)
library(baguette)
library(finetune)
library(doParallel) 
library(bloom)
```


```{r}
summary(bug_report)

```


```{r}
summary(teste_v2)

```

```{r}
#teste_v2$date_diff <- as.numeric(as.Date(as.character(teste_v2$endTime), format="%Y-%m-%d")-
#              as.Date(as.character(teste_v2$initialTime), format="%Y-%m-%d"))


all_reports$date_diff <- as.numeric(as.Date(as.character(all_reports$endTime), format="%Y-%m-%d")-
                                      as.Date(as.character(all_reports$initialTime), format="%Y-%m-%d"))

all_reports$resolution <- NULL
```


```{r}
#closed = all_reports %>%
#      filter(!is.na(date_diff))

closed = all_reports %>% 
  filter(!is.na(date_diff))



open = all_reports %>% 
  filter(is.na(date_diff))
```


```{r}
#bkpClosed = closed
#bkpClosed$TimeQualify <- if date_diff <= 1 = 'FAST'
#bkpClosed[date_diff <= 1] <- 'FAST'
bkpClosed = bkpClosed %>%
  mutate(quality = case_when(date_diff <= 1 ~ 'FAST',
                             date_diff > 1 & date_diff < 11 ~ 'NORMAL',
                             date_diff >= 11 & date_diff < 31 ~ 'LAZY',
                             date_diff >= 31~ 'VERY LAZY'))

```



```{r}
mset <- metric_set(accuracy) # metric is accuracy
control <- control_grid(save_workflow = TRUE,
                        save_pred = TRUE,
                        extract = extract_model) 
```






```{r}
splitClosed <- resample_partition(closed, c(test = 0.3, train = 0.7))

#fit <- rpart(date_diff ~ ., data = splitClosed$train$data)

```

```{r}
dt_rec <- recipe(date_diff ~ ., data = splitClosed$train$data)
dt_model <- bag_tree(cost_complexity = tune(),
                     tree_depth = tune(),
                     min_n = tune()) %>% # param to be tuned
  set_engine("rpart", times = 25) %>% # nb bootstraps
  set_mode("regression") 

dt_wf <- 
  workflow() %>% 
  add_model(dt_model) %>% 
  add_recipe(dt_rec)
```

```{r}

set.seed(2021)
spl <- initial_split(closed)
train <- training(spl)
test <- testing(spl)

train_5fold <- train %>%
  vfold_cv(5)


#folds <- vfold_cv(closed, v = 5)


```


```{r}
dt_tune <- dt_wf %>%
  tune_race_anova(
    train_5fold,
    grid = 30,
    param_info = dt_model %>% parameters(),
    metrics = metric_set(accuracy),
    control = control_race(verbose_elim = TRUE))
```


```{r}
reg_tree_spec <- tree_spec %>%
  set_mode("regression")

reg_tree_fit <- fit(reg_tree_spec, date_diff ~ ., splitClosed$train$data)


```

```{r}
reg_tree_fit

```

```{r}
augment(reg_tree_fit, new_data = splitClosed$test$data) %>%
  rmse(truth = date_diff, estimate = .pred)
```


```{r}
rpart.plot(fit)
```

```{r}
plot(fit, uniform = TRUE, main="Árvore de Regressão")
text(fit, use.n=TRUE, all=FALSE, cex=.8)
```





```{r}
lapply(splitData, dim)
```


```{r}
#head(splitClosed$test$data)
print(predict(fit, splitClosed$test))

```

```{r}
mae(model = fit, data = splitClosed$test)
```

```{r}
summary(splitClosed$test$data$date_diff)
```
```{r}

```


```{r}
head(closed)
```





```{r}
#open$date_diff = NULL
open = open %>%
  filter(!is.na(lastEditTime))
```

```{r}
open$time_online <- as.numeric(as.Date(as.character(open$lastEditTime), format="%Y-%m-%d")-
                                 as.Date(as.character(open$initialTime), format="%Y-%m-%d"))
```



```{r}
splitOpen <- resample_partition(open, c(test = 0.3, train = 0.7))

fit <- rpart(time_online ~ ., data = splitOpen$train$data)
```

```{r}
summary(splitOpen$test$data$time_online)
```



```{r}
print(predict(fit, splitOpen$test))

```


```{r}
mae(model = fit, data = splitOpen$test)

```










```{r}
library(randomForest)
library(modelr)
```

```{r}
splitData <- resample_partition(all_reports, c(test = 0.3, train = 0.7))

```


```{r}
final <- na.roughfix(splitData) 
```

```{r}
fit.randomForest <- randomForest(date_diff ~ .,
                                 data=splitData$train, 
                                 importance=TRUE, 
                                 ntree=2000,
                                 prOximity=TRUE,
                                 na.action=na.roughfix)
```






```{r}





#a <- ggplot(data=teste_v2, aes(y = date_diff, x = date_diff, group=1))
#a <- ggplot(teste_v2, aes(y = date_diff, x = seq(1, length(teste_v2$date_diff)))) + geom_point()
a <- ggplot(bug_report, aes(y = date_diff, x = seq(1, 1000))) + geom_point()

show(a)
#factor(bug_report$date_diff)
```


```{r}
#ggplot(teste_v2, aes(date_diff)) +
#  geom_bar(fill = "#0073C2FF") +
#  theme_pubclean()

quantile(bug_report$date_diff, probs = seq(.1, .95, by = .05))
```
```{r}
#bug_reports %>% 
#    ggplot(aes(x = date_diff)) + 
#    geom_histogram(fill = "coral", color = "black") 

set.seed(14)
hist(bug_report$date_diff, main = "Histograma Tempos em dias", xlab = "Dias")
```
```{r}
rm teste
```

```{r}
sapply(teste_v2, class)
```

```{r}
teste_v2$xxxx <- as.numeric(teste_v2$Platform_size, replace = TRUE)

```

```{r}
#teste_v2$xxxx <- if(!is.numeric(teste_v2$Platform_size)) 0 else as.numeric(teste_v2$Platform_size)
teste_v2$`Steps to reproduce_numberLines` <- as.numeric(teste_v2$`Steps to reproduce_numberLines`) 
#teste_v2$xxxx <- if(is.na(teste_v2$xxxx)) 0

#> DF1[is.na(DF1)] = 0
teste_v2$`Steps to reproduce_numberLines`[is.na(teste_v2$`Steps to reproduce_numberLines`)] =  0
show(teste_v2$`Steps to reproduce_numberLines`)


```
```{r}
#filter(teste_v2, teste_v2$date_diff <= 35)
#show(teste_v2$date_diff)
alvo<-filter(teste_v2$,date_diff <= 35)
```

```{r}


cor(teste_v2$comment_count, teste_v2$date_diff) 
cor(teste_v2$xxxx, teste_v2$date_diff) 
cor(teste_v2$`Steps to reproduce_numberLines`, teste_v2$date_diff) 

```
```{r}

new = teste_v2[['reproduce_numberLines', 'reproduce_numberLines']]
#novo = teste_v2[['comment_count', 'date_diff', 'reproduce_numberLines']].copy()
cor(new) # Corr matrix

```
```{r}
bug_reports <- bug_reports %>%
  replace_with_na(replace = list(Platform_size  = "None", comment_count  = "None", 
                                 Platform_numberLines  = "None", Preconditions_size  = "None", Preconditions_numberLines  = "None", 
                                 `Steps to reproduce_size`  = "None", `Steps to reproduce_numberLines` = "None", 
                                 `Actual result_size` = "None", `Actual result_numberLines` = "None", Reproducible_size  = "None", 
                                 Reproducible_numberLines  = "None", `User Agent_size`  = "None", Platform_size  = "None", 
                                 Platform_size  = "None", Platform_size  = "None", `User Agent_numberLines` = "None", 
                                 `_size`  = "None", `_numberLines`  = 'None', Platform_text = "None", Preconditions_text = "None", 
                                 `Expected result_text` = "None", `Steps to reproduce_size` = "None", `Expected result_numberLines` = "None", 
                                 `Actual result_text` = "None", Reproducible_text = "None", `User Agent_text` = "None", `_text` = "None", 
                                 `Expected result_size` = "None", `Steps to reproduce_text` = "None"))
```

```{r}
res_time_steps_fixed <- bug_report %>% 
  filter(resolution == "INVALID") 

res_time_steps_fixed <- cor.test(res_time_steps_fixed$date_diff, res_time_steps_fixed$`Steps to reproduce_size`, 
                                 method = "pearson")
res_time_steps_fixed

```

```{r}
bug_report %>% 
  filter(!is.na(date_diff), !is.na(`Steps to reproduce_size`)) %>% 
  ggscatter(x = "total_time", y = "Steps to reproduce_size",           add = "reg.line", conf.int = TRUE, 
            cor.coef = TRUE, cor.method = "pearson",
            xlab = "Time (h)", ylab = "Total caracteres on S2R")
```



```{r}
summary(bug_report)
```



```{r}


plot(predict(fit, splitClosed$test), splitOpen$test$data$time_online)
```


