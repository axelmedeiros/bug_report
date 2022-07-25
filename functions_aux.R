
fun_createDatasetSetting01 <- function(dataset) {
  dataset = dataset %>%
    mutate(quality = factor(case_when(date_diff <= 24 ~ 'D0_1',
                                      date_diff > 24 & date_diff <= 240 ~ 'D02_10',
                                      date_diff > 240 & date_diff <= 720 ~ 'D11_30',
                                      date_diff > 720 & date_diff <= 1440 ~ 'D31_60',
                                      date_diff > 1440 ~ 'D61_+')))
  
  return(dataset)
}

fun_createDatasetSetting02 <- function(dataset) {
  dataset = dataset %>%
    mutate(quality = factor(case_when(date_diff <= 240  ~ 'D0_10',
                                      date_diff >= 240 & date_diff <= 720 ~ 'D11_30',
                                      date_diff >= 720 & date_diff <= 1440 ~ 'D31_60',
                                      date_diff >= 1440 ~ 'D61_+')))
  
  return(dataset)
}

fun_createDatasetSetting03 <- function(dataset) {
  dataset = dataset %>%
    mutate(quality = factor(case_when(date_diff <= 360  ~ 'D0_15',
                                      date_diff >= 360 & date_diff <= 744 ~ 'D16_31',
                                      date_diff >= 744 & date_diff <= 1128 ~ 'D32_47',
                                      date_diff >= 1128 ~ 'D48_+')))
  return(dataset)
}


fun_checkFeatureSelect <- function(dataset) {
  marsModel <- earth(quality ~
                       bugId + 
                       severity + 
                       status + 
                       classification + 
                       comment_count + 
                       Priority + 
                       Platform_size + 
                       Platform_numberLines + 
                       Preconditions_size + 
                       Preconditions_numberLines +
                       Steps_to_reproduce_size +
                       Steps_to_reproduce_numberLines +
                       Expected_result_size +
                       Expected_result_numberLines +
                       Actual_result_numberLines +
                       Reproducible_size +
                       Reproducible_numberLines +
                       User_Agent_size + 
                       endTime +
                       initialTime +
                       lastEditTime, 
                     data=dataset) # build model
  ev <- evimp (marsModel)
  return(ev)
}

fun_checkFeatureSelectOpen <- function(dataset) {
  marsModel <- earth(quality ~ 
                       bugId + 
                       severity + 
                       status + 
                       classification + 
                       comment_count + 
                       Priority + 
                       Platform_size + 
                       Platform_numberLines + 
                       Preconditions_size + 
                       Preconditions_numberLines +
                       Steps_to_reproduce_size +
                       Steps_to_reproduce_numberLines +
                       Expected_result_size +
                       Expected_result_numberLines +
                       Actual_result_numberLines +
                       Reproducible_size +
                       Reproducible_numberLines +
                       User_Agent_size + 
                       User_Agent_numberLines +
                       lastEditTime +
                       initialTime,
                     data=dataset) # build model
  ev <- evimp (marsModel)
  return(ev)
}

fun_sliptDataset <- function(dataset) {
  set.seed(2021)
  return(initial_split(dataset))
}

fun_checkValuesNA <- function(dataset) {
  na_count1 <-sapply(dataset, function(y) sum(length(which(is.na(y)))))
  na_count1 <- data.frame(na_count1)
  return(na_count1)
}

plotConfusionMatrix <- function(fit, datasetTest) {
  augment(fit, new_data = datasetTest) %>%
    conf_mat(truth = quality, estimate = .pred_class) %>%
    autoplot(type = "heatmap") + labs(y="Prediction", x="Truth")
}

plotMetrics <- function(fit, test, metrics) {
  augment(fit, new_data = test) %>%
    metrics(truth = quality, estimate = .pred_class) 
}

plotClassMetrics <- function(dataset) {
  results = ml_test(dataset$predict, dataset$quality,  output.as.table = TRUE)
  results = results %>%
    select(precision, recall, F1)
  return(results)
}


savePdf <- function(filename, dataset) {
  pdf(filename,height=5, width=10)
  grid.table(dataset)
  invisible(dev.off())
}


createPredictDataset <- function(fit, datasetTest) {
  predictDataframe = datasetTest
  predictDataframe$obs = datasetTest$quality
  
  predictDataframe$predict = as.data.frame(predict(fit, new_data = datasetTest))$.pred_class
  return(predictDataframe)
}

