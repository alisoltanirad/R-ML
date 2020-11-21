# https://github.com/alisoltanirad/R-ML.git
# Regression method: SVR
library(caTools)
library(e1071)
library(ggplot2)
install.packages('ggplot2')

main <- function(){
    data = preprocess_data(get_data())
    predictions = train(data)
    evaluate_regressor(data, predictions)
}

evaluate_regressor <- function(data_set, predictions){
    n_items = length(predictions)
    value_sum = 0
    error_sum = 0
    for (i in 1:n_items){
        value_sum = value_sum + data_set$test$Profit[i]
        error_sum = error_sum + abs(predictions[i] - data_set$test$Profit[i])
    }
    
    mean_value = value_sum / n_items
    mean_error = error_sum / n_items
    
    mean_error = mean_error / mean_value
    print(sprintf('Error Percentage: %s', mean_error*100))
}

train <- function(data_set){
    regressor = svm(formula=Profit~.,
                    data=data_set$train,
                    type='eps-regression',
                    kernel='radial')
    predictions = predict(regressor, newdata=data_set$test)
    return(predictions)
}

preprocess_data <- function(data){
    data$State = factor(data$State,
                        levels=c('New York', 'California', 'Florida'),
                        labels=c(1, 2, 3))
    
    split = sample.split(data$Profit, SplitRatio=0.8)
    train_set = subset(data, split==TRUE)
    test_set = subset(data, split==FALSE)
    
    data_set = list(train_set, test_set)
    names(data_set) = c('train', 'test')
    
    return(data_set)
}

get_data <- function(){
    return(read.csv('Sturtups.csv')[3:5])
}

if (!interactive()){
    main()
}