# https://github.com/alisoltanirad/R-ML.git
# Regression method: SVR
library(caTools)
library(e1071)

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
