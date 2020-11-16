# https://github.com/alisoltanirad/R-ML.git
# Classification method: Random Forest
library(caTools)

main <- function(){
    #
}

preprocess_data <- function(data){
    data_set$Purchased = factor(data$Purchased, levels=c(0, 1))
    
    split = sample.split(data$Purchased, SplitRatio=0.75)
    train_set = subset(data, split==TRUE)
    test_set = subset(data, split==FALSE)
    
    train_set[-3] = scale(train_set[-3])
    test_set[-3] = scale(test_set[-3])
    
    data_set = c(train_set, test_set)
    names(data_set) = c('train', 'test')
    return(data_set)
}

get_data <- function(){
    return(read.csv('Social_Network_Ads.csv')[3:5])
}

if (!interactive()){
    main()
}