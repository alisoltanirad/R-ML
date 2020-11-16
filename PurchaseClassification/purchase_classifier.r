# https://github.com/alisoltanirad/R-ML.git
# Classification method: Random Forest
library(caTools)
library(randomForest)

main <- function(){
    data = preprocess_data(get_data())
    predictions = classify(data)
    evaluate_classifier(data_set, predictions)
}

classify <- function(data_set){
    print(data_set['train'][-1])
    classifier = randomForest(x=data_set['train'][-3],
                              y=data_set['train'][-1],
                              ntree=500)
    print('flag')
    predictions = predict(classifier, newdata=data_set['test'][-3])
    return(predictions)
}

evaluate_classifier <- function(data_set, predictions){
    cm = table(data_set['test'][3], predictions)
    print(cm)
}

preprocess_data <- function(data){
    data$Purchased = factor(data$Purchased, levels=c(0, 1))
    
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
