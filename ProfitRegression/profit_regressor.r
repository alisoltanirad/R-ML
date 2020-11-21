# https://github.com/alisoltanirad/R-ML.git
# Regression method: SVR
library(caTools)
library(e1071)

get_data <- function(){
    return(read.csv('Sturtups.csv')[3:5])
}

if (!interactive()){
    main()
}
