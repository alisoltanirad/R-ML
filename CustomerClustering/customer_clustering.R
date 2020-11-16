# https://github.com/alisoltanirad/R-ML.git
library(cluster)

main <- function(){
    dataset = get_dataset()
    clusters = get_clusters(dataset)
    plot_clusters(dataset, clusters)
}


plot_clusters <- function(dataset, clusters){
    clusplot(dataset, clusters, lines=0, shade=TRUE, color=TRUE, labels=TRUE,
             plotchar=FALSE, span=TRUE, main=paste('Clusters of Customers'),
             xlab='Annual Income', ylab='Spending Score')
}


get_clusters <- function(dataset){
    return(kmeans(x=dataset, centers=5)$cluster)
}


get_cluster_numbers <- function(dataset){
    wcss = vector()
    for (i in 1:10) wcss[i] = sum(kmeans(dataset, i)$withinss)
    plot(1:10, wcss, type='b', main=paste('The Elbow Method'), 
         xlab='Number of Clusters', ylab='WCSS')
}


get_dataset <- function(){
    return(read.csv('Mall_Customers.csv')[4:5])
}


if (!interactive()){
    main()
}
