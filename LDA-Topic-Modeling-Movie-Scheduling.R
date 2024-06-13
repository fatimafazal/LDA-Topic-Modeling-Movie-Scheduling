#' ---
#' title: "LDA-Topic-Modeling-Movie-Scheduling"
#' author: "Fatima Fazal"
#' date: "`r format(Sys.Date(), '%B %d, %Y')`"
#' output:
#'   html_document:
#'     toc: true
#'     toc_depth: 3
#' ---

###### Script: Movie_Analysis.R

###### R script for for analyzing movie similarities

# Requires the following files:
#  opus_movies.txt              Movie characteristics of wide releases from 2006-2014
#  opus_movielens_tags.txt      Keywords that describe the movie from MovieLens
#  opus_keywords.txt            Keywords that describe the movie from Opus
#
# The data included for this exercise is for internal use only and
# may not be posted or distributed further.
# Specifically the files opus_movies.txt and opus_keywords.txt
# is data that is provided by The Numbers (http://www.the-numbers.com),
# powered by OpusData (http://www.opusdata.com).
# The opus_movielens_tags.txt is available from Movielens
# which is located at http://grouplens.org/datasets/movielens/latest


##################### setup environment  ######################

# setup environment
if (!require(stringr)) {install.packages("stringr"); library(stringr)}
if (!require(plyr)) {install.packages("plyr"); library(plyr)}

# setup environment, make sure this library has been installed
if (!require(tree)) {install.packages("tree"); library(tree)}
if (!require(rpart)) {install.packages("rpart"); library(rpart)}
# if you are using a mac then install XQuartz from http://xquartz.macosforge.org first
if (!require(rattle)) {install.packages("rattle"); library(rattle)}
if (!require(rpart.plot)) {install.packages("rpart.plot"); library(rpart.plot)}
if (!require(RColorBrewer)) {install.packages("RColorBrewer"); library(RColorBrewer)}
if (!require(party)) {install.packages("party"); library(party)}
if (!require(partykit)) {install.packages("partykit"); library(partykit)}

# a better scatterplot matrix routine
if (!require(car)) {install.packages("car"); library(car)}

# better summary tables
if (!require(psych)) {install.packages("psych"); library(psych)}

# setup clustering
if (!require(fclust)) {install.packages("fclust"); library(fclust)}
if (!require(lattice)) {install.packages("lattice"); library(lattice)}
if (!require(gmodels)) {install.packages("gmodels"); library(gmodels)}  # nicer cross tabulations
if (!require(ggplot2)) {install.packages("ggplot2"); library(ggplot2)}

# setup topic modeling
if (!require(reshape2)) {install.packages("reshape2"); library(reshape2)}
if (!require(NLP)) {install.packages("NLP"); library(NLP)}
if (!require(topicmodels)) {install.packages("topicmodels"); library(topicmodels)}
if (!require(tm)) {install.packages("tm"); library(tm)}
if (!require(slam)) {install.packages("slam"); library(slam)}




##################### input the data  ######################

## read in the data

# set to your correct working directory
setwd("~/Library/Mobile Documents/com~apple~CloudDocs/CMU Tepper/Analytical Marketing/TopicModels")

# read in movie datasets
movies=read.delim("opus_movies.txt",header=T)  # the Opus movie data
tags=read.delim("opus_movielens_tags.txt",header=T)  # just the tags from movielens
keywords=read.delim("opus_keywords.txt",header=T) # the Opus keywords data

## make modifications to the dataset

# convert to native encoding and remove invalid multibyte characters
movies$display_name <- iconv(movies$display_name, from = "UTF-8", to = "ASCII//TRANSLIT")

# trim the display_name to a maximum of 20 characters
movies$short_name <- strtrim(enc2native(as.character(movies$display_name)), 20)

# change data formats
movies$release_date=as.Date(as.character(movies$release_date),format="%Y-%m-%d")
movies$release_month=format(movies$release_date,"%m")
movies$release_monthyear=format(movies$release_date,"%m-%Y")
tags$odid=as.factor(tags$odid)
keywords$odid=as.factor(keywords$odid)
# map the months to seasons
movies$release_season=rep('1Winter',length(movies$release_month))
movies$release_season[movies$release_month %in% c('03','04')]='2Spring'
movies$release_season[movies$release_month %in% c('05','06','07')]='3Summer'
movies$release_season[movies$release_month %in% c('08','09','10')]='4Fall'
movies$release_season[movies$release_month %in% c('11','12')]='5Holiday'
# remove punctuation from genre and rating
movies$rating=revalue(movies$rating,c("PG-13"="PG13"))
movies$genre=revalue(movies$genre,c("Black Comedy"="BlackComedy","Concert/Performance"="Performance","Romantic Comedy"="RomanticComedy","Thriller/Suspense"="Thriller"))
# create a matrix with genre and rating as 
dummygenre=model.matrix(~genre,movies)[,-1]  # omit the intercept in the first column
dummyrating=model.matrix(~rating,movies)[,-1]  # omit the intercept in the first column
# since these are matrix, we coerce them to lists, merge them, and then overwrite movies
movies=cbind(movies,as.data.frame(cbind(dummygenre,dummyrating)))
valgenre=colnames(dummygenre)
valrating=colnames(dummyrating)

# Create a standardized version of the data
nvariables <- sapply(movies, is.numeric)
nvariables <- names(nvariables[nvariables])
smovies <- scale(movies[, nvariables])

# Convert tags$odid from factor to character
tags$odid <- as.character(tags$odid)

# Handle invalid characters in tags$tag
tags$tag <- iconv(tags$tag, from = "", to = "UTF-8", sub = "byte")

# Replace non-printable characters in tags$tag
tags$tag <- gsub("[^[:print:]]", "", tags$tag)

# Use simple_triplet_matrix for MovieLens tags
# Put data in sparse matrix form using simple_triplet_matrix as needed by LDA
library(slam)
mterms <- simple_triplet_matrix(
  i = as.integer(as.factor(tags$odid)),
  j = as.integer(as.factor(tags$tag)),
  v = tags$count,
  dimnames = list(levels(as.factor(tags$odid)), levels(as.factor(tags$tag)))
)

# Keep words that are used frequently (by at least 20 movies)
mterms <- mterms[, apply(mterms, 2, sum) >= 20]

# Delete any movies that do not have any terms
mterms <- mterms[apply(mterms, 1, sum) > 0, ]

# use this definition of mterms for Opus Keywords
# put data in sparse matrix form using simple_triplet_matrix as needed by LDA
##mterms=simple_triplet_matrix(i=as.integer(keywords$odid),j=as.integer(keywords$keyword),
##                                v=rep(1,length(keywords$keyword)),
##                                dimnames=list(levels(keywords$odid),levels(keywords$keyword)))

# determine dimensions of mterms
umovies=movies[movies$odid %in% as.integer(rownames(mterms)),]   # create a subset of the movies that have terms
lmterms=apply(mterms,1,sum)   # compute the sum of each of the rows (# of terms per movie)
lwterms=apply(mterms,2,sum)   # compute the sum of each of the columns (# of times word used)

# also create another version as DocumentTermMatrix
tmterms = as.DocumentTermMatrix(mterms,weight=weightTfIdf)

# create a vector with the names of the most frequent terms
topterms = findFreqTerms(tmterms,20)
idxtopterms = (1:ncol(mterms))[colnames(mterms) %in% topterms]  # get the indices of the topterms

# create a matrix with just the top keywords (cast this as a dense matrix)
movieterms = as.matrix(mterms[,topterms])




##################### decision tree to predict release season  ######################

# estimate a tree to predict the release month
ctree = rpart(release_season~genre+rating+production_budget,data=movies, control=rpart.control(cp=0.01))
summary(ctree)
plot(ctree)
text(ctree)
prp(ctree)
fancyRpartPlot(ctree,cex=.7)




##################### k-means clustering of movies based upon characteristics ######################

# make a list of variables to include in a kmeans solution
qlist=c("production_budget","sequel",valgenre,valrating)

# compute a k=9 solution
set.seed(569123)   # initialize the random number generator so we all get the same results
grpA=kmeans(smovies[,qlist],centers=9)
valclusters=1:9

# plot the solutions against the production value and genreAction
# since the data is categorical most of the plots will overlay one another,
# so instead we jitter the points -- which adds a small random number to each
par(mfrow=c(1,1),mar=c(5,4,4,1)+.1)
plot(movies$production_budget, jitter(movies$genreAction),col=grpA$cluster)
points(grpA$centers[,c("production_budget","genreAction")],col=valclusters,pch=8,cex=2)
legend("topright",pch=8,bty="n",col=valclusters,as.character(valclusters))

# compare the cluster solutions with the Release Season
table(movies$release_season,cluster=grpA$cluster)
table(movies$genre,cluster=grpA$cluster)
table(movies$rating,cluster=grpA$cluster)
table(bigbudget=movies$production_budget>100000000,cluster=grpA$cluster)

CrossTable(movies$release_season,grpA$cluster)   # slightly nicer cross tabulation

# summarize the centroids
grpAcenter=t(grpA$centers)
rownames(grpAcenter)=strtrim(colnames(movies[,qlist]),40)
print(grpAcenter[qlist,])
parallelplot(t(grpAcenter[qlist,]),lwd=2,
             main="Movie Clusters based upon Budget, Genre and Rating",
             auto.key=list(text=as.character(valclusters),space="bottom",columns=3,lines=T,lwd=2))

# print a table with the movies assigned to each cluster
for (i in valclusters) {
  print(paste("* * * Movies in Cluster #",i," * * *"))
  print(movies$display_name[grpA$cluster==i])
}




##################### estimate an LDA topic model using keywords  ######################

# setup the parameters for LDA control vector
burnin=1000     # number of initial iterations to discard for Gibbs sampler (for slow processors use 500)
iter=5000       # number of iterations to use for estimation  (for slow processors use 1000)
thin=50         # only save every 50th iteration to save on storage
seed=list(203,5,63,101,765)  # random number generator seeds
nstart=5        # number of repeated random starts
best=TRUE       # only return the model with maximum posterior likelihood
set.seed(142123) # initialize the random number generator so we all get the same results

# estimate a series of LDA models (each run can take a few minutes depending upon your processor)
ClusterOUT6 = LDA(mterms,6,method="Gibbs",control=list(nstart=nstart,seed=seed,best=best,burnin=burnin,iter=iter,thin=thin))


### analyze a particular model with k topics

# select a cluster to analyze, all the following lines use this cluster
# so change to a different cluster and repeat
ClusterOUT=ClusterOUT6


#
# let's understand the topics
#

# matrix with probabilities of each term per topic
ClustTopics = exp(ClusterOUT@beta)
colnames(ClustTopics)=colnames(mterms)
write.table(t(ClustTopics),file="topics_allterms.txt")  # save to a file to import into Excel

# show the topics and associated terms
parallelplot(ClustTopics[,idxtopterms],main="Topic associated with selected Terms")
print(format(t(ClustTopics),digits=1,scientific=FALSE))   # print topics in columns and probabilities in rows

# to better understand the topics lets print out the 20 most likely terms used for each
results=matrix('a',nrow=20,ncol=6)
for (i in 1:6) {
  idxtopterms=order(ClustTopics[i,],decreasing=TRUE)  # find the indices associated with the topic
  topterms=ClustTopics[i,idxtopterms[1:20]]  # identify the terms with the highest probabilities
  results[,i]=names(topterms)              # save the names
}
print(results)
write.table(results,file="topics_top20terms.txt")

#
# let's understand the movies
#

# probability of topic assignments (each movie has its own unique profile)
ClustAssign = ClusterOUT@gamma   # this is a matrix with the row as the movie and column as the topic
rownames(ClustAssign)=umovies$display_name
write.table(ClustAssign,file="topics_allmovies.txt")
ClustBest = apply(ClustAssign,1,which.max)  # determine the best guess of a cluster, a vector with best guess
head(cbind(ClustAssign,ClustBest),n=10)   # show the actual topic probabilities and best guess associated with the first 10 movies

# to better understand the topics lets print out the 20 most likely movies for each
results=matrix('a',nrow=20,ncol=6)
for (i in 1:6) {
  idxtopmovies=order(ClustAssign[,i],decreasing=TRUE)   # find the indices associated with the topic
  topmovies=ClustAssign[idxtopmovies[1:20],i]   # identify the terms with the highest probaiblities
  results[,i]=names(topmovies)                # save the names
}
print(results)
write.table(results,file="topics_top20movies.txt")

#
# let's compare our target movie with the others
#

# Find the index associated with our target movie
imovie <- which(umovies$display_name == "The Maze Runner")
print(umovies[imovie,])

# Show the topics associated with the selected movie
barplot(ClustAssign[imovie,], names.arg=1:ncol(ClustAssign), main=paste("Topics Associated with selected movie", umovies$display_name[imovie]))

# Compare it with a couple other movies
imovie2 <- which(umovies$display_name == "Titanic")
barplot(ClustAssign[imovie2,], names.arg=1:ncol(ClustAssign), main=paste("Topics Associated with selected movie", umovies$display_name[imovie2]))

imovie3 <- which(umovies$display_name == "Transformers: Revenge of the Fallen")
barplot(ClustAssign[imovie3,], names.arg=1:ncol(ClustAssign), main=paste("Topics Associated with selected movie", umovies$display_name[imovie3]))

# Visualize the distribution of topics across the movies
library(lattice)
parallelplot(ClustAssign, groups=ClustBest, ylab="Topic", main="Topic Assignments for each movie")
parallelplot(ClustAssign, groups=umovies$genre, ylab="Topic", main="Topic Assignments for each movie",
             auto.key=list(space="bottom", columns=3, lines=TRUE))
boxplot(ClustAssign, xlab="Topic", ylab="Probability of Topic across Movies")

# Compute the distance between a target movie and all other movies in the "topics" space
topicdist <- sweep(ClustAssign, 2, ClustAssign[imovie,])
topicdistss <- sqrt(rowSums(topicdist^2))  # Take the square root to put this back on the same scale
augmovies <- cbind(umovies, topicdistss)
augmovies <- augmovies[-imovie, ]  # Remove "The Maze Runner" from our set

# For each release week, compare our movies to those that were released that week
# This will allow us to identify the week that would be best for a release
mweek <- as.Date('2014-01-01', format="%Y-%m-%d")

# Save our results to a data frame for each week
results <- data.frame(week=1:52, date=mweek, display_name=umovies$display_name[imovie], 
                      averagedist=0, mostsimilar=0, genre=umovies$genre[imovie], rating=umovies$rating[imovie])

nweek <- 1
while (mweek <= as.Date('2014-12-12', format="%Y-%m-%d")) {
  # Compute the subset of movies released that week or within two weeks earlier
  mpriorweek <- mweek - 14   # Compute the date for two weeks earlier
  mcompare <- augmovies[augmovies$release_date >= mpriorweek & augmovies$release_date <= mweek, 
                        c("topicdistss", "display_name", "release_date", "genre", "rating")]
  mcompare <- mcompare[order(mcompare$topicdistss),]  # Sort the results by topicdist (so most similar is in first row)
  
  # Save results
  results$date[nweek] <- mweek
  results$display_name[nweek] <- mcompare[1, "display_name"]
  results$rating[nweek] <- mcompare[1, "rating"]
  results$genre[nweek] <- mcompare[1, "genre"]
  results$mostsimilar[nweek] <- mcompare[1, "topicdistss"]
  results$averagedist[nweek] <- mean(mcompare$topicdistss)
  
  # Print results for this week
  print(paste("*** Results for", format(mweek, "%m-%d-%y"), "***"))
  print(mcompare)
  
  # Increment for the next week
  mweek <- mweek + 7
  nweek <- nweek + 1
}

print(results)
results <- results[-51:-52, ]  # Remove incomplete weeks
write.table(results, file="topics_byweek.txt", row.names=FALSE)

# Plot the results
plot(results$date, results$mostsimilar, ylim=c(0, .7), xlab="Week", ylab="Similarity Measure", main="Comparison to Maze Runner")
lines(results$date, results$mostsimilar)
lines(results$date, results$averagedist, lty=2)


## last we can use our model to compute a best guess

# Calculate the best guess for each movie/term combination
ClustGuess <- (ClustAssign %*% ClustTopics) * lmterms

# Determine the optimal number of clusters using the elbow method
wss <- sapply(1:10, function(k) {
  kmeans(ClustAssign, centers = k, nstart = 10)$tot.withinss
})

plot(1:10, wss, type = "b", pch = 19, frame = FALSE, 
     xlab = "Number of clusters", 
     ylab = "Total within-clusters sum of squares",
     main = "Elbow Method for Optimal Number of Clusters")

# Based on the elbow plot, choose the optimal number of clusters
optimal_clusters <- 7  # Change this based on the elbow plot result

# Perform k-means clustering on ClustAssign with the optimal number of clusters
set.seed(123)  # Setting seed for reproducibility
kmeans_result <- kmeans(ClustAssign, centers = optimal_clusters)  
grpKmeans <- list(cluster = kmeans_result$cluster)

# Find the index associated with our target movie
imovie <- which(umovies$display_name == "The Maze Runner")

# Ensure imovie is not NA or invalid
if (length(imovie) == 0) {
  stop("The target movie 'The Maze Runner' is not found in the dataset.")
}

# Compare the predictions for the selected movie
mcompare <- cbind(ClustGuess[imovie, ], as.vector(mterms[imovie, ]))
print(mcompare)
write.table(mcompare, file = "topics_prediction.txt")

# Focus on the topic terms
idxtopterms <- order(ClustGuess[imovie, ], decreasing = TRUE)  # Find the indices associated with the topic
topterms <- ClustAssign[idxtopterms[1:20], ]  # Identify

# Cross-tabulate the k-means clusters with the most likely topic model assignments
comparison_table <- xtabs(~ grpKmeans$cluster + ClustBest)
print(comparison_table)

