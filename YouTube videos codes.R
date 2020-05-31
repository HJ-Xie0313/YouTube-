data <- read.csv("US.csv",encoding = "UTF-8")
N <- nrow(data)
## Choose the end 5000 data
n <- 5000
data <- tail(data,n)

## Create the adjacency based on channel titles
ch_title <- data$channel_title
ch_title <- as.vector(ch_title)
ch_title <- as.character(ch_title)
t <- table(ch_title)
A <- matrix(0,n,n)
for (i in 1:(n-1)) {
  for (j in (i+1):n) {
    if(ch_title[i]==ch_title[j]) A[i,j]=A[j,i]=1
  }
}

## Calculate regularized graph Laplacian 
diag <- apply(A, 1, sum)
#D <- diag(diag)
tau <- mean(diag)
diag2 <- 1/sqrt(diag+tau)
#diag2 <- numeric(n)
#for (i in 1:n) {
#  if(diag[i]>0) diag2[i] <- 1/sqrt(diag[i])
#}
D2 <- diag(diag2)
L <- D2%*%A
L <- L%*%D2

## We use tags to create Covariance matrix
tags <- data$tags
tags <- as.vector(tags)
tags <- as.character(tags)
tag.list <- list(NULL)
length(tag.list) <- n
for (k in 1:n) {
  phrase <- strsplit(tags[k],split = "[[:punct:]]")[[1]]
  phrase <- phrase[nchar(phrase)>0]
  tag.list[[k]] <- phrase
}

tag.all <- unlist(tag.list)
tt <- sort(table(tag.all),decreasing = T)
keys <- names(tt[1:500])

X <- matrix(0,nrow=n,ncol = 500)
for (i in 1:n) {
  tags <- tag.list[[i]]
  len <- length(tags)
  for (j in 1:500) {
    for (k in 1:len) {
      if(tags[k]==keys[j]) X[i,j]=1
    }
  }
}

## calculate the similarity matrix
W <- t(X)%*%L%*%X
Cw <- X%*%W%*%t(X)
Sw <- L+Cw

## k-means
E.Sw <- eigen(Sw)
K=12
U.Sw <- E.Sw$vectors[,1:K]
mod <- apply(U.Sw,1,function(x) sqrt(sum(x^2)))
index <- which(mod==0)
U.star <- U.Sw
U.star[-index,] <- U.star[-index,]/mod[-index]
clust.Sw <- kmeans(U.star, centers=K, iter.max=100, nstart=25)

tag.list1 <- tag.list[clust.Sw$cluster==1]
tags1 <- unlist(tag.list1)
freq1 <- sort(table(tags1),decreasing = T)
keys1 <- names(freq1)

tag.list2 <- tag.list[clust.Sw$cluster==2]
tags2 <- unlist(tag.list2)
freq2 <- sort(table(tags2),decreasing = T)
keys2 <- names(freq2)

tag.list3 <- tag.list[clust.Sw$cluster==3]
tags3 <- unlist(tag.list3)
freq3 <- sort(table(tags3),decreasing = T)
keys3 <- names(freq3)

tag.list4 <- tag.list[clust.Sw$cluster==4]
tags4 <- unlist(tag.list4)
freq4 <- sort(table(tags4),decreasing = T)
keys4 <- names(freq4)

tag.list5 <- tag.list[clust.Sw$cluster==5]
tags5 <- unlist(tag.list5)
freq5 <- sort(table(tags5),decreasing = T)
keys5 <- names(freq5)

tag.list6 <- tag.list[clust.Sw$cluster==6]
tags6 <- unlist(tag.list6)
freq6 <- sort(table(tags6),decreasing = T)
keys6 <- names(freq6)

tag.list7 <- tag.list[clust.Sw$cluster==7]
tags7 <- unlist(tag.list7)
freq7 <- sort(table(tags7),decreasing = T)
keys7 <- names(freq7)

tag.list8 <- tag.list[clust.Sw$cluster==8]
tags8 <- unlist(tag.list8)
freq8 <- sort(table(tags8),decreasing = T)
keys8 <- names(freq8)

tag.list9 <- tag.list[clust.Sw$cluster==9]
tags9 <- unlist(tag.list9)
freq9 <- sort(table(tags9),decreasing = T)
keys9 <- names(freq9)

tag.list10 <- tag.list[clust.Sw$cluster==10]
tags10 <- unlist(tag.list10)
freq10 <- sort(table(tags10),decreasing = T)
keys10 <- names(freq10)

tag.list11 <- tag.list[clust.Sw$cluster==11]
tags11 <- unlist(tag.list11)
freq11 <- sort(table(tags11),decreasing = T)
keys11 <- names(freq11)

tag.list12 <- tag.list[clust.Sw$cluster==12]
tags12 <- unlist(tag.list12)
freq12 <- sort(table(tags12),decreasing = T)
keys12 <- names(freq12)

## wordcloud pics
library(wordcloud)
wordcloud(keys1,freq1,min.freq=20,colors=brewer.pal(6,"Dark2"),random.order = F)
wordcloud(keys2,freq2,min.freq=20,colors=brewer.pal(6,"Dark2"),random.order = F)
wordcloud(keys3,freq3,min.freq=20,colors=brewer.pal(6,"Dark2"),random.order = F)
wordcloud(keys4,freq4,min.freq=20,colors=brewer.pal(6,"Dark2"),random.order = F)
wordcloud(keys5,freq5,min.freq=20,colors=brewer.pal(6,"Dark2"),random.order = F)
wordcloud(keys6,freq6,min.freq=20,colors=brewer.pal(6,"Dark2"),random.order = F)
wordcloud(keys7,freq7,min.freq=20,colors=brewer.pal(6,"Dark2"),random.order = F)
wordcloud(keys8,freq8,min.freq=20,colors=brewer.pal(6,"Dark2"),random.order = F)
wordcloud(keys9,freq9,min.freq=20,colors=brewer.pal(6,"Dark2"),random.order = F)
wordcloud(keys10,freq10,min.freq=20,colors=brewer.pal(6,"Dark2"),random.order = F)
wordcloud(keys11,freq11,min.freq=20,colors=brewer.pal(6,"Dark2"),random.order = F)
wordcloud(keys12,freq12,min.freq=20,colors=brewer.pal(6,"Dark2"),random.order = F)

## calculate the number of videos, average views and likes of each cluster
mean(data$views)
data$likes <- as.numeric(data$likes)
mean(data$likes)
vie <- length(K)
lik <- length(K)
num <- length(K)
for (i in 1:K) {
  ind <- which(clust.Sw$cluster==i)
  vie[i] <- mean(data$views[ind])
  lik[i] <- mean(data$likes[ind])
  num[i] <- length(ind)
}
vie
lik
num


