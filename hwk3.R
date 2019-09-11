data <- read.csv("C:/Users/shuji/Desktop/BANA 200/HW3/winequality-red.csv", sep=";")
install.packages("dplyr")
library(dplyr)
library(ade4)

#Only retain records where we know the quality of wine.
data$quality<-as.character(data$quality)
data <- filter(data, quality!="")
data$quality<-as.numeric(data$quality)

#Replace a missing value with the mean. 
data$alcohol[data$alcohol>=100] <- NULL
data$alcohol[is.na(data$alcohol)] <- mean(data$alcohol,na.rm = TRUE)

data$pH[data$pH>=12&data$pH<=2] <- NULL
data$pH[is.na(data$pH)] <- mean(data$pH,na.rm = TRUE)

anyNA(data)



#Create alcohol buckets
data <- mutate(data, alcohol_group = ifelse(alcohol<10 ,"Low wine",
                                            ifelse(alcohol>=10 ,"Moderate wine",
                                                   "N/A")))

#Create a unique ID by concatenating id, alcohol group and wine quality
data$id <- seq.int(nrow(data))
data$uid <-  paste(data$id,data$alcohol_group,data$quality, sep="-")


#Creat a new variable reviews_per_year

data <- mutate(data, acid_per_alcohol = fixed.acidity / alcohol)

###### Let's do some clustering!
#Let's remove columns that won't be too useful for clustering.
clustering_data<-subset(data,select=-c(quality,alcohol_group,id,uid,acid_per_alcohol))
head(clustering_data)
#Let's normalize the data before doing our cluster analysis.
normalize <- function(x){
    return ((x - min(x))/(max(x) - min(x)))
}

clustering_data_normal = mutate(clustering_data,
                                fixed.acidity = normalize(fixed.acidity),
                                volatile.acidity = normalize(volatile.acidity),
                                citric.acid = normalize(citric.acid),
                                residual.sugar = normalize(residual.sugar),
                                chlorides = normalize(chlorides),
                                free.sulfur.dioxide = normalize(free.sulfur.dioxide),
                                total.sulfur.dioxide = normalize(total.sulfur.dioxide),
                                density = normalize(density),
                                pH = normalize(pH),
                                sulphates = normalize(sulphates),
                                alcohol = normalize(alcohol))


###########
SSE_curve <- c()
for (n in 1:9) {
    kcluster <- kmeans(clustering_data_normal, n)
    sse <- sum(kcluster$withinss)
    SSE_curve[n] <- sse
}

SSE_curve
print("SSE cuve for the ideal k value")
plot(1:9, SSE_curve, type="b", xlab="Number of Clusters", ylab="SSE")

#Subseting 
results <- kmeans(clustering_data_normal, 6)
attributes(results)
results$size
new_marks_data1 <- subset(data ,results$cluster==1)
new_marks_data2 <- subset(data ,results$cluster==2)
new_marks_data3 <- subset(data ,results$cluster==3)
new_marks_data4 <- subset(data ,results$cluster==4)
new_marks_data5 <- subset(data ,results$cluster==5)
new_marks_data6 <- subset(data ,results$cluster==6)


##Plot
plot(data[c("pH","alcohol")],col = results$cluster)
plot(data[c("fixed.acidity","volatile.acidity")],col = results$cluster)
plot(data[c("pH","total.sulfur.dioxide")],col = results$cluster)
plot(data[c("density","chlorides")],col = results$cluster)

##multivariate linear regression
fit1 = lm(quality~fixed.acidity + volatile.acidity + citric.acid + chlorides + total.sulfur.dioxide + density + pH + alcohol, data = new_marks_data1)
summary(fit1)

fit2 = lm(quality~fixed.acidity + volatile.acidity + citric.acid + chlorides + total.sulfur.dioxide + density + pH + alcohol, data = new_marks_data2)
summary(fit2)

fit3 = lm(quality~fixed.acidity + volatile.acidity + citric.acid + chlorides + total.sulfur.dioxide + density + pH + alcohol, data = new_marks_data3)
summary(fit3)

fit4 = lm(quality~fixed.acidity + volatile.acidity + citric.acid + chlorides + total.sulfur.dioxide + density + pH + alcohol, data = new_marks_data4)
summary(fit4)

fit5 = lm(quality~fixed.acidity + volatile.acidity + citric.acid + chlorides + total.sulfur.dioxide + density + pH + alcohol, data = new_marks_data5)
summary(fit5)

fit6 = lm(quality~fixed.acidity + volatile.acidity + citric.acid + chlorides + total.sulfur.dioxide + density + pH + alcohol, data = new_marks_data6)
summary(fit6)


