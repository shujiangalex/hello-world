install.packages("Hmisc")
install.packages("xtable")
library("Hmisc")
library(xtable)
#HW2 
cwurData <- read.csv("C:/Users/shuji/Desktop/BANA 200/HW1/cwurData.csv")
c = cwurData

plot(c$country,main="College Ranking Data", 
     xlab="Countries", ylab="Count")

boxplot(c$score,main="Score Boxplot", ylab="Score")

plot(c$quality_of_education,c$world_rank,col = "blue", main = "Education vs World Ranking", xlab = "Quality of Education Ranking", ylab = "World Ranking")
abline(lm(formula = c$world_rank ~ c$quality_of_education))

plot(c$quality_of_faculty,c$world_rank,col = "red", main = "Faculty vs World Ranking", xlab = "Quality of faculty Ranking", ylab = "World Ranking")
abline(lm(formula = c$world_rank ~ c$quality_of_faculty))

rank_cor = cor(c[,c(1,5:12)])
cor_2 <- rcorr(as.matrix(rank_cor))
cor_2


fit1 = lm(world_rank~quality_of_education + alumni_employment + quality_of_faculty + publications + influence + citations + broad_impact + patents, data = cwurData)
summary(fit1)