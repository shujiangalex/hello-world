# This program is the first installment of data clean-up and pre-procesing for the Sun Country Airlines case

# We start by reading the data file, which is a 1% random sample of the original airline reservation data

# We set the worling directory using setwd()

setwd("C:/Rick/200/Week 3")

# Then we read the data using the read.csv function

data <- read.csv("SC_Data.csv")

# Use head() to see the first 6 rows of the data

# We will be using the dplyr package for clean up nd pre-processing
# We start by installing the dplyr package

install.packages("dplyr")
#install.packages("dplyr",lib="C:/Rick/R/Rlib")

library(dplyr)
#library(backports,lib="C:/Rick/R/Rlib")
#library(vctrs,lib="C:/Rick/R/Rlib")
#library(crayon,lib="C:/Rick/R/Rlib")
#library(dplyr,lib="C:/Rick/R/Rlib")

#library(fansi,lib="C:/Rick/R/Rlib")
#library(utf8,lib="C:/Rick/R/Rlib")
#library(cli,lib="C:/Rick/R/Rlib")

#Only keep records where we know your birthdate
data <- filter(data, !is.na(birthdateid))

#Only retain records where we know the gender
data$GenderCode<-as.character(data$GenderCode)
data <- filter(data, GenderCode!="")
data$GenderCode<-as.factor(data$GenderCode)

#Some odd age values... we'll replace with the median.
data$Age[data$Age < 0] <- median(data$Age)
data$Age[data$Age > 120] <- median(data$Age)

#If you don't have a reward number we assign it a 0
data$UFlyRewardsNumber[is.na(data$UFlyRewardsNumber)] <- 0

#We construct a reward status factor variable.
data$UflyMemberStatus<-as.character(data$UflyMemberStatus)
data$UflyMemberStatus[data$UflyMemberStatus==''] <-"non-ufly"

#Discard duplicate records
data <- group_by(data, PNRLocatorID,CouponSeqNbr,PaxName,ServiceStartCity,ServiceEndCity,ServiceStartDate)
filter(data, n() == 1)

#Replace odd one off booking channels with 'Other'
data$BookingChannel<-as.character(data$BookingChannel)
data$BookingChannel[data$BookingChannel!="Outside Booking" & data$BookingChannel!="SCA Website Booking" & data$BookingChannel!="Tour Operator Portal" & data$BookingChannel!="Reservations Booking" & data$BookingChannel!="SY Vacation"] <- "Other"
data$BookingChannel<-as.factor(data$BookingChannel)

#Only keep records that involve SunCountry airlines tickets.
data$MarketingAirlineCode<-as.character(data$MarketingAirlineCode) 
data <- filter(data, MarketingAirlineCode=="SY")
data$MarketingAirlineCode<-as.factor(data$MarketingAirlineCode)

#Ditch PNRs that have odd values and indicate an error.
data <- group_by(data, PNRLocatorID)
data <- mutate(data, error=ifelse(min(CouponSeqNbr)!=1,1,0))
filter(data, error==0)

#Create a unique customer ID by concatenating name, gender and birthday
#data <- mutate(data, uid=paste(EncryptedName,GenderCode,birthdateid,sep=""))

data$uid <-  paste(data$EncryptedName,data$GenderCode,data$birthdateid,sep="")

#Create Age buckets
data <- mutate(data, age_group = ifelse(Age>=0 & Age<18,"0-17",
                            ifelse(Age>=18 & Age < 25,"18-24",
                                   ifelse(Age>=25&Age<35,"25-34",
                                          ifelse(Age>=35 & Age<55,"35-54",
                                                 ifelse(Age>=55,"55+","N/A"))))))

#For a given PNR, figure out true origin city (source of first leg)

true_origins<-data%>%
  arrange(PNRLocatorID,CouponSeqNbr)%>% 
  group_by(PNRLocatorID,PaxName)%>% 
  do(data.frame(true_origin=first(.$ServiceStartCity)))

data<-merge(data,true_origins, by.x=c("PNRLocatorID","PaxName"),by.y = c("PNRLocatorID","PaxName"))

#For a given PNR, figure out final destination (target of last leg)
final_destination<-data%>%
  arrange(PNRLocatorID,CouponSeqNbr)%>% 
  group_by(PNRLocatorID,PaxName)%>% 
  do(data.frame(final_destination=last(.$ServiceEndCity)))

data<-merge(data,final_destination, by.x=c("PNRLocatorID","PaxName"),by.y = c("PNRLocatorID","PaxName"))
rm(true_origins)
rm(final_destination)

install.packages("lubridate")
#install.packages("lubridate",lib="C:/Rick/R/Rlib")
library(lubridate)
#library(lubridate,lib="C:/Rick/R/Rlib")


#Now figure out "true" destination, city in which customer spent the most time
diff1<-data%>%arrange(PNRLocatorID,CouponSeqNbr)%>% 
  group_by(PNRLocatorID,PaxName)%>%
  mutate(stay=lead(date(ServiceStartDate))-date(ServiceStartDate),default=0)%>% 
  select(PNRLocatorID,PaxName,ServiceStartCity,ServiceEndCity,ServiceStartDate,stay)

diff1$stay[is.na(diff1$stay)]<-0 
diff1$stay<-as.numeric(diff1$stay) 
true_destination<-diff1%>%
  group_by(PNRLocatorID,PaxName)%>% 
  do(data.frame(true_destination=first(.$ServiceEndCity[.$stay==max(.$stay)])))

data<-merge(data,true_destination, by.x=c("PNRLocatorID","PaxName"),by.y = c("PNRLocatorID","PaxName"))
rm(diff1)
rm(true_destination)

#######

#Figure out whether flight was one-way or round trip
data<-data%>%
  mutate(round_trip = ifelse(as.character(true_origin)==as.character(final_destination), 1, 0))

#Figure out size of the traveling group (# parties)
data<-data%>%
  group_by(PNRLocatorID)%>% 
  mutate(group_size = length(unique(uid)))

#Binary indicator of whether it was a group or single party traveling.
data<-data%>%
  group_by(PNRLocatorID)%>% 
  mutate(group= ifelse(group_size>1,1,0))

#Figure out which calendar quarter the trip took place in (season)
data$ServiceStartDate<-as.Date(data$ServiceStartDate)
data<-data%>%
  group_by(PNRLocatorID,PaxName)%>% 
  mutate(seasonality= 
           ifelse(month(ServiceStartDate)>=1 & month(ServiceStartDate)<=3,"Q1", 
                  ifelse(month(ServiceStartDate)>=4 & month(ServiceStartDate)<=6,"Q2", 
                         ifelse(month(ServiceStartDate)>=7 & month(ServiceStartDate)<=9,"Q3", 
                                ifelse(month(ServiceStartDate)>=10 & month(ServiceStartDate)<=12,"Q4",0)))))

#How many days in advance was the trip booked?
data$PNRCreateDate<-as.Date(data$PNRCreateDate) 
data$ServiceStartDate<-as.Date(data$ServiceStartDate) 
data<-data%>%
  mutate(days_pre_booked=as.numeric(floor(difftime(ServiceStartDate,PNRCreateDate,units=c("days")))))


###### Let's do some clustering!

#Let's aggregate up to the customer-trip level.
customer_data<-data%>% 
  group_by(PNRLocatorID,uid)%>%
  summarise(PaxName=first(PaxName),
            BookingChannel=first(BookingChannel), 
            amt=max(TotalDocAmt), 
            UFlyRewards=first(UFlyRewardsNumber), 
            UflyMemberStatus=first(UflyMemberStatus), 
            age_group=first(age_group), 
            true_origin=first(true_origin), 
            true_destination=first(true_destination), 
            round_trip=first(round_trip), 
            group_size=first(group_size), 
            group=first(group), 
            seasonality=first(seasonality), 
            days_pre_booked=max(days_pre_booked))

#Let's remove columns that won't be too useful for clustering, like IDs, names.
clustering_data<-subset(customer_data,select=-c(PNRLocatorID,uid,PaxName,UFlyRewards))

head(clustering_data)

#Let's normalize the data before doing our cluster analysis.
normalize <- function(x){
  return ((x - min(x))/(max(x) - min(x)))
}

clustering_data = mutate(clustering_data,
                         amt = normalize(amt),
                         days_pre_booked = normalize(days_pre_booked), 
                         group_size=normalize(group_size))

# Dummy code variables

library(ade4)
#install.packages("ade4",lib="C:/Rick/R/Rlib")
#library(ade4,lib="C:/Rick/R/Rlib")

clustering_data <- as.data.frame(clustering_data)
clustering_data <-  clustering_data %>% 
  cbind(acm.disjonctif(clustering_data[,c("BookingChannel","age_group",
                                          "true_origin","true_destination","UflyMemberStatus","seasonality")]))%>% 
  ungroup()

head(clustering_data)

#Remove the original (non-dummy-coded) variables
clustering_data<-clustering_data %>%select(-BookingChannel,-age_group,-true_origin,-true_destination,-UflyMemberStatus,-seasonality)

#Remove columns that were created for factor levels that were not represented in the sample.
clustering_data <- clustering_data[, colSums(clustering_data != 0, na.rm = TRUE) > 0]

#Now run k-Means and look at the within SSE curve - 3 or 5 seems like the best
#solution here...

SSE_curve <- c()
for (n in 1:15) {
  kcluster <- kmeans(clustering_data, n)
  sse <- sum(kcluster$withinss)
  SSE_curve[n] <- sse
}

SSE_curve

print("SSE cuve for the ideal k value")
plot(1:15, SSE_curve, type="b", xlab="Number of Clusters", ylab="SSE")

#Let's go with 4...
kcluster<- kmeans(clustering_data, 4)

names(kcluster)

print("the size of each of the clusters")
kcluster$size

#What do the centers of each of the resulting clusters look like?
kcluster$centers

#Let's add a new column with the cluster assignment for each obs in the sample.
segment<-kcluster$cluster 
clustering_data<-cbind(clustering_data,segment)
customer_data <- as.data.frame(customer_data)
segment <- data.frame(segment, col.names="segment" )
dim(customer_data)
customer_data <- cbind(customer_data,segment)
uid_seg <- customer_data[c("uid","segment")]
data <- merge(data, uid_seg, by = "uid")
data <- data[,-4]
names(data)

write.csv(data, "clustering_data.csv")

