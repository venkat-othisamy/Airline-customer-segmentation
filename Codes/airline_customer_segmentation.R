## ----setup, include=FALSE------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)
library(magrittr)
library(dplyr) #Attaching dplyr library
library(tidyr) #Attaching tidyr library
#install.packages("lubridate")
library(lubridate) #R library to work with date times.
#install.packages("fastcluster")
library(fastcluster)
library(arules)
library(klaR)
library(mclust)
library(dbscan)

## ------------------------------------------------------------------------
folder="C:/Users/venka/Documents/Fall Semester/Exploratory Data Analysis/airline_data"
setwd(folder)

data<-read.csv("airline_data.csv") #Data is stored in the dataframe called data

#data_orig <- data
#sample_rows <- sample.int(nrow(data),100000)
#data <- data[sample_rows,]
#write.csv(data,"data_small.csv")

## ------------------------------------------------------------------------
str(data)

## ------------------------------------------------------------------------
summary(data)

## ------------------------------------------------------------------------
#Filtering out records which have NA for BirthdateID
#same as data <- data %>%filter(!is.na(birthdateid)) 
data%<>%filter(!is.na(birthdateid)) 

data$GenderCode<-as.character(data$GenderCode)
data%<>%filter(GenderCode!="")

#Filtering out records which have “” for GenderCode
data$GenderCode<-as.factor(data$GenderCode)

## ------------------------------------------------------------------------
#Replacing negative ages with median value
data$Age[data$Age < 0] <- median(data$Age)

#Replacing age values greater than 120 with median value
data$Age[data$Age > 120] <- median(data$Age)

## ------------------------------------------------------------------------
#Replace NAs with 0
data$UFlyRewardsNumber[is.na(data$UFlyRewardsNumber)] <- 0

## ------------------------------------------------------------------------
#Convert factor level data to string
data$UflyMemberStatus<-as.character(data$UflyMemberStatus)

#Replace missing values with “non-ufly” 
data$UflyMemberStatus[data$UflyMemberStatus==''] <-"non-ufly"

## ------------------------------------------------------------------------
data%<>%
  group_by(PNRLocatorID,CouponSeqNbr,PaxName,ServiceStartCity,ServiceEndCity,ServiceStartDate)%>%
  filter(n()==1)

## ------------------------------------------------------------------------
data$BookingChannel<-as.character(data$BookingChannel)
data$BookingChannel[data$BookingChannel!="Outside Booking" & 
                      data$BookingChannel!="SCA Website Booking" & 
                      data$BookingChannel!="Tour Operator Portal" & 
                      data$BookingChannel!="Reservations Booking" & 
                      data$BookingChannel!="SY Vacation"] <- "Other"
data$BookingChannel<-as.factor(data$BookingChannel)

## ------------------------------------------------------------------------
data$MarketingAirlineCode<-as.character(data$MarketingAirlineCode)
data%<>%filter(MarketingAirlineCode=="SY")
data$MarketingAirlineCode<-as.factor(data$MarketingAirlineCode)

## ------------------------------------------------------------------------
data%<>%group_by(PNRLocatorID)%>%
  mutate(error= ifelse(min(CouponSeqNbr)!=1,1,0))

## ------------------------------------------------------------------------
data%<>%filter(error==0)
nrow(data)

## ------------------------------------------------------------------------
#Obtain Unique PNRs
uniquePNRs<-unique(data$PNRLocatorID) 

#To produce the same samples every time the code is run
set.seed(1234567)


sample_PNRs<-sample(uniquePNRs,10000)

#Obtaining data related to the sampled 10,000 PNRs
sample_data<-data%>%filter(PNRLocatorID %in% sample_PNRs)

## ------------------------------------------------------------------------

sample_data<-sample_data%>% mutate(uid=paste(EncryptedName,GenderCode,birthdateid,sep=""))

## ------------------------------------------------------------------------
sample_data%<>%mutate(age_group = 
                        ifelse(Age>=0 & Age<18,"0-17",
                               ifelse(Age>=18 & Age < 25,"18-24",
                                      ifelse(Age>=25&Age<35,"25-34",
                                             ifelse(Age>=35 & Age<55,"35-54",
                                                    ifelse(Age>=55,"55+",0)
                                                    )
                                             )
                                      )
                               )
                    )

## ------------------------------------------------------------------------
true_origins<-sample_data%>%
  arrange(PNRLocatorID,CouponSeqNbr)%>%
  group_by(PNRLocatorID,PaxName)%>%
  do(data.frame(true_origin=first(.$ServiceStartCity)))

sample_data<-merge(sample_data,true_origins,
                   by.x=c("PNRLocatorID","PaxName"),
                   by.y = c("PNRLocatorID","PaxName"))

## ------------------------------------------------------------------------
final_destination<-sample_data%>%
  arrange(PNRLocatorID,CouponSeqNbr)%>%
  group_by(PNRLocatorID,PaxName)%>% 
  do(data.frame(final_destination=last(.$ServiceEndCity)))

sample_data<-merge(sample_data,final_destination,
                   by.x=c("PNRLocatorID","PaxName"),
                   by.y = c("PNRLocatorID","PaxName"))

## ------------------------------------------------------------------------
#Convert Service Start date to Date type
sample_data$ServiceStartDate<-as.Date(sample_data$ServiceStartDate)

#The place of maximum stay during the trip.
diff1<-sample_data%>%
  arrange(PNRLocatorID,CouponSeqNbr)%>%
  group_by(PNRLocatorID,PaxName)%>%
  mutate(stay=lead(ServiceStartDate)-ServiceStartDate,default=0)%>%
  select(PNRLocatorID,PaxName,ServiceStartCity,ServiceEndCity,ServiceStartDate,stay)

diff1$stay[is.na(diff1$stay)]<-0
diff1$stay<-as.numeric(diff1$stay)

true_destination<-diff1%>%
  group_by(PNRLocatorID,PaxName)%>%
  do(data.frame(true_destination= first(as.character(.$ServiceEndCity)[.$stay==max(.$stay)])))

sample_data<-merge(sample_data,true_destination,
                   by.x=c("PNRLocatorID","PaxName"),
                   by.y = c("PNRLocatorID","PaxName"))

## ------------------------------------------------------------------------
sample_data%<>%
  mutate(round_trip = ifelse(as.character(true_origin)==as.character(final_destination), 1, 0))

## ------------------------------------------------------------------------
sample_data%<>%
  group_by(PNRLocatorID)%>%
  mutate(group_size= length(unique(uid)))

## ------------------------------------------------------------------------
sample_data%<>%
  group_by(PNRLocatorID)%>%
  mutate(group= ifelse(group_size>1,1,0))

## ------------------------------------------------------------------------
sample_data$ServiceStartDate<-as.Date(sample_data$ServiceStartDate)
#Convert ServiceStartDate from factor to Date format
sample_data%<>%
  group_by(PNRLocatorID,PaxName)%>%
  mutate(seasonality= ifelse(month(ServiceStartDate)>=1 & month(ServiceStartDate)<=3,"Q1",
                             ifelse(month(ServiceStartDate)>=4 & month(ServiceStartDate)<=6,"Q2",
                                    ifelse(month(ServiceStartDate)>=7 & month(ServiceStartDate)<=9,"Q3",
                                           ifelse(month(ServiceStartDate)>=10 & month(ServiceStartDate)<=12,"Q4",0)
                                           )
                                    )
                             )
         )

## ------------------------------------------------------------------------
sample_data$PNRCreateDate <- as.Date(sample_data$PNRCreateDate) 
sample_data$ServiceStartDate <- as.Date(sample_data$ServiceStartDate)
sample_data%<>% 
  mutate(days_pre_booked=as.numeric(floor( difftime(ServiceStartDate,
                                                    PNRCreateDate,units=c("days")))))

## ------------------------------------------------------------------------
sample_data%<>%
  select(PNRLocatorID, uid, PaxName, ServiceStartDate, BookingChannel, TotalDocAmt,
         UFlyRewardsNumber,UflyMemberStatus, age_group,true_origin,true_destination,
         round_trip,group_size,group, seasonality,days_pre_booked)

#This may take a considerable amount of time
customer_data <- sample_data %>%
  group_by(PNRLocatorID,uid,PaxName) %>%
  summarise(ServiceStartDate=first(ServiceStartDate),
            BookingChannel=first(BookingChannel), 
            avg_amt=max(TotalDocAmt),
            UFlyRewards=first(UFlyRewardsNumber),
            UflyMemberStatus=first(UflyMemberStatus),
            age_group=last(age_group),
            true_origin=first(true_origin),
            true_destination=first(true_destination),
            round_trip=first(round_trip),
            group_size=first(group_size),
            group=first(group), 
            seasonality=last(seasonality), 
            days_pre_booked=max(days_pre_booked))

#Retaining only those attributes that are meaningful for clustering
customer_data%<>%
  select(-PNRLocatorID,-uid,-PaxName,-ServiceStartDate,-UFlyRewards)
  nrow(sample_data)

  #Granularity of data was reduced to customer level
  nrow(customer_data)


## ------------------------------------------------------------------------
#Min-Max normalization: x= x-max/max-min
customer_data_t=read.csv("customer_data.csv")
customer_data=customer_data_t[,-1]
normalize <- function(x){return ((x - min(x))/(max(x) - min(x)))}

ungrouped <- ungroup(customer_data)

customer_data_km = mutate(ungrouped,
                     avg_amt = normalize(avg_amt),
                     days_pre_booked = normalize(days_pre_booked),
                     group_size=normalize(group_size))

write.csv(customer_data_km,"customer_data_km.csv")

## ------------------------------------------------------------------------
customer_data_fc=data.frame(sapply(customer_data_km[,c("BookingChannel","UflyMemberStatus","age_group","true_origin","true_destination","seasonality")],as.factor))
customer_data_tmp=data.frame(sapply(customer_data_fc,as.numeric))
customer_data_nm=cbind(customer_data_km[,c(4,9,10,11,13)],customer_data_tmp)
customer_data_norm=sapply(customer_data_nm[,-1],FUN = normalize)


## ------------------------------------------------------------------------
#Calculating Gower distance

#Converting columns to Factor variables
customer_data_fc=data.frame(sapply(customer_data_km[,c("BookingChannel","UflyMemberStatus","age_group","true_origin","true_destination","seasonality","group","round_trip")],as.factor))

customer_data_nm=cbind(customer_data_km[,c(4,10,13)],customer_data_fc)
library(cluster)
memory.limit(size = 1000000)

## ------------------------------------------------------------------------
#Performing hierarchial clustering
gower_dist=daisy(customer_data_nm, metric = "gower")
customer_hcl=hclust(gower_dist,method="ward.D2")

#plot
plot(customer_hcl,hang=0,label=F,main="Cluster Dendogram")

travel_groups = cutree(customer_hcl,5)
customer_num_agg = aggregate(customer_data_km[,c(4,10,13)],list(travel_groups),median)

#Calculating mode
getmode <- function(x) {
     ux <- unique(x)
     ux[which.max(tabulate(match(x, ux)))]
 }
customer_data_ch= data.frame(sapply(customer_data_km[,c("BookingChannel","UflyMemberStatus","age_group","true_origin","true_destination","seasonality","group","round_trip")],as.character))

customer_cat_agg = aggregate(customer_data_ch,list(travel_groups),FUN=getmode)

travel_group_characteristcs=merge(customer_cat_agg,customer_num_agg)
travel_group_characteristcs

sil_hclust=silhouette(travel_groups,gower_dist)
summary(sil_hclust)

## ------------------------------------------------------------------------
#Performing PAM clustering
library(fpc)
kmed<-pam(gower_dist,8)

sil_pam=silhouette(kmed,gower_dist)
summary(sil_pam)




## ------------------------------------------------------------------------
#Performing k-modes clustering
customer_data_modes=customer_data
#Creating bins to perform k-modes

get_amt_bins <- function(x) {
     if (x<164){'100-164'}
  else if (x>164 & x<278){'165-275'}
  else if (x>278 & x<389){'276-400'}
  else ('>400')
}

get_days_bins <- function(x) {
     if (x<20){'1-20'}
  else if (x>20 & x<50){'21-50'}
  else if (x>51 & x<80){'51-80'}
  else ('>80')
   }

customer_data_modes$avg_amt=t(data.frame(lapply(customer_data[,4],get_amt_bins)))

customer_data_modes$days_pre_booked=t(data.frame(lapply(customer_data$days_pre_booked,get_days_bins)))

customer_modes_factor=data.frame(sapply(customer_data_modes[,-c(1,2)],as.factor))

customer_kmodes=kmodes(customer_modes_factor,8)

#Calculating Gower distance for the pruned dataset
gower_dist_modes=daisy(customer_modes_factor, metric = "gower")

sil_kmodes=silhouette(customer_kmodes$cluster,gower_dist_modes)
summary(sil_kmodes)


## ------------------------------------------------------------------------
kmed_binned<-pam(gower_dist_modes,8)

sil_pam_binned=silhouette(kmed_binned,gower_dist_modes)
summary(sil_pam_binned)


## ------------------------------------------------------------------------
#k-modes clustering
plot(customer_data_nm[,c(1,3:9)], col = (kmed$clustering), main = "K medoids clustering",pch = 20, cex = 2)

#hierarchical clustering
plot(customer_data_nm[,c(1,3:9)], col = (travel_groups), main = "Hierarchical clustering",pch = 20, cex = 2)

#k-medoids clustering
plot(customer_data_nm[,c(1,3:9)], col = (customer_kmodes$cluster), main = "K-modes clustering",pch = 20, cex = 2)

## ------------------------------------------------------------------------
by_booking_channel=customer_data%>%group_by(BookingChannel)%>%summarise(sum(avg_amt))
by_booking_channel=setNames(by_booking_channel,c("Booking_channel","sum_of_AVG_amount"))

#g1<-ggplot(customer_data)+aes(customer_data$days_pre_booked,customer_data$avg_amt)+geom_point(pch=15,color=kmed_binned$clustering,size=1)
#g1

## ------------------------------------------------------------------------
g1=ggplot(data=customer_data,aes(x=seasonality))+geom_bar()
g1

## ------------------------------------------------------------------------
g2=ggplot(data=customer_data,aes(x=UflyMemberStatus))+geom_bar()
g2

## ------------------------------------------------------------------------
g3=ggplot(data=customer_data, aes(x=avg_amt, fill=age_group)) + geom_density(alpha=.4)
g3

## ------------------------------------------------------------------------
ggplot(by_booking_channel,aes(x=Booking_channel,y=sum_of_AVG_amount))+ geom_point(position = 'jitter',color='blue',alpha=.5) +geom_line(aes(colour = sum_of_AVG_amount, group = sum_of_AVG_amount))

## ------------------------------------------------------------------------
g4=ggplot(data=customer_data, aes(x=avg_amt, fill=seasonality)) + geom_density(alpha=.4)
g4

