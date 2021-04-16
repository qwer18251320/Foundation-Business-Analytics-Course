# First read the data into R (10 Points)

data <- read.table("assignment1.txt", sep="\t", header=T)

# Double check rows and columns
dim(data)

###

# A. Frequency counts of surveys and by month (20 points)

# drop Na value from satis_survey and survey_date
library(tidyr)
A.cleaned <- data %>% drop_na(satis_survey, survey_date)

satis_survey_table <- table(A.cleaned$satis_survey)
satis_survey_table_v <- as.vector(satis_survey_table)
satis_survey_value <- c(1, 2, 3, 4, 5)
satis_survey_rating <- c("very dissatisfied", "dissatisfied", "neither satisfied nor dissatisfied", "satisfied", "very satisfied")

# 1 report counts of survey values
satis_survey_table

# repots percentages of survey values
for (i in 1:5) {
  print(paste(round(satis_survey_table_v[i]*100/sum(satis_survey_table_v)), "% of surveys had a", satis_survey_rating[i], "rating of", satis_survey_value[i]))
}

# 3
library(lubridate)
date_v <- as.Date(A.cleaned$survey_date, format = '%m/%d/%Y')
month <- month(date_v)
counts <- c(table(month))

as.data.frame(counts, row.names = month.name)

###

# B. Remove customers with impossible values (20 points)
attach(data)

target = ifelse(total_investments < 0 | is.na(total_investments) == TRUE |
                  cust_age <= 0 | is.na(cust_age) == TRUE |
                  cust_tenure <= 0 | is.na(cust_tenure) == TRUE |
                  tottrans <= 0 | is.na(tottrans) == TRUE, 1, 0)

target_cust <- cust_id[target == 1]

targetID <- unique(target_cust)

#1
# customers removed
length(targetID)

# rows removed
B.rows_removed <- cust_id %in% targetID
length(B.rows_removed[B.rows_removed==TRUE])

# cleaning process
B.cleaned <- data[!B.rows_removed,]

# remaining rows
nrow(B.cleaned)

# remaining customers
length(unique(B.cleaned$cust_id))

###

# C. Remove any customers with outlier values (20 Points)

upper.monthly.investments <- quantile(B.cleaned$total_investments, .99)
upper.monthly.transactions <- quantile(B.cleaned$tottrans, .99)

#1
# top 1% values of investments
upper.monthly.investments

# top 1% values of transactions
upper.monthly.transactions

is.an.outlier <- ifelse(B.cleaned$total_investments >= upper.monthly.investments |
                          B.cleaned$tottrans >= upper.monthly.transactions, 1,0)

outlier_cust <- B.cleaned$cust_id[is.an.outlier == 1]
outlier_ID <- unique(outlier_cust)

# additional customers removed
length(outlier_ID)

# additional rows removed
C.rows_removed <- B.cleaned$cust_id %in% outlier_ID
length(C.rows_removed[C.rows_removed==TRUE])

# cleaning process
C.cleaned <- B.cleaned[!C.rows_removed,]

# remaining rows
nrow(C.cleaned)

# remaining customers
length(unique(C.cleaned$cust_id))


###

# D. Data Visualization (50 Points)

library(dplyr)
library(reshape2)
library(ggplot2)
library(lubridate)

options(scipen=999)

#Clean first
D.cleaned <- na.omit(C.cleaned)

#append column for years only
date_v <- as.Date(D.cleaned$survey_date, format = '%m/%d/%Y')
D.years <- year(date_v)
D.withyear <- cbind(D.cleaned, D.years)

# grouped sum(total investments) dataframe
myframe <- data.frame(matrix(NA,5,0))

for (i in 2011:2019) {
  temp_set <- D.withyear[D.withyear$D.years == i,]
  temp_group <- group_by(temp_set, satis_survey)
  mytable <- summarise(temp_group,col = sum(total_investments))
  myframe <- cbind(myframe,mytable[,2])
}

colnames(myframe) <- c(2011:2019)
myframe$satis_survey <- c(1:5)


# This process makes a dataframe with conbination of values in to single columns
#Just run it and take a look
myframe_melted = melt(myframe, id.vars = 'satis_survey')


#change data type of satis_survey from int to str
myframe_melted$satis_survey <- as.character(myframe_melted$satis_survey)


# 1
#investments vs rating
ggplot(myframe_melted, aes(x = variable, y = value)) + 
  geom_line(aes(color = satis_survey, group = satis_survey)) +
  ggtitle("Sum of Total Investments vs Satisfactory Rating") +
  xlab("Year") + ylab("Total Investment") + 
  labs(color = "Satisfaction Rating") +   
  scale_y_continuous(labels=scales::dollar_format(scale = .000001, suffix = "M"))
ggsave("SumInvest_SatisRate.png", dpi = 300)

# 2
#Average customer tenureby grouping each satisfaction survey rating

D.cleaned1<-D.cleaned%>%group_by(satis_survey)%>%summarize(avg_T=mean(cust_tenure))
ggplot(D.cleaned1,aes(x=satis_survey,y=avg_T))+geom_col(fill = 4)+ ggtitle("Average Customer Tenure vs Satisfactory Rating") + xlab("Satisfactory Rating") + ylab("Average customer Tenure")

# 3
#transactions vs survey rating
age_grp <- c(paste(seq(0, 55, by = 15), seq(0 + 15 - 1, 60 - 1, by = 15),
                   sep = "-"), paste(60, "+", sep = ""))
AgeGroup <- cut(D.cleaned$cust_age, breaks = c(seq(0, 60, by = 15), 
                                                Inf), labels = age_grp, right = FALSE)

ggplot(D.cleaned, aes(x=satis_survey, y=tottrans/1000, color = AgeGroup, fill= AgeGroup)) + 
  geom_col() + ggtitle("Total Transactions vs Satisfactory Rating") + 
  xlab("Satisfactory Rating") + ylab("Total Transactions [Thousand]")
ggsave("TotTrans_SatisRate.png", dpi = 300)
