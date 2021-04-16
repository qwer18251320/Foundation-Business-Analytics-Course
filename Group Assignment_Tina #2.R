# First read the data into R (10 Points)
data <- read.table("assignment 1b.txt", sep="\t", header=T)

# Double check rows and columns
dim(data)

###

# Part E: Drop any customers who have noncontiguous row of data
library(dplyr) # use for data cleaning: filter, arrange, select, mutate, groupby, %>%
library(tidyr) # use fore data cleaning: gather, spread, separate
library(lubridate)

# 1. group id based on it cust_id
# group customer id, inside the customer id %>%
# 2. mutate(): use to make a new column, 
# use mutate and inside that group by, find the different between year month
# 3. lag(column, n): use for time sorting, to catch the previous number  
# use lag(): to calculate "yearmonth - lag(yearmonth, default = )" for previous number what you want 
# also change the default to other number because na is default
# default = yearmonth[1], means start at 0, not na
# time_diff is the name for new column
group_id <- data %>% group_by(cust_id) %>% 
  mutate(time_diff = yearmonth - lag(yearmonth, default = yearmonth[1]))
# 解釋: data裡面的cust_id把他group_by住, 然後用%>%把他圈起來 gogo往下, 
# 使用mutate去創造一個新的column, 命名為time_diff, 
# lag去使得後面一行揀去前面一行會得出的結果, 然後沒有的, 不顯示na改為0
 
# select the time_diff you want
# as we know that time_diff value 0,1,89 is correct
# 4. [!newcol %in% c(0,1,89)]
# %in%: does value a is include or not in table, true or false
# %in%: tells you which items from the left hand side are also in the right hand side
E.target_cust <- group_id$cust_id[!group_id$time_diff %in% c(0,1,89)] 
  # false = 0,1,89; true = not 0,1,89
  # value not 0,1,89 in time_diff from group_id
E.target_ID <- unique(E.target_cust)
  # unique: special one, the TRUE one

# customers removed 
# remove 4219 id
length(E.target_ID)

# rows removed
E.rows_removed_processing <- group_id$cust_id %in% E.target_ID
  # group_id中cust_id這個column在target_id裡有符合這些id的row
E.rows_removed <- length(E.rows_removed_processing[E.rows_removed_processing == TRUE])
E.rows_removed
  # remove 251728 rows

# cleaning process
E.cleaned <- data[!E.rows_removed_processing,]
  # extract出來(！不是, 不屬於)rows_removed的rows

# remaining customers
length(unique(E.cleaned$cust_id))
  # unique: extract 出在cleaned這個data裡面cust_id的columns, 現在有多少id
  # remaining 2809 customers

# remaining rows
nrow(E.cleaned)
  # calculate the rows not belong with rows_removed, remaining 117140 rows

######

#Part F. Remove any customers who were surveyed more than 1 time, or not at all
# 1. 先抓取出有survey_date轉化 date format, 並且create 一個column 給這些survey date
# convert to factor variable to a date variable, and create a new column to count times of survey_date later 
# 把cleaned這個date抓出來gogo, mutate, 創一個新column 叫survey_dates, 
# 它是由cleaned 這個data裡面survey_date這個column轉化出來要, 
# 要轉化他的data format 從factor variable必成date variable
# 新增的column裡面會顯示有survey_date的在上面, 沒有的會是N/A
F.data <- E.cleaned %>%
  mutate(survey_dates = as.Date(survey_date, format = '%m/%d/%Y'))

# 2. 再計算survey date 中每個cust_id有幾次survey date, 要先group_by cust_id
# create new column to count the survey__times
# is.na to check the survey_date for non-missing values through mutate
# 在date_v 裡面 gogo group_by裡面的cust_id圈住 再gogo, 
# 以mutate 創一個新的column 叫survey_times, 計算有多少survey_dates, 
# !is.na 是沒有n/a value 在survey_date裡面的. 在每個cust_id裡面
# survey_times 顯示的是每個cust_time有幾個survey_date
# 創新的column 把前一個column的日期轉成次數
F.data <- F.data %>% group_by(cust_id) %>% 
  mutate(survey_times = length(survey_dates[!is.na(survey_dates)])) 

# a frequency table showing the counts of the numbers of customers falling into 0, 1, 2, 3…K surveys,
frequency_0 <- length(unique(F.data$cust_id[F.data$survey_times == 0]))
frequency_1 <- length(unique(F.data$cust_id[F.data$survey_times == 1]))
frequency_2 <- length(unique(F.data$cust_id[F.data$survey_times == 2]))
frequency_3 <- length(unique(F.data$cust_id[F.data$survey_times == 3]))
frequency_4 <- length(unique(F.data$cust_id[F.data$survey_times == 4]))
frequency_5 <- length(unique(F.data$cust_id[F.data$survey_times == 5]))
frequency_6 <- length(unique(F.data$cust_id[F.data$survey_times == 6]))

frequency_table <- data.frame(Survey_Times = c(0,1,2,3,4,5,6),
                              Counts_of_customers = c(frequency_0,frequency_1,frequency_2,frequency_3,frequency_4,frequency_5,frequency_6))

frequency_table

# another method of frequency table
F.frequency <- F.data %>% group_by(cust_id) %>% count(survey_times)
table(F.frequency$survey_times)

# count cust_id who has 1 survey times
  # 2476 customers has 1 survey
length(unique(F.data$cust_id[F.data$survey_times == 1]))

# count the cust_id who have invalid value
  # 333 customers has invalid valye
length(unique(F.data$cust_id[F.data$survey_times != 1]))

# count the maximum number of times any customer was surveyed in the data
  # maximum survey times is 6
max(F.data$survey_times)

# count how many customers were surveyed this maximum number of times?
  # only 1 customer has 6 survey times
length(unique(F.data$cust_id[F.data$survey_times == 6]))

### Processing - Removing data 
F.target_cust <- F.data$cust_id[F.data$survey_times != 1]

# customers removed
  # 333 customers removed
cust_removed_processing <- unique(F.target_cust)
cust_id_removed <- length(cust_removed_processing)
cust_id_removed

# rows removed
  # 17094 row removed
row_removed_processing <- E.cleaned$cust_id %in% cust_removed_processing
row_removed <- length(row_removed_processing[row_removed_processing == TRUE])
row_removed

### Processing - cleaning data
F.cleaned <- E.cleaned[!row_removed_processing,]

# remaining customers
  # 2475 customers remain
remained_cust <- length(unique(F.cleaned$cust_id))
remained_cust

# remaining rows
  # 100046 rows remain
nrow(F.cleaned)

######

# Part G.Compare 1 month before being surveyed to 3 months after being surveyed 

# a:Inv_1M_Bef
# every customer surveyed must have a months_since_survey = -1
# 1. 先找出每一個cust_id的months_since_survey,要為-1開頭, 
# 2. remove那些不是-1的
    # How many customers should you remove?
# 3. For customers that have -1的, extract the cust_id and total_investments where months_since_survey = -1. Rename total_investments to Inv_1M_Bef.

# 2158 個customers have a month_since_survey == -1
G_a.cust <-F.cleaned$cust_id[F.cleaned$months_since_survey == -1]
G_a.cust_1 <- unique(G_a.cust)
length(G_a.cust_1)

# 318 個 remove cust_id do not have month_since_survey != -1
G_a.cust_remove <- unique(F.cleaned$cust_id[!F.cleaned$cust_id %in% G_a.cust])
length(G_a.cust_remove)  

# extract cust_id and total_investments
# extract cust_id & total_investment belong with months_since_survey == -1
G_a.total <- F.cleaned$total_investments[F.cleaned$months_since_survey == -1]
G_a.data <- data.frame(cust_id = G_a.cust,
                       Inv_1M_Bef = c(G_a.total))

###  
# b:Inv_3M_Aft
# 2369 個 cust_id have a month_since_survey >= 3
G_b.cust_L <- F.cleaned$cust_id[F.cleaned$months_since_survey >= 3]
G_b.cust_L1 <- unique(G_b.cust_L)
length(G_b.cust_L1)

# How many customers in the data set are there that left before months_since_survey = 3
# 107 個cust_id have a month_since_survey < 3
G_b.cust_S <- F.cleaned$cust_id[!F.cleaned$cust_id %in% G_b.cust_L]
G_b.cust_S1 <- unique(G_b.cust_S)
length(G_b.cust_S1)  

# set INV_3M_Aft = total_investments for customers where months_since_survey = 3
G_b.data_3 <- F.cleaned %>% group_by(cust_id) %>% #group_id 
  filter(max(months_since_survey) >= 3) %>% # max(month_since_survey) 為91, >= 3 為true
  filter(months_since_survey == 3) %>% # 要等於3的
  select(cust_id, months_since_survey, Inv_3M_Aft = total_investments)

# set INV_3M_Aft = total_investments = 0 for months_since_survey < 3
G_b.data_0 <- F.cleaned %>% group_by(cust_id) %>% #group_id 
  filter(max(months_since_survey) < 3) %>% # max(month_since_survey) 為91, < 3 為false
  filter(months_since_survey == max(months_since_survey)) %>% # 要<3
  select(cust_id, months_since_survey, Inv_3M_Aft = total_investments)

G_b.data_0$Inv_3M_Aft <- 0

# combine and delete column "month_since_survey" 
G_b.data <- rbind(G_b.data_3, G_b.data_0)
G_b.data <- G_b.data[,-2] 

###
# c
# extract the cust_id and satis_survey for each customer where months_since_survey = 0. 
# Create a frequency table of the counts of the number of customers answering 1, 2, 3, 4, and 5. Report those values below.
G_c.data <- F.cleaned %>% filter(months_since_survey == 0) %>% select(cust_id, satis_survey)
table(G_c.data$satis_survey)

###
# d
# 1. once you’ve performed all of these calculations, create one row for each customer by combining the data you just extracted from Steps Ga), Gb) and Gc) above
# 2. Calculate the change in investments as follows: Inv_Chg = Inv_3M_Aft – Inv_1M_Bef.
    # use merge or join on cust_id
    # Only cust_ids that are in all three tables created in steps a, b, and c above should be included in the final dataset.
# 3. Exclude any cust_ids in some tables but not others.
  # How many customers are there in the final dataset, that meet the criteria of everything above?
# 4. if a customer answered a survey with a “1” or a “2” calculate the average change in investment dollars for these customers (e.g. mean(Inv_Chg) where satis_survey = 1 or 2).
# 5. calculate the average change in investment dollars where customers answered a “4” or a “5” (e.g. mean(Inv_Chg) where satis_survey = 4 or 5) 
# 6. Report the average change in investment dollars for customers for bad service experiences and also for customers with good service encounters, rounded to two decimal places
# 7. Finally, calculate the difference between these two values (average Inv_Chg when satis_survey = 4,5 – average Inv_Chg when satis_survey = 1,2)
  # On average, how much money is the firm losing per customer by having a bad customer service experience (satis_survey = 1,2) vs. a good service experience (satis_survey = 4,5)? Does the difference seem important or significant? Please comment.
G_d.data <- merge(G_a.data, G_b.data, by = intersect(1, 1))
G_d.data <- merge(G_d.data, G_c.data, by = intersect(1, 1))

# customers in the final dataset
length(unique(G_d.data$cust_id))

G_d.data <- G_d.data %>% mutate(Inv_Chg = Inv_3M_Aft - Inv_1M_Bef)

# Mean of investments change for customers answeared 1 or 2 in satis_survey
bad_service_exp <- G_d.data %>% 
  filter(satis_survey == 1 | satis_survey == 2) %>% 
  summarize(Mean = round(mean(Inv_Chg), digits=2)) # 小數點後第二位

bad_service_exp

# Mean of investments change for customers answeared 1 or 2 in satis_survey
good_service_exp <- G_d.data %>% 
  filter(satis_survey == 4 | satis_survey == 5) %>%
  summarize(Mean = round(mean(Inv_Chg), digits=2))

good_service_exp

# difference between good_service_exp and bad_service_exp
good_service_exp - bad_service_exp



