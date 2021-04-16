# First read the data into R (10 Points)
data <- read.table("hw3_data.txt", sep="\t", header=T)

# Double check rows and columns
dim(data)

# estimate the number of clusters
  # 前三個是cluster analysis 要用到的package
library(cluster)
library(NbClust)
library(factoextra)
library(dplyr)

# Part A. Standardize Data
X <- data[,c("Inv_1M_Bef", "cust_age", "cust_tenure", "tottrans")] #[row,column]
X.scaled <- scale(X, center=TRUE, scale = TRUE) # standardize data - scale
X.scaled

######

# Part B. Elbow Plot 
# Estimating the number of clusters: Elbow method
# elbow plot - showing the results for 1 up to 20 clusters 
  # fviz_nbclust - Dertermining and Visualizing the Optimal Number of Clusters
  # Set the maximum number of clusters to consider = 20.
  # Set the number of starting values to 1000.
  # Set the number of iterations for the algorithm to 1000.
  # Set the y range of the elbow plot to go from 0 to 8000.
fviz_nbclust(x=X.scaled, FUNcluster = kmeans, nstart=1000, iter.max=1000, method="wss", k.max = 20) + labs(title="Optimal Number of Clusters: Elbow Plot") + coord_cartesian(ylim=c(0,8000)) + geom_line(size=2)
  # 最多的是5, 所以optimal number of clusters = 5

# save 
  # ggsave是一個方便的功能，節省了圖。它默認為保存最後的圖，顯示的默認大小使用當前圖形設備的大小
  # 保存R基礎圖形, is a convenient function for saving a plot
ggsave("Elbow Plot.png", dpi = 300) # Elbow Plot 為檔案名字

######

# Part C. NbClust approach to number of clusters
  # estimate the optimal number of clusters 
set.seed(1234) #因为method选择的是kmeans，所以如果不设定种子，每次跑得结果可能不同
nb_clust <- NbClust(X.scaled, distance = "euclidean", max.nc=20, method = "kmeans")
  # visualization + save
fviz_nbclust(nb_clust)
ggsave("NbClust.png", dpi = 300)

# 1,2,3,4,5 為segment 1,2,3,4,5

######

# Part D.  Run K-Means cluster analysis using the optimal number of clusters found in Steps B and C
# Let's run a K-means cluster analysis on 5 clusters
# centers = k, k為之前求出來的 optimal number of clusters
set.seed(123) # use for random but same everytime?
cluster.results <- kmeans(X.scaled, centers=5, iter.max=1000, nstart=1000)
cluster.results # 各自都標上了1,2,3,4,5

# 上面是run the cluster analysis, 
# 下面是reorder your cluster numbers. 

# examine cluster centers 
# 2 means 小數點後第二位, t()為行列轉換
t(round(cluster.results$centers,4))

# extract the Inv_1M_Bef column
Inv_centers <- as.data.frame(cluster.results$centers[,1])
colnames(Inv_centers) <- c("Inv_1M_Bef") # rename column

# old order and new order # 不懂這part
# 運行相同的算法兩次，並獲得不同的群集編號標籤去relabel/reorder
# 自動按照大到小label, 在round一次變成小到大
Inv_centers <- Inv_centers %>% 
  mutate(Old.Clust.Num = c(1:5)) %>% 
  arrange(Inv_centers) %>% 
  mutate(New.Clust.Num = c(1:5))

# round Inv_1M_Bef to four decimal places
Inv_centers[,1] <- round(Inv_centers[,1],4)

# 創一個table, 以下三個columns (我覺得是錯的,上面那個就是答案)
  # 1) Average value of Inv_1M_Bef (cluster center values from your k-means algorithm)
  # 2) Old cluster assignment number, and 
  # 3) new cluster assignment number. Round the average values of Inv_1M_Bef to four decimal places.
table <- cbind("Average value of Inv_1M_Bef" = round(mean(Inv_centers$Inv_1M_Bef), 4), 
                 "Old cluster assignment number" = Inv_centers$Old.Clust.Num,
                 "New cluster assignment number" = Inv_centers$New.Clust.Num)
table

# report the frequency count of the number of customers in each new cluster number by using the table 
# how many customers are in each segment? (named: New.Clust.Num)
# 把那兩個column加到X.scaled, 然後從中取得他的New.Clust.Num的table
X.scaled <- X.scaled %>% as.data.frame %>% 
  mutate(Old.Clust.Num = cluster.results$cluster) %>% 
  # can you for loop and then use cbind a whole column first and then change each column first
  mutate(New.Clust.Num = case_when(Old.Clust.Num == 1 ~ 3, 
                                   Old.Clust.Num == 2 ~ 4,
                                   Old.Clust.Num == 3 ~ 1,
                                   Old.Clust.Num == 4 ~ 5,
                                   Old.Clust.Num == 5 ~ 2)) 

table(X.scaled$New.Clust.Num)
------------------------------------------------------------------------------
# different method to map the old clust num to new clust num
X.scaled <- X.scaled %>% as.data.frame %>% 
  mutate(Old.Clust.Num = cluster.results$cluster) %>% 
  mutate(New.Clust.Num = cluster.results$cluster)

X.scaled$New.Clust.Num[X.scaled$New.Clust.Num == 3] <- 0
X.scaled$New.Clust.Num[X.scaled$New.Clust.Num == 1] <- 3
X.scaled$New.Clust.Num[X.scaled$New.Clust.Num == 0] <- 1
X.scaled$New.Clust.Num[X.scaled$New.Clust.Num == 4] <- 0
X.scaled$New.Clust.Num[X.scaled$New.Clust.Num == 2] <- 4
X.scaled$New.Clust.Num[X.scaled$New.Clust.Num == 5] <- 2
X.scaled$New.Clust.Num[X.scaled$New.Clust.Num == 0] <- 5

table(X.scaled$New.Clust.Num)

# create 一個table 為new.clust.num 跟 the average cluster center values for Inv_1M_Bef, cust_age, cust_tenure, and tottrans 
# use raw data
# aggregate function 聚合函數
data_2 <- data %>% mutate(New.Clust.Num = X.scaled$New.Clust.Num)
table_2 <- data_2 %>% group_by(New.Clust.Num) %>% summarise(Average_Inv_1M_Bef = round(mean(Inv_1M_Bef), 2),
                                                          Average_cust_age = round(mean(cust_age), 2), 
                                                          Average_cust_tenure = round(mean(cust_tenure), 2),
                                                          Average_tottrans = round(mean(tottrans),2))
table_2 <- data.frame(table_2)
table_2  

######

# Part E. Interpretation of the segments

######

# Part F. Amount of money lost by cluster and good – bad service
# calculate average of Inv_Chg by categ (bad and good)
Average_Inv_Chg <- data_2 %>% group_by(categ, New.Clust.Num) %>% 
  summarise(Average_Inv_Chg = mean(Inv_Chg))
Average_Inv_Chg <- data.frame(Average_Inv_Chg) # 算出bad跟good 分別的平均


# calculate average of Inv_Chg by new.clust.num first and cate
Average_Inv_Chg_2 <- data_2 %>% group_by(New.Clust.Num, categ) %>% 
  summarise(Average_Inv_Chg = mean(Inv_Chg))
Average_Inv_Chg_2 <- data.frame(Average_Inv_Chg) # 算出bad跟good 分別的平均


# 1) New Cluster Number, 2) Average Inv_Chg BAD, 3) Average Inv_Chg GOOD, 4) Average Inv_Chg GOOD – Average Inv_Chg BAD.
F_table <- cbind("New_Clust.Num" = c(1:5), 
                 "Average_Inv_Chg_BAD" = Average_Inv_Chg[1:5,3],
                 "Average_Inv_Chg_GOOD" = Average_Inv_Chg[6:10,3])

# add one more column and then round to two decimal places
F_table <- F_table %>% data.frame %>% mutate("Average_Inv_Chg" = Average_Inv_Chg_GOOD - Average_Inv_Chg_BAD) %>% as.data.frame

F_table$Average_Inv_Chg <- c(round("Average_Inv_Chg_GOOD" - "Average_Inv_Chg_BAD"))

F_table <- round(F_table,2)

F_table

######

# Part G. Identifying future customers who might take out a lot of money with bad service

data_G <- read.table("12000 new customers.txt", sep="\t", header=T)

# Double check rows and columns
dim(data_G)

# 1. Standardize the new dataset to a mean of 0 and a standard deviation of 1.
G <- data_G[,c("Inv_1M_Bef", "cust_age", "cust_tenure", "tottrans")] #[row,column]
G.scaled <- scale(G, center=TRUE, scale = TRUE) # standardize data - scale
G.scaled

# 2. Calculate the Euclidean distance for each one of these standardized new customers to each one of your standardized cluster centers
# 2-1 extract the matrix results$centers from Part D.
G.centers <- cluster.results$centers

# 2-2 reorder/remap it to match the new cluster number (不懂)
centers.ordered <- G.centers %>% as.data.frame %>% arrange(Inv_1M_Bef)

rownames(centers.ordered) <- NULL # 變正序

# 2-3 assign 12000 customers to the nearest clusters center
# 在k均值聚類後為新數據分配聚類的簡單方法
Nearest.cluster.center <- function(x) {
    cluster.dist <- apply(centers.ordered, 1, function(y) sqrt(sum((x-y)^2)))
    return(which.min(cluster.dist)[1])
  }  
  
clusters <- apply(G.scaled, 1, Nearest.cluster.center)

Newdata <- cbind(data_G, clusters)

table(clusters) 
