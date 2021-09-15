library(tidyverse)
library(cluster)
library(Rtsne)
library(factoextra)

the_data <- read_tsv("dataset/marketing_campaign.csv")

## check missing information
colSums(is.na(the_data))

## remove records with missing income
the_data <- the_data[!is.na(the_data$Income), ]

## look at summary
summary(the_data)
## there seems to be an outlier in the income variable. We can 
## see that the maximum value is much larger than the value
## of the 3rd quantile. This becomes clear when we plot the box plot:

ggplot(the_data) + geom_boxplot(aes(Income))

## It is best to remove outlier
the_data <- the_data %>% filter(Income < 600000)

## we also note that the two variables Z_costcontact and Z_revenue do not
## change. We therefore remove them since they add no information:

the_data <- the_data[, -which(names(the_data) %in% c("Z_CostContact", "Z_Revenue"))]

## we also note that the minimum of year of birth is 1893. A 
## closer look reveals that three customers have year of birth
## as 1900, 1893, and 1899. 

the_data[the_data$Year_Birth < 1920, ]

## we choose to remove these customers since they are outliers
## and there is a high probability that the data is not correct

the_data <- the_data[the_data$Year_Birth >= 1920, ]

## look at the structure
str(the_data)

## convert date to date object
the_data$Dt_Customer <- as.Date(the_data$Dt_Customer, "%d-%m-%Y")

## note that there are strange values in marital status
unique(the_data$Marital_Status)
## rename them as single
the_data <- the_data %>% mutate(Marital_Status = replace(Marital_Status, Marital_Status == "YOLO" | Marital_Status == "Absurd", "Single"))
  
## covert some character variables to factors
the_data$Education <- as.factor(the_data$Education)
the_data$Marital_Status <- as.factor(the_data$Marital_Status)

## let us create some useful variables. First, we create
## the total amount spent by each individual. We also create
## the percent spent on the items wine, meat + fish, fruits,
## sweets, and gold
the_data <- the_data %>% mutate(total_spent = MntWines + MntFruits + 
                                  MntMeatProducts + MntFishProducts +
                                  MntSweetProducts + MntGoldProds,
                                pct_wine = MntWines / total_spent,
                                pct_fruit = MntFruits / total_spent,
                                pct_meat = (MntMeatProducts + MntFishProducts) / total_spent,
                                pct_sweets = MntSweetProducts / total_spent,
                                pct_gold = MntGoldProds / total_spent)

## next we create a variable that stores the number of months
## since the individual has been enrolled. We do not know
## the date at which the data was collected, but it doesnt
## matter since we care about the relative values. We pick
## 2015 as the date since the largest date of enrollement in
## the data set is 2014-06-29

end_date = as.Date("01-01-2015", "%d-%m-%Y")
the_data <- the_data %>% 
  mutate(months_enrolled = 
           as.numeric(difftime(end_date, 
                               Dt_Customer, 
                               units = 'days'))/(365.25/12))

## now visualise the data
the_data %>% 
  group_by(Education) %>% 
  summarise(avg_wine = mean(pct_wine, na.rm = TRUE)) %>% 
  ggplot() + geom_bar(aes(Education, avg_wine), stat = "identity",
                      fill = "blue")

the_data %>% 
  group_by(Marital_Status) %>% 
  summarise(avg_wine = mean(pct_wine, na.rm = TRUE)) %>% 
  ggplot() + geom_bar(aes(Marital_Status, avg_wine), stat = "identity",
                      fill = "blue") + 
  theme(axis.text.x = element_text(angle = 45, vjust = 0.5))

the_data %>% 
  group_by(Kidhome) %>% 
  summarise(avg_wine = mean(pct_wine, na.rm = TRUE)) %>% 
  ggplot() + geom_bar(aes(Kidhome, avg_wine), stat = "identity",
                      fill = "blue") 

the_data %>% 
  group_by(Teenhome) %>% 
  summarise(avg_wine = mean(pct_wine, na.rm = TRUE)) %>% 
  ggplot() + geom_bar(aes(Teenhome, avg_wine), stat = "identity",
                      fill = "blue") 

ggplot(the_data, aes(Income, pct_wine)) + geom_point() + 
  geom_smooth() + scale_x_log10()

ggplot(the_data, aes(Income, total_spent)) + geom_point() + 
  geom_smooth() + scale_x_log10()

## cluster analysis
the_data <- the_data %>% select(Year_Birth, Education, Marital_Status,
                                Income, Kidhome, Teenhome, Recency,
                                NumDealsPurchases, NumWebPurchases, NumWebVisitsMonth, 
                                Complain, total_spent, pct_wine, pct_fruit,
                                pct_meat, pct_sweets, pct_gold,
                                months_enrolled)



gower_df <- daisy(the_data, metric = "gower")
summary(gower_df)

cluster_number <- 2:10
sil_values <- map_dbl(cluster_number, function(x){
  pam_clusters = pam(as.matrix(gower_df), diss = TRUE, k = x)
  pam_clusters$silinfo$avg.width
})
cluster_data <- data.frame(Clusters = cluster_number, Silhouette_width = sil_values)
ggplot(cluster_data, aes(Clusters, Silhouette_width)) + geom_point() + 
  geom_line() + scale_x_continuous(breaks = c(1:10))
## note that the above could have been done using the code
# fviz_nbclust(as.matrix(gower_df), pam, method = "silhouette")

## we can use the elbow method using the following:
fviz_nbclust(as.matrix(gower_df), pam, method = "wss")
## this method suggests 3 clusters as well

## perform the cluster analysis using the optimal number of
## clusters
pam_data = pam(gower_df, diss = TRUE, k = 2)

## we can visualise the results using the following commands
pam_data2 = pam(the_data, k = 2)
fviz_cluster(pam_data2)

## let us look at the medoids for each cluster
the_data[pam_data$medoids, ]

## add the clustering to the data set
the_data <- the_data %>% mutate(cluster = pam_data$clustering)

## now produce some visuals to tell us about each cluster:

ggplot(the_data) + 
  geom_boxplot(aes(factor(cluster), Income, color = factor(cluster)))

ggplot(the_data) + 
  geom_boxplot(aes(factor(cluster), total_spent, color = factor(cluster)))

ggplot(the_data) + 
  geom_boxplot(aes(factor(cluster), months_enrolled, color = factor(cluster)))

ggplot(the_data) + 
  geom_boxplot(aes(factor(cluster), Kidhome + Teenhome, color = factor(cluster)))

ggplot(the_data) + 
  geom_point(aes(Income, total_spent, 
                 color = factor(cluster))) + 
  scale_x_log10()




tsne_object <- Rtsne(gower_df, is_distance = TRUE)
tsne_df <- tsne_object$Y %>% data.frame() %>% setNames(c("X", "Y")) %>% 
  mutate(cluster = factor(pam_data$clustering))
ggplot(tsne_df, aes(X, Y)) + geom_point(aes(color = cluster))
