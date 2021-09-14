library(tidyverse)
library(cluster)
library(Rtsne)

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

## It is best to remove this record because it is very far from
## all other values

the_data <- the_data[the_data$Income < 666666, ]

## we also note that the two variables Z_costcontact and Z_revenue do not
## change. We therefore remove them since they add no information:

the_data <- the_data[, -which(names(the_data) %in% c("Z_CostContact", "Z_Revenue"))]

## look at the structure
str(the_data)

## convert date to date object
the_data$Dt_Customer <- as.Date(the_data$Dt_Customer, "%d-%m-%Y")

## note that there are strange values in marital status
unique(the_data$Marital_Status)
## remove them
the_data <- the_data[the_data$Marital_Status != "YOLO" & 
                       the_data$Marital_Status != "Absurd", ]

## covert some character variables to factors
the_data$Education <- as.factor(the_data$Education)
the_data$Marital_Status <- as.factor(the_data$Marital_Status)








the_data <- the_data %>% mutate(total_spent = MntWines + MntFruits + 
                                  MntMeatProducts + MntFishProducts +
                                  MntSweetProducts + MntGoldProds,
                                pct_wine = MntWines / total_spent,
                                pct_fruit = MntFruits / total_spent,
                                pct_meat = (MntMeatProducts + MntFishProducts) / total_spent,
                                pct_sweets = MntSweetProducts / total_spent,
                                pct_gold = MntGoldProds / total_spent)

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

## cluster analysis
the_data <- the_data %>% select(Year_Birth, Education, Marital_Status,
                                Income, Kidhome, Teenhome, Recency,
                                NumDealsPurchases, NumWebPurchases, NumCatalogPurchases,
                                NumStorePurchases, NumWebVisitsMonth, AcceptedCmp3,
                                AcceptedCmp4, AcceptedCmp5, AcceptedCmp1, AcceptedCmp2,
                                Complain, Response, total_spent, pct_wine, pct_fruit,
                                pct_meat, pct_sweets, pct_gold)

gower_df <- daisy(the_data, metric = "gower", type = list(logratio = 4))
summary(gower_df)

silhouette <- c()
silhouette <- c(silhouette, NA)
for(i in 2:10) {
  pam_clusters = pam(as.matrix(gower_df), diss = TRUE, k = i)
  silhouette = c(silhouette, pam_clusters$silinfo$avg.width)
}
cluster_data <- data.frame(Clusters = 1:10, Silhouette_width = silhouette)
ggplot(cluster_data, aes(Clusters, Silhouette_width)) + geom_point() + 
  geom_line() + scale_x_continuous(breaks = c(1:10))

pam_data = pam(gower_df, diss = TRUE, k = 3)
the_data[pam_data$medoids, ]
the_data <- the_data %>% mutate(cluster = pam_data$clustering)

ggplot(the_data) + geom_point(aes(total_spent, Income, 
                                  color = factor(cluster)),
                              alpha = 0.6)

tsne_object <- Rtsne(gower_df, is_distance = TRUE)
tsne_df <- tsne_object$Y %>% data.frame() %>% setNames(c("X", "Y")) %>% 
  mutate(cluster = factor(pam_data$clustering))
ggplot(tsne_df, aes(X, Y)) + geom_point(aes(color = cluster))
