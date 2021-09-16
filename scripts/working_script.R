packages <- c("tidyverse", "cluster", "Rtsne", "factoextra",
              "lares")

lapply(packages, function(x) {
  if (!require(x, character.only = TRUE)) {
    install.packages(x, dependencies = TRUE)
    library(x, character.only = TRUE)
  }  
})

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
ggplot(the_data) + geom_boxplot(aes(Year_Birth))

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
the_data <- the_data %>% mutate(Marital_Status = replace(Marital_Status, Marital_Status == "YOLO" | Marital_Status == "Absurd" | Marital_Status == "Alone", "Single"))
## we also collapse divorced and widow to single
the_data <- the_data %>% mutate(Marital_Status = replace(Marital_Status, Marital_Status == "Divorced" | Marital_Status == "Widow", "Single"))
## we also collapse together and married into the same category
the_data <- the_data %>% mutate(Marital_Status = replace(Marital_Status, Marital_Status == "Together" | Marital_Status == "Married", "Couple"))
  
## next we collapse education into graduate and non-graduate
the_data <- the_data %>% mutate(Education = replace(Education, Education == "Graduation" | Education == "PhD" | Education == "Master", "Graduate"))
the_data <- the_data %>% mutate(Education = replace(Education, Education == "Basic" | Education == "2n Cycle", "non-Graduate"))

## covert some character variables to factors
the_data$Education <- as.factor(the_data$Education)
the_data$Marital_Status <- as.factor(the_data$Marital_Status)

## let us create some useful variables. First, we create
## the total amount spent by each individual. 
the_data <- the_data %>% mutate(total_spent = MntWines + MntFruits + 
                                  MntMeatProducts + MntFishProducts +
                                  MntSweetProducts + MntGoldProds)

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

## now we create a variable for the total number of kids
## and we create a binary variable to record whether there is
## a teenager at home
the_data <- the_data %>% mutate(children = Kidhome + Teenhome, teen = ifelse(Teenhome >0, TRUE, FALSE))

## now we create a variable which records the percent of 
## purchases that were made online
the_data <- the_data %>% mutate(technology = NumWebPurchases / (NumWebPurchases + NumCatalogPurchases + NumStorePurchases))
## some NA's are generated due to the fact that the denominator
## is zero in 6 cases. Remove these.
the_data <- the_data[!is.na(the_data$technology), ]

## now visualise the data

## Look at the histogram of income:
ggplot(the_data) + geom_histogram(aes(Income), fill = "blue",
                                  color = "white")
## Look at the histogram of total amount spent
ggplot(the_data) + geom_histogram(aes(total_spent), fill = "blue",
                                  color = "white")

## Look at differences in income between some groups:

ggplot(the_data) + geom_boxplot(aes(Education, Income))

ggplot(the_data) + geom_boxplot(aes(Marital_Status, Income))

## Now look at differences in spending between some groups:
ggplot(the_data) + geom_boxplot(aes(factor(children), MntWines))
## People with no children (teen plus kids) buy the most wine
ggplot(the_data) + geom_boxplot(aes(teen, MntWines))
## People with a teenager at home buy slightly more wine

## Do people who have a higher income spend more?
ggplot(the_data, aes(Income, total_spent)) + geom_point() + 
  geom_smooth() + scale_x_log10()

## Is the use of technology correlated negatively with the 
## year of birth?
ggplot(the_data) + geom_point(aes(Year_Birth, technology))
## Doesnt seem that younger people use technology more

## Let us look at the variables that correlate the most with 
## income
corr_var(the_data, Income, top = 12)
## Interestingly, the higher the income, the more the in-store
## and catalogue purchases, and the lower the web site visits.
## It seems that people with high income do not buy online as
## much as those with lower incomes

## Who complains the most? Do older people complain more?
ggplot(the_data) + geom_boxplot(aes(factor(Complain), Year_Birth))
## Doesnt seem like it. Do richer people complain more?
ggplot(the_data) + geom_boxplot(aes(factor(Complain), Income))
## Do people with children complain more?
ggplot(the_data) + geom_boxplot(aes(factor(Complain), children))
## It seems that people with children complain more. 

## Cluster analysis
## First we keep the variables that we are mostly interested
## in
the_data <- the_data %>% select(Year_Birth, Education, Marital_Status,
                                Income, children, teen, Recency,
                                NumDealsPurchases, technology, 
                                Complain, total_spent, MntWines, MntFruits, MntMeatProducts, MntFishProducts, MntSweetProducts, MntGoldProds,
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

## We also use the elbow method to verify the result:
fviz_nbclust(as.matrix(gower_df), pam, method = "wss")
## This method suggests 3 clusters as well

## Perform the cluster analysis using the optimal number of
## clusters
pam_data = pam(gower_df, diss = TRUE, k = 3)

## We can visualise the results using the following commands
pam_data2 = pam(the_data, k = 3)
fviz_cluster(pam_data2)

## Another way to visualise the results is this (perhaps better)
tsne_object <- Rtsne(gower_df, is_distance = TRUE)
tsne_df <- tsne_object$Y %>% data.frame() %>% setNames(c("X", "Y")) %>% 
  mutate(cluster = factor(pam_data$clustering))
ggplot(tsne_df, aes(X, Y)) + geom_point(aes(color = cluster))

## Let us look at the medoids for each cluster
the_data[pam_data$medoids, ]

## Add the clustering to the data set
the_data <- the_data %>% mutate(cluster = pam_data$clustering)

## Now produce some visuals to tell us about each cluster:
corr_var(the_data, cluster)
## We see that the variable that is the most different between
## the clusters is income. This means that the market
## is mostly segmented by income and by the amount that
## individuals can, and do, spend. Interestingly, marital status
## is way down the list. What matters is whether there are
## children and not whether the individual is currently in a 
## relationship or not. 

## Let us now produce graphs that will help us better understand
## the individuals that make up each cluster. First we look
## at income
ggplot(the_data, aes(Income)) + 
  geom_histogram(aes(fill = factor(cluster)), color = "white") + 
  theme(legend.position = "bottom") + labs(fill = "Cluster")
## Cluster 1 is made up of people with the highest income,
## while cluster 3 is made up of people with the lowest income.
## These differences in income are reflected in their spending
## habits

ggplot(the_data, aes(total_spent)) + 
  geom_histogram(aes(fill = factor(cluster)), color = "white") + 
  theme(legend.position = "bottom") + labs(fill = "Cluster")


ggplot(the_data) + 
  geom_boxplot(aes(factor(cluster), technology, color = factor(cluster)))

ggplot(the_data) + 
  geom_boxplot(aes(factor(cluster), NumDealsPurchases, color = factor(cluster)))


ggplot(the_data) + 
  geom_boxplot(aes(factor(cluster), Income, color = factor(cluster)))

ggplot(the_data) + 
  geom_boxplot(aes(factor(cluster), total_spent, color = factor(cluster)))

ggplot(the_data) + 
  geom_boxplot(aes(factor(cluster), months_enrolled, color = factor(cluster)))

ggplot(the_data) + 
  geom_boxplot(aes(factor(cluster), children, color = factor(cluster)))

ggplot(the_data) + 
  geom_point(aes(Income, total_spent, 
                 color = factor(cluster))) + 
  scale_x_log10() + theme(legend.position = "bottom")





