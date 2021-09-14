library(tidyverse)

the_data <- read_tsv("dataset/marketing_campaign.csv")

## check missing information
colSums(is.na(the_data))

## remove records with missing income
the_data <- the_data[!is.na(the_data$Income), ]

## look at summary
summary(the_data)
## there seems to be an outlier in the income variable.

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


ggplot(the_data[the_data$Income < 666666, ]) + geom_point(aes(Income, pct_wine))
summary(the_data$Income)
