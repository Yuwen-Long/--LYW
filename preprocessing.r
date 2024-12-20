library(bigmemory)
library(tidyverse)

Data <- read.big.matrix("final/data/purchase/train.csv", header=TRUE)
Data <- as.data.frame(as.matrix(Data))

Data <- Data[Data$click_bool==1,]
dim(Data)
colnames(Data)
useless_labels <- c("srch_id", "date_time", "site_id", "prop_id",
"visitor_location_country_id", "prop_country_id", "prop_id",
"position", "srch_destination_id", "gross_booking_usd")
Data <- Data[, !(colnames(Data) %in% useless_labels)]
Data <- filter(Data, random_bool==1)

na_cols <- c("random_bool")
for (col in colnames(Data))
{   
    if (mean(is.na(Data[, col])) > 0.15)
    {
        na_cols <- c(na_cols, col)
    }
}

Data <- Data[, !(colnames(Data) %in% na_cols)]

Data <- na.omit(Data)
dim(Data)
summary(Data)

factor_col <- c("prop_brand_bool", "promotion_flag", "srch_saturday_night_bool", "click_bool", "booking_bool")
for (col in factor_col)
{
    Data[, col] <- factor(Data[, col])
}
