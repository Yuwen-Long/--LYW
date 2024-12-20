library(ggplot2)
library(patchwork)
library(GGally)
library(gghalves)
library(ggpattern)
load("/home/longyuwen/Data_visualization/final/cleaned_data.RData")
library(devtools)
install_github("coolbutuseless/ggpattern")
Data <- Data[Data$click_bool==1, -14]
summary(Data)
colnames(Data)
dim(Data)
Data <- Data[, -15]

tempt_fig <- replicate(6 ,NULL, simplify="list")
count <- 1
for (col in colnames(Data))
{
    if (class(Data[,col]) != "factor")
    {
        fig <- ggplot(Data, aes(x=.data[[col]], y=after_stat(density))) +
        geom_histogram(bins=10) +
        geom_density() +
        labs(title=paste("Histogram of", col))

        tempt_fig[[ceiling(count/2)]] <- tempt_fig[[ceiling(count/2)]] | fig

        count <- count + 1
    }
}

fig_density <- NULL
for (i in 1:6)
{
    fig_density <- fig_density / tempt_fig[[i]]
}
fig_density

Data$price_usd <- log(Data$price_usd+1)
Data$srch_booking_window <- log(Data$srch_booking_window+1)
ggplot(Data, aes(x=price_usd, y=after_stat(density))) +
        geom_histogram(bins=10) +
        geom_density() +
        labs(title="Histogram of price_usd") |
ggplot(Data, aes(x=srch_booking_window, y=after_stat(density))) +
        geom_histogram(bins=10) +
        geom_density() +
        labs(title="Histogram of srch_booking_window")

tempt_fig <- replicate(6 ,NULL, simplify="list")
count <- 1
for (col in colnames(Data))
{
    if (class(Data[,col]) != "factor")
    {
        fig <- ggplot(Data, aes(x=booking_bool, y=.data[[col]])) +
        geom_half_violin(side="r") +
        geom_half_boxplot(side="l", outliers=FALSE) +
        labs(title=paste("Violin plot and boxplot of", col), y=NULL)

        tempt_fig[[ceiling(count/2)]] <- tempt_fig[[ceiling(count/2)]] | fig

        count <- count + 1
    }
}
fig_vio <- NULL
for (i in 1:6)
{
    fig_vio <- fig_vio / tempt_fig[[i]]
}
fig_vio

prop.table(table(Data$booking_bool))

fig <- ggplot(Data, aes(x=prop_brand_bool, group=booking_bool, fill=booking_bool)) +
        geom_bar(position="dodge") +
        labs(title="Barplot of prop_brand_bool")
fig <- fig | (ggplot(Data, aes(x=promotion_flag, group=booking_bool)) +
        geom_bar(position="dodge") +
        labs(title="Barplot of promotion_flag")) | 
        (ggplot(Data, aes(x=srch_saturday_night_bool, group=booking_bool)) +
        geom_bar(position="dodge") +
        labs(title="Barplot of srch_saturday_night_bool"))
