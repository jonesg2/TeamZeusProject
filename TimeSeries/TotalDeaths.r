df <- read.csv("LCD_Month_Region.csv")

df_1 <- aggregate(x = df$Deaths, by = list(df$Month,df$Year), FUN = sum)
df_1
library(plyr)
revalue(df_1$Group.1, c("January" = "01")) -> df_1$Group.1
revalue(df_1$Group.1, c("February" = "02")) -> df_1$Group.1
revalue(df_1$Group.1, c("March" = "03")) -> df_1$Group.1
revalue(df_1$Group.1, c("April" = "04")) -> df_1$Group.1
revalue(df_1$Group.1, c("May" = "05")) -> df_1$Group.1
revalue(df_1$Group.1, c("June" = "06")) -> df_1$Group.1
revalue(df_1$Group.1, c("July" = "07")) -> df_1$Group.1
revalue(df_1$Group.1, c("August" = "08")) -> df_1$Group.1
revalue(df_1$Group.1, c("September" = "09")) -> df_1$Group.1
revalue(df_1$Group.1, c("October" = "10")) -> df_1$Group.1
revalue(df_1$Group.1, c("November" = "11")) -> df_1$Group.1
revalue(df_1$Group.1, c("December" = "12")) -> df_1$Group.1

## Turn month into number
df_1$Date <- paste(df_1$Group.2,df_1$Group.1,sep="-")
## Clean data frame by removing unwanted columns
df_Time <- (df_1[c(4,3)]) ## 
df_Time

## Reorder dataframe in order of date by year-month
df_Time_Order <- df_Time[order(df_Time$Date),]
df_Time_Order

## SEE IF YOU CAN GET THIS TO WORK ##
library(ggplot2)
ggplot(df_Time_Order, aes(Date, x)) + geom_line() 
