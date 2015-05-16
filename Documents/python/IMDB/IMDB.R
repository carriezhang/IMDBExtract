library(ggplot2)
##### Read in data ######
a = read.table("year.txt")
rate = read.table("rate.txt",sep = ",")
rateCount = read.table("rating.txt",sep = ",")
genre = read.table("genreCounts.txt",sep = ",")
rateCount[,2:3] = rateCount[,1:2]
rateCount[,1] = as.numeric(row.names(rateCount))
row.names(rateCount) = NULL

##### Trim year and month from time #####
year = as.numeric(apply(a,1,substr,start = 1, stop = 4))
month = as.numeric(apply(a,1,substr,start = 6, stop = 7))

##### Plot histogram for year and month #####
qplot(month, geom = "histogram",binwidth = 1, colour="#FF9999")
qplot(year, geom = "histogram", binwidth = 1)

##### Correlation matrix #######
data = data.frame(rate,rateCount,as.numeric(year))
write.table(round(cor(data),4),file = "result.csv", sep = ",")

#### Plot the histogram for genre #####
rank = genre[order(genre[,2], decreasing = TRUE)[1:5],] # Show top 5 genres
ggplot(data=genre, aes(x=V1, y=V2)) +
  geom_bar(stat="identity") + xlab("Genre") +
  ylab("Total Counts")

  