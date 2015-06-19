require(ggplot2)
require(gdata)
data = read.csv("data1.csv")
location = read.csv("location.csv")
category = read.csv("category.csv")
rate = data$tquotes / data$tInvites
day = data$day - 181
rate = data.frame(day = day,rate=rate)
days = as.numeric(names(table(day)))
avg = aggregate(rate ~ day,data = rate,mean)
p = ggplot(avg, aes(x = day, y = rate)) + 
  geom_point() + 
  geom_line() +
  geom_smooth() +
  labs(title = "July 1 to August 31 in 2013", y = "Average Response Rate")

# Plot of all response rate by day
p = ggplot(rate, aes(x = day, y = rate)) + 
  geom_point() + 
  geom_smooth() +
  labs(title = "July 1 to August 31 in 2013", y = "Response Rate")
# Total number of requests by location
loc = table(data$location_id)
loc = data.frame(loc,location$state)
colnames(loc) = c("location_id","Freq","location")
loc = loc[order(loc$Freq,decreasing= TRUE),]
loc = data.frame(count=c(loc[1:10,2],sum(loc[11:96,2])),location=c(as.character(loc[1:10,3]),"Other"))
p <- ggplot(loc, aes(x=1, y=count, fill=location)) +
  geom_bar(stat="identity") +
  coord_polar(theta='y') +
  ggtitle("Total Number of Request By Location") +
  scale_fill_brewer(palette="Spectral") +
  theme(axis.ticks=element_blank(),  
        axis.title=element_blank(),  
        axis.text.y=element_blank()) 
p

# Total number of requests by category
cat = table(data$category_id)
cat = data.frame(cat,category$name)
colnames(cat) = c("category_id","Freq","category")
cat = cat[order(cat$Freq,decreasing= TRUE),]
cat = data.frame(count=c(cat[1:10,2],sum(cat[11:96,2])),category=c(as.character(cat[1:10,3]),"Other"))
p <- ggplot(cat, aes(x=1, y=count, fill=category)) +
  geom_bar(stat="identity") +
  coord_polar(theta='y') +
  ggtitle("Total Number of Request By Category") +
  scale_fill_brewer(palette="Spectral") +
  theme(axis.ticks=element_blank(),  
        axis.title=element_blank(),  
        axis.text.y=element_blank()) 
p
# Testing if there is any changes in distribution of quoting rate with rulsif
PEs = numeric(length(days))
for (i in 1:length(days)){
  day = days[i]
  cur = c(max(c(1,days[i] - 4)):day)
  nextTime = c(min(c(day+1,length(days))):min(c(day+5,length(days))))
  dat1 = rate[rate$day %in% cur,2]
  dat2 = rate[rate$day %in% nextTime,2]
  PE = rulsif(dat1,dat2,0.1) + rulsif(dat2,dat1,0.1)
  PEs[i] = PE
}

PEs2 = numeric(length(days))
for (i in 1:length(days)){
  day = days[i]
  cur = c(max(c(1,days[i] - 9)):day)
  nextTime = c(min(c(day+1,length(days))):min(c(day+10,length(days))))
  dat1 = rate[rate$day %in% cur,2]
  dat2 = rate[rate$day %in% nextTime,2]
  PE = rulsif(dat1,dat2,0.1) + rulsif(dat2,dat1,0.1)
  PEs2[i] = PE
}

PEs3 = numeric(length(days))
for (i in 1:length(days)){
  day = days[i]
  cur = c(max(c(1,days[i] - 19)):day)
  nextTime = c(min(c(day+1,length(days))):min(c(day+20,length(days))))
  dat1 = rate[rate$day %in% cur,2]
  dat2 = rate[rate$day %in% nextTime,2]
  PE = rulsif(dat1,dat2,0.1) + rulsif(dat2,dat1,0.1)
  PEs3[i] = PE
}

save(PEs,PEs2,PEs3,file= "result1")
# Plot the results
pe = data.frame(time = 1:63,pe1 = PEs,pe2 = PEs2,pe3 = PEs3)
ggplot(pe, aes(time, y = value, color = variable)) + 
  geom_point(aes(y = pe1, col = "n = 5")) + 
  geom_point(aes(y = pe2, col = "n = 10")) +
  geom_point(aes(y = pe3, col = "n = 20")) +
  geom_line(aes(y = pe1, col = "n = 5")) +
  geom_line(aes(y = pe2, col = "n = 10")) +
  geom_line(aes(y = pe3, col = "n = 20")) +
  labs(title = "Pearson Divergence from July 1 to August 31 in 2013", y = "Pearson Divergence calculated")
# Testing if there is any changes in mean of quoting rate distribution
result = numeric(length(days))
for (i in 1:length(days)){
  day = days[i]
  cur = c(max(c(1,days[i] - 4)):day)
  nextTime = c(min(c(day+1,length(days))):min(c(day+5,length(days))))
  dat1 = rate[rate$day %in% cur,2]
  dat2 = rate[rate$day %in% nextTime,2]
  t = t.test(dat1,dat2)$p.val
  result[i] = t
}

result2 = numeric(length(days))
for (i in 1:length(days)){
  day = days[i]
  cur = c(max(c(1,days[i] - 9)):day)
  nextTime = c(min(c(day+1,length(days))):min(c(day+10,length(days))))
  dat1 = rate[rate$day %in% cur,2]
  dat2 = rate[rate$day %in% nextTime,2]
  t = t.test(dat1,dat2)$p.val
  result2[i] = t
}

result3 = numeric(length(days))
for (i in 1:length(days)){
  day = days[i]
  cur = c(max(c(1,days[i] - 19)):day)
  nextTime = c(min(c(day+1,length(days))):min(c(day+20,length(days))))
  dat1 = rate[rate$day %in% cur,2]
  dat2 = rate[rate$day %in% nextTime,2]
  t = t.test(dat1,dat2)$p.val
  result3[i] = t
}
#Plot the results
ts = data.frame(time = 1:63,t1 = result,t2 = result2,t3 = result3)
ggplot(ts, aes(time, y = value, color = variable)) + 
  geom_point(aes(y = t1, col = "n = 5")) + 
  geom_point(aes(y = t2, col = "n = 10")) +
  geom_point(aes(y = t3, col = "n = 20")) +
  geom_line(aes(y = t2, col = "n = 10")) +
  geom_line(aes(y = t3, col = "n = 20")) +
  geom_hline(aes(yintercept=0.05)) +
  labs(title = "Two-sample T test from July 1 to August 31 in 2013", y = "p value")


# Testing if there is any changes in variance of quoting rate distribution
var1 = numeric(length(days))
for (i in 1:length(days)){
  day = days[i]
  cur = c(max(c(1,days[i] - 4)):day)
  nextTime = c(min(c(day+1,length(days))):min(c(day+5,length(days))))
  dat1 = rate[rate$day %in% cur,2]
  dat2 = rate[rate$day %in% nextTime,2]
  p = var.test(dat1,dat2)$p.val
  var1[i] = p
}

var2 = numeric(length(days))
for (i in 1:length(days)){
  day = days[i]
  cur = c(max(c(1,days[i] - 9)):day)
  nextTime = c(min(c(day+1,length(days))):min(c(day+10,length(days))))
  dat1 = rate[rate$day %in% cur,2]
  dat2 = rate[rate$day %in% nextTime,2]
  p = var.test(dat1,dat2)$p.val
  var2[i] = p
}

var3 = numeric(length(days))
for (i in 1:length(days)){
  day = days[i]
  cur = c(max(c(1,days[i] - 19)):day)
  nextTime = c(min(c(day+1,length(days))):min(c(day+20,length(days))))
  dat1 = rate[rate$day %in% cur,2]
  dat2 = rate[rate$day %in% nextTime,2]
  p = var.test(dat1,dat2)$p.val
  var3[i] = p
}
var = data.frame(time = 1:63,var1 = var1,var2 = var2,var3 = var3)
p = ggplot(var, aes(time, y = value, color = variable)) + 
  geom_point(aes(y = var1, col = "n = 5")) + 
  geom_point(aes(y = var2, col = "n = 10")) +
  geom_point(aes(y = var3, col = "n = 20")) +
  geom_line(aes(y = var1, col = "n = 5")) +
  geom_line(aes(y = var2, col = "n = 10")) +
  geom_line(aes(y = var3, col = "n = 20")) +
  geom_hline(aes(yintercept=0.05)) +
  labs(title = "F-test from July 1 to August 31 in 2013", y = "P Value of F Test")
p

dat1 = data.frame(rate,group = as.factor(ifelse(rate$day > 28,"After July 28th","Before July 28th")))
t.test(dat1[dat1$group == "Before July 28th",2],dat1[dat1$group!="Before July 28th",2])
m = ggplot(dat1, aes(x = rate,fill = group)) + geom_density(alpha = 0.4) +
  labs(title = "Density plot before and after July 28th")
m

# Calculate Pearson Divergence by location (Top 10 locations with highest total number of requests)
# Before and After the break point
locs = NULL
for (j in 1:10){
  tmp = 0
  dat1 = rate[(rate$day <= 28 ) & (data$location_id == j),2]
  dat2 = rate[(rate$day > 28) & (data$location_id == j),2]
  PE = rulsif(dat1,dat2,0.1) + rulsif(dat2,dat1,0.1)
  locs = c(locs,PE)
}

# Plot density plot of highest changes groups
pval = numeric(4)
for (j in 1:4){
dat1 = rate[(rate$day <= 28 ) & (data$location_id == j),2]
dat2 = rate[(rate$day > 28) & (data$location_id == j),2]
pval[j] = t.test(dat1,dat2)$p.val
dat = rbind(cbind(dat1,"Before"),cbind(dat2,"After"))
dat = data.frame(dat)
colnames(dat) = c("rate","group")
dat[,1] = as.numeric(dat[,1])
dat[,2] = as.factor(dat[,2])
assign(paste("m", j, sep=""), ggplot(dat, aes(x = rate,fill = group)) + geom_density(alpha = 0.4) +
  labs(title = paste("Density plot before and after July 28th",loc[j,2])))
} 
multiplot(m1, m2, m3, m4, cols=2)
