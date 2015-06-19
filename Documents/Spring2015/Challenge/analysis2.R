data = read.csv("data1.csv")
rate = data$tquotes / data$tInvites
day = data$day - 181
rate = data.frame(day = day,rate=rate)

days = as.numeric(names(table(day)))

PEs = numeric(length(days))
for (i in 1:length(days)){
  day = days[i]
  cur = c(max(c(1,days[i] - 6)):day)
  nextTime = c(min(c(day+1,length(days))):min(c(day+7,length(days))))
  dat1 = rate[rate$day %in% cur,2]
  dat2 = rate[rate$day %in% nextTime,2]
  PE = rulsif(dat1,dat2,0.1) + rulsif(dat2,dat1,0.1)
  PEs[i] = PE
}

PEs2 = numeric(length(days))
for (i in 1:length(days)){
  day = days[i]
  cur = c(max(c(1,days[i] - 14)):day)
  nextTime = c(min(c(day+1,length(days))):min(c(day+15,length(days))))
  dat1 = rate[rate$day %in% cur,2]
  dat2 = rate[rate$day %in% nextTime,2]
  PE = rulsif(dat1,dat2,0.1) + rulsif(dat2,dat1,0.1)
  PEs2[i] = PE
}

PEs3 = numeric(length(days))
for (i in 1:length(days)){
  day = days[i]
  cur = c(max(c(1,days[i] - 24)):day)
  nextTime = c(min(c(day+1,length(days))):min(c(day+25,length(days))))
  dat1 = rate[rate$day %in% cur,2]
  dat2 = rate[rate$day %in% nextTime,2]
  PE = rulsif(dat1,dat2,0.1) + rulsif(dat2,dat1,0.1)
  PEs3[i] = PE
}




tmp2 = tmp[order(data$day),]
avg = aggregate(tmp)
plot(avg[,1],avg[,2], type = "l")
colnames(tmp2) = c("day","rate")
dInvite = aggregate(tInvites ~ day, data = data, sum)
dQuotes = aggregate(tquotes ~ day, data = data, sum)
plot(avg[,1],dQuotes[,2]/dInvite[,2], type = "l")

id = cbind(day[-63],day[-1])
val = apply(id,1,ks)
ks = function(b){
  k = ks.test(tmp[day < b,2], tmp[day >= b,2])
  return(k$p.value)
}
val2 = sapply(day[-c(1,63)],ks)
a = t1/t2
ansmeanvar=cpt.meanvar(as.vector(a))
plot(ansmeanvar,cpt.width=3)
t3 = table(tmp2[tmp2[,2] == 0,1])
ansmeanvar=cpt.meanvar(as.vector(t3/t2)[-63])
plot(ansmeanvar,cpt.width=3)
