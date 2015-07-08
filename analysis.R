# load raw data and remove errors
dataRaw<-read.table("trialData.csv",sep=",",header=TRUE)
data<-subset(dataRaw,subset=error!=1) # remove errors

# PERFORMANCE MEASURES
# RT
agg=aggregate(RT~subject+condition,data=data,FUN="mean") # RT performance data aggregated by subjectconsistent=subset(agg,subset=condition==1)
consistent=subset(agg,subset=condition==1)
inconsistent=subset(agg,subset=condition==2)
t.test(consistent$RT,inconsistent$RT,var.equal=TRUE,paired=TRUE)

# init
agg=aggregate(init.time~subject+condition,data=data,FUN="mean") # RT performance data aggregated by subjectconsistent=subset(agg,subset=condition==1)
consistent=subset(agg,subset=condition==1)
inconsistent=subset(agg,subset=condition==2)
t.test(consistent$init.time,inconsistent$init.time,var.equal=TRUE,paired=TRUE)

# MD
agg=aggregate(MD~subject+condition,data=data,FUN="mean") # RT performance data aggregated by subjectconsistent=subset(agg,subset=condition==1)
consistent=subset(agg,subset=condition==1)
inconsistent=subset(agg,subset=condition==2)
t.test(consistent$MD,inconsistent$MD,var.equal=TRUE,paired=TRUE)



# Bimodality coefficient
library("moments")
incongruentTrials=data$z.MD.separate[data$condition==2]
s=skewness(incongruentTrials)
k=kurtosis(incongruentTrials)
n=length(incongruentTrials)
BC=(s^2+1)/(k+(3*(n-1)^2)/((n-2)*(n-3)))
BC


library("diptest")
dip.test(incongruentTrials)

