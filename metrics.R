hv2_offground<-hv2$`C2M signed distances`[hv2$Classification == 1]
hv5_offground<-hv5_2$`C2M signed distances`[hv5_2$Classification == 1]
hvr2_offground<-hvr2$`C2M signed distances`[hvr2$Classification == 1]
hvr4_offground<-hvr4$`C2M signed distances`[hvr4$Classification == 1]
oud1_offground<-oud1$`C2M signed distances`[oud1$Classification == 1]
oudr_offground<-oudr$`C2M signed distances`[oudr$Classification == 1]

hv2_offground_0<-hv2$`C2M signed distances`[hv2$Classification == 1 & hv2$`C2M signed distances` >=0]
hv5_offground_0<-hv5_2$`C2M signed distances`[hv5_2$Classification == 1 & hv5_2$`C2M signed distances` >=0]
hvr2_offground_0<-hvr2$`C2M signed distances`[hvr2$Classification == 1 & hvr2$`C2M signed distances` >=0]
hvr4_offground_0<-hvr4$`C2M signed distances`[hvr4$Classification == 1 & hvr4$`C2M signed distances` >=0]
oud1_offground_0<-oud1$`C2M signed distances`[oud1$Classification == 1 & oud1$`C2M signed distances` >=0]
oudr_offground_0<-oudr$`C2M signed distances`[oudr$Classification == 1 & oudr$`C2M signed distances` >=0]


#hert 0 tot 1.8m
hv2_hert <- hv2_offground[hv2_offground <= 1.8 & hv2_offground >=0.3]
hv5_hert<-hv5_offground[hv5_offground <= 1.8 & hv5_offground >=0.3]
hvr2_hert<-hvr2_offground[hvr2_offground <= 1.8 & hvr2_offground >=0.3]
hvr4_hert<-hvr4_offground[hvr4_offground <= 1.8 & hvr4_offground >=0.3]
oud1_hert<-oud1_offground[oud1_offground <= 1.8 & oud1_offground >=0.3]
oudr_hert<-oudr_offground[oudr_offground <= 1.8 & oudr_offground >=0.3]

#gras
hv2_gras <- hv2_offground[hv2_offground <= 0.3 & hv2_offground >=0]
hv5_gras<-hv5_offground[hv5_offground <= 0.3 & hv5_offground >=0]
hvr2_gras<-hvr2_offground[hvr2_offground <= 0.3 & hvr2_offground >=0]
hvr4_gras<-hvr4_offground[hvr4_offground <= 0.3 & hvr4_offground >=0]
oud1_gras<-oud1_offground[oud1_offground <= 0.3 & oud1_offground >=0]
oudr_gras<-oudr_offground[oudr_offground <= 0.3 & oudr_offground >=0]

#rest


min(hv2_offground_0)
max(hv2_offground)
hist(hv2_offground)
plot(density(hv2_offground_0))
plot(density(hvr2_offground_0))
hist(hvr2_offground)
hist(oud1_hert)
hist(oudr_hert)


metrics<-c('max','mean','median','sd','var','% offground','veg density','density 1.8',)
df<-data.frame(metrics)

#hv2 metrics
hv2max<-max(hv2_offground_0)
hv2mean<-mean(hv2_offground_0)
hv2median<-median(hv2_offground_0)
hv2sd<-sd(hv2_offground_0)
hv2var<-var(hv2_offground_0)
hv2quant<-quantile(hv2_offground_0,probs = seq(0, 1, 1/10))
hv2percent<-sum(with(hv2,Classification == 1 ))*100/sum(with(hv2,Classification == 1 | Classification == 2)) #% of points are offground
hv2vegdens<-length(hv2_offground_0)/400
hv2denshert<-length(hv2_hert)/400 #points per m² within the reach of the fallow deer
hv2densgras<-length(hv2_gras)/400

hvr2max<-max(hvr2_offground_0)
hvr2mean<-mean(hvr2_offground_0)
hvr2median<-median(hvr2_offground_0)
hvr2sd<-sd(hvr2_offground_0)
hvr2var<-var(hvr2_offground_0)
hvr2quant<-quantile(hvr2_offground_0,probs = seq(0, 1, 1/10))
hvr2percent<-sum(with(hvr2,Classification == 1 ))*100/sum(with(hvr2,Classification == 1 | Classification == 2)) #% of points are offground
hvr2vegdens<-length(hvr2_offground_0)/400
hvr2denshert<-length(hvr2_hert)/400 #points per m² within the reach of the fallow deer
hvr2densgras<-length(hvr2_gras)/400

hv5max<-max(hv5_offground_0)
hv5mean<-mean(hv5_offground_0)
hv5median<-median(hv5_offground_0)
hv5sd<-sd(hv5_offground_0)
hv5var<-var(hv5_offground_0)
hv5quant<-quantile(hv5_offground_0,probs = seq(0, 1, 1/10))
hv5percent<-sum(with(hv5,Classification == 1 ))*100/sum(with(hv5,Classification == 1 | Classification == 2)) #% of points are offground
hv5vegdens<-length(hv5_offground_0)/400
hv5denshert<-length(hv5_hert)/400 #points per m² within the reach of the fallow deer
hv5densgras<-length(hv5_gras)/400

hvr4max<-max(hvr4_offground_0)
hvr4mean<-mean(hvr4_offground_0)
hvr4median<-median(hvr4_offground_0)
hvr4sd<-sd(hvr4_offground_0)
hvr4var<-var(hvr4_offground_0)
hvr4quant<-quantile(hvr4_offground_0,probs = seq(0, 1, 1/10))
hvr4percent<-sum(with(hvr4,Classification == 1 ))*100/sum(with(hvr4,Classification == 1 | Classification == 2)) #% of points are offground
hvr4vegdens<-length(hvr4_offground)/400
hvr4denshert<-length(hvr4_hert)/400 #points per m² within the reach of the fallow deer
hvr4densgras<-length(hvr4_gras)/400

oud1max<-max(oud1_offground_0)
oud1mean<-mean(oud1_offground_0)
oud1median<-median(oud1_offground_0)
oud1sd<-sd(oud1_offground_0)
oud1var<-var(oud1_offground_0)
oud1quant<-quantile(oud1_offground_0,probs = seq(0, 1, 1/10))
oud1percent<-sum(with(oud1,Classification == 1 ))*100/sum(with(oud1,Classification == 1 | Classification == 2)) #% of points are offground
oud1vegdens<-length(oud1_offground_0)/400
oud1denshert<-length(oud1_hert)/400 #points per m² within the reach of the fallow deer
oud1densgras<-length(oud1_gras)/400

oudrmax<-max(oudr_offground_0)
oudrmean<-mean(oudr_offground_0)
oudrmedian<-median(oudr_offground_0)
oudrsd<-sd(oudr_offground_0)
oudrvar<-var(oudr_offground_0)
oudrquant<-quantile(oudr_offground_0,probs = seq(0, 1, 1/10))
oudrpercent<-sum(with(oudr,Classification == 1 ))*100/sum(with(oudr,Classification == 1 | Classification == 2)) #% of points are offground
oudrvegdens<-length(oudr_offground_0)/400
oudrdenshert<-length(oudr_hert)/400 #points per m² within the reach of the fallow deer
oudrdensgras<-length(oudr_gras)/400

metrics<-c('max','mean','median','sd','var','% offground','veg density','density 1.8','density 0.3')
df<-data.frame(metrics)
df$hv2<-c(hv2max,hv2mean,hv2median,hv2sd,hv2var,hv2percent,hv2vegdens,hv2denshert,hv2densgras)
df$hvr2<-c(hvr2max,hvr2mean,hvr2median,hvr2sd,hvr2var,hvr2percent,hvr2vegdens,hvr2denshert,hvr2densgras)
df$hv5<-c(hv5max,hv5mean,hv5median,hv5sd,hv5var,hv5percent,hv5vegdens,hv5denshert,hv5densgras)
df$hvr4<-c(hvr4max,hvr4mean,hvr4median,hvr4sd,hvr4var,hvr4percent,hvr4vegdens,hvr4denshert,hvr4densgras)
df$oud1<-c(oud1max,oud1mean,oud1median,oud1sd,oud1var,oud1percent,oud1vegdens,oud1denshert,oud1densgras)
df$oudr<-c(oudrmax,oudrmean,oudrmedian,oudrsd,oudrvar,oudrpercent,oudrvegdens,oudrdenshert,oudrdensgras)
library("writexl")
write_xlsx(df,"Metrics.xlsx")

hv2percentiles<-quantile(hv2_offground_0,probs = seq(0, 1, 1/10))
hvr2percentiles<-quantile(hvr2_offground_0,probs = seq(0, 1, 1/10))
hv5percentiles<-quantile(hv5_offground_0,probs = seq(0, 1, 1/10))
hvr4percentiles<-quantile(hvr4_offground_0,probs = seq(0, 1, 1/10))
oud1percentiles<-quantile(oud1_offground_0,probs = seq(0, 1, 1/10))
oudrpercentiles<-quantile(oudr_offground_0,probs = seq(0, 1, 1/10))
df_percentiles<-data.frame(hv2percentiles,hvr2percentiles,hv5percentiles,hvr4percentiles,oud1percentiles,oudrpercentiles)
write_xlsx(df_percentiles,"Percentiles.xlsx")
