library(ggplot2)
library(cowplot)
st.err = function(x) {sd(x)/sqrt(length(x))}

# Datasets ----
## Lizards
lizards<-read.csv("Ch1Lizards.csv")
lizards$date<-as.Date(lizards$date, format="%Y-%m-%d")
lizards$Year<-as.factor(lizards$Year)

## Environmental data from CRC
# Daily values- Used in chapter as of 7/11
CRC.Env=read.table("CRCData2016-2020.txt", header=T, sep=",")
CRC.Env=CRC.Env[,c(1,14,15,16,31,32,33,25)]
CRC.Env$date_time<-as.Date(CRC.Env$date_time, format="%Y-%m-%d")
CRC.Env$Temp_Range<-CRC.Env$airt_max-CRC.Env$airt_min

# Lizard Analyses ----
# Differences between species for Table 1
summary(aov(CTmin~species, data=subset(lizards, Year=="2017")))
summary(aov(CTmin~species, data=subset(lizards, Year=="2018")))
summary(aov(CTmin~species, data=subset(lizards, Year=="2019")))

summary(aov(CTmax~species, data=subset(lizards, Year=="2017")))
summary(aov(CTmax~species, data=subset(lizards, Year=="2018")))
summary(aov(CTmax~species, data=subset(lizards, Year=="2019")))

summary(aov(Tbreadth~species, data=subset(lizards, Year=="2017")))
summary(aov(Tbreadth~species, data=subset(lizards, Year=="2018")))
summary(aov(Tbreadth~species, data=subset(lizards, Year=="2019")))

summary(aov(Tb~species, data=subset(lizards, Year=="2017")))
summary(aov(Tb~species, data=subset(lizards, Year=="2018")))
summary(aov(Tb~species, data=subset(lizards, Year=="2019")))

# Differences among sampling periods
lizards.U<-subset(lizards, species=="U. stansburiana")
lizards.U<-droplevels(lizards.U)
lizards.S<-subset(lizards, species=="S. tristichus")
lizards.S<-droplevels(lizards.S)

# Table 2
aov1.us<-aov(CTmin~Year, lizards.U)
summary(aov1.us)
TukeyHSD(aov1.us)
aov1.st<-aov(CTmin~Year, lizards.S)
summary(aov1.st)
TukeyHSD(aov1.st)
aov2.us<-aov(CTmax~Year, lizards.U)
summary(aov2.us) # NS
aov2.st<-aov(CTmax~Year, lizards.S)
summary(aov2.st) # NS
aov3.us<-aov(Tbreadth~Year, lizards.U)
summary(aov3.us)
TukeyHSD(aov3.us)
aov3.st<-aov(Tbreadth~Year, lizards.S)
summary(aov3.st)
TukeyHSD(aov3.st)
aov4.us<-aov(Tb~Year, lizards.U)
summary(aov4.us) # NS
aov4.st<-aov(Tb~Year, lizards.S)
summary(aov4.st) # NS

## Yearly Environmenal Differences
# Environmental conditions on 15 days before and after median sampling date
med.date<-aggregate(date~Year, median, data=lizards)
med.date$date.past<-med.date$date-15
med.date$date.future<-med.date$date+15

Range.2017<-subset(CRC.Env, date_time >= med.date$date.past[1] & date_time <= med.date$date.future[1])
Range.2017$Year<-as.factor("2017")
Range.2018<-subset(CRC.Env, date_time >= med.date$date.past[2] & date_time <= med.date$date.future[2])
Range.2018$Year<-as.factor("2018")
Range.2019<-subset(CRC.Env, date_time >= med.date$date.past[3] & date_time <= med.date$date.future[3])
Range.2019$Year<-as.factor("2019")
YearlyRange<-rbind(Range.2017,Range.2018,Range.2019)

# Comparison of temperature range (TAmax-TAmin)
aov5<-aov(Temp_Range~Year, data=YearlyRange)
summary(aov5)
TukeyHSD(aov5)

## Correlations
List = list()
for(i in 1:length(lizards$id)){
  ind.data = lizards[i,]
  
  dat.min.30 = ind.data$date-30
  
  sub.data = subset(CRC.Env, date_time >= dat.min.30 & date_time < ind.data$date)
  
  TAavg = mean(sub.data$airt_avg)
  TAmin = mean(sub.data$airt_min)
  TAmax = mean(sub.data$airt_max)
  TArange = mean(sub.data$Temp_Range)
  
  List[[length(List)+1]] = data.frame(id = ind.data$id, TAavg = TAavg, TAmin = TAmin, TAmax = TAmax, TArange = TArange, Date = ind.data$date, Date.30 = dat.min.30)
}
List = plyr::ldply(List)

lizards1<-merge(lizards,List, by="id")
colnames(lizards1)[2]<-"Species"
lizards1.U<-subset(lizards1, Species=="U. stansburiana")
lizards1.S<-subset(lizards1, Species=="S. tristichus")

# TAmin
cor.test(lizards1.U$TAmin, lizards1.U$CTmin, method="pearson")
cor.test(lizards1.U$TAmin, lizards1.U$CTmax, method="pearson")
cor.test(lizards1.U$TAmin, lizards1.U$Tbreadth, method="pearson")
cor.test(lizards1.U$TAmin, lizards1.U$Tb, method="pearson")

cor.test(lizards1.S$TAmin, lizards1.S$CTmin, method="pearson")
cor.test(lizards1.S$TAmin, lizards1.S$CTmax, method="pearson")
cor.test(lizards1.S$TAmin, lizards1.S$Tbreadth, method="pearson")
cor.test(lizards1.S$TAmin, lizards1.S$Tb, method="pearson")

# TAmax
cor.test(lizards1.U$TAmax, lizards1.U$CTmin, method="pearson")
cor.test(lizards1.U$TAmax, lizards1.U$CTmax, method="pearson")
cor.test(lizards1.U$TAmax, lizards1.U$Tbreadth, method="pearson")
cor.test(lizards1.U$TAmax, lizards1.U$Tb, method="pearson")

cor.test(lizards1.S$TAmax, lizards1.S$CTmin, method="pearson")
cor.test(lizards1.S$TAmax, lizards1.S$CTmax, method="pearson")
cor.test(lizards1.S$TAmax, lizards1.S$Tbreadth, method="pearson")
cor.test(lizards1.S$TAmax, lizards1.S$Tb, method="pearson")

# TArange
cor.test(lizards1.U$TArange, lizards1.U$CTmin, method="pearson")
cor.test(lizards1.U$TArange, lizards1.U$CTmax, method="pearson")
cor.test(lizards1.U$TArange, lizards1.U$Tbreadth, method="pearson")
cor.test(lizards1.U$TArange, lizards1.U$Tb, method="pearson")

cor.test(lizards1.S$TArange, lizards1.S$CTmin, method="pearson")
cor.test(lizards1.S$TArange, lizards1.S$CTmax, method="pearson")
cor.test(lizards1.S$TArange, lizards1.S$Tbreadth, method="pearson")
cor.test(lizards1.S$TArange, lizards1.S$Tb, method="pearson")

## Figures ----
# Figure 1
Yearly.mean<-aggregate(Temp_Range~Year, mean, data=YearlyRange)
Yearly.se<-aggregate(Temp_Range~Year, st.err, data=YearlyRange)
YearlyMeanData<-cbind(Yearly.mean,Yearly.se$Temp_Range)
colnames(YearlyMeanData)<-c("Year","Mean","SE")

fig1.box<-ggplot(data=YearlyRange, aes(x=Year, y=Temp_Range))+
  geom_boxplot(outlier.shape=NA)+
  geom_point(position=position_jitter(seed=2,width=0.15), color="gray")+
  geom_point(data=YearlyMeanData, aes(x=Year, y=Mean), size=4, shape=18)+
  theme_bw()+
  theme(axis.title.x=element_blank())+
  theme(panel.grid.major=element_line(colour="#FFFFFF"),panel.grid.minor=element_line(colour="#FFFFFF"))+
  theme(axis.text=element_text(size=12,face="bold"), axis.title=element_text(size=14,face="bold"))+
  theme(legend.position="")+
  xlab("")+
  ylab("Air Temperature Range (°C)")+
  annotate("text",
           x = c(1, 2, 3),
           y = c(23, 25, 25.8),
           label = c("A", "B", "B"), fontface="bold",
           size=6)
#ggsave("Fig1box.jpeg", plot=fig1.box, height=5, width=5)

# Figure 2
a<-aggregate(cbind(CTmin,Tbreadth,CTmax)~Year+species, mean, data=lizards)
b<-aggregate(cbind(CTmin,Tbreadth,CTmax)~Year+species, st.err, data=lizards)
c<-cbind(a,b$CTmin,b$Tbreadth,b$CTmax)
colnames(c)<-c("Year","Species","CTmin","Tbreadth","CTmax","CTmin.se","Tbreadth.se","CTmax.se")

c.us<-subset(c, Species=="U. stansburiana")
c.us<-droplevels(c.us)
c.st<-subset(c, Species=="S. tristichus")
c.st<-droplevels(c.st)

fig2A.box<-ggplot(data=lizards.U, aes(x=Year, y=CTmin))+
  geom_boxplot(outlier.shape=NA)+ # including outlier.shape=NA removes the outlier symbol so the jitter doesn't add extra points.
  geom_point(position=position_jitter(seed=2,width=0.15), color="gray")+
  geom_point(data=c.us, aes(x=Year, y=CTmin), size=4, shape=18)+
  theme_bw()+
  theme(panel.grid.major=element_line(colour="#FFFFFF"),panel.grid.minor=element_line(colour="#FFFFFF"))+
  theme(legend.position="")+
  labs(title="U. stansburiana")+
  theme(plot.title = element_text(size=16, face="bold.italic", hjust=0.5))+
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())+
  theme(axis.text=element_text(size=12,face="bold"), axis.title=element_text(size=14,face="bold"))+
  theme(legend.position="")+
  annotate("text",
           x = c(1, 2, 3),
           y = c(12, 11.3, 10),
           label = c("A", "B", "B"), fontface="bold",
           size=6)+
  xlab("")+
  ylab("CTmin (°C)")

fig2B.box<-ggplot(data=lizards.S, aes(x=Year, y=CTmin))+
  geom_boxplot(outlier.shape=NA)+ 
  geom_point(position=position_jitter(seed=2,width=0.15), color="gray")+
  geom_point(data=c.st, aes(x=Year, y=CTmin), size=4, shape=18)+
  theme_bw()+
  theme(panel.grid.major=element_line(colour="#FFFFFF"),panel.grid.minor=element_line(colour="#FFFFFF"))+
  theme(legend.position="")+
  labs(title="S. tristichus")+
  theme(plot.title = element_text(size=16, face="bold.italic", hjust=0.5))+
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())+
  theme(axis.text=element_text(size=12,face="bold"), axis.title=element_text(size=14,face="bold"))+
  theme(legend.position="")+
  annotate("text",
           x = c(1, 2, 3),
           y = c(16.5, 11.3, 13.1),
           label = c("A", "B", "B"), fontface="bold",
           size=6)+
  xlab("")+
  ylab("")

fig2C.box<-ggplot(data=lizards.U, aes(x=Year, y=Tbreadth))+
  geom_boxplot(outlier.shape=NA)+ 
  geom_point(position=position_jitter(seed=2,width=0.15), color="gray")+
  geom_point(data=c.us, aes(x=Year, y=Tbreadth), size=4, shape=18)+
  theme_bw()+
  theme(panel.grid.major=element_line(colour="#FFFFFF"),panel.grid.minor=element_line(colour="#FFFFFF"))+
  theme(legend.position="")+
  theme(plot.title = element_text(size=16, face="bold.italic", hjust=0.5))+
  theme(axis.title.x=element_blank())+
  theme(axis.text=element_text(size=12,face="bold"), axis.title=element_text(size=14,face="bold"))+
  theme(legend.position="")+
  annotate("text",
           x = c(1, 2, 3),
           y = c(36, 38.5, 38.7),
           label = c("A", "B", "B"), fontface="bold",
           size=6)+
  xlab("")+
  ylab("Thermal Tolerance Breadth (°C)")

fig2D.box<-ggplot(data=lizards.S, aes(x=Year, y=Tbreadth))+
  geom_boxplot(outlier.shape=NA)+ 
  geom_point(position=position_jitter(seed=2,width=0.15), color="gray")+
  geom_point(data=c.st, aes(x=Year, y=Tbreadth), size=4, shape=18)+
  theme_bw()+
  theme(panel.grid.major=element_line(colour="#FFFFFF"),panel.grid.minor=element_line(colour="#FFFFFF"))+
  theme(legend.position="")+
  theme(plot.title = element_text(size=16, face="bold.italic", hjust=0.5))+
  theme(axis.title.x=element_blank())+
  theme(axis.text=element_text(size=12,face="bold"), axis.title=element_text(size=14,face="bold"))+
  theme(legend.position="")+
  annotate("text",
           x = c(1, 2, 3),
           y = c(34, 34.4, 34.4),
           label = c("A", "B", "B"), fontface="bold",
           size=6)+
  xlab("")+
  ylab("")

# Make Panel
Fig2.box=plot_grid(fig2A.box, fig2B.box, fig2C.box, fig2D.box,
                   labels = "AUTO", ncol = 2, align="v")
#ggsave("Fig2box.jpeg", width=7, height=8, plot=Fig2.box)

# Figure 3
# Legend
CorrelationLegend<-ggplot(data=lizards1, aes(x=TAmin, y=CTmin, group=Species, shape=Species))+
  geom_point(size=4)+
  geom_smooth(aes(linetype=Species), method='lm', formula= y~x, se=F, color="black")+
  theme_bw()+
  theme(panel.grid.major=element_line(colour="#FFFFFF"),panel.grid.minor=element_line(colour="#FFFFFF"))+
  theme(axis.text=element_text(size=12,face="bold"), axis.title=element_text(size=14,face="bold"))+
  theme(legend.position="bottom",
        legend.key.size = unit(2, 'cm'),
        legend.title = element_text(size=25, face="bold"),
        legend.text = element_text(size=10, face="bold"))+
  xlab("Minimum Air Temperature (°C)")+
  ylab("CTmin (°C)")

CTminxTAmin<-ggplot(data=lizards1, aes(x=TAmin, y=CTmin, group=Species, shape=Species))+
  geom_point(size=2)+
  geom_smooth(aes(linetype=Species), method='lm', formula= y~x, se=F, color="black")+
  theme_bw()+
  theme(panel.grid.major=element_line(colour="#FFFFFF"),panel.grid.minor=element_line(colour="#FFFFFF"))+
  theme(axis.text=element_text(size=12,face="bold"), axis.title=element_text(size=14,face="bold"))+
  theme(legend.position="")+
  xlab("Minimum Air Temperature (°C)")+
  ylab("CTmin (°C)")
TbreadthxTAmin<-ggplot(data=lizards1, aes(x=TAmin, y=Tbreadth, group=Species, shape=Species))+
  geom_point(size=2)+
  geom_smooth(aes(linetype=Species), method='lm', formula= y~x, se=F, color="black")+
  theme_bw()+
  theme(panel.grid.major=element_line(colour="#FFFFFF"),panel.grid.minor=element_line(colour="#FFFFFF"))+
  theme(axis.text=element_text(size=12,face="bold"), axis.title=element_text(size=14,face="bold"))+
  theme(legend.position="")+
  xlab("Minimum Air Temperature (°C)")+
  ylab("Thermal Tolerance Breadth (°C)")

CTminxTAmax<-ggplot(data=lizards1, aes(x=TAmax, y=CTmin, group=Species, shape=Species))+
  geom_point(size=2)+
  geom_smooth(aes(linetype=Species), method='lm', formula= y~x, se=F, color="black")+
  theme_bw()+
  theme(panel.grid.major=element_line(colour="#FFFFFF"),panel.grid.minor=element_line(colour="#FFFFFF"))+
  theme(axis.text=element_text(size=12,face="bold"), axis.title=element_text(size=14,face="bold"))+
  theme(legend.position="")+
  xlab("Maximum Air Temperature (°C)")+
  ylab("CTmin (°C)")
TbreadthxTAmax<-ggplot(data=lizards1, aes(x=TAmax, y=Tbreadth, group=Species, shape=Species))+
  geom_point(size=2)+
  geom_smooth(aes(linetype=Species), method='lm', formula= y~x, se=F, color="black")+
  theme_bw()+
  theme(panel.grid.major=element_line(colour="#FFFFFF"),panel.grid.minor=element_line(colour="#FFFFFF"))+
  theme(axis.text=element_text(size=12,face="bold"), axis.title=element_text(size=14,face="bold"))+
  theme(legend.position="")+
  xlab("Maximum Air Temperature (°C)")+
  ylab("Thermal Tolerance Breadth (°C)")

CTminxTArange<-ggplot(data=lizards1, aes(x=TArange, y=CTmin, group=Species, shape=Species))+
  geom_point(size=2)+
  geom_smooth(aes(linetype=Species), method='lm', formula= y~x, se=F, color="black")+
  theme_bw()+
  theme(panel.grid.major=element_line(colour="#FFFFFF"),panel.grid.minor=element_line(colour="#FFFFFF"))+
  theme(axis.text=element_text(size=12,face="bold"), axis.title=element_text(size=14,face="bold"))+
  theme(legend.position="")+
  xlab("Air Temperature Range (°C)")+
  ylab("CTmin (°C)")
TbreadthxTArange<-ggplot(data=lizards1, aes(x=TArange, y=Tbreadth, group=Species, shape=Species))+
  geom_point(size=2)+
  geom_smooth(aes(linetype=Species), method='lm', formula= y~x, se=F, color="black")+
  theme_bw()+
  theme(panel.grid.major=element_line(colour="#FFFFFF"),panel.grid.minor=element_line(colour="#FFFFFF"))+
  theme(axis.text=element_text(size=12,face="bold"), axis.title=element_text(size=14,face="bold"))+
  theme(legend.position="")+
  xlab("Air Temperature Range (°C)")+
  ylab("Thermal Tolerance Breadth (°C)")

# Make Panel
c.legend=get_legend(CorrelationLegend)
correlations=plot_grid(CTminxTAmin, TbreadthxTAmin, CTminxTAmax, TbreadthxTAmax, CTminxTArange, TbreadthxTArange,
                       labels = "AUTO", ncol = 2, nrow=3, align="v")
correlations1=plot_grid(correlations,c.legend, ncol = 1, rel_heights = c(2,.15))
#ggsave("Correlations.png", height=13, width=10, plot=correlations1)
