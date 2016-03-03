rm(list=ls())
setwd("~/Projects/generalBc/data/labInterns")
myData <- read.csv("BR_selectMedia.csv")


names(myData)
myData$IsoBYpda <- paste(myData$Isolate, myData$PDAConc, sep='.') 
attach(myData)
Media.lm <- lm(Count ~ Isolate * PDAConc + Date, data=myData)
Media.lm2 <- lm(Count ~ IsoBYpda + Date, data=myData)
anova(Media.lm)
anova(Media.lm2)
pairwise.t.test(Count, PDAConc, p.adj="none")

#remove rows with NA
myFigDat <- myData[complete.cases(myData),]

myFigDat <- ddply(myFigDat, c("Isolate", "PDAConc"), na.rm=T, summarise,
                 N    = length(Count),
                 mean = mean(Count),
                 sd   = sd(Count),
                 se   = sd / sqrt(N))

limits <- aes(ymax = mean + se, ymin=mean - se)
ggplot(myFigDat, aes(x = factor(Isolate), y = mean))+
  geom_bar(stat="identity", fill="dodgerblue3")+
  theme_bw()+
  theme(text = element_text(size=24), axis.text.x = element_text(angle = 45, hjust = 1))+
labs(y=expression(Mean ~ Wavy ~ Index), x=element_blank())+
  geom_errorbar(limits, width=0.25)+
  facet_grid(.~PDAConc, scales="free")+ 
  scale_y_continuous(limits = c(0,4.2)) 