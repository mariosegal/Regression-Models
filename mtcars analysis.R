library(ggplot2);library(plyr);library(dplyr);library(reshape2)

data(mtcars)
data <- mtcars
data$am <- factor(data$am,levels=c(0,1),labels=c("Automatic","Manual"))
data$car <- row.names(data)

#Do some EDA
ggplot(data,aes(y=mpg,x=am))+geom_violin(aes(color=am))+geom_boxplot(aes(fill=am),alpha=0.4)+
  geom_jitter(position = position_jitter(width = .1,height=0),color="blue")+theme_bw()+
  theme(legend.position="none")+scale_y_continuous("MPG")+scale_x_discrete("Transmission")+
  ggtitle("MPG by Transmission Type")

data1 <- melt(data,id.var=c("car","am","mpg"))
ggplot(data1,aes(y=mpg,x=value,color=am))+geom_point()+stat_smooth(se=F,method="lm")+
  facet_wrap(~variable,scales="free")+theme_bw()+theme(legend.position="bottom")+
  ggtitle("MPG versus Other Variables by Type of Transmission")+scale_y_continuous("MPG")+
  scale_x_continuous("Variable Value")+scale_color_discrete("Transmission\nType")

library(corrgram)
corrgram(mtcars, order=TRUE, lower.panel=panel.shade,
         upper.panel=panel.pie, text.panel=panel.txt,
         main="Car Milage Data in PC2/PC1 Order")
  



#Now some models
library(caret)
model1 <- train(mpg~am,data,method="lm")
summary(model1)

#it seems that am is very significnt and that from a starting point of 17.1 MPG 
#being manual adds 7.2MPG on average, however this is not that good a fit - 
#predicts the average, and obvisouly other factors matter
ggplot(data,aes(x=am,y=mpg,color=am))+geom_point()+
  geom_hline(yintercept=24.392,color=1)+geom_hline(yintercept=17.147,color=2)
library(plyr)
ddply(data,.(am),summarise,mean(mpg))


#try a stepwise search
model2 <- train(mpg~.,data[-12],method="lmStepAIC",direction="backward",trace=F)
model3 <- train(mpg~.,data[-12],method="lmStepAIC",direction="forward")
model4 <- train(mpg~.,data[-12],method="lmStepAIC")

summary(model2)
summary(model3)
summary(model4)

#try some interactions

# do some diagnositc plots that are needed