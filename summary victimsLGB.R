###################LGBT situation
#####LGB unwanted sex
df <- data.frame(Percent <- c(64*100/109994,10*100/706,28*100/706,633*100/110057),
                 Labels <-c("Straight","LGB","LGB","Straight"),
                 Assault <-c("Sexual Assault","Sexual Assault","Assault with weapon","Assault with weapon"))

barplot(height=df$Percent....c.64...100.109994..10...100.706..28...100.706..633..., names=df$Labels....c..Straight....LGB....LGB....Straight.., col=coul,main= "Percentage of Victims Suffer from Physical Assault" )
library(ggplot2)
ggplot(df,                                    
       aes(x = Assault,
           y = Percent,
           fill = Labels)) +
  geom_bar(stat = "identity",
           position = "dodge")+
  ggtitle("Victims Suffering from Physical Assault")
########call the police
wtype<- c("V3048","V3084")
wtype <- da38090.0003[wtype]

sex2 <-wtype %>% na.omit()
summary(sex2) 
sex2$orientation <- ifelse(sex2$V3084 == "(4) Something else ", "Other",
                           ifelse(sex2$V3084 == "(5) I don't know the answer", NA,
                                  ifelse(sex2$V3084 == "(6) Refused", "Other",
                                         ifelse(sex2$V3084 == "(8) Residue", NA,
                                                ifelse(sex2$V3084 =="(2) Straight, that is, not lesbian or gay", "Straight",
                                                       ifelse(sex2$V3084 == "(1) Lesbian or gay " | sex2$V3084 == "(3) Bisexual", "LGB", NA))))))
sex2$frequent <-ifelse(sex2$V3048 == "(1) Yes", "Yes",
                       ifelse(sex2$V3048 == "(2) No", "No", NA))
LGB = subset(sex2,sex2$orientation == "LGB")
summary(LGB)
library(ggplot2)

df1 <- data.frame(
  Answer = c("No","Yes"),
  Freq1 = c(106368,1491)
)
head(df1)
bp<- ggplot(df1, aes(x="", y=Freq1, fill=Answer))+
  geom_bar(width = 1, stat = "identity")
bp
barplot(LGB,col=Freq, main = "LGB attacked Frequency")
pie <- bp + coord_polar("y", start=0)
pie
install.packages("scales")
library(scales)
blank_theme <- theme_minimal()+
  theme(
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    panel.border = element_blank(),
    panel.grid=element_blank(),
    axis.ticks = element_blank(),
    plot.title=element_text(size=14, face="bold")
  )
percentlabels <- round(100*df1$Freq1/sum(df1$Freq1), 2)
pielabels<- paste(percentlabels, "%", sep="")
pie + scale_fill_brewer("Answer") + blank_theme +
  theme(axis.text.x=element_blank())+
  geom_text(aes(y = Freq1/2 + c(0, cumsum(Freq1)[-length(Freq1)]), 
                label = pielabels), size=7)+
  ggtitle("Did Straight victims call the police for reporting a crime?")

Straight = subset(sex2,sex2$orientation == "Straight")
summary(sex2)
summary(Straight)
######frequency table summary
df <- data.frame(samplesize <- c(55,706,2150),
                 
                 Category <-c("Crime Type","Assault","Call the police"))
df <- data.frame(samplesize <- c(1491,110000,107859),
                 
                 Category <-c("Crime Type","Assault","Call the police"))
ggplot(df,                                    
       aes(x = Category,
           y = samplesize,
           fill = Category)) +
  geom_bar(stat = "identity",
           position = "dodge")+
  geom_text(
    aes(label = samplesize),
    colour = "black", size = 3,
    vjust = 1.5, position = position_dodge(.9)
  )+
  ggtitle("Sample size of each category in LGB Victims")
