wsex<- c("V3046","V3084")
wsex <- da38090.0003[wsex]
sex2 <-wsex %>% na.omit()
summary(sex2)
sex2$orientation <- ifelse(sex2$V3084 == "(4) Something else ", "Other",
                           ifelse(sex2$V3084 == "(5) I don't know the answer", NA,
                                  ifelse(sex2$V3084 == "(6) Refused", "Other",
                                         ifelse(sex2$V3084 == "(8) Residue", NA,
                                                ifelse(sex2$V3084 =="(2) Straight, that is, not lesbian or gay", "Straight",
                                                       ifelse(sex2$V3084 == "(1) Lesbian or gay " | sex2$V3084 == "(3) Bisexual", "LGB", NA))))))
sex2$frequent <-ifelse(sex2$V3046 == "(1) Yes", "Yes",
                       ifelse(sex2$V3046 == "(2) No", "No", NA))
View(sex2)
LGB = subset(sex2,sex2$orientation == "LGB")
View(LGB)
LGB1 <- table(LGB$frequent)
View(LGB1)
install.packages("RColorBrewer")
library(RColorBrewer)
df <- data.frame(
  labels = c("No","Yes"),
  Freq = c(696,10)
)
head(df)
bp<- ggplot(df, aes(x="", y=Freq, fill=labels))+
  geom_bar(width = 1, stat = "identity")
bp
barplot(LGB1,col=Freq, main = "LGB unwanted sex Frequency")
pie <- bp + coord_polar("y", start=0)
pie
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
pie + scale_fill_brewer("Answer") + blank_theme +
  theme(axis.text.x=element_blank())+
  geom_text(aes(y = Freq/2 + c(0, cumsum(Freq)[-length(Freq)]), 
                label = percent(Freq/sum(Freq))), size=7)+
  ggtitle("LGB unwanted sex")
