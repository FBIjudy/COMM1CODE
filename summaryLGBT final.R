####summray LGBT final
#####straight report police
df1 <- data.frame(
  Answer = c("No","Yes"),
  Freq1 = c(106368,1491)
)
head(df1)
bp<- ggplot(df1, aes(x="", y=Freq1, fill=Answer))+
  geom_bar(width = 1, stat = "identity")
bp
barplot(LGB,col=Freq, main = "LGBT attacked Frequency")
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