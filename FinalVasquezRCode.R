install.packages('usmap')
library(ggplot2)
library(usmap)
library(dplyr)
library(stringr)
library("tidyverse")
adoption<-
plot_usmap(color="black", fill = 'purple', include = c("AK","SD","HI","WA", "OR", "CA", "NV", "CO","NM","ND","IA","IL","MI","KY","WV","NH","VT","ME","MA","RI","CT","NJ","DE","MD","DC","NY")) +
  labs(title = "FOSTER AND ADOPTION LAWS",
       subtitle = "Gov. prohibits discrimination in adoption based on sexual orientation/ gender identity.")

adoption


marital <- read_csv("Marital.csv")

newplot<-ggplot(data = marital, aes(x=GenderOrientation, y = Value, fill = Status)) + 
  geom_bar(stat='identity')+
  xlab("Gender + Orientation")+
  ylab("Percent")+
  coord_flip()+
  ggtitle("Percent Marital Status According to Gender and Sexuality")+
  theme(
    panel.background = element_blank(),
    axis.ticks = element_blank(),
    axis.title.y = element_blank(),
    axis.title.x = element_blank(),
    axis.text.x = element_blank()
  ) 

newplot


Marriage <- read_csv("Marriage_Vertical.csv")

plot_less_ink<-
  ggplot(Marriage, aes(x=Year, y=Value, color = Decision, group = Decision, label = Decision)) +
  geom_point()+
  geom_line()+
  geom_label(data=Marriage%>%filter(Value == 69 | Value == 70),
             aes(label=Decision))+
  scale_colour_manual(values=c("Oppose" = "blue","Favor" = "red"), 
                      labels = c("Oppose", "Favor"))+
  ggtitle("Gay Marriage Opinions in US over Time")+
  ylab("Percent")+
  theme(panel.grid = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        axis.title.x = element_blank(),
        legend.position = "none", panel.background = element_rect(fill = "white"), 
        axis.line = element_line(size = 0.5, colour = "black"))

plot_less_ink


load(file = "37938-0001-Data.rda")

transdata<-
  da37938.0001

df<-
da37938.0001%>%
  filter(GENDER == "(2) Woman" | GENDER == "(4) Trans woman (MTF)")

transdata1<-
transdata%>%
  select(c("Q47","RACE"))%>%
  group_by(Q47,RACE)%>%
  summarise(n=n())%>%
  na.omit()

genderperceptionrace<-
ggplot(data = transdata1, aes(fill=Q47,y=n,x=RACE))+
  geom_bar(position = "fill", stat="identity")+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
  xlab("Race")+
  ylab("Percent")+
  scale_fill_discrete(name="Q47: I have to work hard for people to see my
gender accurately")+
  ggtitle("Gender Perception According to Race")
  

genderperceptionrace

#Q57 - think about trans 57
#tell others 58


genderfind<-
transdata%>%
  select(c("Q58", "RACE"))%>%
  na.omit()

ggplot(data = genderfind, aes(x=RACE, y = Q58 ))+
  geom_point(alpha = 0.5)+
  ylab("Age")+
  theme(
    panel.background = element_blank(),
    axis.ticks = element_blank(),
    axis.title.x = element_blank(),
    axis.text.y = element_blank(),
    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)
  )+geom_jitter()+
  ggtitle("At what age did you start telling people you were trans?")




  