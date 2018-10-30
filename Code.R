#1. Data & Library Load
#dplyr, tidyr, ggplot2
library(dplyr)
library(tidyr)
library(ggplot2)

life_expectancy <- UNdata_Export_20181017_172727969_
#for jupyter notebook
options(repr.plot.width = 6, repr.plot.width = 6)

#Manipulate dataset to contain male and female life expectancy for each country
subdata <- life_expectancy %>%
  filter(Year=="2000-2005")%>%
  # Rearrange columns using dplyr's select()
  select(`Country or Area`,Subgroup, Value)%>%
  #spread(): makes dataset wide
  spread(Subgroup,Value)

#Visualizing Data
ggplot(subdata,aes(x=Male,y=Female))+
  geom_point()
#Add reference lines & axis limits
ggplot(subdata,aes(x=Male,y=Female))+
  geom_point()+
  geom_abline(intercept=0,slope=1,linetype=2)+
  scale_x_continuous(limits=c(35,85))+
  scale_y_continuous(limits=c(35,85))
#Add title...etc
ggplot(subdata,aes(x=Male,y=Female))+
  geom_point()+
  geom_abline(intercept=0,slope=1,linetype=2)+
  scale_x_continuous(limits=c(35,85))+
  scale_y_continuous(limits=c(35,85))+
  labs(title="Life Expectancy at Birth by Country",
       subtitle="Years. Period: 2000-2005. Average.",
       caption="Source:United Nations Statistics Division",
       x="Males",
       y="Females")

#Find out which countries' life expectanncy perform good 
top_Female <- subdata %>%
  arrange(Female-Male)%>%
  head(3)
top_Male <- subdata%>%
  arrange(Male-Female)%>%
  head(3)
# Adding text to the previous plot to label countries of interest
ggplot(subdata,aes(x=Male,y=Female,label=`Country or Area`))+
  geom_point(colour="white",fill="chartreuse3",shape=21,alpha=.55,size=5)+
  geom_abline(intercept=0,slope=1,linetype=2)+
  scale_x_continuous(limits=c(35,85))+
  scale_y_continuous(limits=c(35,85))+
  labs(title="Life Expectancy at Birth by Country",
       subtitle="Years. Period: 2000-2005. Average.",
       caption="Source:United Nations Statistics Division",
       x="Males",
       y="Females")+
  geom_text(data=top_Male,size=3)+
  geom_text(data=top_Female,size=3)+
  theme_bw()

#Find out how life exptancy by gender evoloved between 1985-1990 & 2000-2005
subdata2 <- life_expectancy %>%
  filter(Year %in% c("1985-1990","2000-2005"))%>%
  mutate(Sub_Year=paste(Subgroup,Year,sep="_"))%>%
  mutate(Sub_Year=gsub("-","_",Sub_Year))%>%
  select(-Subgroup,-Year)%>%
  spread(Sub_Year,Value)%>%
  mutate(diff_Female=Female_2000_2005-Female_1985_1990,diff_Male=Male_2000_2005-Male_1985_1990)
head(subdata2)

#Visualizing Data
ggplot(subdata2,aes(x=diff_Male, y=diff_Female,label=`Country or Area`))+
  geom_point(colour="white",fill="chartreuse3",shape=21,alpha=.55,size=5)+
  geom_abline(intercept=0,slope=1,linetype=2)+
  scale_x_continuous(limits=c(-25,25))+
  scale_y_continuous(limits=c(-25,25))+
  geom_hline(yintercept=0,linetype=2)+
  geom_vline(xintercept=0,linetype=2)+
  labs(title="Life Expectancy at Birth by Country",
       subtitle="Years. Difference between 1985-1990 and 2000-2005. Average",
       caption="Source: United Nations Statistics Division",
       x="Males",
       y="Females")+
  theme_bw()

#Find out countries that performa best and worst
top <- subdata2 %>% 
  arrange(diff_Male+diff_Female)%>%
  head(3)
bottom <- subdata2 %>% 
  arrange(desc(diff_Male+diff_Female))%>%
  head(3)

#Visualizing Data with top and bottom
ggplot(subdata2,aes(x=diff_Male, y=diff_Female,label=`Country or Area`))+
  geom_point(colour="white",fill="chartreuse3",shape=21,alpha=.55,size=5)+
  geom_abline(intercept=0,slope=1,linetype=2)+
  scale_x_continuous(limits=c(-25,25))+
  scale_y_continuous(limits=c(-25,25))+
  geom_hline(yintercept=0,linetype=2)+
  geom_vline(xintercept=0,linetype=2)+
  labs(title="Life Expectancy at Birth by Country",
       subtitle="Years. Difference between 1985-1990 and 2000-2005. Average",
       caption="Source: United Nations Statistics Division",
       x="Males",
       y="Females")+
  geom_text(data=top, size=3)+
  geom_text(data=bottom, size=3)+
  theme_bw()