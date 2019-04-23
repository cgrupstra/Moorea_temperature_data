#Prepare workspace, load packages, load data
setwd("C:/Users/Cas/Google Drive/Rice/Projects/MVP")
library(tidyverse)
hobotemp <- read.csv('hobodata.csv') 
library(markdown)

#Generate new dataframes with alternative date columns
hobotemplongdate <- tidyr::separate(hobotemp, 'date',
                                    into = c('longdate', 'time'),
                                    sep= ' ') 

#Check dataframe variables. Should have both 'longdate' and 'month, 'day', 'year' and 'time'.
head(hobotemplongdate)

#Plot temperature data wrapped divided between LTER number and reef zones 
ntempgraph <- ggplot(data=hobotemplongdate, 
    aes(x=as.Date(longdate, format = "%m / %d / %Y"), 
    y=temp, colour=zone)) +
    geom_point(size=1, alpha = 1/10)+ theme_bw()+
    facet_grid(fct_relevel(zone, "fringe", "back", "fore")~lter)+ 
    theme(axis.text.x = element_text(angle=45, margin = margin(t=20)))

ntempgraph 

##TESTING FOR DIFFERENCES IN TEMPERATURE RANGES BETWEEN SITES
#select only zones and sites with data betwen april and august 2018. Remove fore reef data.
hobotemplongdate1 <- hobotemplongdate %>%
  filter(lter %in% c("1", "2", "3", "3.5", "4", "5", "5.5")) %>%
  filter(zone %in% c("fringe", "back"))

#Check whether filtering went correctly
  unique(hobotemplongdate1$lter)
  unique(hobotemplongdate1$zone)

#--> It seems that 'fore' is still in the dataset but it's not present when using View() and filtering the dataframe. 
#--> Ignoring this for now
View(hobotemplongdate1)

#Separate the previous date column into three separate columns (mm/dd/yyyy)
#and filter only the may-august 2018 data.
hobo <- hobotemplongdate1 %>%
  tidyr::separate('longdate',
                into = c('month', 'day', 'year'),
                sep= '/',
                remove = FALSE)%>%
  filter(year == "2018") %>%
  filter(month %in% c("5", "6", "7", "8"))

#Check column names and filtering.
head(hobo)
unique(hobo$year)
unique(hobo$month)

#Make figure to visualize this new curtailed dataset
hobotestg <- ggplot(data=hobo, 
                    aes(x=as.Date(longdate, format = "%m / %d / %Y"), 
                        y=temp, colour=zone)) +
  geom_point(size=1, alpha = 1/5)+ theme_bw()+
  facet_grid(fct_relevel(zone, "fringe", "back")~lter)+ 
  theme(axis.text.x = element_text(angle=45, margin = margin(t=20)))

hobotestg

#Calculate daily minimum and maximum temperatures for each combination of LTER site and reef zone
#Calculate the daily temperature range as max(temp)-min(temp)
hoborange <- hobo %>%
  group_by(month, day, zone, lter, longdate)%>%
  summarise(min_temp = min(temp), max_temp = max(temp))%>%
  mutate(range = max_temp-min_temp)

#Check dataset
View(hoborange)

#Plot the mean temperature range for fringe and back reefs between may and august 2018 
testplot <- ggplot(data=hoborange, 
                   aes(x=as.Date(longdate, format = "%m / %d / %Y"), 
                       y=range, colour=zone)) +
  geom_smooth()+ theme_bw()+
  theme(axis.text.x = element_text(margin = margin(t=5)))

testplot

#Calculate the mean of temperature ranges over time for each combination of site and zone, resulting in 14 datapoints
hoborangemeans <- hoborange %>%
  group_by(lter, zone)%>%
  summarise(meanrange = mean(range))

#Check dataframe, should be just 14 datapoints
hoborangemeans

#Make boxplot to compare temperature ranges between may and august for back and fringe reefs
hrmplot <- ggplot(hoborangemeans, aes(x=zone, y=meanrange))+
  geom_boxplot() + theme_bw()

hrmplot

#Conduct a t-test for significance between fringe and back sites
t.test(hoborange$range~hoborange$zone)
#--> There is a significant difference between fringe and back reefs 
#--> but 2 back is an outlier.

##REDOING ANALYSIS WITHOUT EXCLUDING SITES OR MONTHS
#Check whether the dataset 'hobotemplongdate' has all the lter sites and zones
unique(hobotempdate$zone)
unique(hobotemplongdate$lter)

hobofull <- hobotemplongdate %>%
  tidyr::separate('longdate',
                  into = c('month', 'day', 'year'),
                  sep= '/',
                  remove = FALSE)

#Check wehter the new dataset has all years and months 
unique(hobofull$month)
unique(hobofull$year)

View(hobofull)

#calculate daily temperature ranges 
hobofullrange <- hobofull %>%
  group_by(year, month, day, zone, lter, longdate)%>%
  summarise(min_temp = min(temp), max_temp = max(temp))%>%
  mutate(range = max_temp-min_temp)

#Check dataframe
View(hobofullrange)

#Create plot of daily ranges for full dataset
testplotfull <- ggplot(data=hobofullrange, 
                   aes(x=as.Date(longdate, format = "%m / %d / %Y"), 
                       y=range, colour=zone)) +
  geom_smooth()+ theme_bw()+
  theme(axis.text.x = element_text(margin = margin(t=5)))

testplotfull

#Calculate daily means 
hobofullrangemeans <- hobofullrange %>%
  group_by(year, month, lter, zone)%>%
  summarise(meanrange = mean(range))

#There should be more rows this time: 9 sites with back and fringe (=18), 3 with fore (=21). And up to 12 months (max. 12*21= 252) 
hobofullrangemeans
unique(hobofullrangemeans$lter)
unique(hobofullrangemeans$zone)
unique(hobofullrangemeans$month)
#--> There are 216 rows which makes sense because we don't have data for all sites every month

#Make boxplots of mean daily ranges over the entire duration
hobofullrangemeans$month <- factor(hobofullrangemeans$month, levels=c("9", "10", "11", "12", "1", "2", "3", "4", "5", "6", "7", "8"))
hrmplot <- ggplot(hobofullrangemeans, aes(x=month, y=meanrange))+
  geom_boxplot() + theme_bw()+
  geom_point(alpha=3/4, aes(colour=as.factor(lter)))+
  facet_grid(~fct_relevel(zone, "fringe", "back", "fore"))

hrmplot

#Test for differences between months and zones using a two-way ANOVA

lm <- lm(meanrange~zone + month, data=hobofullrangemeans)
summary(lm)

View(hobofullrangemeans)

##REDOING ANALYSIS WITH ONLY SITES 3, 3.5, 4, 5, 5.5
#Check whether the dataset 'hobotemplongdate' has all the lter sites and zones
unique(hobotemplongdate$zone)
unique(hobotemplongdate$lter)

hoboc <- hobotemplongdate %>%
  tidyr::separate('longdate',
                  into = c('month', 'day', 'year'),
                  sep= '/',
                  remove = FALSE)%>%
  filter(lter %in% c("3", "3.5", "4", "5", "5.5"))

#Check wehter the new dataset has all years and months 
unique(hoboc$month)
unique(hoboc$year)
unique(hoboc$lter)

View(hoboc)

#calculate daily temperature ranges 
hobocrange <- hoboc %>%
  group_by(year, month, day, zone, lter, longdate)%>%
  summarise(min_temp = min(temp), max_temp = max(temp))%>%
  mutate(range = max_temp-min_temp)

#Check dataframe
View(hobocrange)

#Create plot of daily ranges for full dataset
testplotc <- ggplot(data=hobocrange, 
                       aes(x=as.Date(longdate, format = "%m / %d / %Y"), 
                           y=range, colour=zone)) +
  geom_smooth()+ theme_bw()+
  theme(axis.text.x = element_text(margin = margin(t=5)))

testplotc

#Calculate daily means 
hobocrangemeans <- hobocrange %>%
  group_by(year, month, lter, zone)%>%
  summarise(meanrange = mean(range))

#There should be fewer rows this time: 5 sites with back and fringe (=10). And up to 12 months (max. 120) 
View(hobocrangemeans)
unique(hobocrangemeans$lter)
unique(hobocrangemeans$zone)
unique(hobofullrangemeans$month)
#--> There are 120 rows which makes sense

#Make boxplots of mean daily ranges over the entire duration
hobocrangemeans$month <- factor(hobocrangemeans$month, levels=c("9", "10", "11", "12", "1", "2", "3", "4", "5", "6", "7", "8"))
hrmcplot <- ggplot(hobocrangemeans, aes(x=month, y=meanrange))+
  geom_boxplot() + theme_bw()+
  geom_point(alpha=3/4, aes(colour=as.factor(lter)))+
  facet_grid(~fct_relevel(zone, "fringe", "back"))

hrmcplot

#Test for differences between months and zones using a two-way ANOVA

lmc <- lm(meanrange~zone + month, data=hobocrangemeans)
summary(lmc)

View(hobocrangemeans)

############END##########














bptempgraph <- ggplot(data=hoborange, aes(x=as.Date(month, format = "%m"), y=temp, colour=zone)) +
  geom_boxplot(size=1, alpha = 1/10)+ theme_bw()+
  geom_boxplot()+
  facet_grid(zone~lter)

bptempgraph

nlightgraph <- ggplot(data=hobotemplongdate, aes(x=as.Date(longdate, format = "%m / %d / %Y"), y=light, colour=zone)) +
  geom_point(size=1, alpha = 1/2)+ theme_bw()+
  facet_grid(zone~lter)

nlightgraph

#head(temp)
#as.factor(hobotemp$lter)

#View(hobotemp)

#Split the date string into several strings

#june (06) - november (11) are the cold and dry months. December (12)- May (5) are the warm and wet months
#Add vector that says 'warm' and 'cold'
#add vector that says 'night' and 'day'
library("tidyselect")
#hobotemp <- as.Date(hobotemp$date, format = "%m / %d / %y %H:%M")

hobotemplongdate <- tidyr::separate(hobotemp, 'date',
                                    into = c('longdate', 'time'),
                                    sep= ' ')
#View(hobotemplongdate)

ntempgraph <- ggplot(data=hobotemplongdate, aes(x=as.Date(longdate, format = "%m / %d / %Y"), y=temp, colour=zone)) +
  #geom_line() +
  #geom_ribbon(ymin=25, ymax=35)+
  geom_point(size=1, alpha = 1/2)+ theme_bw()+
  #geom_smooth()+
  #geom_hline(aes(yintercept = max(hobotemplongdate$temp)))+
  facet_grid(zone~lter)

ntempgraph

ntempgraph <- ggplot(data=hobotemplongdate, aes(x=as.Date(longdate, format = "%m / %d / %Y"), y=light, colour=zone)) +
  #geom_line() +
  #geom_ribbon(ymin=25, ymax=35)+
  geom_point(size=1, alpha = 1/2)+ theme_bw()+
  #geom_smooth()+
  #geom_hline(aes(yintercept = max(hobotemplongdate$temp)))+
  facet_grid(zone~lter)


boxtempgraph <- ggplot(data=hobotemplongdate, aes(x=as.Date(longdate, format = "%m / %d / %Y"), y=temp, colour=zone)) +
  geom_boxplot()+
  facet_grid(zone~lter)


boxtempgraph


View(hobotemp)
nntempgraph <- ggplot(data=hobotemp, aes(x=as.Date(hobotemp$date, format = "%m/%d/%Y  %H:%M"), y=temp, colour=zone)) +
  #geom_smooth(method = 'lm', formula = y ~ poly(x, 2))+
  geom_point()+
  facet_grid(~hobotemp$zone~hobotemp$lter)+
  ylim(25,35)

nntempgraph

View(hobotemp)
nntempgraph <- ggplot(data=hobotemp, aes(x=as.Date(hobotemp$date, format = "%m / %d / %Y  %H:%M"), y=temp, group=zone, colour=zone)) +
  geom_line() +
  geom_point()+
  facet_grid(~hobotemp$zone~hobotemp$lter)+
  ylim(25,35)

nntempgraph

testtempgraph <- ggplot(data=hobotemp, aes(x=as.Date(hobotemp$date, format = "%m / %d / %Y  %H:%M"), y=temp, group=zone, colour=zone)) +
  geom_line() +
  geom_point()+
  ylim(25,35)

testtempgraph

nnlightgraph <- ggplot(data=hobotemp, aes(x=as.Date(hobotemp$date, format = "%m / %d / %Y  %H:%M"), y=light, group=zone, colour=zone)) +
  geom_line() +
  geom_point()+
  facet_grid(~as.numeric(hobotemp$zone)~as.numeric(hobotemp$lter))

nnlightgraph

n_distinct(hobotemp)

as.Date(hobotemplongdate$longdate, format = "%m / %d / %Y")

View(hobotemplongdate)

northshore<- subset(hobotemplongdate$lter==c("0","1","2"))

hobotemplongdate <- tidyr::separate(hobotemp, 'date',
                             into = c('longdate', 'time'),
                             sep= ' ')
#View(hobotemplongdate)

ntempgraph <- ggplot(data=hobotemplongdate, aes(x=as.Date(longdate, format = "%m / %d / %Y"), y=temp, colour=zone)) +
  #geom_line() +
  #geom_ribbon(ymin=25, ymax=35)+
  geom_point(size=1, alpha = 1/2)+ theme_bw()+
  #geom_smooth()+
  geom_hline(aes(yintercept = max(hobotemplongdate$temp)))+
  facet_grid(zone~lter)

ntempgraph

install.packages("tidyverse")
require("tidyverse")
install.packages("glue")
require("glue")

hobotempshort <- tidyr::separate(hobotemplongdate, 'longdate',
                          into = c('month', 'day', 'year'),
                          sep='/')
tempgraph <- ggplot(data=hobotempshort, aes(x=as.numeric(hobotempshort$day), y=temp, group=lter, colour=zone)) +
  geom_line() +
  geom_point()+
  facet_wrap(~as.numeric(hobotempshort$month))

tempgraph

lightgraph <- ggplot(data=hobotempshort, aes(x=as.numeric(hobotempshort$day), y=light, group=lter, colour=zone)) +
  geom_line() +
  geom_point()+
  facet_wrap(~as.numeric(hobotempshort$month))

lightgraph


p <- ggplot(hobotemp, aes(x=zone, y=temp)) +
  geom_boxplot()

p + facet_grid(hobotemp$lter~hobotemp$month)

head(temp)
datetime <-str_split_fixed(temp$date, " ", n=2)

head(datetime)

names(datetime)<-c('longdate','time')

dates <-str_split_fixed(datetime, "/", n=3)

#header strings

head(datetime)



View(datescols)

tempmatrix <-cbind(temp,datetime,dates)

head(tempmatrix)
separate(temp,date, sep=" ", remove=FALSE)
