
install.packages("tidyverse")
install.packages("plotly")
install.packages("GGally")
install.packages("viridis")
install.packages("treemapify")
install.packages("cowplot")

library(tidyverse)
library(readxl)
library(dplyr)
library(plotly)
library(tidyr)
library(ggplot2)
library(GGally)
library(viridis)
library(treemapify)
library(maps)
library(ggrepel)
library(cowplot)
library(viridis)

Sys.setenv("plotly_username"="ylin81")
Sys.setenv("plotly_api_key"="NItTWLCplQD4wpXRDo6C")

##### 3.1.1 Poverty trend in BDG from 1983 to 2016 data
poverty_trend <- file.path("B_WDIEXCEL.xlsx")
poverty_trend <- read_excel(poverty_trend,sheet= "Poverty_rate")
names(poverty_trend)[names(poverty_trend) == "Poverty headcount ratio at $1.90 a day (2011 PPP) (% of population)"] <- "phr_1.9"
names(poverty_trend)[names(poverty_trend) == "Poverty headcount ratio at $3.20 a day (2011 PPP) (% of population)"] <- "phr_3.2"
names(poverty_trend)[names(poverty_trend) == "Poverty headcount ratio at $5.50 a day (2011 PPP) (% of population)"] <- "phr_5.5"
names(poverty_trend)[names(poverty_trend) == "Poverty headcount ratio at national poverty lines (% of population)"] <- "phr_national"

poverty_trend <- poverty_trend %>% select(Year,phr_1.9,phr_3.2,phr_5.5)
poverty_trend <- na.omit(poverty_trend)
view(poverty_trend)

##### Data Visualisation 
#####  3.1.1 Poverty trend in BDG
ggplot(poverty_trend, aes(x=Year, label=phr_1.9))+
  geom_text(aes(y=phr_1.9), hjust=0.4, vjust=-1, size=5)+
  geom_line(aes(y=phr_1.9, 
                col="Poverty headcount ratio at $1.9 a day (2011 PPP)", group=1), size=2)+
  geom_line(aes(y= phr_3.2, 
                col="Poverty headcount ratio at $3.2 a day (2011 PPP)", group=1), size=2) +
  geom_line(aes(y= phr_5.5, 
                col="Poverty headcount ratio at $5.5 a day (2011 PPP)", group=1), size=2)+
  labs(x="Year", y="Poverty Headcount Ratio (% of population)", 
       title="Poverty Headcount Ratio in Bangladesh from 1983 to 2016",
  caption="Data source: World Development Indicators")+ theme_bw()+
  geom_point(aes(y=phr_1.9), shape=21, color= "black", fill="#69b3a2", size=4)+
  geom_point(aes(y=phr_3.2), shape=21, color= "black", fill="#FF8000", size=4)+
  geom_point(aes(y=phr_5.5), shape=21, color= "black", fill="#CC00CC", size=4)+
  scale_colour_manual(name="Group", values= c("Poverty headcount ratio at $1.9 a day (2011 PPP)"="red",
                                             "Poverty headcount ratio at $3.2 a day (2011 PPP)"="blue",
                                             "Poverty headcount ratio at $5.5 a day (2011 PPP)"="#006600"))

##### 3.1.2 Distribution of income data
gap_income <- file.path("B_WDIEXCEL.xlsx")
gap_income <- read_excel(gap_income,sheet= "Income_share")
head(gap_income)
gap_income <- gap_income %>% mutate(group=fct_relevel(Indicator, "Income share held by highest 20%", 
                                         "Income share held by second 20%", "Income share held by third 20%", 
                                         "Income share held by fourth 20%", "Income share held by lowest 20%")) 
##### Data Visualisation 
##### 3.1.2 income inequality
ggplot(data=gap_income, aes(x=Year, y= value, fill= group)) +
  geom_bar(stat= "identity", position="dodge") + scale_fill_viridis(discrete= TRUE, name="")+ 
  labs(x="Year", y=" % ",title="Income Inequality in Bangladesh",
       caption="Data source: World Development Indicators") + theme_bw() + ylim(0,50)

##### 3.1.3 Inequality trend (GINI)
inequality_trend <- file.path("B_WDIEXCEL.xlsx")
inequality_trend <- read_excel(inequality_trend, sheet="GINI")
inequality_trend<-na.omit(inequality_trend)
names(inequality_trend)[names(inequality_trend) == "GINI Index (World Bank estimate)"] <- "GINI_index"
inequality_trend$Year <- as.character(inequality_trend$Year)

##### Data Visualisation 
##### 3.1.3 Inequality trend (GINI)
ggplot(data=inequality_trendI, aes(x=Year, y=GINI_index, group=1))+
  geom_line(colour="blue", size=2)+
  annotate(geom="text", x=as.character("2016"), y=32.4,
           label="33%\n(2016)",vjust= -0.1, size= 5, fontface=2, color= "red")+
  labs(x="Year", y="GINI index (World Bank estimate) (%)", title="Inequality Trend in Bangladesh",
     caption="Data source: World Development Indicators")+ ylim(25,40)+ theme_bw()
  
##### 3.1.4 unployment rate by gender data
gender_Umemployment <- file.path("B_WDIEXCEL.xlsx")
gender_Umemployment <- read_excel(gender_Umemployment, sheet="Unemployment")
names(gender_Umemployment)[names(gender_Umemployment) == "Unemployment, female (% of female labor force) (modeled ILO estimate)"] <- "Unemploy_female"
names(gender_Umemployment)[names(gender_Umemployment) == "Unemployment, male (% of male labor force) (modeled ILO estimate)"] <- "Unemploy_male"
names(gender_Umemployment)[names(gender_Umemployment) == "Unemployment, total (% of total labor force) (modeled ILO estimate)"] <- "Unemploy_total"

gender_Umemployment <- Umemployment %>% select(Year, Unemploy_female, Unemploy_male)
gender_Umemployment <- na.omit(gender_Umemployment)
gender_Umemployment$Year <- as.character(gender_Umemployment$Year)

##### Data Visualisation
##### 3.1.4 unployment rate by gender (as percent of labor force)

plot4 <- ggplot(data=gender_Umemployment, aes(x=Year))+
  geom_line(aes(y=Unemploy_male,col="Males", group=1),size=2)+
  geom_point(aes(y=Unemploy_male), shape=21, color="black", fill="#FF8000", size=3)+
  geom_line(aes(y=Unemploy_female, col="Females", group=1), size=2)+
  geom_point(aes(y=Unemploy_female), shape=21, color="black", fill="#69b3a2", size=3)+
  labs(x="Year", y="Unemployment Rate (%)", title="Unemployment Rate by Gender in Bangladesh from 1991 to 2019",
       caption="Data source: World Development Indicators")+ theme_bw()+ ylim(0,10)+
  scale_colour_manual(name="Gender",values=c("Males"= "blue", "Females"= "red"))

p4<- ggplotly(plot4)
api_create(p4)

##### 3.1.5 Employment trend by sector data 
sector_employment <- file.path("B_WDIEXCEL.xlsx")
sector_employment <- read_excel(sector_employment, sheet="Employment_sector_t")

##### Data Visualisation
##### 3.1.5 Employment by sector
ggplot(data=sector_employment, aes(x=Year, y=value, fill=group)) +
  geom_bar(stat="identity", width =0.5) +
  scale_fill_viridis(discrete=TRUE, name="")+
  labs(x="Year", y="Percent of Total Employment (%)", title="Employment Trend in the Bangladesh",
       caption="Data source: World Development Indicators")+ theme_bw()
  
##### 3.2 Measuring World Development Indicators on Regions
##### 3.2.1 Poverty Indicator
region_poverty <- file.path("BGD_data.xlsx")
region_poverty <- read_excel(region_poverty, sheet="Poverty")

names(region_poverty)[names(region_poverty) == "Number of poor"] <- "pop_poor"
names(region_poverty)[names(region_poverty) == "Povertyheadcountratio"] <- "phr"
names(region_poverty)[names(region_poverty) == "Numberofextremepoor"] <- "pop_expoor"
names(region_poverty)[names(region_poverty) == "Extreme poverty headcount ratio"] <- "ephr"

region_poverty<-mutate(region_poverty, new_col = region_poverty$pop_poor/1000000)
region_poverty<-mutate(region_poverty, new_pop_expoor = region_poverty$pop_expoor/1000000)
region_poverty$DivisionName <- as.character(Poverty$DivisionName)

##### Data Visualisation 
##### 3.2.1 Poverty Indicator
ggplot(region_poverty, aes(x=DivisionName, y=new_col)) + 
  geom_bar(aes(fill=DivisionName),stat="identity")+ 
  labs(x="Regions", y="Number of People in Poverty (millions)", 
       title="The Number of People in Extreme Poverty by Regions in Bangladesh",
       fill="Regions",caption="Data source: Bangladesh Interactive Poverty Map")+
  ylim(0,20)+theme_bw()

##### 3.2.2 Primary Employment Indicator data 
region_employment <- file.path("BGD_data.xlsx")
region_employment<-read_excel(region_employment, sheet="Primary_employment")

##### Data Visualisation
##### 3.2.2 Primary Employment Indicator 
plot7<-ggplot(region_employment, aes(fill=Primary_employment, y=value, x=Primary_employment)) + 
  geom_bar(position="dodge", stat="identity") +
  scale_fill_viridis(discrete = T, option = "E") + theme_bw()+
  ggtitle("Modes of Primary Employment in Bangladesh") +
  labs(fill= "Categories", y="%", x="",caption = "Data source: Bangladesh Interactive Poverty Map")+
  facet_wrap(~DivisionName)

p7<- ggplotly(plot7)
api_create(p7)

##### 3.2.3 School Attendance Indicator data 
school_attendance <- file.path("BGD_data.xlsx")
school_attendance <- read_excel(school_attendance, sheet="School_attendance")
school_attendance <- mutate(school_attendance, pop = school_attendance$pop/1000000)

##### Data Visualisation
##### 3.2.3 School Attendance Indicator 
school_attendance<- school_attendance %>%
  mutate(Indicator= fct_relevel(Indicator,"High secondary level","Junior level","Secndary level","Primary level")) 

ggplot(school_attendance, aes(fill=Indicator,y=value, x=DivisionName)) +
  geom_bar(position="fill", stat="identity") +
  scale_fill_viridis(discrete = T)+ ggtitle("Educational Attainment by Regions")+ theme_bw()+
  labs(fill= "Categories", y="Percent Distribution", x=" ", caption = "Data source: Bangladesh Interactive Poverty Map")

##### 3.2.4 Health & Nutrition Indicator data
undernourishment<-file.path("BGD_data.xlsx")
undernourishment<-read_excel(undernourishment, sheet="undernourishment")
head(undernourishment)

Underweight<-file.path("BGD_data.xlsx")
Underweight<-read_excel(Underweight, sheet="Underweight")

##### Data Visualisation
##### 3.2.4 Health & Nutrition Indicator 

ggplot(undernourishment, aes(fill=group, y=value, x=DivisionName))+ 
  geom_bar(position="dodge", stat="identity")+theme_bw()+
  ggtitle("Stunted v.s. Severely Stunted Children")+
  labs(fill="Categories", y=" % ", x=" ", caption ="Data source: Bangladesh Interactive Poverty Map")+
  scale_fill_manual(values=c( "#9b1b30", "#003499" ))+
  ylim(0,55)
  
ggplot(Underweight, aes(fill=group, y=value, x=DivisionName)) + 
  geom_bar(position="dodge", stat="identity")+ theme_bw()+
  ggtitle("Underweight v.s. Severely Underweight Children") +
  labs(fill= "Categories", y=" % ", x=" ", caption = "Data source: Bangladesh Interactive Poverty Map")+
  scale_fill_manual(values=c("#9b1b30", "#003499"))+
  ylim(0,55)

##### 3.2.5 Energy, Water & Sanitation Indicator data
E_W_S<-file.path("BGD_data.xlsx")
E_W_S<-read_excel(E_W_S,sheet="Data")
names(E_W_S)[names(E_W_S) == "Households with tap water (N)"] <- "htw"
names(E_W_S)[names(E_W_S) == "Households with tubewell water  (N)"] <- "htww"
names(E_W_S)[names(E_W_S) == "Households with Electricity (N)"] <- "hw"
names(E_W_S)[names(E_W_S) == "Households with flush toilet  (N)"] <- "hft"
names(E_W_S)[names(E_W_S) == "Households with non-flush, latrine (N)"] <- "hnfl"

##### Data Visualisation
##### 3.2.5 Energy, Water & Sanitation Indicator 
E_W_S <- E_W_S %>%
  arrange(hw) %>%
  mutate( name=factor(name, unique(name))) %>%
  mutate(hw=hw/1000) 

##### Energy 
p8<- ggplot(E_W_S, aes(x=name, y=hw)) +
  geom_segment(aes(x=name ,xend=name, y=0, yend=hw), color="black") +
  geom_point(size=4, color="blue") +
  coord_flip() + theme_bw()+
  theme(panel.grid.minor.y = element_blank(),
        panel.grid.major.y = element_blank(),
        legend.position="none") + xlab("")+
  labs(y="Households with access to electricity (thousands)", title="Household with Electricity in Bangladesh",
       caption="Data source: Bangladesh Interactive Poverty Map") 

ggplotly(p8)
api_create(p8)
##### Water & Sanitation
water <- file.path("BGD_data.xlsx")
water <- read_excel(water,sheet="Water")
new_value <- water$Value/1000

ggplot(water, aes(fill=Indicators, y=new_value,x=DivisionName))+ 
  geom_bar(position="dodge", stat="identity") +
  scale_fill_brewer(palette="Paired")+
  ggtitle("Household water in Bangladesh") + theme_bw()+
  labs(fill= "Indicators", y=" Household with access to water (thousand)", x="",caption = "Data source: World Development Indicators")

##### Sanitation
sanitation <- file.path("BGD_data.xlsx")
sanitation <- read_excel(sanitation,sheet="Sanitation")

new_value<-Sanitation$value/1000

ggplot(sanitation, aes(fill=Indicators, y=new_value, x=DivisionName)) + 
  geom_bar(position="dodge", stat="identity") +
  scale_fill_brewer(palette="Paired")+
  ggtitle("Household sanitation in Bangladesh") + theme_bw()+
  labs(fill= "Indicators", y=" Household with access to sanitation (thousand)", x="", caption = "Data source: Bangladesh Interactive Poverty Map")

##### 3.3 Poverty Map data
bangladesh <- map_data("world") %>% filter(region=="Bangladesh")
data<-file.path("BGD_data.xlsx")
data<-read_excel(data,sheet="Data")

data <- data %>%
  arrange(pop) %>%
  mutate( name=factor(name, unique(name))) %>%
  mutate(pop=pop/1000000) 

data <- data %>%
  arrange(pop_poor) %>%
  mutate( name=factor(name, unique(name))) %>%
  mutate(pop_poor =pop_poor/1000000) 

data <- data %>%
  arrange(pop_extrempoor) %>%
  mutate( name=factor(name, unique(name))) %>%
  mutate(pop_extrempoor =pop_extrempoor/1000000) 

##### Data Visualisation
##### 3.3 Poverty Map
ggplot() +
  geom_polygon(data = Bangladesh, aes(x=long, y = lat, group = group), fill="grey", alpha=0.3) +
  geom_point( data=data, aes(x=long, y=lat)) +
  theme_void()+ coord_map() 

##### graphic with names of the 7 biggest cities
map1 <- ggplot() +
  geom_polygon(data = Bangladesh, aes(x=long, y = lat, group = group), fill="grey", alpha=0.3) +
  geom_point(data=data, aes(x=long, y=lat, alpha=pop)) +
  geom_text_repel(data=data %>% arrange(pop_poor) %>% tail(7), aes(x=long, y=lat, label=name), size=5) +
  geom_point( data=data %>% arrange(pop_poor) %>% tail(7), aes(x=long, y=lat), color="red", size=3) +
  theme_void()+coord_map() +
  theme(legend.position="none")+ ggtitle("Bangladesh Map")

map2 <- ggplot() +
  geom_polygon(data = Bangladesh, aes(x=long, y = lat, group = group), fill="grey", alpha=0.3) +
  geom_point( data=data, aes(x=long, y=lat, size=pop, color=pop)) +
  scale_size_continuous(range=c(1,12)) +
  theme_void() + coord_map()+ guides(size = FALSE)+
  labs(title="Population in Bangladesh",caption="Data source: Bangladesh Interactive Poverty Map", size="size",
       color="Population (millions)") 

##### combine mutiple maps
cowplot::plot_grid(map1, map2, nrow = 1)

##### use size and color
map3 <- ggplot()+
  geom_polygon(data = Bangladesh, aes(x=long, y = lat, group = group),fill="grey", alpha=0.3)+ 
  geom_point(data=data,aes(x=long, y=lat,color=pop_poor,size=pop_poor))+
  theme_void() + coord_map()+ guides(size = FALSE)+ scale_colour_gradient(low='blue', high='#FF0000')+
  labs(title="Poverty in Bangladesh",color="The number of poor(millions)",
       caption="Data source: Bangladesh Interactive Poverty Map")

map4 <- ggplot()+
  geom_polygon(data = Bangladesh, aes(x=long, y = lat, group = group),fill="grey", alpha=0.3)+ 
  geom_point(data=data,aes(x=long, y=lat,color=pop_extrempoor,size= pop_extrempoor))+
  theme_void() + coord_map()+ guides(size = FALSE)+ scale_colour_gradient(low='blue', high='#FF0000')+
  labs(title="Extreme Poverty in Bangladesh",color="The number of extreme poor(millions)",
       caption="Data source: Bangladesh Interactive Poverty Map")

##### combine mutiple maps
cowplot::plot_grid(map3, map4, nrow = 1)

