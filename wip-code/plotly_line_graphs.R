---
title: "FE550_Group Project"
author: "Yang Zhao"
date: "5/2/2018"
output: html_document
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```

```{r}
set.seed(1)
hdi <- read.csv("/Users/yangzhao/Desktop/Yang/1_2018Spring/4_FE 550 _ Wendsday/2_group project/FE550 data_sadc/1_Human Development Index (HDI).csv", stringsAsFactors = F, na.strings = "0")
dim(hdi)
#### to convert into numeric
for(i in 3:18) {
    hdi[, i] <- as.numeric(hdi[, i])
}
summary(hdi)
###### Transforming the matrix, it should look like this:
#Country	Country_Code	Year	HDI
#Seychelles	SYC	2000	0.714
#Mauritius	MUS	2000	0.673
#Botswana	BWA	2000	0.560
#South Africa	ZAF	2000	0.629
#Namibia	NAM	2000	0.556
#Zambia	ZMB	2000	0.424
#############
library(ggplot2)
library(reshape2)
library(stringr)
head(hdi)
hdi1 <- melt(hdi, id = c("Country", "Country.Code"))
names(hdi1) <- c("Country", "Country_Code", "Year", "HDI")
# required if the year column has X in front of it
hdi1$Year <- format(strptime(str_sub(hdi1$Year, 2), format = "%Y"), format = "%Y")
head(hdi1)
#Plotting
library(plotly)
q <- ggplot(hdi1,aes(x=Year,y=HDI,colour=Country,group=Country)) + geom_line() + geom_point() + labs(x = "Years", y = "HDI")
ggplotly(q)
```

```{r}
set.seed(1)
population <- read.csv("/Users/yangzhao/Desktop/Yang/1_2018Spring/4_FE 550 _ Wendsday/2_group project/FE550 data_sadc/2_Population, total (millions).csv", stringsAsFactors = F, na.strings = "0")
population[,3]<-NULL
dim(population)
#### to convert into numeric
for(i in 3:10) {
    population[, i] <- as.numeric(population[, i])
}
summary(population)
###### Transforming the matrix, it should look like this:
#Country	Country_Code	Year	HDI
#Seychelles	SYC	2000	0.714
#Mauritius	MUS	2000	0.673
#Botswana	BWA	2000	0.560
#South Africa	ZAF	2000	0.629
#Namibia	NAM	2000	0.556
#Zambia	ZMB	2000	0.424
#############
library(ggplot2)
library(reshape2)
library(stringr)
head(population)
population1 <- melt(population, id = c("Country", "Country.Code"))
names(population1) <- c("Country", "Country_Code", "Year", "PopulationinMillions")
# required if the year column has X in front of it
population1$Year <- format(strptime(str_sub(population1$Year, 2), format = "%Y"), format = "%Y")
head(population1)
#Plotting
library(plotly)
q <- ggplot(population1,aes(x=Year,y=PopulationinMillions,colour=Country,group=Country)) + geom_line() + geom_point() + labs(x = "Years", y = "Population (Millions)")
ggplotly(q)
```

```{r}
set.seed(1)
urban <- read.csv("/Users/yangzhao/Desktop/Yang/1_2018Spring/4_FE 550 _ Wendsday/2_group project/FE550 data_sadc/2_Population, urban (%).csv", stringsAsFactors = F, na.strings = "0")
urban[,3]<-NULL
dim(urban)
#### to convert into numeric
for(i in 3:10) {
    urban[, i] <- as.numeric(urban[, i])
}
summary(urban)
###### Transforming the matrix, it should look like this:
#Country	Country_Code	Year	HDI
#Seychelles	SYC	2000	0.714
#Mauritius	MUS	2000	0.673
#Botswana	BWA	2000	0.560
#South Africa	ZAF	2000	0.629
#Namibia	NAM	2000	0.556
#Zambia	ZMB	2000	0.424
#############
library(ggplot2)
library(reshape2)
library(stringr)
head(urban)
urban1 <- melt(urban, id = c("Country", "Country.Code"))
names(urban1) <- c("Country", "Country_Code", "Year", "UrbanPopulationRate")
# required if the year column has X in front of it
urban1$Year <- format(strptime(str_sub(urban1$Year, 2), format = "%Y"), format = "%Y")
head(urban1)
#Plotting
library(plotly)
q <- ggplot(urban1,aes(x=Year,y=UrbanPopulationRate,colour=Country,group=Country)) + geom_line() + geom_point() + labs(x = "Years", y = "Urban Population Rate (%)")
ggplotly(q)
```

```{r}
set.seed(1)
mfration <- read.csv("/Users/yangzhao/Desktop/Yang/1_2018Spring/4_FE 550 _ Wendsday/2_group project/FE550 data_sadc/2_Sex ratio at birth (male to female births).csv", stringsAsFactors = F, na.strings = "0")
mfration[,3]<-NULL
dim(mfration)
#### to convert into numeric
for(i in 3:6) {
    mfration[, i] <- as.numeric(mfration[, i])
}
summary(mfration)
###### Transforming the matrix, it should look like this:
#Country	Country_Code	Year	HDI
#Seychelles	SYC	2000	0.714
#Mauritius	MUS	2000	0.673
#Botswana	BWA	2000	0.560
#South Africa	ZAF	2000	0.629
#Namibia	NAM	2000	0.556
#Zambia	ZMB	2000	0.424
#############
library(ggplot2)
library(reshape2)
library(stringr)
head(mfration)
mfration1 <- melt(mfration, id = c("Country", "Country.Code"))
names(mfration1) <- c("Country", "Country_Code", "Year", "MaletoFemaleRatioatBirth")
# required if the year column has X in front of it
mfration1$Year <- format(strptime(str_sub(mfration1$Year, 2), format = "%Y"), format = "%Y")
head(mfration1)
#Plotting
library(plotly)
q <- ggplot(mfration1,aes(x=Year,y=MaletoFemaleRatioatBirth,colour=Country,group=Country)) + geom_line() + geom_point() + labs(x = "Years", y = "Male to Female Ratio at Birth (%)")
ggplotly(q)

```

```{r}
set.seed(1)
schooling <- read.csv("/Users/yangzhao/Desktop/Yang/1_2018Spring/4_FE 550 _ Wendsday/2_group project/FE550 data_sadc/3_Expected years of schooling (years).csv", stringsAsFactors = F, na.strings = "0")
schooling[,3]<-NULL
dim(schooling)
#### to convert into numeric
for(i in 3:7) {
    schooling[, i] <- as.numeric(schooling[, i])
}
summary(schooling)
###### Transforming the matrix, it should look like this:
#Country	Country_Code	Year	HDI
#Seychelles	SYC	2000	0.714
#Mauritius	MUS	2000	0.673
#Botswana	BWA	2000	0.560
#South Africa	ZAF	2000	0.629
#Namibia	NAM	2000	0.556
#Zambia	ZMB	2000	0.424
#############
library(ggplot2)
library(reshape2)
library(stringr)
head(schooling)
schooling1 <- melt(schooling, id = c("Country", "Country.Code"))
names(schooling1) <- c("Country", "Country_Code", "Year", "ExpectedYearsofSchooling")
# required if the year column has X in front of it
schooling1$Year <- format(strptime(str_sub(schooling1$Year, 2), format = "%Y"), format = "%Y")
head(schooling1)
#Plotting
library(plotly)
q <- ggplot(schooling1,aes(x=Year,y=ExpectedYearsofSchooling,colour=Country,group=Country)) + geom_line() + geom_point() + labs(x = "Years", y = "Expected Years of Schooling (years)")
ggplotly(q)
```

```{r}
set.seed(1)
eduexpend <- read.csv("/Users/yangzhao/Desktop/Yang/1_2018Spring/4_FE 550 _ Wendsday/2_group project/FE550 data_sadc/3_Government expenditure on education (% of GDP).csv", stringsAsFactors = F, na.strings = "0")
eduexpend[,3]<-NULL
dim(eduexpend)
#### to convert into numeric
for(i in 3:9) {
    eduexpend[, i] <- as.numeric(eduexpend[, i])
}
summary(eduexpend)
###### Transforming the matrix, it should look like this:
#Country	Country_Code	Year	HDI
#Seychelles	SYC	2000	0.714
#Mauritius	MUS	2000	0.673
#Botswana	BWA	2000	0.560
#South Africa	ZAF	2000	0.629
#Namibia	NAM	2000	0.556
#Zambia	ZMB	2000	0.424
#############
library(ggplot2)
library(reshape2)
library(stringr)
head(eduexpend)
eduexpend1 <- melt(eduexpend, id = c("Country", "Country.Code"))
names(eduexpend1) <- c("Country", "Country_Code", "Year", "EducationExpenditure")
# required if the year column has X in front of it
eduexpend1$Year <- format(strptime(str_sub(eduexpend1$Year, 2), format = "%Y"), format = "%Y")
head(eduexpend1)
#Plotting
library(plotly)
q <- ggplot(eduexpend1,aes(x=Year,y=EducationExpenditure,colour=Country,group=Country)) + geom_line() + geom_point() + labs(x = "Years", y = "Government expenditure on education (% of GDP)")
ggplotly(q)

```
```{r}
set.seed(1)
school <- read.csv("/Users/yangzhao/Desktop/Yang/1_2018Spring/4_FE 550 _ Wendsday/2_group project/FE550 data_sadc/3_Mean years of schooling (years).csv", stringsAsFactors = F, na.strings = "0")
school[,3]<-NULL
dim(school)
for(i in 3:18) {
    school[, i] <- as.numeric(school[, i])
}
summary(school)
library(ggplot2)
library(reshape2)
library(stringr)
head(school)
school1 <- melt(school, id = c("Country", "Country.Code"))
names(school1) <- c("Country", "Country_Code", "Year", "MeanYearsofSchooling")
school1$Year <- format(strptime(str_sub(school1$Year, 2), format = "%Y"), format = "%Y")
head(school1)
library(plotly)
q <- ggplot(school1,aes(x=Year,y=MeanYearsofSchooling,colour=Country,group=Country)) + geom_line() + geom_point() + labs(x = "Years", y = "Mean years of schooling (years)")
ggplotly(q)
```
```{r}
set.seed(1)
dropout <- read.csv("/Users/yangzhao/Desktop/Yang/1_2018Spring/4_FE 550 _ Wendsday/2_group project/FE550 data_sadc/3_Primary school dropout rate (% of primary school cohort).csv", stringsAsFactors = F, na.strings = "0")
dropout[,3]<-NULL
dim(dropout)
for(i in 3:9) {
    dropout[, i] <- as.numeric(dropout[, i])
}
summary(dropout)
library(ggplot2)
library(reshape2)
library(stringr)
head(dropout)
dropout1 <- melt(dropout, id = c("Country", "Country.Code"))
names(dropout1) <- c("Country", "Country_Code", "Year", "Primaryschooldropoutrate")
dropout1$Year <- format(strptime(str_sub(dropout1$Year, 2), format = "%Y"), format = "%Y")
head(dropout1)
library(plotly)
q <- ggplot(dropout1,aes(x=Year,y=Primaryschooldropoutrate,colour=Country,group=Country)) + geom_line() + geom_point() + labs(x = "Years", y = "Primary School Dropout Rate")
ggplotly(q)

```

```{R}
set.seed(1)
HIV <- read.csv("/Users/yangzhao/Desktop/Yang/1_2018Spring/4_FE 550 _ Wendsday/2_group project/FE550 data_sadc/4_HIV prevalence, adult (% ages 15-49), total.csv", stringsAsFactors = F, na.strings = "0")
HIV[,3]<-NULL
dim(HIV)
for(i in 3:9) {
    HIV[, i] <- as.numeric(HIV[, i])
}
summary(HIV)
library(ggplot2)
library(reshape2)
library(stringr)
head(HIV)
HIV1 <- melt(HIV, id = c("Country", "Country.Code"))
names(HIV1) <- c("Country", "Country_Code", "Year", "HIVprevalenceofadult")
HIV1$Year <- format(strptime(str_sub(HIV1$Year, 2), format = "%Y"), format = "%Y")
head(HIV1)
library(plotly)
q <- ggplot(HIV1,aes(x=Year,y=HIVprevalenceofadult,colour=Country,group=Country)) + geom_line() + geom_point() + labs(x = "Years", y = "HIV prevalence of adult (% ages 15-49)")
ggplotly(q)

```
```{r}
set.seed(1)
life <- read.csv("/Users/yangzhao/Desktop/Yang/1_2018Spring/4_FE 550 _ Wendsday/2_group project/FE550 data_sadc/4_Life expectancy Index.csv", stringsAsFactors = F, na.strings = "0")
life[,3]<-NULL
dim(life)
for(i in 3:9) {
    life[, i] <- as.numeric(life[, i])
}
summary(life)
library(ggplot2)
library(reshape2)
library(stringr)
head(life)
life1 <- melt(life, id = c("Country", "Country.Code"))
names(life1) <- c("Country", "Country_Code", "Year", "LifeExpectancyIndex")
life1$Year <- format(strptime(str_sub(life1$Year, 2), format = "%Y"), format = "%Y")
head(life1)
library(plotly)
q <- ggplot(life1,aes(x=Year,y=LifeExpectancyIndex,colour=Country,group=Country)) + geom_line() + geom_point() + labs(x = "Years", y = "Life expectancy Index")
ggplotly(q)
```


```{r}
set.seed(1)
health <- read.csv("/Users/yangzhao/Desktop/Yang/1_2018Spring/4_FE 550 _ Wendsday/2_group project/FE550 data_sadc/4_Public health expenditure (% of GDP).csv", stringsAsFactors = F, na.strings = "0")
health[,3]<-NULL
dim(health)
for(i in 3:9) {
    health[, i] <- as.numeric(health[, i])
}
summary(health)
library(ggplot2)
library(reshape2)
library(stringr)
head(health)
health1 <- melt(health, id = c("Country", "Country.Code"))
names(health1) <- c("Country", "Country_Code", "Year", "PublicHealthExpenditure")
health1$Year <- format(strptime(str_sub(health1$Year, 2), format = "%Y"), format = "%Y")
head(health1)
library(plotly)
q <- ggplot(health1,aes(x=Year,y=PublicHealthExpenditure,colour=Country,group=Country)) + geom_line() + geom_point() + labs(x = "Years", y = "Public health expenditure % of GDP")
ggplotly(q)


```

```{R}
set.seed(1)
empratio <- read.csv("/Users/yangzhao/Desktop/Yang/1_2018Spring/4_FE 550 _ Wendsday/2_group project/FE550 data_sadc/5_Employment to population ratio (% ages 15 and older).csv", stringsAsFactors = F, na.strings = "0")
empratio[,3]<-NULL
dim(empratio)
for(i in 3:10) {
    empratio[, i] <- as.numeric(empratio[, i])
}
summary(empratio)
library(ggplot2)
library(reshape2)
library(stringr)
head(empratio)
empratio1 <- melt(empratio, id = c("Country", "Country.Code"))
names(empratio1) <- c("Country", "Country_Code", "Year", "EmploymenttoPopulationRatio")
empratio1$Year <- format(strptime(str_sub(empratio1$Year, 2), format = "%Y"), format = "%Y")
head(empratio1)
library(plotly)
q <- ggplot(empratio1,aes(x=Year,y=EmploymenttoPopulationRatio,colour=Country,group=Country)) + geom_line() + geom_point() + labs(x = "Years", y = "Employment to Population Ratio")
ggplotly(q)
```

```{r}
set.seed(1)
unempratio <- read.csv("/Users/yangzhao/Desktop/Yang/1_2018Spring/4_FE 550 _ Wendsday/2_group project/FE550 data_sadc/5_Total unemployment rate (% of labour force).csv", stringsAsFactors = F, na.strings = "0")
unempratio[,3]<-NULL
dim(unempratio)
for(i in 3:10) {
    unempratio[, i] <- as.numeric(unempratio[, i])
}
summary(unempratio)
library(ggplot2)
library(reshape2)
library(stringr)
head(unempratio)
unempratio1 <- melt(unempratio, id = c("Country", "Country.Code"))
names(unempratio1) <- c("Country", "Country_Code", "Year", "TotalUnemploymentRate")
unempratio1$Year <- format(strptime(str_sub(unempratio1$Year, 2), format = "%Y"), format = "%Y")
head(unempratio1)
library(plotly)
q <- ggplot(unempratio1,aes(x=Year,y=TotalUnemploymentRate,colour=Country,group=Country)) + geom_line() + geom_point() + labs(x = "Years", y = "Total unemployment rate (% of labour force)")
ggplotly(q)
```
```{R}
set.seed(1)
unempyouth <- read.csv("/Users/yangzhao/Desktop/Yang/1_2018Spring/4_FE 550 _ Wendsday/2_group project/FE550 data_sadc/5_Youth unemployment rate (% of labour force ages 15-24).csv", stringsAsFactors = F, na.strings = "0")
unempyouth[,3]<-NULL
dim(unempyouth)
for(i in 3:11) {
    unempyouth[, i] <- as.numeric(unempyouth[, i])
}
summary(unempyouth)
library(ggplot2)
library(reshape2)
library(stringr)
head(unempyouth)
unempyouth1 <- melt(unempyouth, id = c("Country", "Country.Code"))
names(unempyouth1) <- c("Country", "Country_Code", "Year", "YouthUnemploymentRate")
unempyouth1$Year <- format(strptime(str_sub(unempyouth1$Year, 2), format = "%Y"), format = "%Y")
head(unempyouth1)
library(plotly)
q <- ggplot(unempyouth1,aes(x=Year,y=YouthUnemploymentRate,colour=Country,group=Country)) + geom_line() + geom_point() + labs(x = "Years", y = "Youth unemployment rate (% of labour force ages 15-24)")
ggplotly(q)

```

```{R}
set.seed(1)
GII <- read.csv("/Users/yangzhao/Desktop/Yang/1_2018Spring/4_FE 550 _ Wendsday/2_group project/FE550 data_sadc/6_Gender Inequality Index (GII).csv", stringsAsFactors = F, na.strings = "0")
GII[,3]<-NULL
dim(GII)
for(i in 3:10) {
    GII[, i] <- as.numeric(GII[, i])
}
summary(GII)
library(ggplot2)
library(reshape2)
library(stringr)
head(GII)
GII1 <- melt(GII, id = c("Country", "Country.Code"))
names(GII1) <- c("Country", "Country_Code", "Year", "GenderInequalityIndex")
GII1$Year <- format(strptime(str_sub(GII1$Year, 2), format = "%Y"), format = "%Y")
head(GII1)
library(plotly)
q <- ggplot(GII1,aes(x=Year,y=GenderInequalityIndex,colour=Country,group=Country)) + geom_line() + geom_point() + labs(x = "Years", y = "Gender Inequality Index")
ggplotly(q)
```
