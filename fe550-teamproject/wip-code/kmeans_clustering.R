---
title: "Clustering"
output:
  html_document: default
  html_notebook: default
  pdf_document: default
---

**Smit Mehta**  
**10429779**


Some of the factors we have looked at are:
1. Population
2. Access to electricity (% of total population)
3. Adolescent fertility rate (births per 1,000 women ages 15-19)
4. Bribery incidence (% of firms experiencing at least one bribe payment request)
5. Firms with female participation in ownership (% of firms)
6. GNI (constant 2010 US$)
7. GDP per capita (constant 2010 US$)
8. Incidence of HIV (% of uninfected population ages 15-49)
9. Literacy rate, youth (ages 15-24), gender parity index (GPI) #Dropped later on (only 1 value)
10. Maternal mortality ratio (modeled estimate, per 100,000 live births)
11. Prevalence of HIV, total (% of population ages 15-49)
12. Unemployment, total (% of total labor force) (modeled ILO estimate)

     
#_Kmeans Clustering_
Using the categorized features to develop a Kmeans Clustering model(with k=2)

##Reading in the data

```{r, echo=FALSE, message=FALSE, warning=FALSE}
a <- read.csv("bribe.csv")
b <- read.csv("electricity.csv")
c <- read.csv("female_firms.csv")
d <- read.csv("fertility_rate.csv")
e <- read.csv("gdp.csv")
f <- read.csv("gni.csv")
g <- read.csv("gpi.csv")
h <- read.csv("hiv.csv")
i <- read.csv("hiv2.csv")
j <- read.csv("maternaldeaths.csv")
k <- read.csv("total_population.csv")
l <- read.csv("unemp.csv")
temp <- a[, c(1,2)]
```

##Data Preparation

```{r, echo=FALSE, message=FALSE, warning=FALSE}
a <- na.omit(a)
b <- na.omit(b)
c <- na.omit(c)
d <- na.omit(d)
e <- na.omit(e)
f <- na.omit(f)
g <- na.omit(g)
h <- na.omit(h)
i <- na.omit(i)
j <- na.omit(j)
k <- na.omit(k)
l <- na.omit(l)

a <- a[, c(2, 5)]
b <- b[, c(2, 5)]
c <- c[, c(2, 5)]
d <- d[, c(2, 5)]
e <- e[, c(2, 5)]
f <- f[, c(2, 5)]
g <- g[, c(2, 5)]
h <- h[, c(2, 5)]
i <- i[, c(2, 5)]
j <- j[, c(2, 5)]
k <- k[, c(2, 3)]
l <- l[, c(2, 5)]


#Merging


temp1 <- merge(temp, a, by = "Country_Code", all.x = TRUE)
temp2 <- merge(temp1, b, by = "Country_Code", all.x = TRUE)
temp3 <- merge(temp2, c, by = "Country_Code", all.x = TRUE)
temp4 <- merge(temp3, d, by = "Country_Code", all.x = TRUE)
temp5 <- merge(temp4, e, by = "Country_Code", all.x = TRUE)
temp6 <- merge(temp5, f, by = "Country_Code", all.x = TRUE)
temp7 <- merge(temp6, g, by = "Country_Code", all.x = TRUE)
temp8 <- merge(temp7, h, by = "Country_Code", all.x = TRUE)
temp9 <- merge(temp8, i, by = "Country_Code", all.x = TRUE)
temp10 <- merge(temp9, j, by = "Country_Code", all.x = TRUE)
temp11 <- merge(temp10, k, by = "Country_Code", all.x = TRUE)
temp12 <- merge(temp11, l, by = "Country_Code", all.x = TRUE)

sadc <- subset(temp12, Country_Code == "AGO" | Country_Code == "BWA" | Country_Code == "COD" | Country_Code == "LSO" | Country_Code == "MDG" | Country_Code == "MWI" | Country_Code == "MUS" | Country_Code == "MOZ" | Country_Code == "NAM" | Country_Code == "SYC" | Country_Code == "ZAF" | Country_Code == "SWZ" | Country_Code == "TZA" | Country_Code == "ZMB" | Country_Code == "ZWE")

##Dropping GPI
sadc$GPI_2016 <- NULL

#Transforming the variables
sadc$Bribery_Incindents_2016 <- sadc$Bribery_Incindents_2016*0.01
sadc$Bribery_Incindents_2016[is.na(sadc$Bribery_Incindents_2016)] <- 0

sadc$Access_Electricity_2014 <- sadc$Access_Electricity_2014*0.01


sadc$Female_Firms_2016 <- sadc$Female_Firms_2016*0.01
sadc$Female_Firms_2016[is.na(sadc$Female_Firms_2016)] <- 0

sadc$GNI_norm <- (sadc$GNI_2016 - min(sadc$GNI_2016, na.rm = TRUE))/(max(sadc$GNI_2016, na.rm = TRUE) - min(sadc$GNI_2016, na.rm = TRUE))
sadc$GNI_2016 <- NULL
sadc$GNI_norm[is.na(sadc$GNI_norm)] <- 0

sadc$fertility_norm <- (sadc$Adolescent_fertility_2015 - min(sadc$Adolescent_fertility_2015, na.rm = TRUE))/(max(sadc$Adolescent_fertility_2015, na.rm = TRUE) - min(sadc$Adolescent_fertility_2015, na.rm = TRUE))
sadc$Adolescent_fertility_2015 <- NULL

sadc$gdp_norm <- (sadc$GDP_2016 - min(sadc$GDP_2016, na.rm = TRUE))/(max(sadc$GDP_2016, na.rm = TRUE) - min(sadc$GDP_2016, na.rm = TRUE))
sadc$GDP_2016 <- NULL



sadc$HIV_2016 <- sadc$HIV_2016*0.01
sadc$HIV_2016[is.na(sadc$HIV_2016)] <- 0

sadc$HIV2_2016 <- sadc$HIV2_2016*0.01
sadc$HIV2_2016[is.na(sadc$HIV2_2016)] <- 0

sadc$Maternal_Death_2015 <- sadc$Maternal_Death_2015*0.001
sadc$Maternal_Death_2015[is.na(sadc$Maternal_Death_2015)] <- 0

sadc$pop_norm <- (sadc$Tot_Population - min(sadc$Tot_Population, na.rm = TRUE))/(max(sadc$Tot_Population, na.rm = TRUE) - min(sadc$Tot_Population, na.rm = TRUE))
sadc$Tot_Population <- NULL

sadc$Unemp_2016 <- sadc$Unemp_2016*0.01
sadc$Unemp_2016[is.na(sadc$Unemp_2016)] <- 0



sadc_f <- sadc[, c(3:13)]
rownames(sadc_f) <- sadc$Country

```


##Creating and visualizing the Distance matrix

```{r, echo=FALSE, message=FALSE, warning=FALSE, fig.width = 12, fig.height = 7}
library(cluster)    # clustering algorithms
library(factoextra) # clustering algorithms & visualization

distance <- get_dist(sadc_f)
fviz_dist(distance, gradient = list(low = "#00AFBB", mid = "white", high = "#FC4E07"))
```

##kmeans clustering for k=2

```{r, echo=FALSE, message=FALSE, warning=FALSE}
sadc_cluster <- kmeans(sadc_f, centers = 3, nstart = 25)
#str(sadc_cluster)
```


##Generating visualization

```{r, echo=FALSE, message=FALSE, warning=FALSE, fig.width = 12, fig.height = 7}
fviz_cluster(sadc_cluster, data = sadc_f)
```


