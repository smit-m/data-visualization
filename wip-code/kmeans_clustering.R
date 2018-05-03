---
title: "HW08_Clustering"
output:
  pdf_document: default
  html_notebook: default
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

```{r}

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

```{r}
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


```


##Creating and visualizing the Distance matrix

```{r}
library(cluster)    # clustering algorithms
library(factoextra) # clustering algorithms & visualization

distance <- get_dist(bc3)
fviz_dist(distance, gradient = list(low = "#00AFBB", mid = "white", high = "#FC4E07"))
```

##kmeans clustering for k=2

```{r}
k2 <- kmeans(bc3, centers = 2, nstart = 25)
#str(k2)
```


##Generating visualization

```{r}
fviz_cluster(k2, data = bc3)
```


