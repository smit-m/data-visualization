
```{r, echo=FALSE, message=FALSE, warning=FALSE}

a <- read.csv("SDGData - Copy.csv")
b <- read.csv("SDGCountry.csv")

c <- merge(a, b, by.x = 'Country.Code', by.y = 'ï..Country.Code', all.x = TRUE)
d <- subset(c, Indicator.Name == 'Access to electricity (% of population)')
e <- d[, c(1, 34, 26:29)]


f <- subset(c, Indicator.Name == 'CO2 emissions (metric tons per capita)')



library(reshape2)
library(stringr)
f <- melt(e, variable.name = "Year", id.vars = c("Country.Code", "Short.Name")) # CHANGE Cast to D, W, add BO

f$Year2 <- format(strptime(str_sub(f$Year, 2), format = "%Y"), format = "%Y")


library(plotly)





# light grey boundaries
l <- list(color = toRGB("grey"), width = 0.5)

# specify map projection/options
g1 <- list(
  showframe = FALSE,
  showcoastlines = FALSE,
  projection = list(
    type = 'orthographic',
    rotation = list(
      lon = 0,
      lat = 0,
      roll = 0
    )))





p <- plot_geo(f) %>%
  add_trace(
    data = f[f$Year2 == '2011',], z = ~value, colors = 'Blues', name = '2011',
    text = ~Short.Name, locations = ~Country.Code, marker = list(line = l)
  ) %>%
  colorbar() %>%
  add_trace(
    data = f[f$Year2 == '2012',], z = ~value, colors = 'Oranges', name = '2012',
    text = ~Short.Name, locations = ~Country.Code, marker = list(line = l), visible = FALSE
  ) %>%
  colorbar() %>%
  add_trace(
    data = f[f$Year2 == '2013',], z = ~value, colors = 'Greys', name = '2013',
    text = ~Short.Name, locations = ~Country.Code, marker = list(line = l), visible = FALSE
  ) %>%
  colorbar() %>%
  add_trace(
    data = f[f$Year2 == '2014',], z = ~value, colors = 'PuRd', name = '2014',
    text = ~Short.Name, locations = ~Country.Code, marker = list(line = l), visible = FALSE
  ) %>%
  colorbar() %>%
  layout(
    title = 'ABC',
    geo = g1,
    updatemenus = list(
      
      list(
        y = 0.7,
        buttons = list(
          list(method = "restyle",
               args = list("visible", list(TRUE, FALSE, FALSE, FALSE)),
               label = "2011"),
          list(method = "restyle",
               args = list("visible", list(FALSE, TRUE, FALSE, FALSE)),
               label = "2012"),
          list(method = "restyle",
               args = list("visible", list(FALSE, FALSE, TRUE, FALSE)),
               label = "2013"),
          
          list(method = "restyle",
               args = list("visible", list(FALSE, FALSE, FALSE, TRUE)),
               label = "2014"))))
  ) 



p

```
