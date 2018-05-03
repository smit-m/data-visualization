```{r, echo = FALSE, message=FALSE, warning=FALSE, fig.width = 12, fig.height = 7}

donor_map <- read.csv("donor_map.csv")
airports <- read.csv("airports.csv")
# Plot flight routes
library(ggplot2)
library(ggrepel)
worldmap <- borders("world", colour="#efede1", fill="#efede1") # create a layer of borders
p <- ggplot() + worldmap + 
  geom_curve(data=donor_map, aes(x = lon.x, y = lat.x, xend = lon.y, yend = lat.y, color = Group), size = 1, curvature = .3, alpha = 0.3) + 
  geom_point(data=airports, aes(x = lon, y = lat), col = "#970027") + 
  geom_text_repel(data=airports, aes(x = lon, y = lat, label = airport), col = "black", size = 3, segment.color = NA) + 
  theme(panel.background = element_rect(fill="white"), 
        axis.line = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank()
  )
p
```
