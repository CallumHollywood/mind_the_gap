

# this script highlights the Africa counties 
# that had a dip (at leaset one) in lifeExp over all the years

packs <- c("maps", "mapdata", "rworldmap")
lapply(packs, require, character.only = T)
rm(packs)


#library(rworldmap)
source("r_scripts/eda/eda_setup.R")

lifeexpdrops <- gapminder::gapminder %>% 
  filter(continent == "Africa") %>% 
  select(-c(continent, pop, gdpPercap)) %>% 
  spread(year, lifeExp)

indx <- colSums(apply(lifeexpdrops %>% select(-c(country)), 1L, diff) > 0L) == (ncol(lifeexpdrops%>% select(-c(country))) - 1L)
lifeexpdrops <-  lifeexpdrops[!indx,]$country

africa <- gapminder::gapminder %>% 
  filter(continent == "Africa") %>% 
  select(-c(continent)) %>% 
  filter(year == 2002) %>% 
  mutate(lifeExpDrop = if_else(country %in% lifeexpdrops, "dropped at least once", "has never dropped"))


sPDF <- joinCountryData2Map(africa,
                            joinCode = "NAME",
                            nameJoinColumn = "country")


jpeg("output/maps/africa_lifeExpDips.jpg")

par(mai = c(0,0,0.2,0), xaxs="i", yaxs = "i")
mapCountryData(sPDF, 
               nameColumnToPlot = "lifeExpDrop", 
               catMethod="categorical", 
               numCats = 2,
               addLegend =T,
               mapTitle = "African Countries with (at least 1) Life Expectancy Drops",
               mapRegion = "Africa")

country_coord<-data.frame(coordinates(sPDF),stringsAsFactors=F)
country_coord_B <- country_coord[row.names(country_coord) %in% africa$country,]

text(x = country_coord_B$X1, y = country_coord_B$X2, 
     labels = row.names(country_coord_B))



dev.off()


#############################
# Look for any other countries in world that had lifeExp dips
#############################


lifeexpdrops <- gapminder::gapminder %>% 
  #filter(continent == "Africa") %>% 
  select(-c(continent, pop, gdpPercap)) %>% 
  spread(year, lifeExp)

indx <- colSums(apply(lifeexpdrops %>% select(-c(country)), 1L, diff) > 0L) == (ncol(lifeexpdrops%>% select(-c(country))) - 1L)
lifeexpdrops <-  lifeexpdrops[!indx,]$country

worldLifeExpDrop <- gapminder::gapminder %>% 
  #filter(continent == "Africa") %>% 
  select(-c(continent)) %>% 
  filter(year == 2002) %>% 
  mutate(lifeExpDrop = if_else(country %in% lifeexpdrops, "dropped at least once", "has never dropped"))


worldLifeExpDropExcptAfrica <- worldLifeExpDrop %>% 
  filter(lifeExpDrop == "dropped at least once") %>% 
  filter(!country %in% africa$country)



  

sPDF_World <- joinCountryData2Map(worldLifeExpDrop,
                            joinCode = "NAME",
                            nameJoinColumn = "country")


jpeg("output/maps/world_lifeExpDips.jpg",
     width = 960,
     height = 460)

par(mai = c(0,0,0.2,0), xaxs="i", yaxs = "i")
mapCountryData(sPDF_World, 
               nameColumnToPlot = "lifeExpDrop", 
               catMethod="categorical", 
               numCats = 2,
               addLegend =T,
               mapTitle = "World Countries with (at least 1) Life Expectancy Drops"#,
               #mapRegion = "Africa"
               )

country_coord <- data.frame(coordinates(sPDF_World),stringsAsFactors=F)
country_coord_C <- country_coord[row.names(country_coord) %in% worldLifeExpDropExcptAfrica$country,]

#country_coord_C <- country_coord[!row.names(country_coord) %in% africa$country,]
#country_coord_C <- country_coord_C[!row.names(country_coord_C) %in% worldLifeExpDropExcptAfrica$country,]

text(x = country_coord_C$X1, y = country_coord_C$X2, 
     labels = row.names(country_coord_C))



dev.off()




#############################
# any countries wher the pop decreased?
#############################

pop_drops <- gapminder::gapminder %>% 
  select(-c(continent, lifeExp, gdpPercap)) %>% 
  spread(year, pop)

indx <- colSums(apply(pop_drops %>% select(-c(country)), 1L, diff) > 0L) == (ncol(pop_drops%>% select(-c(country))) - 1L)
pop_drops <-  pop_drops[!indx,]$country

world_popdrops <- gapminder::gapminder %>% 
  select(-c(continent)) %>% 
  filter(year == 2002) %>% 
  mutate(pop_drops = if_else(country %in% pop_drops, "dropped at least once", "has never dropped"))

world_popdrops_slim <- world_popdrops[world_popdrops$pop_drops == 'dropped at least once',]




sPDF_worldpopdrop <- joinCountryData2Map(world_popdrops,
                                  joinCode = "NAME",
                                  nameJoinColumn = "country")

jpeg("output/maps/world_PopDrops.jpg",
     width = 960,
     height = 480)

par(mai = c(0,0,0.2,0), xaxs="i", yaxs = "i")
mapCountryData(sPDF_worldpopdrop, 
               nameColumnToPlot = "pop_drops", 
               catMethod="categorical", 
               numCats = 2,
               addLegend =T,
               mapTitle = "World Countries with (at least 1) Population Drop"#,
               #mapRegion = "Africa"
)

country_coord <- data.frame(coordinates(sPDF_World),stringsAsFactors=F)
country_coord_D <- country_coord[row.names(country_coord) %in% world_popdrops_slim$country,]

#country_coord_C <- country_coord[!row.names(country_coord) %in% africa$country,]
#country_coord_C <- country_coord_C[!row.names(country_coord_C) %in% worldLifeExpDropExcptAfrica$country,]

text(x = country_coord_D$X1, y = country_coord_D$X2, 
     labels = row.names(country_coord_D))

dev.off() 




#############################
# any countries wher the gdp decreased?
# the map below shows that gdp dropping at least once during this period is reasonalby common
# so in addition to this look at next step aslo
#############################


gdp_drops <- gapminder::gapminder %>% 
  select(-c(continent, lifeExp, pop)) %>% 
  spread(year, gdpPercap)

indx <- colSums(apply(gdp_drops %>% select(-c(country)), 1L, diff) > 0L) == (ncol(gdp_drops%>% select(-c(country))) - 1L)
gdp_drops <-  gdp_drops[!indx,]$country




world_gdpdrops <- gapminder::gapminder %>% 
  select(-c(continent)) %>% 
  filter(year == 2002) %>% 
  mutate(gdp_drops = if_else(country %in% gdp_drops, "dropped at least once", "has never dropped"))

world_gdpdrops_slim <- world_gdpdrops[world_gdpdrops$gdp_drops == 'dropped at least once',]


sPDF_worldgdpdrop <- joinCountryData2Map(world_gdpdrops,
                                         joinCode = "NAME",
                                         nameJoinColumn = "country")



par(mai = c(0,0,0.2,0), xaxs="i", yaxs = "i")
mapCountryData(sPDF_worldgdpdrop, 
               nameColumnToPlot = "gdp_drops", 
               catMethod="categorical", 
               numCats = 2,
               addLegend =T,
               mapTitle = "World Countries with (at least 1) GDP Drop"#,
               #mapRegion = "Africa"
)

country_coord <- data.frame(coordinates(sPDF_World),stringsAsFactors=F)
country_coord_E <- country_coord[!row.names(country_coord) %in% world_gdpdrops_slim$country,]

#country_coord_C <- country_coord[!row.names(country_coord) %in% africa$country,]
#country_coord_C <- country_coord_C[!row.names(country_coord_C) %in% worldLifeExpDropExcptAfrica$country,]

text(x = country_coord_E$X1, y = country_coord_E$X2, 
     labels = row.names(country_coord_E))



# ###########################
# from the ggplot below we can see that there is def summit going on with kuwait
# lets have a look at that and all countreis where gdp is less than where it started
# ###########################


ggplot(munge_data, aes(year, gdpPercap, group = country)) +
  geom_line() +
  theme(legend.position = "none") +
  ggtitle("All Countries GDP Per Capita") +
  geom_text(data = munge_data[munge_data$year == 1962,], aes(label = country))


gdp_world_drops <- gapminder::gapminder %>% 
  filter(year %in% c(1952, 2007)) %>% 
  select(-c(lifeExp, pop, continent)) %>% 
  spread(year, gdpPercap) %>% 
  mutate(gdp_reduced = `2007` - `1952`) %>% 
  mutate(gdp_reduced_cat = if_else(gdp_reduced < 0, "Has Reduced Overall", "Has Increased Overall")) %>% 
  filter(gdp_reduced_cat == "Has Reduced Overall")
  
  

sPDF_gdp_world_drops <- joinCountryData2Map(gdp_world_drops,
                                            joinCode = "NAME",
                                            nameJoinColumn = "country")

jpeg("output/maps/world_OverallGDPDrops.jpg",
     width = 960,
     height = 480)



par(mai = c(0,0,0.2,0), xaxs="i", yaxs = "i")
mapCountryData(sPDF_gdp_world_drops, 
               nameColumnToPlot = "gdp_reduced_cat", 
               catMethod="categorical", 
               numCats = 2,
               addLegend =F,
               mapTitle = "World Countries with an Overall GDP Drop"
)

country_coord <- data.frame(coordinates(sPDF_gdp_world_drops),stringsAsFactors=F)
country_coord_F <- country_coord[row.names(country_coord) %in% sPDF_gdp_world_drops$country,]

text(x = country_coord_F$X1, y = country_coord_F$X2, 
     labels = row.names(country_coord_F))

dev.off()













