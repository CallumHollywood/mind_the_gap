
#========================
# Setup
#========================

packs <- c("tidyverse", "dplyr", "gapminder", "reshape2" )
lapply(packs, require, character.only = T)
options(scipen = 999)
plotsave = "output/graphs/"


#========================
# Get data
#========================

munge_data <- gapminder


#========================
# eda
#========================

head(munge_data)
tail(munge_data)


#========================
# plot lifeExp, pop and gdpPercap
# by coutry over time
#========================

ggplot(munge_data, aes(x = year, y = lifeExp, group = country)) +
  geom_line() +
  theme(legend.position = "none") +
  ggtitle("All Countries lifeExp")

ggsave(paste0(plotsave, "allcoutries_lifeExp.png"), units = "cm")


ggplot(munge_data, aes(year, pop, group = country)) +
  geom_line() +
  theme(legend.position = "none") +
  ggtitle("All Countries pop") +
  geom_text(data = munge_data[munge_data$year == 2007,], aes(label = country))

ggsave(paste(plotsave, "allcountries_pop.png"), units = "cm")


ggplot(munge_data, aes(year, gdpPercap, group = country)) +
  geom_line() +
  theme(legend.position = "none") +
  ggtitle("All Countries GDP Per Capita") +
  geom_text(data = munge_data[munge_data$year == 1962,], aes(label = country))

ggsave(paste0(plotsave, "allcountries_gdpPercap.png"), units = "cm")




#========================
# eda by continent
#========================

continent_view <- munge_data %>% 
  group_by(continent, year) %>% 
  summarise(mean_lifeExp = mean(lifeExp, na.rm = T),
            mean_pop = mean(pop, na.rm = T),
            mean_gdpPercap = mean(gdpPercap, na.rm = T))

# life Exp
continent_view %>% 
  select(-c(mean_pop, mean_gdpPercap)) %>% 
  ggplot(aes(year, mean_lifeExp, group = continent)) +
  geom_line() +
  ggtitle("Continent Mean lifeExp") +
  geom_text(data = continent_view %>% filter(year == 2002) ,
            aes(label = continent))
  
ggsave(paste0(plotsave, "ContinentMeanlifeExp.png"), units = "cm")

# pop
continent_view %>% 
  select(-c(mean_lifeExp, mean_gdpPercap)) %>% 
  ggplot(aes(year, mean_pop, group = continent)) +
  geom_line() +
  ggtitle("Continent Mean Pop") +
  geom_text(data = continent_view %>% filter(year == 2002),
            aes(label = continent))

ggsave(paste0(plotsave, "ContinentmeanPop.png"), units = "cm")


# gdpPercap
continent_view %>% 
  select(-c(mean_lifeExp, mean_pop)) %>% 
  ggplot(aes(year, mean_gdpPercap, group = continent)) +
  geom_line() +
  ggtitle("continent mean_gdpPercap") +
  geom_text(data = continent_view %>% filter(year == 2002),
            aes(label = continent))

ggsave(paste0(plotsave, "continentmeangdppercap.png"), units = "cm")



#========================
# cont by cont view
#========================

#-----------------------
# Africa
#-----------------------

africa <- munge_data %>% filter(continent == "Africa") %>% select(-c(continent))

africa_melt <- melt(africa, 
                    id.vars = c("country", "year"),
                    variable.name = "feature",
                    value.name = "value") 

africa_melt %>% 
  ggplot(aes(year, value, group = country)) +
  geom_line() +
  facet_wrap(~ feature, scales = "free_y") +
  geom_text(data = africa_melt %>% filter(year == 2002),
            aes(label = country)) +
  ggtitle("Africa")
  
  
ggsave(paste0(plotsave, "africa_view.png"), units = "cm")
  
  
#-----------------------
# Americas
#-----------------------  
  
americas <- munge_data %>% filter(continent == "Americas") %>%  select(-c(continent))

americas_melt <- melt(americas, 
                      id.vars = c("country", "year"),
                      variable.name = 'feature',
                      value.name = "value")

americas_melt %>% 
  ggplot(aes(year, value, group = country)) +
  geom_line() +
  facet_wrap(~feature, scales = 'free_y') +
  geom_text(data = americas_melt %>% filter(year == 1997),
            aes(label = country)) +
  ggtitle("Americas")


ggsave(paste0(plotsave, "america_view.png"), units = "cm")

#-----------------------
# Asia
#-----------------------  


asia <- munge_data %>% filter(continent == "Asia") %>% select(-c(continent))

asia_melt <- melt(asia, 
                  id.vars = c("country", "year"),
                  variable.name = "feature",
                  value.name = "value")

asia_melt %>% 
  ggplot(aes(year, value, group = country)) +
  geom_line() +
  facet_wrap(~ feature, scales = "free_y") +
  geom_text(data = asia_melt %>% filter(year == 1997),
            aes(label = country)) +
  ggtitle("Asia")

ggsave(paste0(plotsave, "Asia_view.png"), units = 'cm')


#-----------------------
# Europe
#-----------------------  

europe <- munge_data %>% filter(continent == "Europe") %>% select(-c(continent))

europe_melt <- melt(europe,
                    id.vars = c("country", "year"),
                    variable.name = "feature",
                    value.name = "value")


europe_melt %>% 
  ggplot(aes(year, value, group = country)) +
  geom_line() +
  facet_wrap(~ feature, scales = 'free_y') +
  geom_text(data = europe_melt %>% filter(year == 1997),
            aes(label = country)) +
  ggtitle("Europe")

ggsave(paste0(plotsave, "Europe.png"), units = "cm")




#-----------------------
# Oceania
#-----------------------  

oceania <- munge_data %>% filter(continent == "Oceania") %>% select(-c(continent))

oceania_melt <- melt(oceania,
                    id.vars = c("country", "year"),
                    variable.name = "feature",
                    value.name = "value")

oceania_melt %>% 
  ggplot(aes(year, value, group = country)) +
  geom_line() +
  facet_wrap(~ feature, scales = "free_y") +
  geom_text(data = oceania_melt %>% filter(year == 1997),
          aes(label = country)) +
  ggtitle("Oceania")

ggsave(paste0(plotsave, "Oceania.png"), units = "cm")


















