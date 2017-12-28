

# setup
source("r_scripts/eda/eda_setup.R")
source("r_scripts/eda/data_generator.R")


# aim
# make some general continent observations, then
# eda on specific countries of (subjective) interest


# 1st
# ackowledge that life exectancy rising accross all conts
# notable feaure = Africa plateud just >50 from late 80's to 2000's

meanfeatures() %>% 
  ggplot(aes(year, mean_lifeExp, group = continent)) +
  geom_line() +
  ggtitle("Average Life Expectancy by Continent") +
  geom_text(data = meanfeatures() %>% filter(year == 1997),
            aes(label = continent)) +
  ylab("Avrage Age (Years)")
  

# 2nd
# drill into Africa and look at per country
# filter out countries that never took a dip in lifeExp

# here are the coutries where theres been a reduction in lifeExp

lifeexpdrops <- gapminder::gapminder %>% 
  filter(continent == "Africa") %>% 
  select(-c(continent, pop, gdpPercap)) %>% 
  spread(year, lifeExp)

indx <- colSums(apply(lifeexpdrops %>% select(-c(country)), 1L, diff) > 0L) == (ncol(lifeexpdrops%>% select(-c(country))) - 1L)
lifeexpdrops <-  lifeexpdrops[!indx,]$country

# use thse to filter africa
# facet for better visuals

gapminder::gapminder %>% 
  filter(continent == "Africa") %>% 
  filter(country %in% lifeexpdrops) %>% 
  select(-c(continent)) %>% 
  ggplot(aes(year, lifeExp, group = country)) +
  geom_line() +
  facet_wrap(~ country) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))
  

rm(indx, lifeexpdrops)


# handful choice of coutries that look interesting
# ito significant lifeExp drops =

africa_lifeexp_drop_countries <- c("Botswana", "Cameroon", "Central African Republic",
                                   "Congo, Dem. Rep.", "Congo. Rep", "Cote d'Ivoire",
                                   "Gabon", "Kenya", "Lesotho", "Liberia", "Malawi", "Mozambique",
                                   "Namibia", "Rwanda", "South Africa",
                                   "Swaziland", "Tanzania", "Uganda", "Zambia", "Zimbabwe")


# lets lok cloer at these countries

View(gapminder::gapminder %>% 
  filter(country %in% africa_lifeexp_drop_countries)
)

# Idea!!
# can i show these countries highlighted on a map
# make a simple indicator col and condition on that

























