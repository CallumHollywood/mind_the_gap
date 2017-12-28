


mungemelt <- function(){
  
  # inits a melted version of the gapminder data
  
  munge_melt <- gapminder::gapminder %>% 
    melt(id.vars = c("continent", "country", "year"))
  #assign("munge_melt", munge_melt, envir = .GlobalEnv)
  
  
}


meanfeatures <- function(){
  
  # inits a grouped by continent/mean version of the 
  # gapminder data
  
  mean_features <- gapminder::gapminder %>% 
    group_by(continent, year) %>% 
    summarise(mean_lifeExp = mean(lifeExp, na.rm = T),
              mean_pop = mean(pop, na.rm = T),
              mean_gdpPercap = mean(gdpPercap, na.rm = T))
  #assign("mean_features", mean_features, envir = .GlobalEnv)
  
}












