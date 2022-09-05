## Introduction ----
## This script has all the functions used in global.R

## prepare packages ----
## install and load a package if it does not exist
require_package <- function(package_tmp) {
  if(!require(package_tmp, character.only = TRUE)){
    install.packages(package_tmp)
  }
  library(package_tmp, character.only = TRUE)
}


## data cleaning functino ----
## generic data cleaning function
clean_df <- function(df_tmp) {
  df_tmp %>% 
    clean_names() %>% 
    mutate(across(where(is.character), tolower)) %>% 
    mutate(across(where(is.factor), tolower)) %>% 
    mutate(across(everything(), ~ifelse(.=="", NA, .)))
}

## World map plot
get_pisa_plot <- function(tmp_pisa){
  
  tmp_spdf_sf <- world_spdf_sf %>% 
    inner_join(tmp_pisa, by = "iso3")
  
  pal_fun <- colorQuantile("YlOrRd", NULL, n = 5)
  
  p_popup <- paste0("<strong>PISA Score in Math in  </strong>", tmp_year, 
                    "=  </strong>",
                    round(tmp_spdf_sf$score, 1))
  
  leaflet(tmp_spdf_sf) %>%
    addTiles() %>% 
    setView( lat=10, lng=0 , zoom=2) %>% 
    addPolygons(
      stroke = FALSE, # remove polygon borders
      fillColor = ~pal_fun(score), # set fill color with function from above and value
      fillOpacity = 0.8, smoothFactor = 0.5, # make it nicer
      popup = p_popup)   # add popup

}

## Trend plot

get_trend_plot <- function(tmp_df, n_ctr){
  
  if(n_ctr==1){
    
    ggplot(tmp_df, aes(x = year, y = score, group = 1)) +
      geom_point() +
      geom_line() +
      facet_wrap(~ subject) +
      theme_bw() }
  else{
    ggplot(tmp_df, aes(x = year, y = score, group = name, color = name)) +
      geom_point() +
      geom_line() +
      facet_wrap(~ subject) +
      theme_bw() 
    
  }
    
  
  
}

