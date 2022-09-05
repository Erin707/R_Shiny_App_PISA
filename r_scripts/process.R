
## Set relevant parameters ----
years <- c(2009, 2012, 2015, 2018)

## Load data ----
## This only needs to be done once
# data_pisa <- load_student(year = "all") 
# saveRDS(data_pisa, file = paste0(rds_path, "pisa_data.RDS"))

## Load pisa data
data(countrycode)

data_pisa <- readRDS(file = paste0(rds_path, "pisa_data.RDS")) %>%
  filter(year %in% years) %>% 
  # mutate(year = as.numeric(year)) %>% 
  select(year, country,
         school_id, student_id, 
         math, read, science, stu_wgt, escs) %>% 
  left_join(countrycode, by = "country") %>% 
  clean_df

rm(countrycode)

## Prepare long data format
data_pisa_long <- data_pisa %>% 
  tidyr::pivot_longer(cols = c("math", "read", "science"),
                      names_to = "subject",
                      values_to = "score")



## Transform country codes to common codes ISO3
ctr_codes_pisa <- data_pisa_long %>% 
  distinct(country)

library(rworldmap)

data(countrySynonyms)
ctr_codes_iso3 <- countrySynonyms %>% 
  clean_df %>% 
  select(iso3, name1) %>% 
  filter(!is.na(iso3)) %>% 
  rename(country_name = name1)
rm(countrySynonyms)

matched_codes <- ctr_codes_pisa %>% 
  inner_join(ctr_codes_iso3, by = c("country" = "iso3")) %>% 
  mutate(ori_codes = country) %>% 
  rename(iso3 = country)

unmatched_codes <- ctr_codes_pisa %>% 
  anti_join(ctr_codes_iso3, by = c("country" = "iso3"))

unmatched_codes <- unmatched_codes %>% 
  mutate(iso3 = case_when(
    country == "qcn" ~ "chn",
    country == "qch" ~ "chn",
    country == "tap" ~ "twn",
    country == "ksv" ~ "kos",
    country == "qci" ~ "chn"
  )) 

unmatched_codes <- unmatched_codes %>% 
  inner_join(ctr_codes_iso3, by = "iso3") %>% 
  rename(ori_codes = country) %>% 
  select(iso3, country_name, ori_codes)

final_ctr_codes <- 
  rbind(matched_codes, unmatched_codes)

data_pisa_long <- data_pisa_long %>% 
  inner_join(final_ctr_codes %>% 
               select(iso3, ori_codes), 
             by = c("country" = "ori_codes"))



## Download the shapefile. 
# download.file("http://thematicmapping.org/downloads/TM_WORLD_BORDERS_SIMPL-0.3.zip" , 
#               destfile="data/world_shape_file.zip")

## Unzip this file. You can do it with R (as below), or clicking on the object you downloaded.
# system("unzip DATA/world_shape_file.zip")


# Read this shape file with the rgdal library. 
library(rgdal)
world_spdf <- readOGR( 
  dsn= paste0(data_path,"/world_shape_file/") , 
  layer="TM_WORLD_BORDERS_SIMPL-0.3",
  verbose=FALSE
)

world_spdf@data <- world_spdf@data %>% 
  clean_names() %>% 
  mutate(iso3 = tolower(iso3)) 

world_spdf_sf <- st_as_sf(world_spdf) %>% 
  clean_names %>% 
  mutate(iso3 = tolower(iso3))

data_pisa_result_long <- data_pisa_long %>% 
  group_by(year, iso3, subject) %>% 
  summarise(score = weighted.mean(score, stu_wgt, na.rm=TRUE))


## Calculate correlation between ESCS and scores
cor <- data_pisa_long %>% 
  group_by(year, country, subject) %>% 
  group_modify(~correlate(.x$escs, .x$score,
                          method = "pearson",
                          use = "pairwise.complete.obs",
                          quiet = TRUE)) %>% 
  rename(correlation = x) %>% 
  select(-term)

cor_wt <- data_pisa_long %>% 
  group_by(year, iso3, subject) %>% 
  group_modify(~as_tibble(
    cor.wt(data = cbind(.x$escs, .x$score),
           w = .x$stu_wgt)[["r"]][2, 1])) 
# rename(correlation = x) %>% 
# select(-term)



cor_wt <- cor_wt %>% 
  inner_join(data_pisa_result_long, by = c("year", "iso3", 'subject')) %>% 
  group_by(year, subject) %>% 
  mutate(rank = rank(-score, ties.method = "min"),
         score_group = ntile(score, n = 5)) 
