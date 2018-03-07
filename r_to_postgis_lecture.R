#demonstrating linking sf to PostGIS for GEOG 575
#goal is to have a simple query to PostGIS, model results, do some
#light mapping with ggplot, then see if it's possible to save residuals to db

if(!require(pacman)){install.packages("pacman"); library(pacman)}
p_load(tidycensus, tidyr, sf, broom, RPostgres, ggplot2, dplyr)

host <- "http://learn-pgsql.rc.pdx.edu/"
db <- "jamgreen"
user <- "jamgreen"
pw <- scan("batteries.pgpss", what = "")
  
con <- dbConnect(drv = RPostgres::Postgres(), dbname = 'jamgreen',
                 host = 'learn-pgsql.rc.pdx.edu',
                 port = 5432, password = pw, user = 'jamgreen')  

#some convenience functions

dbListTables(conn = con)
dbListFields(conn = con, name = "schools")

#going to look at schools in Multco. Let's do it the SQL way, the R (tidyverse) way, and hybrid

mult_school_pgsql <- st_read_db(conn = con, query = "SELECT c.fips as fips, c.pop10 as tot_pop2010,
c.white as nh_white, c.black as nh_black, c.asian as
                                nh_asian, c.hispanic as hispanic, a.school_id as school_id, 
                                a.school_name as school_name,
                                a.school_grade as school_grade, a.school_type as school_type, 
                                c.county as county,  c.geom
                                FROM (SELECT a.fips as fips,  b.school_id as school_id, 
                                b.name as school_name, b.grade as school_grade, 
                                b.type as school_type, b.county as county, a.geom
                                FROM blockgrp2010 a JOIN
                                schools b ON ST_Intersects(a.geom, b.geom)
                                WHERE b.county = 'Multnomah' AND  b.grade IS NOT NULL
                                ORDER BY a.fips) as a
                                RIGHT JOIN blockgrp2010 c ON
                                c.fips = a.fips
                                WHERE c.county = '051';")

#tidyverse approach----

mult_school_tidy <- st_read_db(conn = con, query = "SELECT a.fips as fips,  b.school_id as school_id, 
b.name as school_name, b.grade as school_grade, 
                                b.type as school_type, b.county as county, a.geom
                                FROM blockgrp2010 a JOIN
                                schools b ON ST_Intersects(a.geom, b.geom)
                                WHERE b.county = 'Multnomah' AND  b.grade IS NOT NULL
                                ORDER BY a.fips;") 

mult_block_grp <- st_read_db(conn = con, query = "SELECT fips, pop10 as total_pop,
white as nh_white, black as nh_black, asian as nh_asian, hispanic as hispanic, geom
                            FROM blockgrp2010
                             WHERE county = '051';")

mult_school_tidy <- right_join(data.frame(mult_school_tidy), mult_block_grp, by = "fips") 

mult_school_tidy <- mult_school_tidy %>% select(-geom.x) %>% 
  rename(geom = geom.y) %>% st_as_sf()

#let's get totals for schools and the like and join back to original table-----

school_count <- mult_school_pgsql %>% filter(!is.na(school_name)) %>% 
  group_by(fips, school_type) %>% summarise(school_count= n()) %>% 
  as.data.frame() %>% select(-geom)

school_count <- spread(school_count, key = school_type, value = school_count)

school_count[is.na(school_count)] <- 0

school_count <- school_count %>% 
  mutate(school_count = Public + Private)

mult_school_pgsql <- mult_school_pgsql %>% 
  left_join(school_count, by = "fips")

mult_school_pgsql <- mult_school_pgsql %>% select(1:6, 12:15) %>% 
  distinct(fips, .keep_all = TRUE)

mult_school_pgsql[is.na(mult_school_pgsql)] <- 0
#we have our basic table now let's set up the modeling table with some ancillary census data

key <- scan("census_key.pgpss", what = "")

#census_api_key(key, install = TRUE)

#v10 <- load_variables(year = 2015, dataset = "acs5")

housing_val <- get_acs(year = 2015, geography = "block group", variables = "B25077_001E",
                       state = "OR", county = "Multnomah", key = key, output = "wide")

housing_val <- housing_val %>% select(GEOID, med_hsg_val = B25077_001E)

mult_school_pgsql <- mult_school_pgsql %>% left_join(housing_val, by = c("fips" = "GEOID"))

mult_school_pgsql <- mult_school_pgsql %>% mutate(nh_white_share = nh_white/tot_pop2010,
                                                  nh_black_share = nh_black/tot_pop2010,
                                                  nh_asian_share = nh_asian/tot_pop2010,
                                                  hisp_share = hispanic/tot_pop2010)

#model it

m1 <- lm(med_hsg_val ~  nh_black_share + nh_asian_share + hisp_share, 
         data = mult_school_pgsql)
m2 <- lm(med_hsg_val ~  nh_black_share + nh_asian_share + 
           hisp_share + Public, data = mult_school_pgsql)
m3 <- lm(med_hsg_val ~  nh_black_share + nh_asian_share + 
           hisp_share + Public + Private, data = mult_school_pgsql)
m4 <- lm(med_hsg_val ~  nh_black_share + nh_asian_share + 
           hisp_share + school_count, data = mult_school_pgsql,
         na.action = na.exclude)

m4_tidy <- augment(m4, mult_school_pgsql)
m4_tidy <- st_as_sf(m4_tidy)
