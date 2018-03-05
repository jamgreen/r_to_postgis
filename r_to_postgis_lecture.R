#demonstrating linking sf to PostGIS for GEOG 575
#goal is to have a simple query to PostGIS, model results, do some
#light mapping with ggplot, then see if it's possible to save residuals to db

if(!require(pacman)){install.packages("pacman"); library(pacman)}
p_load(sf, RPostgres, ggplot2, dplyr)

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
                                nh_asian, c.hispanic as hispanic, a.school_id as school_id, a.school_name as school_name,
                                a.school_grade as school_grade, a.school_type as school_type, c.county as county,  c.geom
                                FROM (SELECT a.fips as fips,  b.school_id as school_id, b.name as school_name, b.grade as school_grade, 
                                b.type as school_type, b.county as county, a.geom
                                FROM blockgrp2010 a JOIN
                                schools b ON ST_Intersects(a.geom, b.geom)
                                WHERE b.county = 'Multnomah' AND  b.grade IS NOT NULL
                                ORDER BY a.fips) as a
                                RIGHT JOIN blockgrp2010 c ON
                                c.fips = a.fips
                                WHERE c.county = '051';")

#tidyverse approach

mult_school_tidy <- st_read_db(conn = con, query = "SELECT a.fips as fips,  b.school_id as school_id, b.name as school_name, b.grade as school_grade, 
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


plot(mult_school_pgsql[3])