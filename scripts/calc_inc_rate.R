# Calculating 5 years disease incidence rate  

library(tidyverse)

# population data ----
# require execution of the "estimate_population_count.R" script
pop <- read.csv("pop_BLA.csv")

# chagas data ----
ch <- read.csv("../aux-data/vector-borne/Vertical/chagas_bd_vert.csv")
head(ch)
ch <- ch %>%
  replace_na(list(cases = 0)) 
head(ch)

d <- ch %>%
  left_join(pop, by = "geocode") 
head(d)

d <- within(d,{  # calculate incidence by period and zone (person-years)
  inc <- NA
  inc[period == "2004-2008" & zone == "rural"] <- 
    cases[period == "2004-2008" & zone == "rural"]/
    (rur2006e[period == "2004-2008" & zone == "rural"]*5)*100000 

  inc[period == "2004-2008" & zone == "urban"] <- 
    cases[period == "2004-2008" & zone == "urban"]/
    (urb2006e[period == "2004-2008" & zone == "urban"]*5)*100000 
  
  inc[period == "2004-2008" & zone == "total"] <- 
    cases[period == "2004-2008" & zone == "total"]/
    (pop_estimated2006[period == "2004-2008" & zone == "total"]*5)*100000 

  inc[period == "2015-2019" & zone == "rural"] <- 
    cases[period == "2015-2019" & zone == "rural"]/
    (rur2006e[period == "2015-2019" & zone == "rural"]*5)*100000 
  
  inc[period == "2015-2019" & zone == "urban"] <- 
    cases[period == "2015-2019" & zone == "urban"]/
    (urb2006e[period == "2015-2019" & zone == "urban"]*5)*100000  
  
  inc[period == "2015-2019" & zone == "total"] <- 
    cases[period == "2015-2019" & zone == "total"]/
    (pop_estimated2017[period == "2015-2019" & zone == "total"]*5)*100000  
    })

d <- d %>% 
  select(geocode,municipio,uf,disease,period,zone,cases,inc)
summary(d)

par(mfrow = c(2,1))
boxplot(inc ~ period + zone, data = d, las = 1, ylim = c(0,20), main = "Chagas", ylab = "cases per 100.000 per year")
boxplot(cases ~ period + zone, data = d, las = 1, ylim = c(0,20), main = "Chagas ", ylab = "cases")

rm(d, ch)

# LCA ----

lca <- read.csv("../aux-data/vector-borne/Vertical/CL_bd_vert.csv")
head(lca)
lca <- lca %>%
  replace_na(list(cases = 0)) %>%
  filter(geocode %in% pop$geocode)  # apenas mun em BLA

d <- lca %>%
  left_join(pop, by = "geocode") 
head(d)
summary(d)

#x <- which(!(lca$geocode %in% pop$geocode))
#lca[x,]

d <- within(d,{
  inc <- NA
  inc[period == "2004-2008" & zone == "rural"] <- 
    cases[period == "2004-2008" & zone == "rural"]/
    (rur2006e[period == "2004-2008" & zone == "rural"]*5)*100000 
  
  inc[period == "2004-2008" & zone == "urban"] <- 
    cases[period == "2004-2008" & zone == "urban"]/
    (urb2006e[period == "2004-2008" & zone == "urban"]*5)*100000 
  
  inc[period == "2004-2008" & zone == "total"] <- 
    cases[period == "2004-2008" & zone == "total"]/
    (pop_estimated2006[period == "2004-2008" & zone == "total"]*5)*100000 
  
  inc[period == "2015-2019" & zone == "rural"] <- 
    cases[period == "2015-2019" & zone == "rural"]/
    (rur2006e[period == "2015-2019" & zone == "rural"]*5)*100000 
  
  inc[period == "2015-2019" & zone == "urban"] <- 
    cases[period == "2015-2019" & zone == "urban"]/
    (urb2006e[period == "2015-2019" & zone == "urban"]*5)*100000  
  
  inc[period == "2015-2019" & zone == "total"] <- 
    cases[period == "2015-2019" & zone == "total"]/
    (pop_estimated2017[period == "2015-2019" & zone == "total"]*5)*100000  
})

d <- d %>% 
  select(geocode,municipio,uf,disease,period,zone,cases,inc)
summary(d)

par(mfrow = c(2,1))
boxplot(inc ~ period + zone, data = d, las = 1, ylim = c(0,200), main = "LCA", ylab = "cases per 100.000 per year")
boxplot(cases ~ period + zone, data = d, las = 1, ylim = c(0,200), main = "LCA ", ylab = "cases")


rm(d, lca)

# LV ----

lv <- read.csv("../aux-data/vector-borne/Vertical/VL_bd_vert.csv")
head(lv)
lv <- lv %>%
  replace_na(list(cases = 0)) %>%
  filter(geocode %in% pop$geocode)  # apenas mun em BLA

d <- lv %>%
  left_join(pop, by = "geocode") 
head(d)
summary(d)

#x <- which(!(lca$geocode %in% pop$geocode))
#lca[x,]

d <- within(d,{
  inc <- NA
  inc[period == "2004-2008" & zone == "rural"] <- 
    cases[period == "2004-2008" & zone == "rural"]/
    (rur2006e[period == "2004-2008" & zone == "rural"]*5)*100000 
  
  inc[period == "2004-2008" & zone == "urban"] <- 
    cases[period == "2004-2008" & zone == "urban"]/
    (urb2006e[period == "2004-2008" & zone == "urban"]*5)*100000 
  
  inc[period == "2004-2008" & zone == "total"] <- 
    cases[period == "2004-2008" & zone == "total"]/
    (pop_estimated2006[period == "2004-2008" & zone == "total"]*5)*100000 
  
  inc[period == "2015-2019" & zone == "rural"] <- 
    cases[period == "2015-2019" & zone == "rural"]/
    (rur2006e[period == "2015-2019" & zone == "rural"]*5)*100000 
  
  inc[period == "2015-2019" & zone == "urban"] <- 
    cases[period == "2015-2019" & zone == "urban"]/
    (urb2006e[period == "2015-2019" & zone == "urban"]*5)*100000  
  
  inc[period == "2015-2019" & zone == "total"] <- 
    cases[period == "2015-2019" & zone == "total"]/
    (pop_estimated2017[period == "2015-2019" & zone == "total"]*5)*100000  
})

d <- d %>% 
  select(geocode,municipio,uf,disease,period,zone,cases,inc)
summary(d)

par(mfrow = c(2,1))
boxplot(inc ~ period + zone, data = d, las = 1, ylim = c(0,20), main = "LV", ylab = "cases per 100.000 per year")
boxplot(cases ~ period + zone, data = d, las = 1, ylim = c(0,20), main = "LV ", ylab = "cases")


# dengue ----
# rural-urban info only in the first years. We will keep only total counts 
de <- read.csv("../aux-data/vector-borne/Vertical/dengue_bd_vert.csv")
head(de)
de <- de %>%
  replace_na(list(cases = 0)) %>%
  filter(geocode %in% pop$geocode)  # apenas mun em BLA

d <- de %>%
  left_join(pop, by = "geocode") 
head(d)
summary(d)

#x <- which(!(lca$geocode %in% pop$geocode))
#lca[x,]

d <- within(d,{
  inc <- NA
  inc[period == "2004-2008" & zone == "rural"] <- 
    cases[period == "2004-2008" & zone == "rural"]/
    (rur2006e[period == "2004-2008" & zone == "rural"]*5)*100000 
  
  inc[period == "2004-2008" & zone == "urban"] <- 
    cases[period == "2004-2008" & zone == "urban"]/
    (urb2006e[period == "2004-2008" & zone == "urban"]*5)*100000 
  
  inc[period == "2004-2008" & zone == "total"] <- 
    cases[period == "2004-2008" & zone == "total"]/
    (pop_estimated2006[period == "2004-2008" & zone == "total"]*5)*100000 
  
  inc[period == "2015-2019" & zone == "rural"] <- 
    cases[period == "2015-2019" & zone == "rural"]/
    (rur2006e[period == "2015-2019" & zone == "rural"]*5)*100000 
  
  inc[period == "2015-2019" & zone == "urban"] <- 
    cases[period == "2015-2019" & zone == "urban"]/
    (urb2006e[period == "2015-2019" & zone == "urban"]*5)*100000  
  
  inc[period == "2015-2019" & zone == "total"] <- 
    cases[period == "2015-2019" & zone == "total"]/
    (pop_estimated2017[period == "2015-2019" & zone == "total"]*5)*100000  
})

d <- d %>% 
  filter(zone == "total") %>%
  select(geocode,municipio,uf,disease,period,zone,cases,inc)
summary(d)

par(mfrow = c(2,1))
boxplot(inc ~ period, data = d, las = 1, ylim = c(0,600), main = "dengue", ylab = "cases per 100.000 per year")
boxplot(cases ~ period, data = d, las = 1, ylim = c(0,400), main = "dengue", ylab = "cases")

rm(d, de)

# malaria ----
# 
ma <- read.csv("../aux-data/vector-borne/Vertical/malaria_bd_vert.csv")
head(ma)
ma <- ma %>%
  replace_na(list(cases = 0)) %>%
  filter(geocode %in% pop$geocode)  %>% # apenas mun em BLA
  mutate(zone = tolower(zone))

tapply(ma$cases, ma$zone, sum)

d <- ma[, c(1,3:7)] %>%
  left_join(pop, by = "geocode") 
head(d)

d$municipio <- str_sub(d$Municipio, 8, str_length(d$Municipio))
summary(d)

d <- within(d,{
  inc <- NA
  inc[period == "2004-2008" & zone == "rural"] <- 
    cases[period == "2004-2008" & zone == "rural"]/
    (rur2006e[period == "2004-2008" & zone == "rural"]*5)*100000 
  
  inc[period == "2004-2008" & zone == "urbana"] <- 
    cases[period == "2004-2008" & zone == "urbana"]/
    (urb2006e[period == "2004-2008" & zone == "urbana"]*5)*100000 
  
  inc[period == "2004-2008" & zone == "total"] <- 
    cases[period == "2004-2008" & zone == "total"]/
    (pop_estimated2006[period == "2004-2008" & zone == "total"]*5)*100000 
  
  inc[period == "2015-2019" & zone == "rural"] <- 
    cases[period == "2015-2019" & zone == "rural"]/
    (rur2006e[period == "2015-2019" & zone == "rural"]*5)*100000 
  
  inc[period == "2015-2019" & zone == "urbana"] <- 
    cases[period == "2015-2019" & zone == "urbana"]/
    (urb2006e[period == "2015-2019" & zone == "urbana"]*5)*100000  
  
  inc[period == "2015-2019" & zone == "total"] <- 
    cases[period == "2015-2019" & zone == "total"]/
    (pop_estimated2017[period == "2015-2019" & zone == "total"]*5)*100000  
})

d <- d %>% 
  select(geocode, municipio, uf, disease, period, zone, cases, inc)
summary(d)

d$zone <- as.factor(d$zone)
levels(d$zone)[levels(d$zone) == "urbana"] <- "urban"

par(mfrow = c(2,1))
boxplot(inc ~ period + zone, data = d, las = 1, ylim = c(0,600), main = "malaria", 
        ylab = "cases per 100.000 per year")
boxplot(cases ~ period + zone, data = d, las = 1, ylim = c(0,400), main = "malaria", ylab = "cases")

