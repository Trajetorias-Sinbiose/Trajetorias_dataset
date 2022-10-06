## population count per municipality 
##
library(tidyverse)
library(stringr)

# urban and rural counts are only available from the census 
# source: datasus

### census 2000
pop2000 <- read.csv("aux-data/population/pop-residente-mun-zona-2000.csv")
dim(pop2000)
head(pop2000)
pop2000$geocode <- as.numeric(str_sub(pop2000$Municipality, 1, 6))
pop2000$municipio <- str_sub(pop2000$Municipality, 8, str_length(pop2000$Municipality))
pop2000 <- pop2000 %>%
  mutate(Urbana = as.numeric(Urbana),
         Rural = as.numeric(Rural))

# format horizontal 
pop2000h <- pop2000[, c(5,6,2,3,4)] %>%
  rename(urb = Urbana,
         rur = Rural,
         tot = Total) %>%
  mutate(rur = ifelse(is.na(rur), 0, rur),
         urb = ifelse(is.na(urb), 0, urb)) %>%
  mutate(prop_urb = urb/tot,
         prop_rur = rur/tot,
         year = 2000)
head(pop2000h)
summary(pop2000h)
dim(pop2000h)

### census 2010
pop2010 <- read.csv("aux-data/population/pop-residente-mun-zona-2010.csv")
dim(pop2010)
head(pop2010)
pop2010$geocode <- as.numeric(str_sub(pop2010$Municipio, 1, 6))
pop2010$municipio <- str_sub(pop2010$Municipio, 8, str_length(pop2010$Municipio))
pop2010 <- pop2010 %>%
  mutate(Rural = as.numeric(Rural))

# format horizontal 
pop2010h <- pop2010[, c(5,6,2,3,4)] %>%
  rename(urb = Urbana,
         rur = Rural,
         tot = Total) %>%
  mutate(rur = ifelse(is.na(rur), 0, rur),
         urb = ifelse(is.na(urb), 0, urb)) %>%
  mutate(prop_urb = urb/tot,
         prop_rur = rur/tot,
         year = 2010)
head(pop2010h)
summary(pop2010h)
dim(pop2010h)

## merge

poph <- pop2000h %>%
  right_join(pop2010h[, c(1, 3:8)], by = "geocode", suffix = c("2000","2010"))

summary(poph)
plot(poph$prop_urb2000, poph$prop_urb2010)
hist(poph$prop_urb2000 - poph$prop_urb2010)


## estimated pop count, 2006 e 2017, from IBGE
pop2006 <- read.csv("aux-data/population/pop-estimada-2006-mun.csv")
pop2017 <- read.csv("aux-data/population/pop-estimada-2017-mun.csv")

pop2006$geocode <- as.numeric(str_sub(pop2006$Municipio, 1, 6))
pop2006$municipio <- str_sub(pop2006$Municipio, 8, 
                             str_length(pop2006$Municipio)) 

pop2006 <- pop2006[, 2:4] %>%
  rename(pop_estimated2006 = Populacao_estimada)

summary(pop2017)
head(pop2017)
pop2017$geocode <- as.numeric(str_sub(pop2017$Municipio, 1, 6))
pop2017$municipio <- str_sub(pop2017$Municipio, 8, 
                             str_length(pop2017$Municipio))

pop2017 <- pop2017[, 2:4] %>%
  rename(pop_estimated2017 = Populacaoestimada)

# merging
poph <- poph %>%
  right_join(pop2006[, 1:2], by = "geocode")

poph <- poph %>%
  right_join(pop2017[, 1:2], by = "geocode")

head(poph)
dim(poph)

## estimating urb and rural pop in 2006 and 2017
## using 2010's ratio urb-rur , mid-period.

pop <- poph %>%
  mutate(urb2006e = prop_urb2010 * pop_estimated2006,
         rur2006e = pop_estimated2006 - urb2006e,
         urb2017e = prop_urb2010 * pop_estimated2017,
         rur2017e = pop_estimated2017 - urb2017e)

head(pop)

summary(pop)

# filtering Amazonia legal
bla <- read.csv("aux-data/population/mun2017-AmLegal.csv")
bla$geocode <- as.numeric(str_sub(bla$Municipio, 1, 6))
bla$municipio <- str_sub(bla$Municipio, 8, 
                             str_length(bla$Municipio))
head(bla)
summary(bla)
bla$geocode7 <- sapply(bla$geocode, function(x) sevendigitgeocode(x))

popbla <- bla %>%
  left_join(pop, by = "geocode")

summary(popbla)
popbla[which(is.na(popbla$pop_estimated2006)),] # Mojui dos Campos

pop <- popbla[, c(1:2,4, 6:10, 12:16, 18:23)]
write.csv(pop, file = "pop_BLA.csv", row.names = FALSE)
names(pop)

