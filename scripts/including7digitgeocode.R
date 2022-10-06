not <- read.csv("health_indicators/alldiseases_inc_vert_final.csv", encoding = "latin1")
names(not)

pop <- read.csv("socioeconomic_indicators/population.csv")
geoc67 <- pop[, 2:3] 

library(tidyverse)
not1 <- not %>%
  left_join(geoc67, by = c("geocode" = "geocode")) %>%
  select(-geocode) %>% 
  rename(geocode = geocode7) 
head(not1)

not <- not1[, c(1:3, 9, 4:8)]
head(not)
write.csv(not, file = "health_indicators/alldiseases_inc_vert_final.csv", row.names = FALSE, fileEncoding = "latin1")

popf <- read.csv("socioeconomic_indicators/pop_BLA_final_version.csv", encoding = "latin1")
popf1 <- popf %>%
      left_join(geoc67, by = c("geocode" = "geocode")) %>%
      select(-geocode) %>% 
      rename(geocode = geocode7) 
head(popf1)
popf <- popf1[, c(1:3,20,4:19)]
head(popf)
write.csv(popf, file = "socioeconomic_indicators/pop_BLA_final_version.csv", 
          row.names = FALSE, fileEncoding = "latin1")

