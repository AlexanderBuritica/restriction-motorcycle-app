
### Setup
cat("\f")
rm(list = ls())
setwd("~/Dropbox/Michael Weintraub/Restriccion Motos/")
listpackages <- c("tidyverse","rgeos","rgdal","maptools","geosphere","haven")
sapply(listpackages,require,character.only=TRUE)
options("scipen"=100, "digits"=4) # Forzar a R a no usar e+

### Functions
read_event = function(file_dta){
           df <- read_dta(file = paste0("document/results/",file_dta)) %>% 
                 .[.$meses==6,] %>% .[,c("coef","ci_lower","ci_upper","months","zone","city_restric","type_crime")]
           return(df)
}

read_effect = function(file_dta){
            df <- read_dta(file = paste0("document/results/",file_dta)) %>% 
                  .[.$meses==6,] %>% .[,c("coef","ci_lower","ci_upper","zone","city_restric","type_crime")]
            return(df)
}

### Load effect estimates
file_effect = list.files("document/results/") %>% .[grep("app",.)]
effect_df = lapply(file_effect, function(x) read_effect(file_dta = x)) %>% data.table::rbindlist(.,use.names = T,fill = T) %>% data.frame(stringsAsFactors = F)


### Load event study
file_event = list.files("document/results/") %>% .[grep("event study",.)]
event_df = lapply(file_event, function(x) read_event(file_dta = x)) %>% data.table::rbindlist(.,use.names = T,fill = T) %>% data.frame(stringsAsFactors = F)

### Save 
df_results <- list(effect_df,event_df)
save(df_results,file = 'github-app/data/df_results.rds')

