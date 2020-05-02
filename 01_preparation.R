library(tidyverse)
getwd()

#GET DATA####
#lac
lac <- read.csv("data/01_LAC_Covid-19_survey_ES_2020_04_21_06_26_27_349795.csv", na.strings = "n/a", skip = 1)
# na
na <- read.csv("data/01_NA_MMC_NA_Covid19_20200404_03_2020_04_21_06_25_07_298280.csv", na.strings = "n/a", skip = 1) 

#inspect####
dim(lac) #242 526
dim(na) #454 393
View(lac)
View(na)

#SELECT VARS####
#lac
lac2 <- lac %>% 
  select(c(1:3, 15, 16, 27, 28,
           83:87, 88:100, 103, 104, 106:111, 113, 114, 115:123, 125, 126,
           328:340, 343, 344, 346:351, 353, 354, 356:362, 364, 365, 367:374, 376,
           378:381, 383, 385, 386, 387, 388:397, 400, 401, 403:411, 413, 415:419, 422, 424,
           425:429, 431, 432, 434, 435:445, 447, 449:455, 457, 458, 460, 461:471, 473,
           475:481, 483, 484, 486, 488, 489, 521,
           20, 34, 490)) %>%
  mutate(Region = as.factor("Latin America"))
dim(lac2) #242 183
View(lac2)

#na
na2 <- na %>% 
  select(c(1:3, 15, 16, 29, 30,
           88:92, 93:105, 107, 108, 110:115, 117, 118, 119:127, 129, 130,
           198:210, 212, 213, 215:220, 222, 223, 225:231, 233, 234, 236:243, 245,
           247:250, 252, 254, 255, 256, 257:266, 268, 269, 271:279, 281, 283:287, 289, 291,
           292:296, 298, 299, 301, 302:312, 314, 316:322, 324, 325, 327, 328:338, 340,
           342:348, 350, 351, 353, 355, 356, 388,
           21, 38, 357)) %>% #na has no 'none' for Q c6, c10, c18, c20
  mutate(Region = as.factor("North Africa"))
dim(na2) #454 183
View(na2)

#create df with colnames
variables_names  <- data.frame(lac = colnames(lac2), na = colnames(na2))
View(variables_names)
write.csv(variables_names, "data_outputs/01_variables_names_20200421_bis.csv")

#harmonize colnames (using na as reference)
colnames(lac2)[7]  <- "Q9..Monitor.observation..Sex.of.the.respondent."
colnames(lac2)[10]  <- "Q32.How.far.do.you.agree.with.the.following.statement..I.am.worried.about.catching.coronavirus.and.its.impact.on.my.health."
colnames(lac2)[11]  <- "Q33.How.far.do.you.agree.with.the.following.statement..I.am.worried.about.transmitting.coronavirus."
colnames(lac2)[12]  <- "Q34.How.far.do.you.agree.with.the.following.statement..I.know.about.coronavirus.and.how.to.protect.myself.and.others."

#BIND####
data <- rbind(lac2, na2)
dim(data) #696 obs, 183 vars
View(data)

#ADD DURATION, discard 4 empty interviews####
data  <- data %>% 
  mutate(Duration = X_duration/60) %>% 
  select(-X_duration) %>% 
  filter(Duration > 7)
dim(data) #obs 692, vars 183

#final vars names
vars_names <- colnames(data)
View(vars_names)
write.csv(vars_names, "data_outputs/01_final_vars_names_20200421_bis.csv")

#CLEAN####


#cleaning to-do
#date
#colnames
#levels names
#add survey duration


#SAVE DF####
#rda
save(data, file = "rda/01_cleaned_data_20200421.rda") 
#csv
write.csv(data, "data_outputs/01_cleaned_data_20200421.csv")











