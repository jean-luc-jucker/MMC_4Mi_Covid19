library(tidyverse)
library(readxl)
getwd()

#GET DATA####
#lac
lac <- read_excel("data/02_MMC_LAC_Covid19_20200424_105328.xlsx", na = "")
# na
na <- read_excel("data/02_MMC_NA_Covid19_20200404_03_2020_04_29_12_12_09_103844.xlsx", na = "")
#wa
wa <- read_excel("data/02_MMC_WA_Covid19_ValidatedData_20200420_20200426.xlsx", na = "")

#inspect####
dim(lac) #314obs 547vars
dim(na) #517obs 414vars
dim(wa) #345obs 424vars
View(lac)
View(na)
View(wa)

#SELECT VARS####

#lac####
lac2 <- lac %>% 
  filter(start != "start") %>% 
  select(c(`_id`, start, Q25, Q23, Q20, Q13_1c, #PROFILES AND COUNTRY
           c1, c2, c3, c4, c5, #covid overall  #AWARENESS AND RISK
           c11,	`c11/Sometimes there are no symptoms`,	`c11/Dry cough`,	`c11/Aches and pains`,	`c11/Fever`,	`c11/Difficulties breathing`,	`c11/Tiredness`,	`c11/Other (specify)`,	`c11/Don't know`,	`c11/Refused`, #symptoms
           c12,	`c12/Babies and children under five`,	`c12/Children and adolesents up to 15`,	`c12/Adults (18 years and over)`,	`c12/Older people (60 years and over)`,	`c12/People who are already ill with another condition`,	`c12/Pregnant women`,	`c12/Health workers`,	`c12/Other (specify)`,	`c12/Don't know`,	`c12/Refused`, #groups at risks
           c13,	`c13/Nothing`,	`c13/Washing my hands more regularly/using hand sanitizer`,	`c13/Not touching my face`,	`c13/Wearing a mask`,	`c13/Wearing gloves`,	`c13/Avoiding crowded spaces`,	`c13/Keeping a large physical distance from other people`,	`c13/Staying at home and isolating myself from others`,	`c13/Other (specify)`,	`c13/Refused`, #protection measures
           c14,	`c14/I do not feel it is necessary`,	`c14/I do not know what precautions to take`,	`c14/I do not have access to protective gear (mask, gloves, sanitizer)`,	`c14/I cannot practice physical distancing due to my living situation`,	`c14/Other (specify)`,	`c14/Refused`, #if not why
           c15, c31, c32, #tested, 1.5 distance, and accommodation
           c6, 	`c6/Friends/family in country of departure`,	`c6/Friends/family in another country`,	`c6/Travel agents`,	`c6/Smugglers`,	`c6/Online community/network`,	`c6/NGOs / UN`,	`c6/Foreign embassies / consulates`,	`c6/National government / authorities`,	`c6/Other migrants`,	`c6/Local people I met on my journey`,	`c6/Health professionals`,	`c6/Spiritual/religious leaders`,	`c6/Community leaders/mobilizers`,	`c6/Other (specify)`,	`c6/I don't remember`,	`c6/Refused`, #INFORMATION #provider
           c7,	`c7/Street advertising (billboards/ leafleting)`,	`c7/Radio/TV/newspapers`,	`c7/Websites (not social media/messaging)`,	`c7/Social media or messaging apps`,	`c7/In-person`,	`c7/Phone call`,	`c7/Other`,	`c7/I don't remember`,	`c7/Refused`, #channel
           c8,	`c8/Facebook`,	`c8/WhatsApp`,	`c8/Instagram`,	`c8/Viber`,	`c8/Twitter`,	`c8/Telegram`,	`c8/YouTube`,	`c8/Snapchat`,	`c8/imo`,	`c8/Other (specify)`,	`c8/Don't know`,	`c8/Refused`, #social media
           c10,	`c10/Friends/family in country of departure`,	`c10/Friends/family in another country`,	`c10/Travel agents`,	`c10/Smugglers`,	`c10/Online community/network`,	`c10/NGOs / UN`,	`c10/Foreign embassies / consulates`,	`c10/National government / authorities`,	`c10/Other migrants`,	`c10/Local people I met on my journey`,	`c10/Health professionals`,	`c10/Spiritual/religious leaders`,	`c10/Community leaders/mobilizers`,	`c10/Other (specify)`,	`c10/I don't remember`,	`c10/Refused`, #trustworthy
           c17,	c18,	`c18/I don't know where to go for healthcare`,	`c18/The advice for testing and treating coronavirus is unclear here`,	`c18/I don't have the money to pay for health services`,	`c18/I don't have the right or the legal documentation to access health services here`,	`c18/I am afraid of being reported to authorities, or arrest, or deportation`,	`c18/Discrimination against foreigners limits access to services`,	`c18/I don't speak the language`,	`c18/General insecurity and conflict prevent me from accessing healthcare`,	`c18/There are no health services here`,	`c18/Services are overwhelmed and access is difficult for everyone`,	`c18/Other (specify)`,	`c18/Don't know`,	`c18/Refused`,#HEALTHCARE
           c23, #ASSISTANCE #received additional
           c24,	`c24/Access to health services`,	`c24/Distribution of sanitary items (sanitizer/ mask/ gloves/ etc)`,	`c24/Cash`,	`c24/Cash to pay for health services`,	`c24/Documentation to access health services`,	`c24/Information about the virus: symptoms/ what to do if I have symptoms/ how to protect myself`,	`c24/Other basic needs: food, water, shelter`,	`c24/Support to return home`,	`c24/Access to work and livelihoods`,	`c24/Psychological assistance`,	`c24/Childcare`,	`c24/Other (specify)`,	`c24/Refused`, #what kind
           c25,	`c25/The government of the country I was/am in`,	`c25/My country of nationality's consulate`,	`c25/UN`,	`c25/NGOs`,	`c25/Local population/community organisations`,	`c25/Family/friends`,	`c25/Fellow migrants`,	`c25/Other (specify)`,	`c25/Don't know`,	`c25/Refused`, #asssitance provider
           c26, #in need of extra help
           c27,	`c27/Access to health services`,	`c27/Distribution of sanitary items (sanitizer/ mask/ gloves/ etc)`,	`c27/Cash`,	`c27/Cash to pay for health services`,	`c27/Documentation to access health services`,	`c27/Information about the virus: symptoms/ what to do if I have symptoms/ how to protect myself`,	`c27/Other basic needs: food, water, shelter`,	`c27/Support to return home`,	`c27/Access to work and livelihoods`,	`c27/Psychological assistance`,	`c27/Childcare`,	`c27/Other (specify)`,	`c27/Refused`, #what kind of extra help
           c20,	`c20/Increased racism and xenophobia`,	`c20/Reduced access to asylum application`,	`c20/Reduced availability of basic goods`,	`c20/Reduced access to work`,	`c20/I am more worried and stressed`,	`c20/Other (specify)`,	`c20/Refused`, #IMPACT OF LIFE #what impact
           c21, #lost income
           c19,	`c19/None`,	`c19/Increased difficulty crossing borders`,	`c19/Increased risk of detention and deportation`,	`c19/Reduced access to smugglers`,	`c19/Increased difficulty moving around inside countries`,	`c19/I was going to be resettled, but this is now delayed`,	`c19/Disembarked / deported back to previous country`,	`c19/I've been delayed because I was sick, or because I had to stop and take care of people who got sick`,	`c19/I feel too afraid to move (to continue my journey or return)`,	`c19/Other (specify)`,	`c19/Refused`, #IMPACT ON JOURNEY #type
           c29 #changed plans
)) %>%
  mutate(Region = as.factor("Latin America"), N_region = length(Region)) %>% 
  group_by(Q13_1c) %>%  mutate(N_country = length(Q13_1c)) %>% ungroup()
dim(lac2) #313 191
View(lac2)

#na####
na2 <- na %>% 
  filter(start != "start") %>% 
  select(c(`_id`, start, Q25, Q23, Q20, Q13_1c, #PROFILES AND COUNTRY
           c1, c2, c3, c4, c5, #covid overall  #AWARENESS AND RISK
           c11,	`c11/Sometimes there are no symptoms`,	`c11/Dry cough`,	`c11/Aches and pains`,	`c11/Fever`,	`c11/Difficulties breathing`,	`c11/Tiredness`,	`c11/Other (specify)`,	`c11/Don't know`,	`c11/Refused`, #symptoms
           c12,	`c12/Babies and children under five`,	`c12/Children and adolesents up to 15`,	`c12/Adults (18 years and over)`,	`c12/Older people (60 years and over)`,	`c12/People who are already ill with another condition`,	`c12/Pregnant women`,	`c12/Health workers`,	`c12/Other (specify)`,	`c12/Don't know`,	`c12/Refused`, #groups at risks
           c13,	`c13/Nothing`,	`c13/Washing my hands more regularly/using hand sanitizer`,	`c13/Not touching my face`,	`c13/Wearing a mask`,	`c13/Wearing gloves`,	`c13/Avoiding crowded spaces`,	`c13/Keeping a large physical distance from other people`,	`c13/Staying at home and isolating myself from others`,	`c13/Other (specify)`,	`c13/Refused`, #protection measures
           c14,	`c14/I do not feel it is necessary`,	`c14/I do not know what precautions to take`,	`c14/I do not have access to protective gear (mask, gloves, sanitizer)`,	`c14/I cannot practice physical distancing due to my living situation`,	`c14/Other (specify)`,	`c14/Refused`, #if not why
           c15, c31, c32, #tested, 1.5 distance, and accommodation
           c6, 	`c6/Friends/family in country of departure`,	`c6/Friends/family in another country`,	`c6/Travel agents`,	`c6/Smugglers`,	`c6/Online community/network`,	`c6/NGOs / UN`,	`c6/Foreign embassies / consulates`,	`c6/National government / authorities`,	`c6/Other migrants`,	`c6/Local people I met on my journey`,	`c6/Health professionals`,	`c6/Spiritual/religious leaders`,	`c6/Community leaders/mobilizers`,	`c6/Other (specify)`,	`c6/I don't remember`,	`c6/Refused`, #INFORMATION #provider
           c7,	`c7/Street advertising (billboards/ leafleting)`,	`c7/Radio/TV/newspapers`,	`c7/Websites (not social media/messaging)`,	`c7/Social media or messaging apps`,	`c7/In-person`,	`c7/Phone call`,	`c7/Other`,	`c7/I don't remember`,	`c7/Refused`, #channel
           c8,	`c8/Facebook`,	`c8/WhatsApp`,	`c8/Instagram`,	`c8/Viber`,	`c8/Twitter`,	`c8/Telegram`,	`c8/YouTube`,	`c8/Snapchat`,	`c8/imo`,	`c8/Other (specify)`,	`c8/Don't know`,	`c8/Refused`, #social media
           c10,	`c10/Friends/family in country of departure`,	`c10/Friends/family in another country`,	`c10/Travel agents`,	`c10/Smugglers`,	`c10/Online community/network`,	`c10/NGOs / UN`,	`c10/Foreign embassies / consulates`,	`c10/National government / authorities`,	`c10/Other migrants`,	`c10/Local people I met on my journey`,	`c10/Health professionals`,	`c10/Spiritual/religious leaders`,	`c10/Community leaders/mobilizers`,	`c10/Other (specify)`,	`c10/I don't remember`,	`c10/Refused`, #trustworthy
           c17,	c18,	`c18/I don't know where to go for healthcare`,	`c18/The advice for testing and treating coronavirus is unclear here`,	`c18/I don't have the money to pay for health services`,	`c18/I don't have the right or the legal documentation to access health services here`,	`c18/I am afraid of being reported to authorities, or arrest, or deportation`,	`c18/Discrimination against foreigners limits access to services`,	`c18/I don't speak the language`,	`c18/General insecurity and conflict prevent me from accessing healthcare`,	`c18/There are no health services here`,	`c18/Services are overwhelmed and access is difficult for everyone`,	`c18/Other (specify)`,	`c18/Don't know`,	`c18/Refused`,#HEALTHCARE
           c23, #ASSISTANCE #received additional
           c24,	`c24/Access to health services`,	`c24/Distribution of sanitary items (sanitizer/ mask/ gloves/ etc)`,	`c24/Cash`,	`c24/Cash to pay for health services`,	`c24/Documentation to access health services`,	`c24/Information about the virus: symptoms/ what to do if I have symptoms/ how to protect myself`,	`c24/Other basic needs: food, water, shelter`,	`c24/Support to return home`,	`c24/Access to work and livelihoods`,	`c24/Psychological assistance`,	`c24/Childcare`,	`c24/Other (specify)`,	`c24/Refused`, #what kind
           c25,	`c25/The government of the country I was/am in`,	`c25/My country of nationality's consulate`,	`c25/UN`,	`c25/NGOs`,	`c25/Local population/community organisations`,	`c25/Family/friends`,	`c25/Fellow migrants`,	`c25/Other (specify)`,	`c25/Don't know`,	`c25/Refused`, #asssitance provider
           c26, #in need of extra help
           c27,	`c27/Access to health services`,	`c27/Distribution of sanitary items (sanitizer/ mask/ gloves/ etc)`,	`c27/Cash`,	`c27/Cash to pay for health services`,	`c27/Documentation to access health services`,	`c27/Information about the virus: symptoms/ what to do if I have symptoms/ how to protect myself`,	`c27/Other basic needs: food, water, shelter`,	`c27/Support to return home`,	`c27/Access to work and livelihoods`,	`c27/Psychological assistance`,	`c27/Childcare`,	`c27/Other (specify)`,	`c27/Refused`, #what kind of extra help
           c20,	`c20/Increased racism and xenophobia`,	`c20/Reduced access to asylum application`,	`c20/Reduced availability of basic goods`,	`c20/Reduced access to work`,	`c20/I am more worried and stressed`,	`c20/Other (specify)`,	`c20/Refused`, #IMPACT OF LIFE #what impact
           c21, #lost income
           c19,	`c19/None`,	`c19/Increased difficulty crossing borders`,	`c19/Increased risk of detention and deportation`,	`c19/Reduced access to smugglers`,	`c19/Increased difficulty moving around inside countries`,	`c19/I was going to be resettled, but this is now delayed`,	`c19/Disembarked / deported back to previous country`,	`c19/I've been delayed because I was sick, or because I had to stop and take care of people who got sick`,	`c19/I feel too afraid to move (to continue my journey or return)`,	`c19/Other (specify)`,	`c19/Refused`, #IMPACT ON JOURNEY #type
           c29 #changed plans
  )) %>%
  mutate(Region = as.factor("North Africa"), N_region = length(Region)) %>% 
  group_by(Q13_1c) %>%  mutate(N_country = length(Q13_1c)) %>% ungroup()
dim(na2) #516 191
View(na2)

#wa####
wa2 <- wa %>% 
  filter(start != "start") %>% 
  select(c(`_id`, start, Q25, Q23, Q20, Q13_1c, #PROFILES AND COUNTRY
           c1, c2, c3, c4, c5, #covid overall  #AWARENESS AND RISK
           c11,	`c11/Sometimes there are no symptoms`,	`c11/Dry cough`,	`c11/Aches and pains`,	`c11/Fever`,	`c11/Difficulties breathing`,	`c11/Tiredness`,	`c11/Other (specify)`,	`c11/Don't know`,	`c11/Refused`, #symptoms
           c12,	`c12/Babies and children under five`,	`c12/Children and adolesents up to 15`,	`c12/Adults (18 years and over)`,	`c12/Older people (60 years and over)`,	`c12/People who are already ill with another condition`,	`c12/Pregnant women`,	`c12/Health workers`,	`c12/Other (specify)`,	`c12/Don't know`,	`c12/Refused`, #groups at risks
           c13,	`c13/Nothing`,	`c13/Washing my hands more regularly/using hand sanitizer`,	`c13/Not touching my face`,	`c13/Wearing a mask`,	`c13/Wearing gloves`,	`c13/Avoiding crowded spaces`,	`c13/Keeping a large physical distance from other people`,	`c13/Staying at home and isolating myself from others`,	`c13/Other (specify)`,	`c13/Refused`, #protection measures
           c14,	`c14/I do not feel it is necessary`,	`c14/I do not know what precautions to take`,	`c14/I do not have access to protective gear (mask, gloves, sanitizer)`,	`c14/I cannot practice physical distancing due to my living situation`,	`c14/Other (specify)`,	`c14/Refused`, #if not why
           c15, c31, c32, #tested, 1.5 distance, and accommodation
           c6, 	`c6/Friends/family in country of departure`,	`c6/Friends/family in another country`,	`c6/Travel agents`,	`c6/Smugglers`,	`c6/Online community/network`,	`c6/NGOs / UN`,	`c6/Foreign embassies / consulates`,	`c6/National government / authorities`,	`c6/Other migrants`,	`c6/Local people I met on my journey`,	`c6/Health professionals`,	`c6/Spiritual/religious leaders`,	`c6/Community leaders/mobilizers`,	`c6/Other (specify)`,	`c6/I don't remember`,	`c6/Refused`, #INFORMATION #provider
           c7,	`c7/Street advertising (billboards/ leafleting)`,	`c7/Radio/TV/newspapers`,	`c7/Websites (not social media/messaging)`,	`c7/Social media or messaging apps`,	`c7/In-person`,	`c7/Phone call`,	`c7/Other`,	`c7/I don't remember`,	`c7/Refused`, #channel
           c8,	`c8/Facebook`,	`c8/WhatsApp`,	`c8/Instagram`,	`c8/Viber`,	`c8/Twitter`,	`c8/Telegram`,	`c8/YouTube`,	`c8/Snapchat`,	`c8/imo`,	`c8/Other (specify)`,	`c8/Don't know`,	`c8/Refused`, #social media
           c10,	`c10/Friends/family in country of departure`,	`c10/Friends/family in another country`,	`c10/Travel agents`,	`c10/Smugglers`,	`c10/Online community/network`,	`c10/NGOs / UN`,	`c10/Foreign embassies / consulates`,	`c10/National government / authorities`,	`c10/Other migrants`,	`c10/Local people I met on my journey`,	`c10/Health professionals`,	`c10/Spiritual/religious leaders`,	`c10/Community leaders/mobilizers`,	`c10/Other (specify)`,	`c10/I don't remember`,	`c10/Refused`, #trustworthy
           c17,	c18,	`c18/I don't know where to go for healthcare`,	`c18/The advice for testing and treating coronavirus is unclear here`,	`c18/I don't have the money to pay for health services`,	`c18/I don't have the right or the legal documentation to access health services here`,	`c18/I am afraid of being reported to authorities, or arrest, or deportation`,	`c18/Discrimination against foreigners limits access to services`,	`c18/I don't speak the language`,	`c18/General insecurity and conflict prevent me from accessing healthcare`,	`c18/There are no health services here`,	`c18/Services are overwhelmed and access is difficult for everyone`,	`c18/Other (specify)`,	`c18/Don't know`,	`c18/Refused`,#HEALTHCARE
           c23, #ASSISTANCE #received additional
           c24,	`c24/Access to health services`,	`c24/Distribution of sanitary items (sanitizer/ mask/ gloves/ etc)`,	`c24/Cash`,	`c24/Cash to pay for health services`,	`c24/Documentation to access health services`,	`c24/Information about the virus: symptoms/ what to do if I have symptoms/ how to protect myself`,	`c24/Other basic needs: food, water, shelter`,	`c24/Support to return home`,	`c24/Access to work and livelihoods`,	`c24/Psychological assistance`,	`c24/Childcare`,	`c24/Other (specify)`,	`c24/Refused`, #what kind
           c25,	`c25/The government of the country I was/am in`,	`c25/My country of nationality's consulate`,	`c25/UN`,	`c25/NGOs`,	`c25/Local population/community organisations`,	`c25/Family/friends`,	`c25/Fellow migrants`,	`c25/Other (specify)`,	`c25/Don't know`,	`c25/Refused`, #asssitance provider
           c26, #in need of extra help
           c27,	`c27/Access to health services`,	`c27/Distribution of sanitary items (sanitizer/ mask/ gloves/ etc)`,	`c27/Cash`,	`c27/Cash to pay for health services`,	`c27/Documentation to access health services`,	`c27/Information about the virus: symptoms/ what to do if I have symptoms/ how to protect myself`,	`c27/Other basic needs: food, water, shelter`,	`c27/Support to return home`,	`c27/Access to work and livelihoods`,	`c27/Psychological assistance`,	`c27/Childcare`,	`c27/Other (specify)`,	`c27/Refused`, #what kind of extra help
           c20,	`c20/Increased racism and xenophobia`,	`c20/Reduced access to asylum application`,	`c20/Reduced availability of basic goods`,	`c20/Reduced access to work`,	`c20/I am more worried and stressed`,	`c20/Other (specify)`,	`c20/Refused`, #IMPACT OF LIFE #what impact
           c21, #lost income
           c19,	`c19/None`,	`c19/Increased difficulty crossing borders`,	`c19/Increased risk of detention and deportation`,	`c19/Reduced access to smugglers`,	`c19/Increased difficulty moving around inside countries`,	`c19/I was going to be resettled, but this is now delayed`,	`c19/Disembarked / deported back to previous country`,	`c19/I've been delayed because I was sick, or because I had to stop and take care of people who got sick`,	`c19/I feel too afraid to move (to continue my journey or return)`,	`c19/Other (specify)`,	`c19/Refused`, #IMPACT ON JOURNEY #type
           c29 #changed plans
  )) %>%
  mutate(Region = as.factor("West Africa"), N_region = length(Region)) %>% 
  group_by(Q13_1c) %>%  mutate(N_country = length(Q13_1c)) %>% ungroup()
dim(wa2) #344 191
View(wa2)

#GET COLNAMES####
variables_names  <- data.frame(lac = colnames(lac2), na = colnames(na2), wa = colnames(wa2))
View(variables_names)
write.csv(variables_names, "data_outputs/02_variables_names_20200430.csv")


#BIND####
data <- rbind(lac2, na2, wa2)
dim(data) #1173 obs, 191 vars
View(data)

#DEFINE VARIABLE TYPES####
str(data)
numerics <- c(4, 190:191)
data[numerics] <- lapply(data[numerics], as.numeric)
factors  <- c(1:3, 5:188)
data[factors] <- lapply(data[factors], as.factor)
str(data)

#SAVE####
#rda
save(data, file = "rda/02_cleaned_data_20200430.rda") 
#csv
write.csv(data, "data_outputs/02_cleaned_data_20200430_bis.csv")

 









