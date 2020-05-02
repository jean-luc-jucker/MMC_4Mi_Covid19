library(tidyverse)
library(scales)
getwd()
load("rda/00_cleaned_data_20200419.rda")
dim(data)
View(data)



#1 Have you heard of coronavirus?
#All participants replied yes. (n/a = 3). Bu it will be interesting as this changes over time. Perhaps a good candidate for a time series plot?

#2 Have you seen people acting more cautiously
data %>% 
  select(Q31.Have.you.seen.people.acting.more.cautiously...keeping.distance..wearing.gloves.and.masks.) %>% 
  filter(Q31.Have.you.seen.people.acting.more.cautiously...keeping.distance..wearing.gloves.and.masks. != "DonÂ´t know") %>% 
  ggplot(aes(x=Q31.Have.you.seen.people.acting.more.cautiously...keeping.distance..wearing.gloves.and.masks., fill=Q31.Have.you.seen.people.acting.more.cautiously...keeping.distance..wearing.gloves.and.masks.))+
  geom_bar(aes(y = (..count..)/sum(..count..)), width = 0.5)+
  scale_y_continuous(labels=scales::percent)+
  ylab("Percent")+
  xlab("")+
  theme_bw()+
  theme(legend.position = "none")
#Don't know = 11, n/a = 3

#3 I am worried about catching coronavirus
data$Q32.How.far.do.you.agree.with.the.following.statement..I.am.worried.about.catching.coronavirus.and.its.impact.on.my.health. <- factor(data$Q32.How.far.do.you.agree.with.the.following.statement..I.am.worried.about.catching.coronavirus.and.its.impact.on.my.health., levels = c("Strongly disagree", "Disagree", "Neither agree nor disagree", "Agree", "Strongly agree", "Refused"))

levels(data$Q32.How.far.do.you.agree.with.the.following.statement..I.am.worried.about.catching.coronavirus.and.its.impact.on.my.health.)

data %>% 
  select(Q32.How.far.do.you.agree.with.the.following.statement..I.am.worried.about.catching.coronavirus.and.its.impact.on.my.health.) %>% 
  filter(Q32.How.far.do.you.agree.with.the.following.statement..I.am.worried.about.catching.coronavirus.and.its.impact.on.my.health. != "Refused") %>% 
  ggplot(aes(x=Q32.How.far.do.you.agree.with.the.following.statement..I.am.worried.about.catching.coronavirus.and.its.impact.on.my.health., fill=Q32.How.far.do.you.agree.with.the.following.statement..I.am.worried.about.catching.coronavirus.and.its.impact.on.my.health.))+
  geom_bar(aes(y = (..count..)/sum(..count..)), width = 0.5)+
  scale_y_continuous(labels=scales::percent)+
  ylab("Percent")+
  xlab("")+
  theme_bw()+
  theme(legend.position = "none")
#Refused = 4, n/a = 3

#4 I am worried about transmitting coronavirus
data$Q33.How.far.do.you.agree.with.the.following.statement..I.am.worried.about.transmitting.coronavirus. <- factor(data$Q33.How.far.do.you.agree.with.the.following.statement..I.am.worried.about.transmitting.coronavirus., levels = c("Strongly disagree", "Disagree", "Neither agree nor disagree", "Agree", "Strongly agree", "Refused"))

levels(data$Q33.How.far.do.you.agree.with.the.following.statement..I.am.worried.about.transmitting.coronavirus.)

data %>% 
  select(Q33.How.far.do.you.agree.with.the.following.statement..I.am.worried.about.transmitting.coronavirus.) %>% 
  filter(Q33.How.far.do.you.agree.with.the.following.statement..I.am.worried.about.transmitting.coronavirus. != "Refused") %>% 
  ggplot(aes(x=Q33.How.far.do.you.agree.with.the.following.statement..I.am.worried.about.transmitting.coronavirus., fill=Q33.How.far.do.you.agree.with.the.following.statement..I.am.worried.about.transmitting.coronavirus.))+
  geom_bar(aes(y = (..count..)/sum(..count..)), width = 0.5)+
  scale_y_continuous(labels=scales::percent)+
  ylab("Percent")+
  xlab("")+
  theme_bw()+
  theme(legend.position = "none")
#Refused = 8, n/a = 3

#5 I know about coronavirus and how to protect myself and others
data$Q34.How.far.do.you.agree.with.the.following.statement..I.know.about.coronavirus.and.how.to.protect.myself.and.others. <- factor(data$Q34.How.far.do.you.agree.with.the.following.statement..I.know.about.coronavirus.and.how.to.protect.myself.and.others., levels = c("Strongly disagree", "Disagree", "Neither agree nor disagree", "Agree", "Strongly agree", "Refused"))

levels(data$Q34.How.far.do.you.agree.with.the.following.statement..I.know.about.coronavirus.and.how.to.protect.myself.and.others.)

data %>% 
  select(Q34.How.far.do.you.agree.with.the.following.statement..I.know.about.coronavirus.and.how.to.protect.myself.and.others.) %>% 
  filter(Q34.How.far.do.you.agree.with.the.following.statement..I.know.about.coronavirus.and.how.to.protect.myself.and.others. != "Refused") %>% 
  ggplot(aes(x=Q34.How.far.do.you.agree.with.the.following.statement..I.know.about.coronavirus.and.how.to.protect.myself.and.others., fill=Q34.How.far.do.you.agree.with.the.following.statement..I.know.about.coronavirus.and.how.to.protect.myself.and.others.))+
  geom_bar(aes(y = (..count..)/sum(..count..)), width = 0.5)+
  scale_y_continuous(labels=scales::percent)+
  ylab("Percent")+
  xlab("")+
  theme_bw()+
  theme(legend.position = "none")
#Refused = 2, n/a = 3

#6 If you have received information on coronavirus and how to protect yourself, who did you receive it from? (multi-select)
#pivot
c6  <- data  %>% 
  select(13:27) %>% 
  pivot_longer(cols = 1:15, names_to = "Options", values_to = "Answer")
View(c6)
#factorize
c6$Answer <- as.factor(c6$Answer)
#summarise
summary(c6$Answer)
#plot
c6 %>% 
  filter(Answer != "I don't remember" & Answer != "Refused" & !is.na(Answer) & Answer != "") %>% 
  group_by(Answer) %>% tally() %>% 
  mutate(Answer = reorder(Answer, n)) %>% 
  mutate(Percent = n/515*100) %>% 
  ggplot(aes(fill=Answer))+
  geom_col(aes(x=Answer, y=Percent), width = 0.5)+
  ylab("Percent")+
  xlab("")+
  theme_bw()+
  theme(legend.position = "none")+
  scale_y_continuous(breaks = seq(0, 100, by = 5))+
  coord_flip()


#7 Through what means did you receive the information (type of media)? (multi-select)
c7  <- data  %>% 
    select(28:35) %>% 
    pivot_longer(cols = 1:8, names_to = "Options", values_to = "Answer")
View(c7)
#factorize
c7$Answer <- as.factor(c7$Answer)
#summarise
summary(c7$Answer)
#plot
c7 %>% 
    filter(Answer != "I don't remember" & Answer != "Refused" & !is.na(Answer) & Answer != "") %>% 
    group_by(Answer) %>% tally() %>% 
    mutate(Answer = reorder(Answer, n)) %>% 
    mutate(Percent = n/515*100) %>% 
    ggplot(aes(fill=Answer))+
    geom_col(aes(x=Answer, y=Percent), width = 0.5)+
    ylab("Percent")+
    xlab("")+
    theme_bw()+
    theme(legend.position = "none")+
    scale_y_continuous(breaks = seq(0, 100, by = 5))+
    coord_flip()

#8  What kinds of social media? (multi-select)
c8  <- data  %>% 
  select(36:46) %>% 
  pivot_longer(cols = 1:11, names_to = "Options", values_to = "Answer")
View(c8)
#factorize
c8$Answer <- as.factor(c8$Answer)
#summarise
summary(c8$Answer)
#plot
c8 %>% 
  filter(Answer != "Don't know" & Answer != "Refused" & !is.na(Answer) & Answer != "") %>% 
  group_by(Answer) %>% tally() %>% 
  mutate(Answer = reorder(Answer, n)) %>% 
  mutate(Percent = n/515*100) %>% #add filter on don't knows etc. here
  ggplot(aes(fill=Answer))+
  geom_col(aes(x=Answer, y=Percent), width = 0.5)+
  ylab("Percent")+
  xlab("")+
  theme_bw()+
  theme(legend.position = "none")+
  scale_y_continuous(breaks = seq(0, 100, by = 5))+
  coord_flip()

#9 Who do you think is a trustworthy source of information on coronavirus? (multi-select)
c10  <- data  %>% 
  select(47:61) %>% 
  pivot_longer(cols = 1:15, names_to = "Options", values_to = "Answer")
View(c10)
#factorize
c10$Answer <- as.factor(c10$Answer)
#summarise
summary(c10$Answer)
#plot
c10 %>% 
  filter(Answer != "I don't remember" & Answer != "Refused" & !is.na(Answer) & Answer != "") %>% 
  group_by(Answer) %>% tally() %>% 
  mutate(Answer = reorder(Answer, n)) %>% 
  mutate(Percent = n/515*100) %>% #add filter on don't knows etc. here
  ggplot(aes(fill=Answer))+
  geom_col(aes(x=Answer, y=Percent), width = 0.5)+
  ylab("Percent")+
  xlab("")+
  theme_bw()+
  theme(legend.position = "none")+
  scale_y_continuous(breaks = seq(0, 100, by = 5))+
  coord_flip()

#10 Q40 What are the symptoms of coronavirus? (multi-select)
c11  <- data  %>% 
  select(62:69) %>% 
  pivot_longer(cols = 1:8, names_to = "Options", values_to = "Answer")
View(c11)
#factorize
c11$Answer <- as.factor(c11$Answer)
#summarise
summary(c11$Answer)
#plot
c11 %>% 
  filter(Answer != "I don't remember" & Answer != "Refused" & !is.na(Answer) & Answer != "") %>% 
  group_by(Answer) %>% tally() %>% 
  mutate(Answer = reorder(Answer, n)) %>% 
  mutate(Percent = n/515*100) %>% #add filter on don't knows etc. here
  ggplot(aes(fill=Answer))+
  geom_col(aes(x=Answer, y=Percent), width = 0.5)+
  ylab("Percent")+
  xlab("")+
  theme_bw()+
  theme(legend.position = "none")+
  scale_y_continuous(breaks = seq(0, 100, by = 5))+
  coord_flip()

#11 Q41 Which groups are most at risk from the disease? (multi-select)
c12  <- data  %>% 
  select(70:78) %>% 
  pivot_longer(cols = 1:9, names_to = "Options", values_to = "Answer")
#factorize
c12$Answer <- as.factor(c12$Answer)
#summarise
summary(c12$Answer)
#plot
c12 %>% 
  filter(Answer != "I don't remember" & Answer != "Refused" & !is.na(Answer) & Answer != "") %>% 
  group_by(Answer) %>% tally() %>% 
  mutate(Answer = reorder(Answer, n)) %>% 
  mutate(Percent = n/515*100) %>% #add filter on don't knows etc. here
  ggplot(aes(fill=Answer))+
  geom_col(aes(x=Answer, y=Percent), width = 0.5)+
  ylab("Percent")+
  xlab("")+
  theme_bw()+
  theme(legend.position = "none")+
  scale_y_continuous(breaks = seq(0, 100, by = 5))+
  coord_flip()

#12 Q42 What are you currently doing to protect yourself against coronavirus? (multi-select)
c13  <- data  %>% 
  select(79:87) %>% 
  pivot_longer(cols = 1:9, names_to = "Options", values_to = "Answer")
#factorize
c13$Answer <- as.factor(c13$Answer)
#summarise
summary(c13$Answer)
#plot
c13 %>% 
  filter(Answer != "I don't remember" & Answer != "Refused" & !is.na(Answer) & Answer != "") %>% 
  group_by(Answer) %>% tally() %>% 
  mutate(Answer = reorder(Answer, n)) %>% 
  mutate(Percent = n/515*100) %>% #add filter on don't knows etc. here
  ggplot(aes(fill=Answer))+
  geom_col(aes(x=Answer, y=Percent), width = 0.5)+
  ylab("Percent")+
  xlab("")+
  theme_bw()+
  theme(legend.position = "none")+
  scale_y_continuous(breaks = seq(0, 100, by = 5))+
  coord_flip()

#13 Q43 If you aren't taking measures, what are the reasons? (multi-select) NOTE N IS ONLY 12
c14  <- data  %>% 
  select(88:92) %>% 
  pivot_longer(cols = 1:5, names_to = "Options", values_to = "Answer")
#factorize
c14$Answer <- as.factor(c14$Answer)
#summarise
summary(c14$Answer)
#plot
c14 %>% 
  filter(Answer != "I don't remember" & Answer != "Refused" & !is.na(Answer) & Answer != "") %>% 
  group_by(Answer) %>% tally() %>% 
  mutate(Answer = reorder(Answer, n)) %>% 
  mutate(Percent = n/12*100) %>% #add filter on don't knows etc. here
  ggplot(aes(fill=Answer))+
  geom_col(aes(x=Answer, y=Percent), width = 0.5)+
  ylab("Percent")+
  xlab("")+
  theme_bw()+
  theme(legend.position = "none")+
  scale_y_continuous(breaks = seq(0, 100, by = 5))+
  coord_flip()

#14 Q44 Have you been tested for coronavirus?
c15 <- data %>% 
  select(Q44.Have.you.been.tested.for.coronavirus.) %>% 
  filter(!is.na(Q44.Have.you.been.tested.for.coronavirus.)) %>% 
  ggplot(aes(x=Q44.Have.you.been.tested.for.coronavirus., fill=Q44.Have.you.been.tested.for.coronavirus.))+
  geom_bar(aes(y = (..count..)/sum(..count..)), width = 0.5)+
  scale_y_continuous(labels=scales::percent)+
  ylab("Percent")+
  xlab("")+
  theme_bw()+
  theme(legend.position = "none")
c15
#5 yes

#15 Q45 Do you mind telling us the result of the test? NOTE N IS 5
c16 <- data %>% 
  select(Q45.Do.you.mind.telling.us.the.result.of.the.test..You.can.refuse.to.answer) %>% 
  filter(!is.na(Q45.Do.you.mind.telling.us.the.result.of.the.test..You.can.refuse.to.answer)) %>% 
  ggplot(aes(x=Q45.Do.you.mind.telling.us.the.result.of.the.test..You.can.refuse.to.answer, fill=Q45.Do.you.mind.telling.us.the.result.of.the.test..You.can.refuse.to.answer))+
  geom_bar(aes(y = (..count..)/sum(..count..)), width = 0.5)+
  scale_y_continuous(labels=scales::percent)+
  ylab("Percent")+
  xlab("")+
  theme_bw()+
  theme(legend.position = "none")
c16
#5 negative, 1 refused

#16 Q46 If you had coronavirus and needed healthcare, would you be able to access health services today?
c17 <- data %>% 
  select(Q46.If.you.had.coronavirus.symptoms.and.needed.healthcare..would.you.be.able.to.access.health.services.today.) %>% 
  filter(!is.na(Q46.If.you.had.coronavirus.symptoms.and.needed.healthcare..would.you.be.able.to.access.health.services.today.)) %>% 
  ggplot(aes(x=Q46.If.you.had.coronavirus.symptoms.and.needed.healthcare..would.you.be.able.to.access.health.services.today., fill=Q46.If.you.had.coronavirus.symptoms.and.needed.healthcare..would.you.be.able.to.access.health.services.today.))+
  geom_bar(aes(y = (..count..)/sum(..count..)), width = 0.5)+
  scale_y_continuous(labels=scales::percent)+
  ylab("Percent")+
  xlab("")+
  theme_bw()+
  theme(legend.position = "none")
c17

#17 Q47 What are the barriers to accessing health services? (multi-select)
c18  <- data  %>% 
  select(96:107) %>% 
  pivot_longer(cols = 1:12, names_to = "Options", values_to = "Answer") %>% 
  mutate(Answer = recode(Answer, 
                          'Discrimination against foreigners limits access to services' = "Discrimination against foreigners",
                         'General insecurity and conflict prevent me from accessing healthcare' = "General insecurity",
                         'I am afraid of being reported to authorities, or arrest, or deportation' = "Afraid of being reported",
                         'I don\'t have the money to pay for health services' = "I don't have the money",
                         'I don\'t have the right or the legal documentation to access health services here' = "I don't have the right/documentation",
                         'The advice for testing and treating coronavirus is unclear here' = "The advice is unclear here",
                         'Services are overwhelmed and access is difficult for everyone' = "Services are overwhelmed",
                         'I don\'t know where to go for healthcare' = "I don't know where to go"))

#factorize
c18$Answer <- as.factor(c18$Answer)
#summarise
summary(c18$Answer)
#plot
c18 %>% 
  filter(Answer != "I don't remember" & Answer != "Refused" & !is.na(Answer) & Answer != "") %>% 
  group_by(Answer) %>% tally() %>% 
  mutate(Answer = reorder(Answer, n)) %>% 
  mutate(Percent = n/515*100) %>% #add filter on don't knows etc. here
  ggplot(aes(fill=Answer))+
  geom_col(aes(x=Answer, y=Percent), width = 0.5)+
  ylab("Percent")+
  xlab("")+
  theme_bw()+
  theme(legend.position = "none")+
  scale_y_continuous(breaks = seq(0, 100, by = 5))+
  coord_flip()

#18 Q48 What impact has the coronavirus crisis had on your migration journey? (multi-select)
c19  <- data  %>% 
  select(108:117) %>% 
  pivot_longer(cols = 1:10, names_to = "Options", values_to = "Answer") %>% 
  mutate(Answer = recode(Answer, 
                         'I\'ve been delayed because I was sick, or because I had to stop and take care of people who got sick' = "Delayed because I or other people were sick",
                         'I feel too afraid to move (to continue my journey or return)' = "I feel to afraid to move",
                         'Increased difficulty moving around inside countries'="Increased difficulty moving around",
                         'I was going to be resettled, but this is now delayed'="About to be resettled, but now delayed",
                         'Disembarked / deported back to previous country'="Deported back to previous country"
                        ))
#factorize
c19$Answer <- as.factor(c19$Answer)
#summarise
summary(c19$Answer)
#plot
c19 %>% 
  filter(Answer != "I don't remember" & Answer != "Refused" & !is.na(Answer) & Answer != "") %>% 
  group_by(Answer) %>% tally() %>% 
  mutate(Answer = reorder(Answer, n)) %>% 
  mutate(Percent = n/515*100) %>% #add filter on don't knows etc. here
  ggplot(aes(fill=Answer))+
  geom_col(aes(x=Answer, y=Percent), width = 0.5)+
  ylab("Percent")+
  xlab("")+
  theme_bw()+
  theme(legend.position = "none")+
  scale_y_continuous(breaks = seq(0, 100, by = 5))+
  coord_flip()

#19 Q49 What impacts has the crisis had on your day-to-day life? (multi-select)
c20  <- data  %>% 
  select(118:123) %>% 
  pivot_longer(cols = 1:6, names_to = "Options", values_to = "Answer")
#factorize
c20$Answer <- as.factor(c20$Answer)
#summarise
summary(c20$Answer)
#plot
c20 %>% 
  filter(Answer != "I don't remember" & Answer != "Refused" & !is.na(Answer) & Answer != "") %>% 
  group_by(Answer) %>% tally() %>% 
  mutate(Answer = reorder(Answer, n)) %>% 
  mutate(Percent = n/515*100) %>% #add filter on don't knows etc. here
  ggplot(aes(fill=Answer))+
  geom_col(aes(x=Answer, y=Percent), width = 0.5)+
  ylab("Percent")+
  xlab("")+
  theme_bw()+
  theme(legend.position = "none")+
  scale_y_continuous(breaks = seq(0, 100, by = 5))+
  coord_flip()

#20 Q50 Have you lost income due to coronavirus restrictions?
c21 <- data %>% 
  select(Q50.Have.you.lost.income.due.to.coronavirus.restrictions.) %>% 
  filter(!is.na(Q50.Have.you.lost.income.due.to.coronavirus.restrictions.)) %>% 
  mutate(Q50.Have.you.lost.income.due.to.coronavirus.restrictions. = recode(Q50.Have.you.lost.income.due.to.coronavirus.restrictions., 'No I have continued to work despite Covid restrictions' = "No I have continued to work")) %>% 
  ggplot(aes(x=Q50.Have.you.lost.income.due.to.coronavirus.restrictions., fill=Q50.Have.you.lost.income.due.to.coronavirus.restrictions.))+
  geom_bar(aes(y = (..count..)/sum(..count..)), width = 0.5)+
  scale_y_continuous(labels=scales::percent)+
  ylab("Percent")+
  xlab("")+
  theme_bw()+
  theme(legend.position = "none")
c21


#21 Q51 What impact has the loss of income had? (multi-select) NOTE N IS 345, and that the NONE is inconsistent
c22  <- data  %>% 
  select(125:131) %>% 
  pivot_longer(cols = 1:7, names_to = "Options", values_to = "Answer")
#factorize
c22$Answer <- as.factor(c22$Answer)
#summarise
summary(c22$Answer)
#plot
c22 %>% 
  filter(Answer != "I don't remember" & Answer != "Refused" & !is.na(Answer) & Answer != "") %>% 
  group_by(Answer) %>% tally() %>% 
  mutate(Answer = reorder(Answer, n)) %>% 
  mutate(Percent = n/345*100) %>% #add filter on don't knows etc. here
  ggplot(aes(fill=Answer))+
  geom_col(aes(x=Answer, y=Percent), width = 0.5)+
  ylab("Percent")+
  xlab("")+
  theme_bw()+
  theme(legend.position = "none")+
  scale_y_continuous(breaks = seq(0, 100, by = 5))+
  coord_flip()

#22 Q52 Have you received additional assistance since the coronavirus crisis began?
c23 <- data %>% 
  select(Q52.Have.you.received.additional.assistance.since.the.coronavirus.crisis.began.) %>% 
  filter(!is.na(Q52.Have.you.received.additional.assistance.since.the.coronavirus.crisis.began.)) %>% 
  ggplot(aes(x=Q52.Have.you.received.additional.assistance.since.the.coronavirus.crisis.began., fill=Q52.Have.you.received.additional.assistance.since.the.coronavirus.crisis.began.))+
  geom_bar(aes(y = (..count..)/sum(..count..)), width = 0.5)+
  scale_y_continuous(labels=scales::percent)+
  ylab("Percent")+
  xlab("")+
  theme_bw()+
  theme(legend.position = "none")
c23


#23 Q53 What assistance was that? (multi-select) NOTE N IS 111, THE YES FROM ABOVE
c24  <- data  %>% 
  select(133:144) %>% 
  pivot_longer(cols = 1:12, names_to = "Options", values_to = "Answer") %>% 
  mutate(Answer = recode(Answer, 
                         'Information about the virus: symptoms/ what to do if I have symptoms/ how to protect myself' = "Information about the virus",
                         'Distribution of sanitary items (sanitizer/ mask/ gloves/ etc)' = "Distribution of sanitary items", 'Other basic needs: food, water, shelter'="Food, water, shelter"))
#factorize
c24$Answer <- as.factor(c24$Answer)
#summarise
summary(c24$Answer)
#plot
c24 %>% 
  filter(Answer != "I don't remember" & Answer != "Refused" & !is.na(Answer) & Answer != "") %>% 
  group_by(Answer) %>% tally() %>% 
  mutate(Answer = reorder(Answer, n)) %>% 
  mutate(Percent = n/111*100) %>% #add filter on don't knows etc. here
  ggplot(aes(fill=Answer))+
  geom_col(aes(x=Answer, y=Percent), width = 0.5)+
  ylab("Percent")+
  xlab("")+
  theme_bw()+
  theme(legend.position = "none")+
  scale_y_continuous(breaks = seq(0, 100, by = 5))+
  coord_flip()

#24 Q54 Who did you receive it from? (multi-select) N is 111
c25  <- data  %>% 
  select(145:153) %>% 
  pivot_longer(cols = 1:9, names_to = "Options", values_to = "Answer") %>% 
  mutate(Answer = recode(Answer, 'Local population/community organisations' = "Local population", 'The government of the country I was/am in' = "Government of country I was in", 'My country of nationality\'s consulate'="Country of nationality consulate"))
#factorize
c25$Answer <- as.factor(c25$Answer)
#summarise
summary(c25$Answer)
#plot
c25 %>% 
  filter(Answer != "I don't remember" & Answer != "Refused" & !is.na(Answer) & Answer != "") %>% 
  group_by(Answer) %>% tally() %>% 
  mutate(Answer = reorder(Answer, n)) %>% 
  mutate(Percent = n/111*100) %>% #add filter on don't knows etc. here
  ggplot(aes(fill=Answer))+
  geom_col(aes(x=Answer, y=Percent), width = 0.5)+
  ylab("Percent")+
  xlab("")+
  theme_bw()+
  theme(legend.position = "none")+
  scale_y_continuous(breaks = seq(0, 100, by = 5))+
  coord_flip()

#25 Q55 Are you in need of extra help since the coronavirus outbreak began?
c26 <- data %>% 
  select(Q55.Are.you.in.need.of.extra.help.since.the.coronavirus.outbreak.began.) %>% 
  filter(!is.na(Q55.Are.you.in.need.of.extra.help.since.the.coronavirus.outbreak.began.)) %>% 
  ggplot(aes(x=Q55.Are.you.in.need.of.extra.help.since.the.coronavirus.outbreak.began., fill=Q55.Are.you.in.need.of.extra.help.since.the.coronavirus.outbreak.began.))+
  geom_bar(aes(y = (..count..)/sum(..count..)), width = 0.5)+
  scale_y_continuous(labels=scales::percent)+
  ylab("Percent")+
  xlab("")+
  theme_bw()+
  theme(legend.position = "none")
c26

#26 Q56 What kind of extra help? (m,ulti-select) N 441
c27  <- data  %>% 
  select(155:166) %>% 
  pivot_longer(cols = 1:12, names_to = "Options", values_to = "Answer") %>% 
  mutate(Answer = recode(Answer, 
                         'Information about the virus: symptoms/ what to do if I have symptoms/ how to protect myself' = "Information about the virus",
                         'Distribution of sanitary items (sanitizer/ mask/ gloves/ etc)' = "Distribution of sanitary items", 'Other basic needs: food, water, shelter'="Food, water, shelter"))
#factorize
c27$Answer <- as.factor(c27$Answer)
#summarise
summary(c27$Answer)
#plot
c27 %>% 
  filter(Answer != "I don't remember" & Answer != "Refused" & !is.na(Answer) & Answer != "") %>% 
  group_by(Answer) %>% tally() %>% 
  mutate(Answer = reorder(Answer, n)) %>% 
  mutate(Percent = n/441*100) %>% #add filter on don't knows etc. here
  ggplot(aes(fill=Answer))+
  geom_col(aes(x=Answer, y=Percent), width = 0.5)+
  ylab("Percent")+
  xlab("")+
  theme_bw()+
  theme(legend.position = "none")+
  scale_y_continuous(breaks = seq(0, 100, by = 5))+
  coord_flip()

# 27 Q57 If you needed extra help, who would you ask for support? (multi-select)
c28  <- data  %>% 
  select(167:175) %>% 
  pivot_longer(cols = 1:9, names_to = "Options", values_to = "Answer") %>% 
  mutate(Answer = recode(Answer, 'Local population/community organisations' = "Local population", 'The government of the country I was/am in' = "Government of country I am in", 'My country of nationality\'s consulate'="Country of nationality consulate"))
#factorize
c28$Answer <- as.factor(c28$Answer)
#summarise
summary(c28$Answer)
#plot
c28 %>% 
  filter(Answer != "I don't remember" & Answer != "Refused" & !is.na(Answer) & Answer != "") %>% 
  group_by(Answer) %>% tally() %>% 
  mutate(Answer = reorder(Answer, n)) %>% 
  mutate(Percent = n/515*100) %>% #add filter on don't knows etc. here
  ggplot(aes(fill=Answer))+
  geom_col(aes(x=Answer, y=Percent), width = 0.5)+
  ylab("Percent")+
  xlab("")+
  theme_bw()+
  theme(legend.position = "none")+
  scale_y_continuous(breaks = seq(0, 100, by = 5))+
  coord_flip()

#28 Q58 Have you changed your plans as a result of the coronavirus outbreak?
c29 <- data %>% 
  select(Q58.Have.you.changed.your.plans.as.a.result.of.the.coronavirus.outbreak.) %>% 
  mutate(Answer=Q58.Have.you.changed.your.plans.as.a.result.of.the.coronavirus.outbreak.) %>%
  filter(!is.na(Answer)) %>% 
  group_by(Answer) %>% tally() %>% 
  mutate(Answer = recode(Answer, 'Yes I have changed my planned routes but my intended destination remains the same' = "Yes I have changed my planned routes", 'Yes I have stopped here for a time because I am stuck' = "Yes I have stopped here for a time", 'Yes I have changed my intended destination'="Yes I have changed my destination")) %>% 
  mutate(Answer = reorder(Answer, n)) %>% 
  mutate(Percent = n/515*100) %>% 
  ggplot(aes(fill=Answer))+
  geom_col(aes(x=Answer, y=Percent), width = 0.5)+
  ylab("Percent")+
  xlab("")+
  theme_bw()+
  theme(legend.position = "none")+
  scale_y_continuous(breaks = seq(0, 100, by = 5))+
  coord_flip()
c29

#29 Q60 Do you think you are able to practice the recommended 1.5 metre of distance between people?
c32 <- data %>% 
  select(Q60.Do.you.think.you.are.able.to.practice.the.recommended.1.5.metre.of.distance.between.people.in.the.place.where.you.live.) %>% 
  filter(!is.na(Q60.Do.you.think.you.are.able.to.practice.the.recommended.1.5.metre.of.distance.between.people.in.the.place.where.you.live.)) %>% 
  ggplot(aes(x=Q60.Do.you.think.you.are.able.to.practice.the.recommended.1.5.metre.of.distance.between.people.in.the.place.where.you.live., fill=Q60.Do.you.think.you.are.able.to.practice.the.recommended.1.5.metre.of.distance.between.people.in.the.place.where.you.live.))+
  geom_bar(aes(y = (..count..)/sum(..count..)), width = 0.5)+
  scale_y_continuous(labels=scales::percent)+
  ylab("Percent")+
  xlab("")+
  theme_bw()+
  theme(legend.position = "none")
c32

#duration
#filter
duration <- data %>% 
  filter(Duration<=90)
dim(duration)
#stats
summary(duration$Duration)
avg <- mean(duration$Duration)
avg
md <- median(duration$Duration)
md
sd  <- sd(duration$Duration)
sd 
avg-sd

duration %>% 
  ggplot(aes(x=Duration, y = ..count..))+
  geom_density()+
  geom_vline(xintercept = 25.94, color="red")+
  xlab("Interview duration in minutes") +
  ylab("Number of interviews") +
  scale_x_continuous(breaks = seq(0, 90, by = 10))+
  scale_y_continuous(breaks = seq(0, 40, by = 5))+
  theme_bw()+
  theme(legend.position = c(0.9, 0.85), legend.direction = "vertical")




summary(data$Q8.How.old.are.you.)
summary(data$Q9..Monitor.observation..Sex.of.the.respondent.)




#NOTES####
#example "Yes I have changed my planned routes but my intended destination remains the same"







  