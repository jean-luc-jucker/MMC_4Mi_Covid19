library(tidyverse)
library(scales)
getwd()
load("rda/01_cleaned_data_20200421.rda")
dim(data)
View(data)


#profiles####
#sex
data %>% 
  group_by(Q2.What.country.are.you.in.right.now., Q9..Monitor.observation..Sex.of.the.respondent.) %>% 
  tally() %>% 
  mutate(Percent=n/sum(n)*100)
#age
data %>% 
  group_by(Q2.What.country.are.you.in.right.now.) %>% 
  summarise(Mean=mean(Q8.How.old.are.you.))
#camp
data %>% 
  group_by(Q13.Have.you.lived.in.a.camp.or.informal.settlement.in.the.past.six.months.) %>% 
  tally() %>% 
  mutate(Percent=round(n/sum(n)*100, digits=1))


#1 Have you heard of coronavirus?####
summary(data$Q30.Have.you.heard.of.coronavirus.COVID.19.)
#All have replied yes


#2 Have you seen people acting more cautiously####
summary(data$Q31.Have.you.seen.people.acting.more.cautiously...keeping.distance..wearing.gloves.and.masks.)

levels(data$Q31.Have.you.seen.people.acting.more.cautiously...keeping.distance..wearing.gloves.and.masks.)

data %>% 
  group_by(Q2.What.country.are.you.in.right.now.) %>% 
  count(Q31.Have.you.seen.people.acting.more.cautiously...keeping.distance..wearing.gloves.and.masks.) %>% 
  mutate(Percent = round(n/sum(n)*100, digits = 1))



#3 I am worried about catching coronavirus####
summary(data$Q32.How.far.do.you.agree.with.the.following.statement..I.am.worried.about.catching.coronavirus.and.its.impact.on.my.health.)

data$Q32.How.far.do.you.agree.with.the.following.statement..I.am.worried.about.catching.coronavirus.and.its.impact.on.my.health. <- factor(data$Q32.How.far.do.you.agree.with.the.following.statement..I.am.worried.about.catching.coronavirus.and.its.impact.on.my.health., levels = c("Strongly disagree", "Disagree", "Neither agree nor disagree", "Agree", "Strongly agree", "Refused"))

levels(data$Q32.How.far.do.you.agree.with.the.following.statement..I.am.worried.about.catching.coronavirus.and.its.impact.on.my.health.)

data %>% 
  group_by(Q2.What.country.are.you.in.right.now.) %>% 
  count(Q32.How.far.do.you.agree.with.the.following.statement..I.am.worried.about.catching.coronavirus.and.its.impact.on.my.health.) %>% 
  mutate(Percent = round(n/sum(n)*100, digits = 1))


#4 I am worried about transmitting coronavirus####
summary(data$Q33.How.far.do.you.agree.with.the.following.statement..I.am.worried.about.transmitting.coronavirus.)

data$Q33.How.far.do.you.agree.with.the.following.statement..I.am.worried.about.transmitting.coronavirus. <- factor(data$Q33.How.far.do.you.agree.with.the.following.statement..I.am.worried.about.transmitting.coronavirus., levels = c("Strongly disagree", "Disagree", "Neither agree nor disagree", "Agree", "Strongly agree", "Refused"))

levels(data$Q33.How.far.do.you.agree.with.the.following.statement..I.am.worried.about.transmitting.coronavirus.)

data %>% 
  group_by(Q2.What.country.are.you.in.right.now.) %>% 
  count(Q33.How.far.do.you.agree.with.the.following.statement..I.am.worried.about.transmitting.coronavirus.) %>% 
  mutate(Percent = round(n/sum(n)*100, digits = 1)) %>% print(n=21)


#5 I know about coronavirus and how to protect myself and others####
summary(data$Q34.How.far.do.you.agree.with.the.following.statement..I.know.about.coronavirus.and.how.to.protect.myself.and.others.)

data$Q34.How.far.do.you.agree.with.the.following.statement..I.know.about.coronavirus.and.how.to.protect.myself.and.others. <- factor(data$Q34.How.far.do.you.agree.with.the.following.statement..I.know.about.coronavirus.and.how.to.protect.myself.and.others., levels = c("Strongly disagree", "Disagree", "Neither agree nor disagree", "Agree", "Strongly agree", "Refused"))

levels(data$Q34.How.far.do.you.agree.with.the.following.statement..I.know.about.coronavirus.and.how.to.protect.myself.and.others.)

data %>% 
  group_by(Q2.What.country.are.you.in.right.now.) %>% 
  count(Q34.How.far.do.you.agree.with.the.following.statement..I.know.about.coronavirus.and.how.to.protect.myself.and.others.) %>% 
  mutate(Percent = round(n/sum(n)*100, digits = 1))


#6 If you have received information on coronavirus and how to protect yourself, who did you receive it from? (multi-select)####
#pivot
c6  <- data  %>% 
  select(179, 13:27) %>% 
  pivot_longer(cols = 2:16, names_to = "Options", values_to = "Answer") %>% 
  filter(Answer != "", !is.na(Answer)) %>% 
  group_by(Q2.What.country.are.you.in.right.now.) %>% 
  count(Answer) %>% 
  mutate(Col = round(n/185*100, digits = 1), Per = round(n/53*100, digits = 1), Lib = round(n/212*100, digits = 1), Tun = round(n/242*100, digits = 1))%>%
  print(n=49)


#7 Through what means did you receive the information (type of media)? (multi-select)####
c7  <- data  %>% 
  select(179, 28:35) %>% 
  pivot_longer(cols = 2:8, names_to = "Options", values_to = "Answer") %>% 
  filter(Answer != "", !is.na(Answer)) %>% 
  group_by(Q2.What.country.are.you.in.right.now.) %>% 
  count(Answer) %>% 
  mutate(Col = round(n/185*100, digits = 1), Per = round(n/53*100, digits = 1), Lib = round(n/212*100, digits = 1), Tun = round(n/242*100, digits = 1))%>%
  print(n=49)


#8  What kinds of social media? (multi-select)####
c8  <- data  %>% 
  select(179, 36:46) %>% 
  pivot_longer(cols = 2:11, names_to = "Options", values_to = "Answer") %>% 
  filter(Answer != "", !is.na(Answer)) %>% 
  group_by(Q2.What.country.are.you.in.right.now.) %>% 
  count(Answer) %>% 
  mutate(Col = round(n/185*100, digits = 1), Per = round(n/53*100, digits = 1), Lib = round(n/212*100, digits = 1), Tun = round(n/242*100, digits = 1))%>%
  print(n=49)

c8_bis  <- data  %>% 
  select(179, 36:46) %>% 
  pivot_longer(cols = 2:11, names_to = "Options", values_to = "Answer") %>% 
  filter(Answer != "", !is.na(Answer)) %>% 
  count(Answer) %>% 
  mutate(Percent = n/692*100)%>%
  print(n=49)


#9 Who do you think is a trustworthy source of information on coronavirus? (multi-select)####
c10  <- data  %>% 
  select(179, 47:61) %>% 
  pivot_longer(cols = 2:16, names_to = "Options", values_to = "Answer") %>% 
  filter(Answer != "", !is.na(Answer)) %>% 
  group_by(Q2.What.country.are.you.in.right.now.) %>% 
  count(Answer) %>% 
  mutate(Col = round(n/185*100, digits = 1), Per = round(n/53*100, digits = 1), Lib = round(n/212*100, digits = 1), Tun = round(n/242*100, digits = 1))%>%
  print(n=49)

c10_plot <- data  %>% 
  select(179, 47:61) %>% 
  pivot_longer(cols = 2:16, names_to = "Options", values_to = "Answer") %>% 
  filter(Answer != "", !is.na(Answer)) %>% 
  count(Q2.What.country.are.you.in.right.now., Answer) %>% 
  pivot_wider(values_from = 3, names_from = Answer) %>% 
  add_column(N_P = c(185, 53, 212, 242)) %>% 
  pivot_longer(cols=2:16, names_to = "Options", values_to = "Answer") %>% 
  mutate(Percent= round(Answer/N_P*100, digits = 1)) %>%
  filter(!is.na(Percent)) %>% 
  print(n=50)

plot2  <- c10_plot %>%
  mutate(Options = reorder(Options, Percent)) %>% 
  filter(Options != "I don't remember" & Options != "Refused") %>% 
  ggplot(aes(fill=Q2.What.country.are.you.in.right.now.))+
  geom_col(aes(x=Options, y=Percent), width = 0.5, position = "dodge")+
  ylab("Percent")+
  xlab("")+
  theme_bw()+
  scale_y_continuous(breaks = seq(0, 100, by = 5)) + 
  coord_flip()+
  theme(legend.title = element_blank())+
  theme(legend.position = c(0.80, 0.15), legend.direction = "vertical", legend.title = element_blank())
plot2

#FIGURE 2 Thickness solved####
plot2  <- c10_plot %>%
  mutate(Options = reorder(Options, Percent)) %>% 
  filter(Options != "I don't remember" & Options != "Refused") %>% 
  ggplot(aes(fill=Q2.What.country.are.you.in.right.now.))+
  geom_col(aes(x=Options, y=Percent), width = 0.6, position = position_dodge2(preserve = "single", padding = 0))+
  ylab("Percent")+
  xlab("")+
  theme_bw()+
  scale_y_continuous(breaks = seq(0, 100, by = 5)) + 
  coord_flip()+
  theme(legend.title = element_blank())+
  theme(legend.position = c(0.85, 0.10), legend.direction = "vertical", legend.title = element_blank())+
  scale_fill_discrete(guide = guide_legend(reverse = TRUE))
plot2

#Produce emf
# NOT RUN {
require(devEMF)
# }
# NOT RUN {
# open file "bar.emf" for graphics output
emf("plot2.emf")
# produce the desired graph(s)
plot(plot2)
dev.off() #turn off device and finalize file
# }


#10 Q40 What are the symptoms of coronavirus? (multi-select)####
c11  <- data  %>% 
  select(179, 62:69) %>% 
  pivot_longer(cols = 2:9, names_to = "Options", values_to = "Answer") %>% 
  filter(Answer != "", !is.na(Answer)) %>% 
  group_by(Q2.What.country.are.you.in.right.now.) %>% 
  count(Answer) %>% 
  mutate(Col = round(n/185*100, digits = 1), Per = round(n/53*100, digits = 1), Lib = round(n/212*100, digits = 1), Tun = round(n/242*100, digits = 1))%>%
  print(n=35)

#11 Q41 Which groups are most at risk from the disease? (multi-select)####
c12  <- data  %>% 
  select(179, 70:78) %>% 
  pivot_longer(cols = 2:9, names_to = "Options", values_to = "Answer") %>% 
  filter(Answer != "", !is.na(Answer)) %>% 
  group_by(Q2.What.country.are.you.in.right.now.) %>% 
  count(Answer) %>% 
  mutate(Col = round(n/185*100, digits = 1), Per = round(n/53*100, digits = 1), Lib = round(n/212*100, digits = 1), Tun = round(n/242*100, digits = 1))%>%
  print(n=35)
  
#12 Q42 What are you currently doing to protect yourself against coronavirus? (multi-select)####
c13  <- data  %>% 
  select(179, 79:87) %>% 
  pivot_longer(cols = 2:9, names_to = "Options", values_to = "Answer") %>% 
  filter(Answer != "", !is.na(Answer)) %>% 
  group_by(Q2.What.country.are.you.in.right.now.) %>% 
  count(Answer) %>% 
  mutate(Col = round(n/185*100, digits = 1), Per = round(n/53*100, digits = 1), Lib = round(n/212*100, digits = 1), Tun = round(n/242*100, digits = 1))%>%
  print(n=35)

#14 Q44 Have you been tested for coronavirus?####
summary(data$Q44.Have.you.been.tested.for.coronavirus.)

levels(data$Q44.Have.you.been.tested.for.coronavirus.)

data %>% 
  group_by(Q2.What.country.are.you.in.right.now.) %>% 
  count(Q44.Have.you.been.tested.for.coronavirus.) %>% 
  mutate(Percent = round(n/sum(n)*100, digits = 1))

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

#16 Q46 If you had coronavirus and needed healthcare, would you be able to access health services today?####

summary(data$Q46.If.you.had.coronavirus.symptoms.and.needed.healthcare..would.you.be.able.to.access.health.services.today.)

levels(data$Q46.If.you.had.coronavirus.symptoms.and.needed.healthcare..would.you.be.able.to.access.health.services.today.)

data %>% 
  group_by(Q2.What.country.are.you.in.right.now.) %>% 
  count(Q46.If.you.had.coronavirus.symptoms.and.needed.healthcare..would.you.be.able.to.access.health.services.today.) %>% 
  mutate(Percent = round(n/sum(n)*100, digits = 1))
c16

#17 Q47 What are the barriers to accessing health services? (multi-select)####

c18 <- data  %>% 
  select(179, 96:107) %>% 
pivot_longer(cols = 2:13, names_to = "Options", values_to = "Answer") %>% 
  filter(Answer != "", !is.na(Answer)) %>% 
  group_by(Q2.What.country.are.you.in.right.now.) %>% 
  count(Answer) %>% 
  mutate(Col = round(n/185*100, digits = 1), Per = round(n/53*100, digits = 1), Lib = round(n/212*100, digits = 1), Tun = round(n/242*100, digits = 1))%>%
  print(n=49)

#plot

c18_plot <- data  %>% 
  select(179, 96:107) %>% 
  pivot_longer(cols = 2:13, names_to = "Options", values_to = "Answer") %>% 
  filter(Answer != "", !is.na(Answer)) %>% 
  count(Q2.What.country.are.you.in.right.now., Answer) %>% 
  pivot_wider(values_from = 3, names_from = Answer) %>% 
  add_column(N_P = c(185, 53, 212, 242)) %>% 
  pivot_longer(cols=2:13, names_to = "Options", values_to = "Answer") %>% 
  mutate(Percent= round(Answer/N_P*100, digits = 1)) %>% 
  filter(!is.na(Percent)) %>% 
  print(n=50)

#FIGURE 3####
plot3  <- c18_plot %>%
  mutate(Options = reorder(Options, Percent)) %>% 
  mutate(Options = recode(Options, 
                          'Discrimination against foreigners limits access to services' = "Discrimination against foreigners",
                          'General insecurity and conflict prevent me from accessing healthcare' = "General insecurity",
                          'I am afraid of being reported to authorities, or arrest, or deportation' = "Afraid of being reported",
                          'I don\'t have the money to pay for health services' = "I don't have the money",
                          'I don\'t have the right or the legal documentation to access health services here' = "I don't have the right/documentation",
                          'The advice for testing and treating coronavirus is unclear here' = "The advice is unclear here",
                          'Services are overwhelmed and access is difficult for everyone' = "Services are overwhelmed",
                          'I don\'t know where to go for healthcare' = "I don't know where to go")) %>% 
  filter(Options != "I don't remember" & Options != "Refused") %>% 
  ggplot(aes(fill=Q2.What.country.are.you.in.right.now.))+
  geom_col(aes(x=Options, y=Percent), width = 0.6, position = position_dodge2(preserve = "single", padding = 0))+
  ylab("Percent")+
  xlab("")+
  theme_bw()+
  scale_y_continuous(breaks = seq(0, 100, by = 5)) + 
  coord_flip()+
  theme(legend.title = element_blank()) + 
  theme(legend.position = c(0.80, 0.15), legend.direction = "vertical", legend.title = element_blank()) +
  scale_fill_discrete(guide = guide_legend(reverse = TRUE))
plot3


# NOT RUN {
require(devEMF)
# }
# NOT RUN {
# open file "bar.emf" for graphics output
emf("plot3.emf")
# produce the desired graph(s)
plot(plot3)
dev.off() #turn off device and finalize file
# }


#18 Q48 What impact has the coronavirus crisis had on your migration journey? (multi-select)####
c19  <- data  %>% 
  select(179, 108:117) %>% 
  pivot_longer(cols = 2:11, names_to = "Options", values_to = "Answer") %>% 
  filter(Answer != "", !is.na(Answer)) %>% 
  group_by(Q2.What.country.are.you.in.right.now.) %>% 
  count(Answer) %>% 
  mutate(Col = round(n/185*100, digits = 1), Per = round(n/53*100, digits = 1), Lib = round(n/212*100, digits = 1), Tun = round(n/242*100, digits = 1))%>%
  print(n=49)

View(c19)
  
c19_plot <- data  %>% 
  select(179, 108:117) %>% 
  pivot_longer(cols = 2:11, names_to = "Options", values_to = "Answer") %>% 
  mutate(Answer = recode(Answer, 
                         'I\'ve been delayed because I was sick, or because I had to stop and take care of people who got sick' = "Delayed because I or other people were sick",
                         'I feel too afraid to move (to continue my journey or return)' = "I feel to afraid to move",
                         'Increased difficulty moving around inside countries'="Increased difficulty moving around",
                         'I was going to be resettled, but this is now delayed'="About to be resettled, but now delayed",
                         'Disembarked / deported back to previous country'="Deported back to previous country"
  )) %>% 
  filter(Answer != "", !is.na(Answer)) %>% 
  count(Q2.What.country.are.you.in.right.now., Answer) %>% 
  pivot_wider(values_from = 3, names_from = Answer) %>% 
  add_column(N_P = c(185, 53, 212, 242)) %>% 
  pivot_longer(cols=2:11, names_to = "Options", values_to = "Answer") %>% 
  mutate(Percent= round(Answer/N_P*100, digits = 1)) %>% 
  filter(!is.na(Percent)) %>% 
  print(n=50)

#FIGURE 5#### 
plot5 <- c19_plot %>%
  mutate(Options = reorder(Options, Percent)) %>% 
  filter(Options != "I don't remember" & Options != "Refused") %>% 
  ggplot(aes(fill=Q2.What.country.are.you.in.right.now.))+
  geom_col(aes(x=Options, y=Percent), width = 0.6, position = position_dodge2(preserve = "single", padding = 0))+
  ylab("Percent")+
  xlab("")+
  theme_bw()+
  scale_y_continuous(breaks = seq(0, 100, by = 5)) + 
  coord_flip()+
  theme(legend.title = element_blank())+
  theme(legend.position = c(0.80, 0.20), legend.direction = "vertical", legend.title = element_blank())+
  scale_fill_discrete(guide = guide_legend(reverse = TRUE))

plot5


# NOT RUN {
require(devEMF)
# }
# NOT RUN {
# open file "bar.emf" for graphics output
emf("plot5.emf")
# produce the desired graph(s)
plot(plot5)
dev.off() #turn off device and finalize file
# }



#19 Q49 What impact has the crisis had on your day-to-day life? (multi-select)####
c20  <- data  %>% 
  select(179, 118:123) %>% 
  pivot_longer(cols = 2:7, names_to = "Options", values_to = "Answer") %>% 
  filter(Answer != "", !is.na(Answer)) %>% 
  group_by(Q2.What.country.are.you.in.right.now.) %>% 
  count(Answer) %>% 
  mutate(Col = round(n/185*100, digits = 1), Per = round(n/53*100, digits = 1), Lib = round(n/212*100, digits = 1), Tun = round(n/242*100, digits = 1))%>%
  print(n=49)

c20_plot <- data  %>% 
  select(179, 118:123) %>% 
  pivot_longer(cols = 2:7, names_to = "Options", values_to = "Answer") %>% 
  filter(Answer != "", !is.na(Answer)) %>% 
  count(Q2.What.country.are.you.in.right.now., Answer) %>% 
  pivot_wider(values_from = 3, names_from = Answer) %>% 
  add_column(N_P = c(185, 53, 212, 242)) %>% 
  pivot_longer(cols=2:7, names_to = "Options", values_to = "Answer") %>% 
  mutate(Percent= round(Answer/N_P*100, digits = 1)) %>% 
  filter(!is.na(Percent)) %>% 
  print(n=50)

#FIGURE 4 #####
plot4 <- c20_plot %>%
  mutate(Options = reorder(Options, Percent)) %>% 
  filter(Options != "I don't remember" & Options != "Refused") %>% 
  ggplot(aes(fill=Q2.What.country.are.you.in.right.now.))+
  geom_col(aes(x=Options, y=Percent), width = 0.6, position = position_dodge2(preserve = "single", padding = 0))+
  ylab("Percent")+
  xlab("")+
  theme_bw()+
  scale_y_continuous(breaks = seq(0, 100, by = 5)) + 
  coord_flip()+
  theme(legend.title = element_blank())+
  theme(legend.position = c(0.80, 0.20), legend.direction = "vertical", legend.title = element_blank())+
  scale_fill_discrete(guide = guide_legend(reverse = TRUE))

plot4


# NOT RUN {
require(devEMF)
# }
# NOT RUN {
# open file "bar.emf" for graphics output
emf("plot4.emf")
# produce the desired graph(s)
plot(plot4)
dev.off() #turn off device and finalize file
# }


#20 Q50 Have you lost income due to coronavirus restrictions?####
c21 <- summary(data$Q50.Have.you.lost.income.due.to.coronavirus.restrictions.)

levels(data$Q50.Have.you.lost.income.due.to.coronavirus.restrictions.)

data %>% 
  group_by(Q2.What.country.are.you.in.right.now.) %>% 
  count(Q50.Have.you.lost.income.due.to.coronavirus.restrictions.) %>% 
  mutate(Percent = round(n/sum(n)*100, digits = 1))
c21
  

#22 Q52 Have you received additional assistance since the coronavirus crisis began?####
summary(data$Q52.Have.you.received.additional.assistance.since.the.coronavirus.crisis.began.)

levels(data$Q52.Have.you.received.additional.assistance.since.the.coronavirus.crisis.began.)

data %>% 
  group_by(Q2.What.country.are.you.in.right.now.) %>% 
  count(Q52.Have.you.received.additional.assistance.since.the.coronavirus.crisis.began.) %>% 
  mutate(Percent = round(n/sum(n)*100, digits = 1))


#23 Q53 What assistance was that? (multi-select) NOTE N IS 149, THE YES FROM ABOVE####
c24  <- data  %>% 
  select(133:144) %>% 
  pivot_longer(cols = 1:12, names_to = "Options", values_to = "Answer") %>% 
  filter(Answer != "", !is.na(Answer)) %>% 
  count(Answer) %>% 
  mutate(Col = round(n/149*100, digits = 1))%>%
  print(n=49)

View(c24)
  

#24 Q54 Who did you receive it from? (multi-select) N is 149####
c25  <- data  %>% 
  select(145:153) %>% 
  pivot_longer(cols = 1:9, names_to = "Options", values_to = "Answer") %>% 
  filter(Answer != "", !is.na(Answer)) %>% 
  count(Answer) %>% 
  mutate(Col = round(n/149*100, digits = 1))%>%
  print(n=49)

View(c25)



#25 Q55 Are you in need of extra help since the coronavirus outbreak began?####
summary(data$Q55.Are.you.in.need.of.extra.help.since.the.coronavirus.outbreak.began.)

levels(data$Q55.Are.you.in.need.of.extra.help.since.the.coronavirus.outbreak.began.)

data %>% 
  group_by(Q2.What.country.are.you.in.right.now.) %>% 
  count(Q55.Are.you.in.need.of.extra.help.since.the.coronavirus.outbreak.began.) %>% 
  mutate(Percent = round(n/sum(n)*100, digits = 1))
  
  
  
#26 Q56 What kind of extra help? (multi-select) N 598####
c27  <- data  %>% 
  select(155:166) %>% 
  pivot_longer(cols = 1:12, names_to = "Options", values_to = "Answer") %>% 
  filter(Answer != "", !is.na(Answer)) %>% 
  count(Answer) %>% 
  mutate(Col = round(n/598*100, digits = 1))%>%
  print(n=49)

View(c27)


#28 Q58 Have you changed your plans as a result of the coronavirus outbreak?####
summary(data$Q58.Have.you.changed.your.plans.as.a.result.of.the.coronavirus.outbreak.)

levels(data$Q58.Have.you.changed.your.plans.as.a.result.of.the.coronavirus.outbreak.)

data %>% 
  group_by(Q2.What.country.are.you.in.right.now.) %>% 
  count(Q58.Have.you.changed.your.plans.as.a.result.of.the.coronavirus.outbreak.) %>% 
  mutate(Percent = round(n/sum(n)*100, digits = 1)) %>% 
  print(n=26)


#FIGURE 1 29 Q60 Do you think you are able to practice the recommended 1.5 metre of distance between people?####
summary(data$Q60.Do.you.think.you.are.able.to.practice.the.recommended.1.5.metre.of.distance.between.people.in.the.place.where.you.live.)


data$Q60.Do.you.think.you.are.able.to.practice.the.recommended.1.5.metre.of.distance.between.people.in.the.place.where.you.live. <- factor(data$Q60.Do.you.think.you.are.able.to.practice.the.recommended.1.5.metre.of.distance.between.people.in.the.place.where.you.live., levels = c("Yes", "No", "DonÂ´t know", "Refused"))


levels(data$Q60.Do.you.think.you.are.able.to.practice.the.recommended.1.5.metre.of.distance.between.people.in.the.place.where.you.live.)

safe_distance <- data %>% 
  group_by(Q2.What.country.are.you.in.right.now.) %>% 
  count(Q60.Do.you.think.you.are.able.to.practice.the.recommended.1.5.metre.of.distance.between.people.in.the.place.where.you.live.) %>% 
  mutate(Percent = round(n/sum(n)*100, digits = 1))
safe_distance


#Plot1####
plot1 <- safe_distance %>% 
  mutate(Country = reorder(Q2.What.country.are.you.in.right.now., Percent)) %>% 
  ggplot(aes(fill=Country))+
  geom_col(aes(x = Q60.Do.you.think.you.are.able.to.practice.the.recommended.1.5.metre.of.distance.between.people.in.the.place.where.you.live., y=Percent), position = "dodge", width = 0.6)+
  xlab("")+
  theme_bw()+
  scale_y_continuous(breaks = seq(0, 100, by = 10))+
  theme(legend.position = c(0.70, 0.95), legend.direction = "horizontal", legend.title = element_blank())
plot1

#FIGURE 1####
#plot1, with thickness solved and refused removed####
plot1 <- safe_distance %>% 
  filter(Q60.Do.you.think.you.are.able.to.practice.the.recommended.1.5.metre.of.distance.between.people.in.the.place.where.you.live.!="Refused") %>% 
  mutate(Country = Q2.What.country.are.you.in.right.now.) %>% 
  mutate(Q60.Do.you.think.you.are.able.to.practice.the.recommended.1.5.metre.of.distance.between.people.in.the.place.where.you.live.=recode(Q60.Do.you.think.you.are.able.to.practice.the.recommended.1.5.metre.of.distance.between.people.in.the.place.where.you.live., 'Yes'="Yes", 'No'="No", 'DonÂ´t know' = "Don't know", 'Refused'="Refused")) %>% 
  mutate(Q60.Do.you.think.you.are.able.to.practice.the.recommended.1.5.metre.of.distance.between.people.in.the.place.where.you.live.=reorder(Q60.Do.you.think.you.are.able.to.practice.the.recommended.1.5.metre.of.distance.between.people.in.the.place.where.you.live., Percent)) %>% 
  ggplot(aes(fill=Country))+
  geom_col(aes(x = Q60.Do.you.think.you.are.able.to.practice.the.recommended.1.5.metre.of.distance.between.people.in.the.place.where.you.live., y=Percent), position = position_dodge2(preserve = "single", padding = 0), width = 0.6)+
  xlab("")+
  theme_bw()+
  scale_y_continuous(breaks = seq(0, 100, by = 10))+
  theme(legend.position = c(0.85, 0.10), legend.direction = "vertical", legend.title = element_blank())+
  coord_flip()+
  scale_fill_discrete(guide = guide_legend(reverse = TRUE))

plot1


#####same, stacked bars
stacked <- safe_distance %>% 
  mutate(Country = Q2.What.country.are.you.in.right.now.) %>% 
  mutate(Q60.Do.you.think.you.are.able.to.practice.the.recommended.1.5.metre.of.distance.between.people.in.the.place.where.you.live.=recode(Q60.Do.you.think.you.are.able.to.practice.the.recommended.1.5.metre.of.distance.between.people.in.the.place.where.you.live., 'Yes'="Yes", 'No'="No", 'DonÂ´t know' = "Don't know", 'Refused'="Refused")) %>% 
  mutate(Q60.Do.you.think.you.are.able.to.practice.the.recommended.1.5.metre.of.distance.between.people.in.the.place.where.you.live.=reorder(Q60.Do.you.think.you.are.able.to.practice.the.recommended.1.5.metre.of.distance.between.people.in.the.place.where.you.live., Percent)) %>% 
  ggplot(aes(fill=Q60.Do.you.think.you.are.able.to.practice.the.recommended.1.5.metre.of.distance.between.people.in.the.place.where.you.live.))+
  geom_col(aes(x = Country, y=Percent))+
  xlab("")+
  theme_bw()+
  scale_y_continuous(breaks = seq(0, 100, by = 10))+
  theme(legend.title = element_blank())

stacked



####SAVE PLOTS AS EMF

# NOT RUN {
require(devEMF)
# }
# NOT RUN {
# open file "bar.emf" for graphics output
emf("plot1_bis.emf")
# produce the desired graph(s)
plot(stacked)
dev.off() #turn off device and finalize file
# }


png("omg.png", 600, 600)
#pdf("omg.pdf", 7, 7)
plot(rnorm(10), rnorm(10))
dev.off()

###############
install.packages("devEMF")
library(devEMF)
library(devEMF)


emf(file = "trial.emf", emfPlus = FALSE)
plot(1:10, seq(10, 100, 10), type = "l", xlab = "Time", ylab = "Distance")
dev.off()



install.packages("tufte")
library(tufte)


#Where do you currently live?####
summary(data$Q61.Where.do.you.currently.live.)

levels(data$Q61.Where.do.you.currently.live.)

data %>% 
  group_by(Q2.What.country.are.you.in.right.now.) %>% 
  count(Q61.Where.do.you.currently.live.) %>% 
  mutate(Percent = round(n/sum(n)*100, digits = 1))

