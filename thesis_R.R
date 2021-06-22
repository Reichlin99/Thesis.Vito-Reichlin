library(haven)
library(dplyr)
library(tidyr)

A2010 <- read_sav("C:/Users/vitor/OneDrive/Documenti/R/Thesis_data/Assets/Assets_2010.sav")
A2012 <- read_sav("C:/Users/vitor/OneDrive/Documenti/R/Thesis_data/Assets/Assets_2012.sav")
A2014 <- read_sav("C:/Users/vitor/OneDrive/Documenti/R/Thesis_data/Assets/Assets_2014.sav")
A2016 <- read_sav("C:/Users/vitor/OneDrive/Documenti/R/Thesis_data/Assets/Assets_2016.sav")
A2018 <- read_sav("C:/Users/vitor/OneDrive/Documenti/R/Thesis_data/Assets/Assets_2018.sav")
A2020 <- read_sav("C:/Users/vitor/OneDrive/Documenti/R/Thesis_data/Assets/Assets_2020.sav")


A2010.0 <- A2010%>% select(nomem_encr, ca10b006)%>% filter(ca10b006== 0)
A2010.1 <- A2010%>% select(nomem_encr, ca10b006)%>% filter(ca10b006== 1)
A2012.0 <- A2012%>% select(nomem_encr, ca12c006)%>% filter(ca12c006== 0)
A2012.1 <- A2012%>% select(nomem_encr, ca12c006)%>% filter(ca12c006== 1)
A2014.0 <- A2014%>% select(nomem_encr, ca14d006)%>% filter(ca14d006== 0)
A2014.1 <- A2014%>% select(nomem_encr, ca14d006)%>% filter(ca14d006== 1)
A2016.0 <- A2016%>% select(nomem_encr, ca16e006)%>% filter(ca16e006== 0)
A2016.1 <- A2016%>% select(nomem_encr, ca16e006)%>% filter(ca16e006== 1)
A2018.0 <- A2018%>% select(nomem_encr, ca18f006)%>% filter(ca18f006== 0)
A2018.1 <- A2018%>% select(nomem_encr, ca18f006)%>% filter(ca18f006== 1)
A2020.0 <- A2020%>% select(nomem_encr, ca20g006)%>% filter(ca20g006== 0)
A2020.1 <- A2020%>% select(nomem_encr, ca20g006)%>% filter(ca20g006== 1)


common1<- intersect(A2010.0$nomem_encr, A2012.0$nomem_encr)
common2<- intersect(common1, A2014.0$nomem_encr)
common3<- intersect(common2, A2016.0$nomem_encr)
common4<- intersect(common3, A2018.0$nomem_encr)
common5<- intersect(common4, A2020.0$nomem_encr)

delta1 <- intersect(common1, A2014.1$nomem_encr)
delta2 <- intersect(common2, A2016.1$nomem_encr)
delta3 <- intersect(common3, A2018.1$nomem_encr)
delta4 <- intersect(common4, A2020.1$nomem_encr)

#########

L2012 <- read_sav("C:/Users/vitor/OneDrive/Documenti/R/Thesis_data/Leisure/leisure_2012.sav")
L2014 <- read_sav("C:/Users/vitor/OneDrive/Documenti/R/Thesis_data/Leisure/leisure_2014.sav")
L2016 <- read_sav("C:/Users/vitor/OneDrive/Documenti/R/Thesis_data/Leisure/leisure_2016.sav")
L2018 <- read_sav("C:/Users/vitor/OneDrive/Documenti/R/Thesis_data/Leisure/leisure_2018.sav")
L2020 <- read_sav("C:/Users/vitor/OneDrive/Documenti/R/Thesis_data/Leisure/leisure_2020.sav")
#########
#variation population
delta1.0 <- intersect(delta1, L2012$nomem_encr)
delta1.1 <- intersect(delta1.0, L2014$nomem_encr)

delta2.0 <- intersect(delta2, L2014$nomem_encr)
delta2.1 <- intersect(delta2.0, L2016$nomem_encr)

delta3.0 <- intersect(delta3, L2016$nomem_encr)
delta3.1 <- intersect(delta3.0, L2018$nomem_encr)

delta4.0 <- intersect(delta4, L2018$nomem_encr)
delta4.1 <- intersect(delta4.0, L2020$nomem_encr)
########
#control population

control1.0 <- intersect(L2012$nomem_encr, L2014$nomem_encr)
control1 <- intersect(control1.0, common2)

control2.0 <- intersect(control1, L2016$nomem_encr)
control2 <- intersect(control2.0, common3)

control3.0 <- intersect(control2, L2018$nomem_encr)
control3 <- intersect(control3.0, common4)

control4.0 <- intersect(control3, L2020$nomem_encr)
control4 <- intersect(control4.0, common5)

############ 
#Assets12-14

pop1 <- c(delta1.1, control1)

Assets0.12 <- A2012 %>% select(nomem_encr, ca12c006, ca12c012) %>% filter(nomem_encr %in% pop1)
Assets1.14 <- A2014 %>% select(nomem_encr, ca14d006, ca14d012) %>% filter(nomem_encr %in% pop1)
Assets12_14 <-full_join(Assets0.12, Assets1.14, by = "nomem_encr") %>% 
  mutate(delta_SMP = ca14d006 - ca12c006)%>%
  mutate(delta_W = ca14d012 - ca12c012)%>%
  select(nomem_encr, delta_SMP, delta_W)%>%
  replace_na(list(delta_W = 0))

################ 
#Leisure12-14
SI.0.12 <- L2012 %>% select(nomem_encr, cs12e006, 
                            cs12e011,
                            cs12e016, 
                            cs12e021, 
                            cs12e026, 
                            cs12e031, 
                            cs12e036, 
                            cs12e041, 
                            cs12e046, 
                            cs12e051, 
                            cs12e056, 
                            cs12e061)%>%
  filter(nomem_encr %in% pop1)
SI.0.12 <- SI.0.12 %>% mutate(SI.2012 = rowSums(SI.0.12[, c(2:13)]) )%>%
  mutate(SI.0.2012 = ifelse(SI.2012 > 0, 1, 0) )%>%
  select(nomem_encr, SI.0.2012)
SI.0.12[is.na(SI.0.12)] = 0

SI.1.14 <- L2014 %>% select(nomem_encr, 
                            cs14g006, 
                            cs14g011,
                            cs14g016, 
                            cs14g021, 
                            cs14g026, 
                            cs14g031, 
                            cs14g036, 
                            cs14g041, 
                            cs14g046, 
                            cs14g051, 
                            cs14g056, 
                            cs14g061)%>%
  filter(nomem_encr %in% pop1)
SI.1.14 <- SI.1.14 %>% mutate(SI.2014 = rowSums(SI.1.14[, c(2:13)]) )%>%
  mutate(SI.1.2014 = ifelse(SI.2014 > 0, 1, 0) )%>%
  select(nomem_encr, SI.1.2014)
SI.1.14[is.na(SI.1.14)] = 0

IU.0.12 <- L2012 %>% select(nomem_encr, cs12e439,  
                         cs12e442 , 
                         cs12e437 ,
                         cs12e441 ,
                         cs12e280 ,
                         cs12e267 ,
                         cs12e443 ,
                         cs12e440 ,
                         cs12e281 ,
                         cs12e276 ,
                         cs12e278 ,
                         cs12e279 ,
                         cs12e282 ) %>%
  filter(nomem_encr %in% pop1)
IU.0.12[is.na(IU.0.12)] = 0

IU.0.12 <- IU.0.12%>% mutate(IU.2012 = rowSums(IU.0.12[, c(2:14)])) %>% 
  mutate(SMU.2012 = rowSums(IU.0.12[,c(2:7)])) %>% 
  mutate(IDU.2012 = rowSums(IU.0.12[,c(8:14)])) %>%
  select(nomem_encr, IU.2012, SMU.2012, IDU.2012)

IU.1.14 <- L2014 %>% select(nomem_encr, 
                            cs14g439, 
                            cs14g437,
                            cs14g487,
                            cs14g280, 
                            cs14g267,
                            cs14g443,
                            cs14g440,
                            cs14g281,
                            cs14g276,
                            cs14g278,
                            cs14g279,
                            cs14g282)%>%
  filter(nomem_encr %in% pop1)
IU.1.14[is.na(IU.1.14)] = 0 

IU.1.14 <- IU.1.14 %>% mutate(IU.2014 = rowSums(IU.1.14[, c(2:13)])) %>%
  mutate(SMU.2014 = rowSums(IU.1.14[,c(2:6)])) %>%
  mutate(IDU.2014 = rowSums(IU.1.14[,c(7:13)])) %>%
  select(nomem_encr, IU.2014, SMU.2014, IDU.2014)
  


Leisure.0.12<-full_join(IU.0.12, SI.0.12, by = "nomem_encr")
Leisure.1.14<-full_join(IU.1.14, SI.1.14, by = "nomem_encr")

Leisure12_14<- full_join(Leisure.0.12, Leisure.1.14, by= "nomem_encr") %>%
  mutate(delta_SI = ifelse(SI.1.14 - SI.0.12 > 0, 1, 0))%>%
  mutate(delta_IU = IU.2014 -IU.2012)%>%
  mutate(delta_SMU = SMU.2014 -SMU.2012)%>%
  mutate(delta_IDU = IDU.2014 -IDU.2012)%>%
  select(nomem_encr, delta_SI, delta_IU, delta_SMU, delta_IDU)

#####

B2012 <- read_sav("C:/Users/vitor/OneDrive/Documenti/R/Thesis_data/Background/Background_2012.sav")
B2014 <- read_sav("C:/Users/vitor/OneDrive/Documenti/R/Thesis_data/Background/Background_2014.sav")
B2016 <- read_sav("C:/Users/vitor/OneDrive/Documenti/R/Thesis_data/Background/Background_2016.sav")
B2018 <- read_sav("C:/Users/vitor/OneDrive/Documenti/R/Thesis_data/Background/Background_2018.sav")
B2020 <- read_sav("C:/Users/vitor/OneDrive/Documenti/R/Thesis_data/Background/Background_2020.sav")


B.0.2012 <- B2012 %>% select(nomem_encr, nettoink_f, burgstat, sted, belbezig )%>%
  filter(nomem_encr %in% pop1)%>%
  rename(net_inc0 = nettoink_f)%>%
  mutate(married0= ifelse(burgstat > 1, 0, 1))%>%
  mutate(unmarried0= ifelse(burgstat > 1, 1, 0))%>%
  mutate(urban0= ifelse(sted > 3, 0, 1))%>%
  mutate(rural0= ifelse(sted > 3, 1, 0))%>%
  mutate(employed0= ifelse(belbezig < 4, 1, 0))%>%
  mutate(unemployed0= ifelse(belbezig > 4 & belbezig < 9, 1, 0))%>%
  mutate(retired0= ifelse(belbezig == 9, 1, 0))%>%
  mutate(other0= ifelse(belbezig >9, 1, 0))%>%
  select(nomem_encr, net_inc0, married0, unmarried0, urban0, rural0, employed0, unemployed0, retired0, other0)

B.1.2014 <- B2014 %>% select(nomem_encr, nettoink_f, burgstat, sted, belbezig )%>%
  filter(nomem_encr %in% pop1)%>%
  rename(net_inc1=  nettoink_f)%>%
  mutate(married1= ifelse(burgstat > 1, 0, 1))%>%
  mutate(unmarried1= ifelse(burgstat > 1, 1, 0))%>%
  mutate(urban1= ifelse(sted > 3, 0, 1))%>%
  mutate(rural1= ifelse(sted > 3, 1, 0))%>%
  mutate(employed1= ifelse(belbezig < 4, 1, 0))%>%
  mutate(unemployed1= ifelse(belbezig > 4 & belbezig < 9, 1, 0))%>%
  mutate(retired1= ifelse(belbezig == 9, 1, 0))%>%
  mutate(other1= ifelse(belbezig >9, 1, 0))%>%
  select(nomem_encr, net_inc1, married1, unmarried1, urban1, rural1, employed1, unemployed1, retired1, other1)

Background12_14 <- full_join(B.0.2012, B.1.2014, by="nomem_encr")%>%
  mutate(delta_income = net_inc1 -net_inc0)%>%
  mutate(delta_married = ifelse(married1 -married0 < 1, 0, 1)) %>%
  mutate(delta_unmarried = ifelse(unmarried1 -unmarried0 < 1, 0, 1)) %>%
  mutate(delta_urban = ifelse(urban1 -urban0 < 1, 0, 1)) %>%
  mutate(delta_rural = ifelse(rural1 -rural0 < 1, 0, 1)) %>%
  mutate(delta_employed = ifelse(employed1 -employed0 < 1, 0, 1)) %>%
  mutate(delta_unemployed = ifelse(unemployed1 -unemployed0 < 1, 0, 1)) %>%
  mutate(delta_retired = ifelse(retired1 -retired0 < 1, 0, 1)) %>%
  mutate(delta_other = ifelse(other1 -other0 < 1, 0, 1))%>%
  replace_na(list(delta_income = 0))%>%
  replace_na(list(delta_married = 0))%>%
  replace_na(list(delta_unmarried = 0))%>%
  replace_na(list(delta_urban = 0))%>%
  replace_na(list(delta_rural = 0))%>%
  replace_na(list(delta_employed = 0))%>%
  replace_na(list(delta_unemployed = 0))%>%
  replace_na(list(delta_retired = 0))%>%
  replace_na(list(delta_other = 0))%>%
  select(nomem_encr, delta_income, delta_married, delta_unmarried, delta_urban, delta_rural, delta_employed, delta_unemployed, delta_retired, delta_other)
#########
#df1
df1 <- full_join(Assets12_14, Leisure12_14, by ="nomem_encr")
df1 <- full_join(df1, Background12_14, by="nomem_encr")

###########
#Assets14-16

pop2 <- c(delta2.1, control2)

Assets0.14 <- A2014 %>% select(nomem_encr, ca14d006, ca14d012) %>% filter(nomem_encr %in% pop2)
Assets1.16 <- A2016 %>% select(nomem_encr, ca16e006, ca16e012) %>% filter(nomem_encr %in% pop2)
Assets14_16 <-full_join(Assets0.14, Assets1.16, by = "nomem_encr") %>% 
  mutate(delta_SMP = ca16e006 - ca14d006)%>%
  mutate(delta_W = ca16e012 - ca14d012)%>%
  select(nomem_encr, delta_SMP, delta_W)%>%
  replace_na(list(delta_W = 0))
#########
#Leisure14-16

SI.0.14 <- L2014 %>% select(nomem_encr,
                            cs14g006, 
                            cs14g011,
                            cs14g016, 
                            cs14g021, 
                            cs14g026, 
                            cs14g031, 
                            cs14g036, 
                            cs14g041, 
                            cs14g046, 
                            cs14g051, 
                            cs14g056, 
                            cs14g061)%>%
  filter(nomem_encr %in% pop2)

SI.0.14 <- SI.0.14 %>% mutate(SI.2014 = rowSums(SI.0.14[, c(2:13)]) )%>%
  mutate(SI.0.2014 = ifelse(SI.2014 > 0, 1, 0) )%>%
  select(nomem_encr, SI.0.2014)
SI.0.14[is.na(SI.0.14)] = 0

SI.1.16 <- L2016 %>% select(nomem_encr, 
                            cs16i006, 
                            cs16i011,
                            cs16i016, 
                            cs16i021, 
                            cs16i026, 
                            cs16i031, 
                            cs16i036, 
                            cs16i041, 
                            cs16i046, 
                            cs16i051, 
                            cs16i056, 
                            cs16i061)%>%
  filter(nomem_encr %in% pop2)
SI.1.16 <- SI.1.16 %>% mutate(SI.2016 = rowSums(SI.1.16[, c(2:13)]) )%>%
  mutate(SI.1.2016 = ifelse(SI.2016 > 0, 1, 0) )%>%
  select(nomem_encr, SI.1.2016)
SI.1.16[is.na(SI.1.16)] = 0

IU.0.14 <- L2014 %>% select(nomem_encr, 
                            cs14g439, 
                            cs14g437,
                            cs14g487,
                            cs14g280, 
                            cs14g267,
                            cs14g443,
                            cs14g440,
                            cs14g281,
                            cs14g276,
                            cs14g278,
                            cs14g279,
                            cs14g282) %>%
  filter(nomem_encr %in% pop2)
IU.0.14[is.na(IU.0.14)] = 0

IU.0.14 <- IU.0.14%>% mutate(IU.2014 = rowSums(IU.0.14[, c(2:13)])) %>% 
  mutate(SMU.2014 = rowSums(IU.0.14[,c(2:6)])) %>% 
  mutate(IDU.2014 = rowSums(IU.0.14[,c(7:13)])) %>%
  select(nomem_encr, IU.2014, SMU.2014, IDU.2014)

IU.1.16 <- L2016 %>% select(nomem_encr, 
                            cs16i439, 
                            cs16i437,
                            cs16i487,
                            cs16i280, 
                            cs16i267,
                            cs16i443,
                            cs16i440,
                            cs16i281,
                            cs16i276,
                            cs16i278,
                            cs16i279,
                            cs16i282)%>%
  filter(nomem_encr %in% pop2)
IU.1.16[is.na(IU.1.16)] = 0 

IU.1.16 <- IU.1.16 %>% mutate(IU.2016 = rowSums(IU.1.16[, c(2:13)])) %>%
  mutate(SMU.2016 = rowSums(IU.1.16[,c(2:6)])) %>%
  mutate(IDU.2016 = rowSums(IU.1.16[,c(7:13)])) %>%
  select(nomem_encr, IU.2016, SMU.2016, IDU.2016)

Leisure.0.14<-full_join(IU.0.14, SI.0.14, by = "nomem_encr")
Leisure.1.16<-full_join(IU.1.16, SI.1.16, by = "nomem_encr")

Leisure14_16<- full_join(Leisure.0.14, Leisure.1.16, by= "nomem_encr") %>%
  mutate(delta_SI = ifelse(SI.1.16 - SI.0.14 > 0, 1, 0))%>%
  mutate(delta_IU = IU.2016 -IU.2014)%>%
  mutate(delta_SMU = SMU.2016 -SMU.2014)%>%
  mutate(delta_IDU = IDU.2016 -IDU.2014)%>%
  select(nomem_encr, delta_SI, delta_IU, delta_SMU, delta_IDU)    
##########
#Backgroun14-16

B.0.2014 <- B2014 %>% select(nomem_encr, nettoink_f, burgstat, sted, belbezig )%>%
  filter(nomem_encr %in% pop2)%>%
  rename(net_inc0 = nettoink_f)%>%
  mutate(married0= ifelse(burgstat > 1, 0, 1))%>%
  mutate(unmarried0= ifelse(burgstat > 1, 1, 0))%>%
  mutate(urban0= ifelse(sted > 3, 0, 1))%>%
  mutate(rural0= ifelse(sted > 3, 1, 0))%>%
  mutate(employed0= ifelse(belbezig < 4, 1, 0))%>%
  mutate(unemployed0= ifelse(belbezig > 4 & belbezig < 9, 1, 0))%>%
  mutate(retired0= ifelse(belbezig == 9, 1, 0))%>%
  mutate(other0= ifelse(belbezig >9, 1, 0))%>%
  select(nomem_encr, net_inc0, married0, unmarried0, urban0, rural0, employed0, unemployed0, retired0, other0)

B.1.2016 <- B2016 %>% select(nomem_encr, nettoink_f, burgstat, sted, belbezig )%>%
  filter(nomem_encr %in% pop2)%>%
  rename(net_inc1=  nettoink_f)%>%
  mutate(married1= ifelse(burgstat > 1, 0, 1))%>%
  mutate(unmarried1= ifelse(burgstat > 1, 1, 0))%>%
  mutate(urban1= ifelse(sted > 3, 0, 1))%>%
  mutate(rural1= ifelse(sted > 3, 1, 0))%>%
  mutate(employed1= ifelse(belbezig < 4, 1, 0))%>%
  mutate(unemployed1= ifelse(belbezig > 4 & belbezig < 9, 1, 0))%>%
  mutate(retired1= ifelse(belbezig == 9, 1, 0))%>%
  mutate(other1= ifelse(belbezig >9, 1, 0))%>%
  select(nomem_encr, net_inc1, married1, unmarried1, urban1, rural1, employed1, unemployed1, retired1, other1)

Background14_16 <- full_join(B.0.2014, B.1.2016, by="nomem_encr")%>%
  mutate(delta_income = net_inc1 -net_inc0)%>%
  mutate(delta_married = ifelse(married1 -married0 < 1, 0, 1)) %>%
  mutate(delta_unmarried = ifelse(unmarried1 -unmarried0 < 1, 0, 1)) %>%
  mutate(delta_urban = ifelse(urban1 -urban0 < 1, 0, 1)) %>%
  mutate(delta_rural = ifelse(rural1 -rural0 < 1, 0, 1)) %>%
  mutate(delta_employed = ifelse(employed1 -employed0 < 1, 0, 1)) %>%
  mutate(delta_unemployed = ifelse(unemployed1 -unemployed0 < 1, 0, 1)) %>%
  mutate(delta_retired = ifelse(retired1 -retired0 < 1, 0, 1)) %>%
  mutate(delta_other = ifelse(other1 -other0 < 1, 0, 1))%>%
  replace_na(list(delta_income = 0))%>%
  replace_na(list(delta_married = 0))%>%
  replace_na(list(delta_unmarried = 0))%>%
  replace_na(list(delta_urban = 0))%>%
  replace_na(list(delta_rural = 0))%>%
  replace_na(list(delta_employed = 0))%>%
  replace_na(list(delta_unemployed = 0))%>%
  replace_na(list(delta_retired = 0))%>%
  replace_na(list(delta_other = 0))%>%
  select(nomem_encr, delta_income, delta_married, delta_unmarried, delta_urban, delta_rural, delta_employed, delta_unemployed, delta_retired, delta_other)
##########
#df2
df2 <- full_join(Assets14_16, Leisure14_16, by ="nomem_encr")
df2 <- full_join(df2, Background14_16, by="nomem_encr")

#############
#Assets16-18

pop3 <- c(delta3.1, control3)

Assets0.16 <- A2016 %>% select(nomem_encr, ca16e006, ca16e012) %>% filter(nomem_encr %in% pop3)
Assets1.18 <- A2018 %>% select(nomem_encr, ca18f006, ca18f012) %>% filter(nomem_encr %in% pop3)
Assets16_18 <-full_join(Assets0.16, Assets1.18, by = "nomem_encr") %>% 
  mutate(delta_SMP = ca18f006 - ca16e006)%>%
  mutate(delta_W = ca18f012 - ca16e012)%>%
  select(nomem_encr, delta_SMP, delta_W)%>%
  replace_na(list(delta_W = 0))
############
#Leisure16-18

SI.0.16 <- L2016 %>% select(nomem_encr, 
                            cs16i006, 
                            cs16i011,
                            cs16i016, 
                            cs16i021, 
                            cs16i026, 
                            cs16i031, 
                            cs16i036, 
                            cs16i041, 
                            cs16i046, 
                            cs16i051, 
                            cs16i056, 
                            cs16i061)%>%
  filter(nomem_encr %in% pop3)
SI.0.16 <- SI.0.16 %>% mutate(SI.2016 = rowSums(SI.0.16[, c(2:13)]) )%>%
  mutate(SI.0.2016 = ifelse(SI.2016 > 0, 1, 0) )%>%
  select(nomem_encr, SI.0.2016)
SI.0.16[is.na(SI.0.16)] = 0

SI.1.18 <- L2018 %>% select(nomem_encr, 
                            cs18k006, 
                            cs18k011,
                            cs18k016, 
                            cs18k021, 
                            cs18k026, 
                            cs18k031, 
                            cs18k036, 
                            cs18k041, 
                            cs18k046, 
                            cs18k051, 
                            cs18k056, 
                            cs18k061)%>%
  filter(nomem_encr %in% pop3)
SI.1.18 <- SI.1.18 %>% mutate(SI.2018 = rowSums(SI.1.18[, c(2:13)]) )%>%
  mutate(SI.1.2018 = ifelse(SI.2018 > 0, 1, 0) )%>%
  select(nomem_encr, SI.1.2018)
SI.1.18[is.na(SI.1.18)] = 0

IU.0.16 <- L2016 %>% select(nomem_encr, 
                            cs16i439, 
                            cs16i437,
                            cs16i487,
                            cs16i280, 
                            cs16i267,
                            cs16i443,
                            cs16i440,
                            cs16i281,
                            cs16i276,
                            cs16i278,
                            cs16i279,
                            cs16i282)%>%
  filter(nomem_encr %in% pop3)
IU.0.16[is.na(IU.0.16)] = 0 

IU.0.16 <- IU.0.16 %>% mutate(IU.2016 = rowSums(IU.0.16[, c(2:13)])) %>%
  mutate(SMU.2016 = rowSums(IU.0.16[,c(2:6)])) %>%
  mutate(IDU.2016 = rowSums(IU.0.16[,c(7:13)])) %>%
  select(nomem_encr, IU.2016, SMU.2016, IDU.2016)


IU.1.18 <- L2018 %>% select(nomem_encr, 
                            cs18k439, 
                            cs18k437,
                            cs18k487,
                            cs18k280, 
                            cs18k267,
                            cs18k443,
                            cs18k440,
                            cs18k281,
                            cs18k276,
                            cs18k278,
                            cs18k279,
                            cs18k282)%>%
  filter(nomem_encr %in% pop3)
IU.1.18[is.na(IU.1.18)] = 0 

IU.1.18 <- IU.1.18 %>% mutate(IU.2018 = rowSums(IU.1.18[, c(2:13)])) %>%
  mutate(SMU.2018 = rowSums(IU.1.18[,c(2:6)])) %>%
  mutate(IDU.2018 = rowSums(IU.1.18[,c(7:13)])) %>%
  select(nomem_encr, IU.2018, SMU.2018, IDU.2018)


Leisure.0.16<-full_join(IU.0.16, SI.0.16, by = "nomem_encr")
Leisure.1.18<-full_join(IU.1.18, SI.1.18, by = "nomem_encr")

Leisure16_18<- full_join(Leisure.0.16, Leisure.1.18, by= "nomem_encr") %>%
  mutate(delta_SI = ifelse(SI.1.18 - SI.0.16 > 0, 1, 0))%>%
  mutate(delta_IU = IU.2018 -IU.2016)%>%
  mutate(delta_SMU = SMU.2018 -SMU.2016)%>%
  mutate(delta_IDU = IDU.2018 -IDU.2016)%>%
  select(nomem_encr, delta_SI, delta_IU, delta_SMU, delta_IDU)
#############
#Background16-18
B.0.2016 <- B2016 %>% select(nomem_encr, nettoink_f, burgstat, sted, belbezig )%>%
  filter(nomem_encr %in% pop3)%>%
  rename(net_inc0 = nettoink_f)%>%
  mutate(married0= ifelse(burgstat > 1, 0, 1))%>%
  mutate(unmarried0= ifelse(burgstat > 1, 1, 0))%>%
  mutate(urban0= ifelse(sted > 3, 0, 1))%>%
  mutate(rural0= ifelse(sted > 3, 1, 0))%>%
  mutate(employed0= ifelse(belbezig < 4, 1, 0))%>%
  mutate(unemployed0= ifelse(belbezig > 4 & belbezig < 9, 1, 0))%>%
  mutate(retired0= ifelse(belbezig == 9, 1, 0))%>%
  mutate(other0= ifelse(belbezig >9, 1, 0))%>%
  select(nomem_encr, net_inc0, married0, unmarried0, urban0, rural0, employed0, unemployed0, retired0, other0)

B.1.2018 <- B2018 %>% select(nomem_encr, nettoink_f, burgstat, sted, belbezig )%>%
  filter(nomem_encr %in% pop3)%>%
  rename(net_inc1=  nettoink_f)%>%
  mutate(married1= ifelse(burgstat > 1, 0, 1))%>%
  mutate(unmarried1= ifelse(burgstat > 1, 1, 0))%>%
  mutate(urban1= ifelse(sted > 3, 0, 1))%>%
  mutate(rural1= ifelse(sted > 3, 1, 0))%>%
  mutate(employed1= ifelse(belbezig < 4, 1, 0))%>%
  mutate(unemployed1= ifelse(belbezig > 4 & belbezig < 9, 1, 0))%>%
  mutate(retired1= ifelse(belbezig == 9, 1, 0))%>%
  mutate(other1= ifelse(belbezig >9, 1, 0))%>%
  select(nomem_encr, net_inc1, married1, unmarried1, urban1, rural1, employed1, unemployed1, retired1, other1)

Background16_18 <- full_join(B.0.2016, B.1.2018, by="nomem_encr")%>%
  mutate(delta_income = net_inc1 -net_inc0)%>%
  mutate(delta_married = ifelse(married1 -married0 < 1, 0, 1)) %>%
  mutate(delta_unmarried = ifelse(unmarried1 -unmarried0 < 1, 0, 1)) %>%
  mutate(delta_urban = ifelse(urban1 -urban0 < 1, 0, 1)) %>%
  mutate(delta_rural = ifelse(rural1 -rural0 < 1, 0, 1)) %>%
  mutate(delta_employed = ifelse(employed1 -employed0 < 1, 0, 1)) %>%
  mutate(delta_unemployed = ifelse(unemployed1 -unemployed0 < 1, 0, 1)) %>%
  mutate(delta_retired = ifelse(retired1 -retired0 < 1, 0, 1)) %>%
  mutate(delta_other = ifelse(other1 -other0 < 1, 0, 1))%>%
  replace_na(list(delta_income = 0))%>%
  replace_na(list(delta_married = 0))%>%
  replace_na(list(delta_unmarried = 0))%>%
  replace_na(list(delta_urban = 0))%>%
  replace_na(list(delta_rural = 0))%>%
  replace_na(list(delta_employed = 0))%>%
  replace_na(list(delta_unemployed = 0))%>%
  replace_na(list(delta_retired = 0))%>%
  replace_na(list(delta_other = 0))%>%
  select(nomem_encr, delta_income, delta_married, delta_unmarried, delta_urban, delta_rural, delta_employed, delta_unemployed, delta_retired, delta_other)
##########
#df3
df3 <- full_join(Assets16_18, Leisure16_18, by ="nomem_encr")
df3 <- full_join(df3, Background16_18, by="nomem_encr")

#############
#Assets18-20

pop4 <- c(delta4.1, control4)

Assets0.18 <- A2018 %>% select(nomem_encr, ca18f006, ca18f012) %>% filter(nomem_encr %in% pop4)
Assets1.20 <- A2020 %>% select(nomem_encr, ca20g006, ca20g012) %>% filter(nomem_encr %in% pop4)
Assets18_20 <-full_join(Assets0.18, Assets1.20, by = "nomem_encr") %>% 
  mutate(delta_SMP = ca20g006 - ca18f006)%>%
  mutate(delta_W = ca20g012 - ca18f012)%>%
  select(nomem_encr, delta_SMP, delta_W)%>%
  replace_na(list(delta_W = 0))
############
#Leisure18-20

SI.0.18 <- L2018 %>% select(nomem_encr, 
                            cs18k006, 
                            cs18k011,
                            cs18k016, 
                            cs18k021, 
                            cs18k026, 
                            cs18k031, 
                            cs18k036, 
                            cs18k041, 
                            cs18k046, 
                            cs18k051, 
                            cs18k056, 
                            cs18k061)%>%
  filter(nomem_encr %in% pop4)
SI.0.18 <- SI.0.18 %>% mutate(SI.2018 = rowSums(SI.0.18[, c(2:13)]) )%>%
  mutate(SI.0.2018 = ifelse(SI.2018 > 0, 1, 0) )%>%
  select(nomem_encr, SI.0.2018)
SI.0.18[is.na(SI.0.18)] = 0

SI.1.20 <- L2020 %>% select(nomem_encr, 
                            cs20m006, 
                            cs20m011,
                            cs20m016, 
                            cs20m021, 
                            cs20m026, 
                            cs20m031, 
                            cs20m036, 
                            cs20m041, 
                            cs20m046, 
                            cs20m051, 
                            cs20m056, 
                            cs20m061)%>%
  filter(nomem_encr %in% pop4)
SI.1.20 <- SI.1.20 %>% mutate(SI.2020 = rowSums(SI.1.20[, c(2:13)]) )%>%
  mutate(SI.1.2020 = ifelse(SI.2020 > 0, 1, 0) )%>%
  select(nomem_encr, SI.1.2020)
SI.1.20[is.na(SI.1.20)] = 0


IU.0.18 <- L2018 %>% select(nomem_encr, 
                            cs18k439, 
                            cs18k437,
                            cs18k487,
                            cs18k280, 
                            cs18k267,
                            cs18k443,
                            cs18k440,
                            cs18k281,
                            cs18k276,
                            cs18k278,
                            cs18k279,
                            cs18k282)%>%
  filter(nomem_encr %in% pop4)
IU.0.18[is.na(IU.0.18)] = 0 

IU.0.18 <- IU.0.18 %>% mutate(IU.2018 = rowSums(IU.0.18[, c(2:13)])) %>%
  mutate(SMU.2018 = rowSums(IU.0.18[,c(2:6)])) %>%
  mutate(IDU.2018 = rowSums(IU.0.18[,c(7:13)])) %>%
  select(nomem_encr, IU.2018, SMU.2018, IDU.2018)


IU.1.20 <- L2020 %>% select(nomem_encr, 
                            cs20m439, 
                            cs20m437,
                            cs20m487,
                            cs20m280, 
                            cs20m267,
                            cs20m443,
                            cs20m440,
                            cs20m281,
                            cs20m276,
                            cs20m278,
                            cs20m279)%>%
  filter(nomem_encr %in% pop4)
IU.1.20[is.na(IU.1.20)] = 0 

IU.1.20 <- IU.1.20 %>% mutate(IU.2020 = rowSums(IU.1.20[, c(2:12)])) %>%
  mutate(SMU.2020 = rowSums(IU.1.20[,c(2:6)])) %>%
  mutate(IDU.2020 = rowSums(IU.1.20[,c(7:12)])) %>%
  select(nomem_encr, IU.2020, SMU.2020, IDU.2020)

Leisure.0.18<-full_join(IU.0.18, SI.0.18, by = "nomem_encr")
Leisure.1.20<-full_join(IU.1.20, SI.1.20, by = "nomem_encr")

Leisure18_20<- full_join(Leisure.0.18, Leisure.1.20, by= "nomem_encr") %>%
  mutate(delta_SI = ifelse(SI.1.20 - SI.0.18 > 0, 1, 0))%>%
  mutate(delta_IU = IU.2020 -IU.2018)%>%
  mutate(delta_SMU = SMU.2020 -SMU.2018)%>%
  mutate(delta_IDU = IDU.2020 -IDU.2018)%>%
  select(nomem_encr, delta_SI, delta_IU, delta_SMU, delta_IDU)
###########
#Background18-20

B.0.2018 <- B2018 %>% select(nomem_encr, nettoink_f, burgstat, sted, belbezig )%>%
  filter(nomem_encr %in% pop4)%>%
  rename(net_inc0 = nettoink_f)%>%
  mutate(married0= ifelse(burgstat > 1, 0, 1))%>%
  mutate(unmarried0= ifelse(burgstat > 1, 1, 0))%>%
  mutate(urban0= ifelse(sted > 3, 0, 1))%>%
  mutate(rural0= ifelse(sted > 3, 1, 0))%>%
  mutate(employed0= ifelse(belbezig < 4, 1, 0))%>%
  mutate(unemployed0= ifelse(belbezig > 4 & belbezig < 9, 1, 0))%>%
  mutate(retired0= ifelse(belbezig == 9, 1, 0))%>%
  mutate(other0= ifelse(belbezig >9, 1, 0))%>%
  select(nomem_encr, net_inc0, married0, unmarried0, urban0, rural0, employed0, unemployed0, retired0, other0)

B.1.2020 <- B2020 %>% select(nomem_encr, nettoink_f, burgstat, sted, belbezig )%>%
  filter(nomem_encr %in% pop4)%>%
  rename(net_inc1=  nettoink_f)%>%
  mutate(married1= ifelse(burgstat > 1, 0, 1))%>%
  mutate(unmarried1= ifelse(burgstat > 1, 1, 0))%>%
  mutate(urban1= ifelse(sted > 3, 0, 1))%>%
  mutate(rural1= ifelse(sted > 3, 1, 0))%>%
  mutate(employed1= ifelse(belbezig < 4, 1, 0))%>%
  mutate(unemployed1= ifelse(belbezig > 4 & belbezig < 9, 1, 0))%>%
  mutate(retired1= ifelse(belbezig == 9, 1, 0))%>%
  mutate(other1= ifelse(belbezig >9, 1, 0))%>%
  select(nomem_encr, net_inc1, married1, unmarried1, urban1, rural1, employed1, unemployed1, retired1, other1)

Background18_20 <- full_join(B.0.2018, B.1.2020, by="nomem_encr")%>%
  mutate(delta_income = net_inc1 -net_inc0)%>%
  mutate(delta_married = ifelse(married1 -married0 < 1, 0, 1)) %>%
  mutate(delta_unmarried = ifelse(unmarried1 -unmarried0 < 1, 0, 1)) %>%
  mutate(delta_urban = ifelse(urban1 -urban0 < 1, 0, 1)) %>%
  mutate(delta_rural = ifelse(rural1 -rural0 < 1, 0, 1)) %>%
  mutate(delta_employed = ifelse(employed1 -employed0 < 1, 0, 1)) %>%
  mutate(delta_unemployed = ifelse(unemployed1 -unemployed0 < 1, 0, 1)) %>%
  mutate(delta_retired = ifelse(retired1 -retired0 < 1, 0, 1)) %>%
  mutate(delta_other = ifelse(other1 -other0 < 1, 0, 1))%>%
  replace_na(list(delta_income = 0))%>%
  replace_na(list(delta_married = 0))%>%
  replace_na(list(delta_unmarried = 0))%>%
  replace_na(list(delta_urban = 0))%>%
  replace_na(list(delta_rural = 0))%>%
  replace_na(list(delta_employed = 0))%>%
  replace_na(list(delta_unemployed = 0))%>%
  replace_na(list(delta_retired = 0))%>%
  replace_na(list(delta_other = 0))%>%
  select(nomem_encr, delta_income, delta_married, delta_unmarried, delta_urban, delta_rural, delta_employed, delta_unemployed, delta_retired, delta_other)
#################
#df4

df4 <- full_join(Assets18_20, Leisure18_20, by ="nomem_encr")
df4 <- full_join(df4, Background18_20, by="nomem_encr")
df4 <- df4 %>% filter(delta_W > -99999999)
###########
df1 <- df1 %>% filter(delta_IU < 169)
df2 <- df2 %>% filter(delta_IU < 169)
df3 <- df3 %>% filter(delta_IU < 169)
df4 <- df4 %>% filter(delta_IU < 169)

df1 <- df1 %>% mutate(period = 1)
df2 <- df2 %>% mutate(period = 2)
df3 <- df3 %>% mutate(period = 3)
df4 <- df4 %>% mutate(period = 4)

df1.1<-zap_label(df1)
df2.1<-zap_label(df2)
df3.1<-zap_label(df3)
df4.1<-zap_label(df4)


df <- bind_rows(df1.1, df2.1)
df <- bind_rows(df, df3.1)
df <- bind_rows(df, df4.1)

df <- df %>% mutate(period1 = ifelse(period > 1, 0, 1)) %>%
  mutate(period2 = ifelse(period>1 & period<3, 1, 0))%>%
  mutate(period3 = ifelse(period>2 & period<4, 1, 0))%>%
  mutate(period4 = ifelse(period>3, 1, 0))%>%
  select(-period)

which(is.na(df))




###########
#Summary Statistics
install.packages("qwraps2")
library(qwraps2)

summary(df4)

mean_sd(df$delta_IU)
mean_sd(df$delta_SMU)
mean_sd(df$delta_IDU)
mean_sd(df$delta_income)
mean_sd(df$delta_W)

mean_sd(df$delta_SMP)
mean_sd(df$delta_SI)
mean_sd(df$delta_married)
mean_sd(df$delta_unmarried)
mean_sd(df$delta_employed)
mean_sd(df$delta_unemployed)
mean_sd(df$delta_retired)
mean_sd(df$delta_other)
mean_sd(df$delta_urban)
mean_sd(df$delta_rural)

summary(df)
##########

#age and sex

BV1 <- B2012 %>% filter(nomem_encr %in% pop1) %>% select(nomem_encr, leeftijd, geslacht)%>% mutate(male = ifelse(geslacht >1, 0, 1))
BV2 <- B2014 %>% filter(nomem_encr %in% pop2) %>% select(nomem_encr, leeftijd, geslacht)%>% mutate(male = ifelse(geslacht >1, 0, 1))
BV3 <- B2016 %>% filter(nomem_encr %in% pop3) %>% select(nomem_encr, leeftijd, geslacht)%>% mutate(male = ifelse(geslacht >1, 0, 1))
BV4 <- B2018 %>% filter(nomem_encr %in% pop4) %>% select(nomem_encr, leeftijd, geslacht)%>% mutate(male = ifelse(geslacht >1, 0, 1))

BV<- bind_rows(BV1, BV2)
BV<- bind_rows(BV, BV3)
BV<- bind_rows(BV, BV4)


summary(BV)
mean_sd(BV$leeftijd)

BV1.1 <- B2012 %>% filter(nomem_encr %in% delta1.1)%>% select(nomem_encr, leeftijd, geslacht)%>% mutate(male = ifelse(geslacht >1, 0, 1))
summary(BV1.1)

##########
#OLS

install.packages("caTools")
library(caTools)
library(ggplot2)

model <- lm(delta_SMP ~ 0 + delta_IU + delta_income + delta_W + delta_SI + delta_married + delta_unmarried + delta_retired + delta_employed + delta_unemployed + delta_other + delta_urban + delta_rural + period1 + period2 + period3 + period4, data = df )
model2 <- lm(delta_SMP ~ 0 + delta_SMU + delta_IDU + delta_income + delta_W + delta_SI + delta_married + delta_unmarried + delta_retired + delta_employed + delta_unemployed + delta_other + delta_urban + delta_rural + period1 + period2 + period3 + period4, data = df )
summary(model)
summary(model2)




#########
install.packages("stargazer")
library(stargazer)




pop1.1 <- df1.1 %>% filter(nomem_encr %in% delta1.1) 
pop2.1 <- df2.1 %>% filter(nomem_encr %in% delta2.1) 
pop3.1 <- df3.1 %>% filter(nomem_encr %in% delta3.1) 
pop4.1 <- df4.1 %>% filter(nomem_encr %in% delta4.1) 

pop.1 <- bind_rows(pop1.1, pop2.1)
pop.1 <- bind_rows(pop.1, pop3.1)
pop.1 <- bind_rows(pop.1, pop4.1)


#########

# Average Interent utilization

data1 <- IU.1.14 %>% mutate(SMP = ifelse(nomem_encr %in% delta1.1, 1, 0)) %>% filter(nomem_encr %in% df1$nomem_encr)
data1.0 <- data1 %>% filter(SMP==0)
data1.1 <- data1 %>% filter(SMP==1)
ss <- stargazer(as.data.frame(data1.0), type = "text", flip = FALSE, out = "C:/Users/vitor/OneDrive/Desktop/THESIS/ss.html")
ss <- stargazer(as.data.frame(data1.1), type = "text", flip = FALSE, out = "C:/Users/vitor/OneDrive/Desktop/THESIS/ss.html")


data2 <- IU.1.16 %>% mutate(SMP = ifelse(nomem_encr %in% delta2.1, 1, 0)) %>% filter(nomem_encr %in% df2$nomem_encr)
data2.0 <- data2 %>% filter(SMP==0)
data2.1 <- data2 %>% filter(SMP==1)
ss <- stargazer(as.data.frame(data2.0), type = "text", flip = FALSE, out = "C:/Users/vitor/OneDrive/Desktop/THESIS/ss.html")
ss <- stargazer(as.data.frame(data2.1), type = "text", flip = FALSE, out = "C:/Users/vitor/OneDrive/Desktop/THESIS/ss.html")



data3 <- IU.1.18 %>% mutate(SMP = ifelse(nomem_encr %in% delta3.1, 1, 0)) %>% filter(nomem_encr %in% df3$nomem_encr)
data3.0 <- data3 %>% filter(SMP==0)
data3.1 <- data3 %>% filter(SMP==1)
ss <- stargazer(as.data.frame(data3.0), type = "text", flip = FALSE, out = "C:/Users/vitor/OneDrive/Desktop/THESIS/ss.html")
ss <- stargazer(as.data.frame(data3.1), type = "text", flip = FALSE, out = "C:/Users/vitor/OneDrive/Desktop/THESIS/ss.html")



data4 <- IU.1.20 %>% mutate(SMP = ifelse(nomem_encr %in% delta4.1, 1, 0)) %>% filter(nomem_encr %in% df4$nomem_encr)
data4.0 <- data4 %>% filter(SMP==0)
data4.1 <- data4 %>% filter(SMP==1)
ss <- stargazer(as.data.frame(data4.0), type = "text", flip = FALSE, out = "C:/Users/vitor/OneDrive/Desktop/THESIS/ss.html")
ss <- stargazer(as.data.frame(data4.1), type = "text", flip = FALSE, out = "C:/Users/vitor/OneDrive/Desktop/THESIS/ss.html")


data1 <- data1 %>% rename(IU = IU.2014) %>% rename(SMU = SMU.2014) %>% rename(IDU = IDU.2014)
data2 <- data2 %>% rename(IU = IU.2016) %>% rename(SMU = SMU.2016) %>% rename(IDU = IDU.2016)
data3 <- data3 %>% rename(IU = IU.2018) %>% rename(SMU = SMU.2018) %>% rename(IDU = IDU.2018)
data4 <- data4 %>% rename(IU = IU.2020) %>% rename(SMU = SMU.2020) %>% rename(IDU = IDU.2020)

data <- bind_rows(data1, data2)
data <- bind_rows(data, data3)
data <- bind_rows(data, data4)

data.0 <- data %>% filter(SMP==0)
data.1 <- data %>% filter(SMP==1)
ss <- stargazer(as.data.frame(data.0), type = "text", flip = FALSE, out = "C:/Users/vitor/OneDrive/Desktop/THESIS/ss.html")
ss <- stargazer(as.data.frame(data.1), type = "text", flip = FALSE, out = "C:/Users/vitor/OneDrive/Desktop/THESIS/ss.html")






