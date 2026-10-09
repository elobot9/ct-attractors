library(tidyverse)
#inputs:
#"../02-data/01-raw-data/01-pairs-subset.csv"
#"../02-data/01-raw-data/02-dt-subset.csv"
#"../02-data/01-raw-data/03to05-esm-raw.csv"
#"../02-data/attractors_codebooks.xlsx"

#subset the full raw S1 & S2 data to just the variables used for the study, consistent with sharing policy
# s1.raw = read_csv("../02-data/01-raw-data/01-PAIRS-esm-raw.csv")
# s2.raw = read_csv("../02-data/01-raw-data/02-DT-esm-raw.csv")
# 
# s1.subset = s1.raw %>%
#   filter(esm.PRO02.w1 != 6) %>% #remove practice sessions
#   select(esm.IDnum.w1,esm.startDateTime.w1,esm.EXT.w1,esm.AGR.w1,esm.CON.w1,esm.NEU.w1) %>%
#   rename(SID=esm.IDnum.w1,Date=esm.startDateTime.w1) %>%
#   rename_with(~str_extract(.x,"(?<=esm\\.)[[:alpha:]]+"),contains("esm"))
# write_csv(s1.subset,"../02-data/01-raw-data/01-pairs-subset.csv")
# 
# s2.subset = s2.raw %>%
#   rename(SID = ID) %>%
#   select(SID,StartDate,SP_personality_1_ER:SP_personality_10_OR)
# write_csv(s2.subset,"../02-data/01-raw-data/02-dt-subset.csv")  


#Import data & codebooks
#codebooks----
s1.cb = readxl::read_xlsx("../02-data/attractors_codebooks.xlsx",sheet="PAIRS-ESM") %>%
  filter(section == 'personality' & format == 'num_score') %>%
  select(item,construct,min,max) %>%
  rename(domain = construct) %>%
  mutate(domain = str_remove(domain, "_scored"))

s1.raw = read_csv("../02-data/01-raw-data/01-pairs-subset.csv")
s2.raw = read_csv("../02-data/01-raw-data/02-dt-subset.csv")
s3to5.raw = read_csv("../02-data/01-raw-data/03to05-esm-raw.csv")
#apply exclusion criteria=======================================================

#prep data-------------------
#recode S1 data + give more normal col names
s1.exc0 = s1.raw %>%
  mutate(Date = str_remove_all(Date, "\\(|\\)"),
         Date = mdy_hms(Date)) %>%
  #reverse-score neuroticism to ES
  mutate(NEU = 6 - NEU)

#get start times for each person
s1.starttimes = s1.exc0 %>%
  group_by(SID) %>%
  slice_min(Date) %>%
  ungroup() %>%
  rename(StartDate = Date) %>%
  select(SID,StartDate) %>%
  unique()

s1.exc = s1.exc0 %>%
  merge(s1.starttimes,by=c('SID')) %>%
  mutate(SID = as.character(SID)) %>%
  mutate(sample = 'pairs')

#do the same for s2
s2.exc0 = s2.raw %>%
  pivot_longer(-c(SID,StartDate),names_to='item',values_to='rating') %>%
  mutate(reversed = as.numeric(str_detect(item,"R$"))) %>%
  #reverse score neuroticism
  mutate(reversed = case_when(
    str_detect(item, "_N") & reversed == 1 ~ 0,
    str_detect(item, "_N") & reversed == 0 ~ 1,
    .default = reversed
  )) %>%
  mutate(domain = str_remove(item,"SP_personality_[:digit:]+_") %>%
           str_remove("R$")) %>%
  mutate(rating = ifelse(reversed, 8 - rating, rating)) %>%
  mutate(domain = plyr::mapvalues(domain,c('E','A','C','N','O'), c('EXT','AGR','CON','NEU','OPE'))) %>%
  group_by(SID,StartDate,domain) %>%
  mutate(mean_rating = mean(rating,na.rm=T)) %>%
  ungroup() %>%
  select(SID,StartDate,domain,mean_rating) %>%
  unique() %>%
  rename(Date = StartDate) %>%
  pivot_wider(names_from=domain,values_from=mean_rating) 

s2.starttimes = s2.exc0 %>%
  group_by(SID) %>%
  slice_min(Date) %>%
  ungroup() %>%
  rename(StartDate = Date) %>%
  select(SID,StartDate) %>%
  unique()

s2.exc =s2.exc0 %>%
  merge(s2.starttimes,by=c('SID')) %>%
  mutate(SID = as.character(SID)) %>%
  mutate(sample = 'dt')

#lightly edit names for s3,4,5
s3to5.exc = s3to5.raw %>%
  select(sample,SID,StartDate,Date,E,A,C,N,O) %>%
  rename(EXT = E, AGR = A, CON=C, NEU=N, OPE=O) %>%
  mutate(SID = as.character(SID))

all.exc = bind_rows(s1.exc,s2.exc,s3to5.exc) %>%
  select(sample,SID,StartDate,Date, everything()) %>%
  group_by(sample,SID) %>%
  #get the lag between each survey
  arrange(Date, .by_group = T) %>%
  mutate(tl = Date - lag(Date)) %>%
  mutate(tl = ifelse(is.na(tl), 0,tl)) %>%
  mutate(tl.m = as.numeric(tl)/60) %>%
  ungroup() %>%
  mutate(too.soon = as.numeric(tl.m <10 & tl.m != 0)) %>%
  #get SD across all surveys for each item + person, flag ppl with SD = 0
  group_by(sample,SID) %>%
  mutate(across(c(EXT,AGR,CON,NEU,OPE),~sd(.x,na.rm=T),.names="{.col}.sd"))%>%
  ungroup() %>%
  mutate(across(contains('sd'),~as.numeric(.x==0),.names="{.col}.novar")) %>%
  #get max number of surveys per person
  group_by(sample,SID) %>%
  mutate(max_n = n()) %>%
  ungroup()


#create clean version with exclusions applied
all.clean = all.exc %>%
  #get rid of responses that arrived <10min after the previous one
  filter(!too.soon) %>%
  #wrangle so that we can eliminate cases with no variability
  select(sample:OPE,contains('novar')) %>%
  rename_with(~paste0(.x,'.','rating'),c(EXT,AGR,CON,NEU,OPE)) %>%
  rename_with(~str_remove(.x,'\\.sd')) %>%
  pivot_longer(-c(sample:Date),names_to=c('domain','.value'),names_sep="\\.") %>%
  #remove cases (domains per person) that didn't vary
  filter(!novar) %>%
  #remove NAs
  filter(!is.na(rating)) %>%
  #remove people x domains with < 35 valid cases
  group_by(sample,SID,domain) %>%
  mutate(n = n()) %>%
  ungroup() %>%
  filter(n >=35) %>%
  #remove ppl who we discovered had all but one survey with the same response (post-hoc)
  filter(!(SID %in% c('628','1256_B','18975')))

tmp = all.exc %>%
  filter(sample == 'dt')

#compare before + after-----
all.exc %>%
  select(sample:OPE) %>%
  pivot_longer(-c(sample:Date),names_to='domain',values_to='rating') %>%
  select(sample,domain,SID)%>%
  unique() %>%
  group_by(sample,domain) %>%
  tally() %>%
  pivot_wider(names_from=domain,values_from = n)
  

all.clean %>%
  select(sample,domain,SID)%>%
  unique() %>%
  group_by(sample,domain) %>%
  tally() %>%
  pivot_wider(names_from=domain,values_from = n)


#set up subject-level data + merge w/ traits=====================================

all.aux = all.clean %>%
  group_by(sample,SID,domain) %>%
  summarise(n_obs = n()) 

traits = read_csv("../02-data/02-clean-data/traits/all-traits-bfi-ANON.csv") %>%
  select(sample,SID,domain,is_central) %>%
  mutate(sample = tolower(sample)) %>%
  unique() %>%
  group_by(sample,SID,domain) %>%
  #if ppl had e.g., trait 1 = E but trait 2 = C; then they got duplicate entries; get rid of them
  mutate(n_entries = n()) %>%
  ungroup() %>%
  mutate(is_central = ifelse(n_entries>1,1,is_central)) %>%
  select(-n_entries) %>%
  unique()

all.subjlvl = all.aux %>%
  merge(traits,by=c('sample','SID','domain'))

write_csv(all.subjlvl,"../02-data/02-clean-data/subj-lvl-data.csv")


#prep ESM for ctsem analyses======================================================
#-transform ratings to pomp scores out of 10
#-rescale time variable to get intervals


#functions:
#function for pomp scoring
pomp <- function(x, mini = 1, maxi = 5) ((x - mini)/(maxi- mini))*10

#function to get time intervals + rescale
rescale_time <- function(x){
  x %>%
    arrange(Date) %>%
    mutate(time = difftime(Date,StartDate,unit='hours')) %>%
    as.data.frame()
}

all_nested = all.clean %>%
  #apply pomp scoring
  mutate(score = 
           case_when(
             sample == 'dt' ~ pomp(rating,mini=1,maxi=7),
             .default = pomp(rating)
           )) %>%
  drop_na(Date,domain,score) %>%
  mutate(SID2 = SID) %>%
  group_by(sample,SID2,domain) %>%
  nest() %>%
  #rescale time
  mutate(data=map(data,rescale_time)) %>%
  #in case we missed someone, but the Ns say we didn't
  filter(map_dbl(data, nrow) >= 35) %>%
  rename(SID = SID2) 

#ok 2 people randomly had start dates set to after they started the study??
tmp = all_nested %>%
  rename(SID2 = SID) %>%
  unnest(cols=c(data)) %>%
  filter(time <0)

bad_times = unique(tmp$SID)


#export the data-----------------------------------
save_dat_fun <- function(x, domain, sid, sample){
  if(sd(x$score) > 1){
    write_csv(x, file = sprintf("../02-data/02-clean-data/univ/csv/%s-%s-%s.csv", sample, sid, domain))
    save(x, file = sprintf("../02-data/02-clean-data/univ/rdata/%s-%s-%s.RData", sample, sid, domain))
    return("variable")
  } else {
    return("no variability")
  }
}

all_nested2 = all_nested %>%
  filter(!(SID %in% bad_times)) %>%
  mutate(dat = pmap_chr(list(data, domain, SID, sample), save_dat_fun))
save(all_nested,file="../02-data/02-clean-data/univ/nested_univ.RData")

