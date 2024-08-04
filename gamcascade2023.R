# reported in 2024, but data for 2023
#dev.off()
rm(list=ls())
## load packages
ipak <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(new.pkg))
    install.packages(new.pkg, dependencies = TRUE)
  sapply(pkg, require, character.only = TRUE)
}
# usage
packages <- c("installr", "RPostgreSQL", "RPostgres", "dplyr","lubridate","ggplot2","tidyr",
              "readxl","readr")
ipak(packages)
### import from postgreslq
#con<-dbConnect(PostgreSQL(), user="OTO", password="oto2213", dbname="aidshis230320_GAM2023")
con<-dbConnect(PostgreSQL(), user="postgres", password="oto2213", dbname="aidshis26032024_GAM")
con <- dbConnect(RPostgres::Postgres(),
                 dbname = "aidshis03072024",
                 # host = localhost,
                 port = 5432,
                 user = "postgres",
                 password = "ototata"
)
### set wirking directory
workwd <- "~/OTO/Oficial/NCDC/GAM 2024"
homewd <- "/Users/oto/Documents/OTO/Oficial/NCDC/GAM 2024"
if(isTRUE(file.info(workwd) [1,"isdir"])){
  setwd(workwd);dir() # dir.create is used to creatge not existing directory
}else{
  (isTRUE(file.info(homewd)[1,"isdir"]))
  setwd(homewd);dir()
}
getwd()
### get data ###

##husdb <- read.xlsx("data/hussurv.xls", header=T, sheetName = "hus") ar mushaobs office 2016 excell
##husdb <- read.xlsx("data/hussurvfnl13122017.xls", header=T, sheetName = "hus")
####
dbGetQuery(con, "SHOW CLIENT_ENCODING")
patient<-dbGetQuery(con, "select * from patient where regnum is not null")
visit <- dbGetQuery(con, "select distinct p.regnum as id,
                    date(v.visitdate) as vdate,
                    v.patstatustid as vstatus,
                    v.personellid as prs,
                    v.createdate as crtdate,
                    v.visitcenterid  as vcntr
                    from
                    patient as p
                    inner JOIN visit as v ON(p.patientid=v.patientid)
                    where
                    v.patstatustid != 251
                    ORDER BY
                    vdate desc;")
dball<-dbGetQuery(con, "select distinct p.regnum as id,
                  date(vst.startdate) as labdate,
                  vst.result_num as rslt,
                  vst.personellid as prs,
                  vst.regdate as crtdate,
                  vst.investigationid as vid --,
                  --CURRENT_DATE as d_date
                  from
                  patient as p
                  LEFT JOIN visit as v ON(p.patientid=v.patientid)
                  LEFT JOIN visitstandard as vst ON(v.visitid=vst.visitid)
                  -- INNER JOIN investigation as inv ON(vst.investigationid=inv.investigationid)
                  -- INNER JOIN code as cod ON(p.genderid=cod.codeid)
                  -- INNER JOIN center as c  ON(vst.centerid=c.centerid)
                  where
                  vst.investigationid in(151, 157) and vst.result_num is not null
                  ORDER BY
                  labdate desc;")
dbarv<-dbGetQuery(con, "select p.regnum as id,
                  date(arv.arvgetdate) as arvgdate,
                  date(arv.createdate) as arvcrdate, 
                  arv.personellid as prscode,
                  arv.arvstatusid as arvst,
                  arv.arvcombinationid as arvcmbid
                  from
                  patient as p
                  inner JOIN arvpatient as arv ON(p.patientid=arv.patientid) ;")
dbDisconnect(con)

###  variables ###
maxvdate <- '2023-12-31'
# define reporting year
repyear <- 2023

### newly registered HIv cases##
regs2023 <- patient %>% 
  mutate(ry = year(regdate),
         sex = recode_factor(genderid, '4' = "M", '5' = "F"), 
         age = ry - year(birthdate), 
         agrgsex = case_when(age < 15 ~ "<15",
                             age > 15 & sex == "M" ~ "M15+",
                             age > 15 & sex == "F" ~ "F15+"),
         agrgsexdet = case_when(age < 5 & sex == "M"  ~ "M<5",
                                age >= 5 & age < 10 & sex == "M" ~ "M5-9",
                                age >= 10 & age < 15  & sex == "M" ~ "M10-14",
                                age >= 15 & age < 20 & sex == "M" ~ "M15 - 19",
                                age >= 20 & age < 25 & sex == "M" ~ "M20 - 24",
                                age >= 25 & age < 30 & sex == "M" ~ "M25 - 29",
                                age >= 30 & age < 35 & sex == "M" ~ "M30 - 34",
                                age >= 35 & age < 40 & sex == "M" ~ "M35 - 40",
                                age >= 40 & age < 45 & sex == "M" ~ "M40 - 44",
                                age >= 45 & age < 50 & sex == "M" ~ "M45 - 49",
                                age >= 50 & sex == "M" ~ "M50+",
                                age < 5  & sex == "F" ~ "F<5",
                                age >= 5 & age < 10 & sex == "F" ~ "F5-9",
                                age >= 10 & age < 15 & sex == "F" ~ "F10-14",
                                age >= 15 & age < 20 & sex == "F" ~ "F15 - 19",
                                age >= 20 & age < 25 & sex == "F" ~ "F20 - 24",
                                age >= 25 & age < 30 & sex == "F" ~ "F25 - 29",
                                age >= 30 & age < 35 & sex == "F" ~ "F30 - 34",
                                age >= 35 & age < 40 & sex == "F" ~ "F35 - 40",
                                age >= 40 & age < 45 & sex == "F" ~ "F40 - 44",
                                age >= 45 & age < 50 & sex == "F" ~ "F45 - 49",
                                age >= 50 & sex == "F" ~ "F50+",
                                
         )
  ) %>%
  filter(ry  == repyear)

glimpse(regs2023)
## countall
regs2023 %>%
  count()
## countsex
regs2023 %>%
  count(sex)
## count 
regs2023 %>%
  count(agrgsexdet)



### In care from newly registereds ##
incare2023 <- patient %>% inner_join(visit, by = c("regnum"="id")) %>%
  mutate(ry = year(regdate), vy = year(vdate)) %>%
  filter(ry  == repyear) %>%
  arrange(regnum, regdate) %>%
  group_by(regnum) %>%
  count()
glimpse(incare2023)

##Percent in care form new cases
dnmnnewcasesd <-  nrow(regs2023)
nnewcasesd <-  nrow(incare2023)
prcntfromnewcases <-  round((nnewcasesd/dnmnnewcasesd) * 100, 0)
paste("NoOfRegisterdHIV: ", prcntfromnewcases, "%", sep = "")

### Late diagnosis CD4<350 or CD4<200 ##
latediangosis <- patient %>% 
  inner_join(dball, by = c("regnum"="id")) %>%
  mutate(ry = year(regdate), cd4y = year(labdate)) %>%
  filter(vid == 157, ry  == repyear, cd4y == repyear) %>%
  arrange(regnum, regdate, labdate, rslt) %>%
  group_by(regnum) %>%
  summarise(cd4fst = first(rslt), cd4fbtd = min(labdate), lgenderid = first(genderid), 
            lregdate = min(regdate), lbirthdate = min(birthdate), ry = last(ry)) %>%
  mutate(sex = recode_factor(lgenderid, '4' = "M", '5' = "F"), lry = year(lregdate),
         lcd4y = year(cd4fbtd), age = ry - year(lbirthdate), 
         agrgsex = case_when(age < 15 ~ "<15",
                             age > 15 & sex == "M" ~ "M15+",
                             age > 15 & sex == "F" ~ "F15+"),
         cd4200 =  ifelse(cd4fst < 200, "<200", "200+"),
         cd4350 =  ifelse(cd4fst < 350, "<350", "350+")
  ) %>%
  select(regnum, lry, lcd4y, sex, age, agrgsex, cd4fst, cd4200, cd4350)

#%>% count(sex)
latediangosis
#setwd("..")
getwd()
write.csv(latediangosis,  paste0(getwd(), "/data/cd4results", Sys.Date(), ".csv", sep = ""), row.names=F, na="")

table(latediangosis$agrgsex, latediangosis$cd4200) 
table(latediangosis$agrgsex, latediangosis$cd4350) 
table(latediangosis$agrgsex) 

### detect last visit
vlst <- visit %>% 
  arrange(vdate, vstatus) %>% 
  #  filter(vdate <= maxvdate) %>%
  group_by(id) %>% 
  summarize(lastvdate = max(vdate), visstt = last(vstatus)) ## select last visit in visits , centri = last(vcntr) 
glimpse(vlst)

write.csv(vlst,  paste0(getwd(), "/data/vlst", Sys.Date(), ".csv", sep = ""), row.names=F, na="")

### anonimous ##
anonims <- read.csv("data/anonimous.csv", header=T)

# filter, registrarion year before 2023 and exclude anonimous and left coutries##
hivdb <- patient %>%  
  left_join(vlst, by = c("regnum" = "id")) %>%
  anti_join(anonims, by = c("regnum" = "id")) %>% # mutate(vdt = year(lst), ddt = year(deathfixdate))
  mutate(sex = ifelse(genderid == 4, "M", "F"), 
         rgy = year(regdate),
         dob = year(birthdate),
         age = repyear - dob,
         deathy = year(deathdate),
         trroute = case_when(transferid == '47' ~ "IDU", 
                             transferid == '48' & sex == "M" ~ "MHETERO",
                             transferid == '48' & sex == "F" ~ "FHETERO",
                             transferid == '49' ~ "HOMO", 
                             transferid %in% c('50', '51', '52') ~ "OTHER",
                             is.na(transferid) ~ "OTHER"),
         agrgsex = case_when(age < 15 ~ "<15",
                             age >= 15 & sex == "M" ~ "M15+",
                             age >= 15 & sex == "F" ~ "F15+"),
         agect = cut(age, c(-1,14,24,1000), c("0-14","15-24", "25+"))) %>%
  filter(!is.na(regnum), rgy <= repyear, (is.na(deathy) | deathy >repyear), (is.na(visstt) | visstt != 255)) %>% # (!is.na(visstt) |
  select(c(2, 57, 19, 87, 88, 89, 92, 94, 95, 96, sex, agect, rgy))
names(hivdb)
glimpse(hivdb)

cascadeall <- hivdb %>%  count(agrgsex) ## 88,91
names(cascadeall)
glimpse(cascadeall)


### arv in 2023
arv23a <- read_excel(paste0(getwd(),"/data/art2023.xlsx"), sheet = "art2023")
arv23a <- arv23a %>% select(1) %>% 
  mutate(id = as.integer(id))
glimpse(arv23a)
### 
arv23 <- dbarv %>% 
  filter(arvgdate >='2023-12-01', arvgdate <= '2023-12-31', 
         !arvst %in% c(268, 302, 354, 357)) %>% 
  group_by(id) %>% summarise(id = last(id)) %>% select(id) %>% arrange(id) %>% 
  mutate(id = as.integer(id))
arv23
glimpse(arv23)

identical(arv23a, arv23)
setdiff(arv23, arv23a)

# ART by age groups##
artgr <- patient %>%  inner_join(arv23a, by = c("regnum" = "id")) %>%
  mutate(sex = ifelse(genderid == 4, "M", "F"), 
         rgy = year(regdate),
         dob = year(birthdate),
         age = repyear - dob,
         deathy = year(deathdate),
         trroute = case_when(transferid == '47' ~ "IDU", 
                             transferid == '48' & sex == "M" ~ "MHETERO",
                             transferid == '48' & sex == "F" ~ "FHETERO",
                             transferid == '49' ~ "HOMO", 
                             transferid %in% c('50', '51', '52') ~ "OTHER",
                             is.na(transferid) ~ "OTHER"),
         agrgsex = case_when(age < 15 ~ "<15",
                             age >= 15 & sex == "M" ~ "M15+",
                             age >= 15 & sex == "F" ~ "F15+"),
         agrgsexidujst = case_when(age < 18 ~ "<18", ## age group categories for ministry of justice
                                   age >= 18 & age < 25  ~ "18-24",
                                   age >= 25 & age < 30  ~ "25-29",
                                   age >= 30 & age < 35  ~ "30-34",
                                   age >= 35 & age < 40  ~ "35-39",
                                   age >= 40 & age < 45  ~ "40-44",
                                   age >= 45 & age < 50  ~ "45-49",
                                   age >= 50 & age < 55  ~ "50-54",
                                   age >= 55 & age < 60  ~ "55-59",
                                   age >= 60 & age < 65  ~ "60-64",
                                   age >= 65 ~ "65+"),
         agrgsexdet = case_when(age < 5 & sex == "M"  ~ "M<5",
                                age >= 5 & age < 10 & sex == "M" ~ "M5-9",
                                age >= 10 & age < 15  & sex == "M" ~ "M10-14",
                                age >= 15 & age < 20 & sex == "M" ~ "M15 - 19",
                                age >= 20 & age < 25 & sex == "M" ~ "M20 - 24",
                                age >= 25 & age < 50 & sex == "M" ~ "M25 - 49",
                                age >= 50 & sex == "M" ~ "M50+",
                                age < 5  & sex == "F" ~ "F<5",
                                age >= 5 & age < 10 & sex == "F" ~ "F5-9",
                                age >= 10 & age < 15 & sex == "F" ~ "F10-14",
                                age >= 15 & age < 20 & sex == "F" ~ "F15 - 19",
                                age >= 20 & age < 25 & sex == "F" ~ "F20 - 24",
                                age >= 25 & age < 50 & sex == "F" ~ "F25 - 49",
                                age >= 50 & sex == "F" ~ "F50+")) %>%
  filter(!is.na(regnum), rgy <=repyear) %>%
  select(c(2,92, 93, 94, sex, age, agrgsexdet, agrgsex, agrgsexidujst)) 
names(artgr)
glimpse(artgr)

artgr %>%
  count(agrgsexdet) 

### report for ministry of justice. N of IDU on ART.
artgr %>% filter(trroute == "IDU") %>%
  count() 



## N of HIV patients initiated ART in repoting year
artNEWry <- patient %>% 
  mutate(arvy = year(arvdate),
         dob = year(birthdate),
         sex = ifelse(genderid == 4, "M", "F"), 
         age = repyear - dob,
         trroute = ifelse(transferid == '47', "IDU", "OTHER"),
         agrg = case_when(age < 5 ~ "<5",
                          age <15 ~ "5-15",
                          age >= 15 ~ "15+")) %>% 
  filter(arvy == repyear) %>% 
  select(regnum, arvy, agrg, sex, trroute) %>% 
  count(agrg, sex, trroute)
head(artNEWry)

## N of HIV patients initaited ART by month categories 3,5 month ART
arv23by3and6 <- dbarv %>% 
  filter(arvgdate >='2023-12-01', arvgdate <= '2023-12-31', 
         !arvst %in% c(268, 302, 354, 357)) %>% 
  mutate(arvstnew = case_when(arvst %in% c(530, 531, 607) ~  "3 month",
                              arvst %in% c(599, 600) ~  "6 month",
                              TRUE ~ "under 3"
  )) %>% 
  group_by(id) %>% summarise(id = last(id), arvt = last(arvstnew)) %>%
  select(id, arvt) %>% 
  arrange(id)
arv23by3and6
glimpse(arv23by3and6)

artNEWrybyd <- patient %>% 
  inner_join(arv23by3and6, by = c("regnum" = "id")) %>%
  mutate(arvy = year(arvdate),
         dob = year(birthdate),
         sex = ifelse(genderid == 4, "M", "F"), 
         age = repyear - dob,
         agrgsex = case_when(age < 15 ~ "<15",
                             age >= 15 & sex == "M" ~ "M15+",
                             age >= 15 & sex == "F" ~ "F15+")) %>% 
  # filter(arvy == 2023) %>% 
  select(regnum, arvy, agrgsex, arvt) %>% 
  count(agrgsex, arvt)
artNEWrybyd
head(artNEWrybyd)


# sre <- setdiff(arv20$id, artgr$regnum)
# sre

### at least one VL in 2023
head(dball)
atlstoneVL <- dball %>% mutate(vy = year(labdate)) %>%
  filter(vid == 151) %>% ## თუ გვინდა მხოლოდ 2023 მაშინ დავტოვოთ ტოლობა
  arrange(labdate) %>% group_by(id) %>% 
  summarize(lVLdt = max(labdate), lvl = last(rslt)) %>% 
  #filter(fcd4 < 350) %>%
  ungroup() 
atlstoneVLcnt <- atlstoneVL %>% count()
head(atlstoneVLcnt) ### number of cases with first VL count less than 350


### routine VL testing in 2023
head(dball)
rutinevltest <- dball %>%
  filter(vid == 151) %>% ## თუ გვინდა მხოლოდ 2023 მაშინ დავტოვოთ ტოლობა
  arrange(labdate) %>% group_by(id) %>% 
  count(id) %>%
  filter(n >1)
rutinevltest
rutinevltestCNT <- rutinevltest %>% select(id) %>% count()
head(rutinevltestCNT) ### number of cases with first cd4 count less than 350


## at least one Vl among those on art in 2023
vlarv <- rutinevltest %>% # adjara cases on art vlunder1000 and vlunder 400
  inner_join(arv23a, by = c("id" = "id"))  %>%
  inner_join(patient, by = c("id" = "regnum")) %>%
  mutate(sex = ifelse(genderid == 4, "M", "F"), 
         rgy = year(regdate),
         dob = year(birthdate),
         age = repyear - dob,
         deathy = year(deathdate),
         trroute = case_when(transferid == '47' ~ "IDU", 
                             transferid == '48' & sex == "M" ~ "MHETERO",
                             transferid == '48' & sex == "F" ~ "FHETERO",
                             transferid == '49' ~ "HOMO", 
                             transferid %in% c('50', '51', '52') ~ "OTHER",
                             is.na(transferid) ~ "OTHER"),
         agrgsex = case_when(age < 15 ~ "<15",
                             age >= 15 & sex == "M" ~ "M15+",
                             age >= 15 & sex == "F" ~ "F15+")) %>%
  filter(rgy <=repyear) %>%
  select(c(1,3, 94, 95, agrgsex)) %>%
  count(agrgsex) %>% 
  ungroup()
names(vlarv)
glimpse(vlarv)
write.csv(vlarv, paste0(getwd(),"/data/vllab", Sys.Date(), ".csv", sep = ""), row.names=F, na="")

vlarv %>% select(-id) %>% 
  count(agrgsex) 

## at least one UNDETECTABLE Vl among those on art in 2023
vlarvUND <- rutinevltest %>% # adjara cases on art vlunder1000 and vlunder 400
  inner_join(atlstoneVL, by = c("id" = "id"))  %>%
  inner_join(arv23a, by = c("id" = "id"))  %>%
  inner_join(patient, by = c("id" = "regnum")) %>%
  mutate(sex = ifelse(genderid == 4, "M", "F"), 
         rgy = year(regdate),
         dob = year(birthdate),
         age = repyear - dob,
         deathy = year(deathdate),
         trroute = case_when(transferid == '47' ~ "IDU", 
                             transferid == '48' & sex == "M" ~ "MHETERO",
                             transferid == '48' & sex == "F" ~ "FHETERO",
                             transferid == '49' ~ "HOMO", 
                             transferid %in% c('50', '51', '52') ~ "OTHER",
                             is.na(transferid) ~ "OTHER"),
         agrgsex = case_when(age < 15 ~ "<15",
                             age >= 15 & sex == "M" ~ "M15+",
                             age >= 15 & sex == "F" ~ "F15+")) %>%
  filter(rgy <=repyear, lvl < 1000) %>%
  select(c(1,3, 94, 95, agrgsex, sex)) %>%
  count(agrgsex) %>% 
  ungroup()
names(vlarvUND)
glimpse(vlarvUND)
write.csv(vlarvUND, paste0(getwd(),"/data/UNDETvllab", Sys.Date(), ".csv", sep = ""), row.names=F, na="")

vlarvUND %>% select(-id) %>% 
  count(agrgsex) 

#Indicator 1.3 Sub-numerator People who are virally suppressed among those tested routinely for viral load
vlarvrouteine <- atlstoneVL %>% # adjara cases on art vlunder1000 and vlunder 400
  inner_join(arv23a, by = c("id" = "id"))  %>%
  inner_join(patient, by = c("id" = "regnum")) %>%
  mutate(sex = ifelse(genderid == 4, "M", "F"), 
         rgy = year(regdate),
         dob = year(birthdate),
         age = repyear - dob,
         deathy = year(deathdate),
         trroute = case_when(transferid == '47' ~ "IDU", 
                             transferid == '48' & sex == "M" ~ "MHETERO",
                             transferid == '48' & sex == "F" ~ "FHETERO",
                             transferid == '49' ~ "HOMO", 
                             transferid %in% c('50', '51', '52') ~ "OTHER",
                             is.na(transferid) ~ "OTHER"),
         agrgsex = case_when(age < 15 ~ "<15",
                             age >= 15 & sex == "M" ~ "M15+",
                             age >= 15 & sex == "F" ~ "F15+")) %>%
  filter(rgy <=repyear, lvl < 1000) %>%
  select(c(1,3, 94, 95, agrgsex)) %>%
  count(agrgsex)
names(vlarvrouteine)
glimpse(vlarvrouteine)
write.csv(vlarvrouteine, paste0(getwd(),"/data/vllab", Sys.Date(), ".csv", sep = ""), row.names=F, na="")


#Indicator 1.3 Sub-numerator People who are virally suppressed among those tested routinely for viral load
vlarvrouteine13 <- atlstoneVL %>% # adjara cases on art vlunder1000 and vlunder 400
  inner_join(arv23a, by = c("id" = "id"))  %>%
  inner_join(patient, by = c("id" = "regnum")) %>%
  mutate(sex = ifelse(genderid == 4, "M", "F"), 
         rgy = year(regdate),
         dob = year(birthdate),
         age = repyear - dob,
         deathy = year(deathdate),
         trroute = case_when(transferid == '47' ~ "IDU", 
                             transferid == '48' & sex == "M" ~ "MHETERO",
                             transferid == '48' & sex == "F" ~ "FHETERO",
                             transferid == '49' ~ "HOMO", 
                             transferid %in% c('50', '51', '52') ~ "OTHER",
                             is.na(transferid) ~ "OTHER"),
         agrgsex = case_when(age < 5 ~ "<5",
                             age >= 5 & age < 10 ~ "5-9",
                             age >= 10 & age < 15 ~ "10-14",
                             age >= 15 & age < 20 & sex == "M" ~ "M15 - 19",
                             age >= 20 & age < 25 & sex == "M" ~ "M20 - 24",
                             age >= 25 & age < 50 & sex == "M" ~ "M25 - 49",
                             age >= 50 & sex == "M" ~ "M50+",
                             age >= 15 & age < 20 & sex == "F" ~ "F15 - 19",
                             age >= 20 & age < 25 & sex == "F" ~ "F20 - 24",
                             age >= 25 & age < 50 & sex == "F" ~ "F25 - 49",
                             age >= 50 & sex == "F" ~ "F50+")) %>%
  filter(rgy <=repyear, lvl < 1000) %>%
  select(c(1,3, 94, 95, agrgsex)) %>%
  count(agrgsex)
names(vlarvrouteine13)
glimpse(vlarvrouteine13)
write.csv(vlarvrouteine13, paste0(getwd(),"/data/vlarvrouteine13", Sys.Date(), ".csv", sep = ""), row.names=F, na="")



### Indicator 1.3 Denumerator People living with HIV on antiretroviral treatment who had viral load test
vlarvUND13 <- atlstoneVL %>% # 
  inner_join(arv23a, by = c("id" = "id"))  %>%
  inner_join(patient, by = c("id" = "regnum")) %>%
  mutate(sex = ifelse(genderid == 4, "M", "F"), 
         rgy = year(regdate),
         dob = year(birthdate),
         age = repyear - dob,
         deathy = year(deathdate),
         trroute = case_when(transferid == '47' ~ "IDU", 
                             transferid == '48' & sex == "M" ~ "MHETERO",
                             transferid == '48' & sex == "F" ~ "FHETERO",
                             transferid == '49' ~ "HOMO", 
                             transferid %in% c('50', '51', '52') ~ "OTHER",
                             is.na(transferid) ~ "OTHER"),
         agrgsex = case_when(age < 5 ~ "<5",
                             age >= 5 & age < 10 ~ "5-9",
                             age >= 10 & age < 15 ~ "10-14",
                             age >= 15 & age < 20 & sex == "M" ~ "M15 - 19",
                             age >= 20 & age < 25 & sex == "M" ~ "M20 - 24",
                             age >= 25 & age < 50 & sex == "M" ~ "M25 - 49",
                             age >= 50 & sex == "M" ~ "M50+",
                             age >= 15 & age < 20 & sex == "F" ~ "F15 - 19",
                             age >= 20 & age < 25 & sex == "F" ~ "F20 - 24",
                             age >= 25 & age < 50 & sex == "F" ~ "F25 - 49",
                             age >= 50 & sex == "F" ~ "F50+")) %>%
  filter(rgy <=repyear) %>%
  select(c(1,3, 94, 95, agrgsex)) %>%
  count(agrgsex)
names(vlarvUND13)
glimpse(vlarvUND13)
write.csv(vlarvUND13, paste0(getwd(),"/data/vlarvUND13", Sys.Date(), ".csv", sep = ""), row.names=F, na="")

### VL done in 2023
head(dball)
vldone <- dball %>% mutate(vy = year(labdate)) %>%
  filter(vy == repyear, vid == 151) %>%
  # arrange(labdate,rslt) %>% group_by(id) %>% 
  # summarize(lvldtdn = max(labdate), lvlrsdn = last(rslt)) %>% 
  # ungroup() #%>% 
  count()
vldone
head(vldone) 


## at least one VL among those on art in 2023
Vlonearv <- atlstoneVL %>% # adjara cases on art vlunder1000 and vlunder 400
  inner_join(arv23, by = c("id" = "id")) 
Vlonearv %>% count()
glimpse(Vlonearv)
write.csv(Vlonearv, paste0(getwd(),"/data/vlartonelab", Sys.Date(), ".csv", sep = ""), row.names=F, na="")



## people initiated or reinitiated art in 2023
arv23stre <- dbarv %>% 
  mutate(ay = year(arvgdate)) %>%
  filter(ay == repyear, arvst %in% c(266, 267)) %>%  # 266 ART start, 267 ART restart
  arrange(arvst, id) %>% 
  group_by(id) %>% 
  summarise(arvfst = first(arvst), rearvt = last(arvst))  %>% 
  select(id, arvfst, rearvt) 
arv23stre

# Check the people start art by "artdate" and arvgetdate those with status "266 დაიწყო" 
arv23fromdb <- arv23stre %>% 
  filter(arvfst == 266) %>% 
  select(id)

arv2023 <- patient %>%
  mutate(ay = year(arvdate), id = regnum) %>%
  filter(ay == repyear, !is.na(regnum)) %>%
  select(id)
glimpse(arv2023)


setdiff(arv2023, arv23fromdb)
setdiff(arv23fromdb, arv2023)

# People initiated ART by age groups##
art2023 <- patient %>%  
  inner_join(arv23stre, by = c("regnum" = "id")) %>%
  mutate(sex = ifelse(genderid == 4, "M", "F"), 
         rgy = year(regdate),
         dob = year(birthdate),
         ay = year(arvdate),
         age = repyear - dob,
         deathy = year(deathdate),
         trroute = case_when(transferid == '47' ~ "IDU", 
                             transferid == '48' & sex == "M" ~ "MHETERO",
                             transferid == '48' & sex == "F" ~ "FHETERO",
                             transferid == '49' ~ "HOMO", 
                             transferid %in% c('50', '51', '52') ~ "OTHER",
                             is.na(transferid) ~ "OTHER"),
         agrgsex = case_when(age < 15 ~ "<15",
                             age >= 15 & sex == "M" ~ "M15+",
                             age >= 15 & sex == "F" ~ "F15+")) %>%
  select(2,86, 87, 95, 96, arvfst, agrgsex) %>%
  filter(arvfst == 266) %>%
  group_by(regnum) %>%
  summarise(lid = last(regnum), agrsex = last(agrgsex)) %>%
  count(agrsex) 
names(art2023)
glimpse(art2023)


# People REinitiated ART by age groups##
reart2023 <- patient %>%  inner_join(arv23stre, by = c("regnum" = "id")) %>%
  mutate(sex = ifelse(genderid == 4, "M", "F"), 
         rgy = year(regdate),
         dob = year(birthdate),
         ay = year(arvdate),
         age = repyear - dob,
         deathy = year(deathdate),
         trroute = case_when(transferid == '47' ~ "IDU", 
                             transferid == '48' & sex == "M" ~ "MHETERO",
                             transferid == '48' & sex == "F" ~ "FHETERO",
                             transferid == '49' ~ "HOMO", 
                             transferid %in% c('50', '51', '52') ~ "OTHER",
                             is.na(transferid) ~ "OTHER"),
         agrgsex = case_when(age < 15 ~ "<15",
                             age >= 15 & sex == "M" ~ "M15+",
                             age >= 15 & sex == "F" ~ "F15+")) %>%
  select(2,86, 87, 95, 96, rearvt, agrgsex) %>%
  filter(rearvt == 267) %>%
  group_by(regnum) %>%
  summarise(lid = last(regnum), agrsex = last(agrgsex)) %>%
  count(agrsex)
names(reart2023)
glimpse(reart2023)

### n of CD4 test done in repyear  
head(dball)
lstcd4in23 <- dball %>% mutate(vy = year(labdate)) %>%
  filter(vy== repyear, vid == 157) %>%
  #  arrange(rslt) %>% group_by(regnum, rgy) %>% 
  #  summarize(fcd4dt = min(labdate), fcd4 = first(rslt)) %>% 
  #filter(fcd4 < 350) %>% 
  #  ungroup() %>%
  count()
head(lstcd4in23) ### number of cases with first cd4 count less than 350



### at least one CD4 in 2023
head(dball)
atlstonecd4 <- dball %>% mutate(vy = year(labdate)) %>%
  filter(vy == repyear, vid == 157) %>%
  arrange(rslt) %>% group_by(id, vy) %>% 
  summarize(lcd4dt = max(labdate), lcd4 = last(rslt)) %>% 
  #filter(fcd4 < 350) %>%
  ungroup() 
atlstonecd4cnt <- atlstonecd4 %>% count()
atlstonecd4cnt ### number of cases with first cd4 count less than 350

## at least one cd4 among those on art in 2023
cd4arv <- atlstonecd4 %>% # adjara cases on art vlunder1000 and vlunder 400
  inner_join(arv23, by = c("id" = "id")) 
cd4arv %>% count()
glimpse(cd4arv)
write.csv(cd4arv, paste0(getwd(),"/data/cd4lab", Sys.Date(), ".csv", sep = ""), row.names=F, na="")

######## cascade all dublin ######


### detect last visit
vlstfrst <- visit %>% 
  arrange(vdate, vstatus) %>% 
  #  filter(vdate <= maxvdate) %>%
  group_by(id) %>% 
  mutate(id = as.integer(id)) %>% 
  summarize(frstvdate = first(vdate), visstt = first(vstatus)) ## select last visit in visits , centri = last(vcntr) 
glimpse(vlstfrst)

# N of new HIV cases engaged into care##
newhivincare <- patient %>%  
  inner_join(vlstfrst, by = c("regnum" =  "id")) %>% 
  mutate(sex = ifelse(genderid == 4, "M", "F"), 
         rgy = year(regdate),
         dob = year(birthdate),
         age = repyear - dob,
         deathy = year(deathdate),
         trroute = case_when(transferid == '47' ~ "IDU", 
                             transferid == '48' & sex == "M" ~ "MHETERO",
                             transferid == '48' & sex == "F" ~ "FHETERO",
                             transferid == '49' ~ "HOMO", 
                             transferid %in% c('50', '51', '52') ~ "OTHER",
                             is.na(transferid) ~ "OTHER"),
         agrgsex = case_when(age < 15 ~ "<15",
                             age >= 15 & sex == "M" ~ "M15+",
                             age >= 15 & sex == "F" ~ "F15+"),
         df3mo = difftime(frstvdate, regdate, units = "days"),
         rg3month = ifelse(as.numeric(df3mo) <= 90, "3 month", "3M+")) %>%
  filter(!is.na(regnum), rgy == repyear) %>% # (!is.na(visstt) |
  select(c(2,94, 92, 93, 95, 89, 87,  transferid, agrgsex, regdate, frstvdate, df3mo, rg3month))

NROW(unique(newhivincare$regnum))

newhivincare %>% count(df3mo) ## 88,91
newhivincare %>% count(rg3month) ## 88,91

names(newhivincare)
glimpse(newhivincare)

# filtre, registrarion year before 2023 and exclude left coutries##
hivdbcascade <- patient %>%  
  left_join(vlst, by = c("regnum" = "id")) %>%
  anti_join(anonims, by = c("regnum" = "id")) %>% # mutate(vdt = year(lst), ddt = year(deathfixdate))
  mutate(sex = ifelse(genderid == 4, "M", "F"), 
         rgy = year(regdate),
         dob = year(birthdate),
         age = repyear - dob,
         deathy = year(deathdate),
         trroute = case_when(transferid == '47' ~ "IDU", 
                             transferid == '48' & sex == "M" ~ "MHETERO",
                             transferid == '48' & sex == "F" ~ "FHETERO",
                             transferid == '49' ~ "HOMO", 
                             transferid %in% c('50', '51', '52') ~ "OTHER",
                             is.na(transferid) ~ "OTHER"),
         agrgsex = case_when(age < 15 ~ "<15",
                             age >= 15 & sex == "M" ~ "M15+",
                             age >= 15 & sex == "F" ~ "F15+")) %>%
  filter(!is.na(regnum), rgy <= repyear, (is.na(deathy) | deathy >repyear), (is.na(visstt) | visstt != 255)) %>% # (!is.na(visstt) |
  select(c(2,94, 92, 93, 95, 89, 87,  transferid, agrgsex))

NROW(unique(hivdbcascade$regnum))

hivdbcascade %>% count(agrgsex) ## 88,91
hivdbcascade %>% count(trroute) ## 88,91

names(hivdbcascade)
glimpse(hivdbcascade)

# incare 2023 and exclude left coutries##
incarecascade <- patient %>%  
  inner_join(vlst, by = c("regnum" = "id")) %>%
  anti_join(anonims, by = c("regnum" = "id")) %>% # mutate(vdt = year(lst), ddt = year(deathfixdate))
  mutate(sex = ifelse(genderid == 4, "M", "F"), 
         rgy = year(regdate),
         dob = year(birthdate),
         age = repyear - dob,
         deathy = year(deathdate),
         trroute = case_when(transferid == '47' ~ "IDU", 
                             transferid == '48' & sex == "M" ~ "MHETERO",
                             transferid == '48' & sex == "F" ~ "FHETERO",
                             transferid == '49' ~ "HOMO", 
                             transferid %in% c('50', '51', '52') ~ "OTHER",
                             is.na(transferid) ~ "OTHER"),
         agrgsex = case_when(age < 15 ~ "<15",
                             age >= 15 & sex == "M" ~ "M15+",
                             age >= 15 & sex == "F" ~ "F15+")) %>%
  filter(!is.na(regnum), rgy <= repyear, (is.na(deathy) | deathy >repyear), (is.na(visstt) | visstt != 255)) %>% # (!is.na(visstt) |
  select(c(2,94, 92, 93, 95, 89, 87, transferid, agrgsex))

NROW(unique(incarecascade$regnum))

incarecascade %>% count(agrgsex) ## 88,91
incarecascade %>% count(trroute) ## 88,91

names(incarecascade)
glimpse(incarecascade)

q1 <- hivdbcascade$regnum[hivdbcascade$agrgsex == "<15"]
q2 <- incarecascade$regnum[incarecascade$agrgsex == "<15"]
setdiff(q2,q1)


# CASCADE ART by age groups##
artcascade <- patient %>%  
  inner_join(arv23a, by = c("regnum" = "id")) %>%
  mutate(sex = ifelse(genderid == 4, "M", "F"), 
         rgy = year(regdate),
         dob = year(birthdate),
         age = repyear - dob,
         deathy = year(deathdate),
         trroute = case_when(transferid == '47' ~ "IDU", 
                             transferid == '48' & sex == "M" ~ "MHETERO",
                             transferid == '48' & sex == "F" ~ "FHETERO",
                             transferid == '49' ~ "HOMO", 
                             transferid %in% c('50', '51', '52') ~ "OTHER",
                             is.na(transferid) ~ "OTHER"),
         agrgsex = case_when(age < 15 ~ "<15",
                             age >= 15 & sex == "M" ~ "M15+",
                             age >= 15 & sex == "F" ~ "F15+")) %>%
  filter(!is.na(regnum), rgy <=repyear) %>%
  select(c(2, 91, 92, 93, agrgsex))

NROW(unique(artcascade$regnum))

artcascade %>% count(agrgsex) ## 88,91
artcascade %>% count(trroute) ## 88,91

names(artcascade)
glimpse(artcascade)


# 2023
# CASCADE ART by age groups##
artcascade <- patient %>%  
  inner_join(arv23, by = c("regnum" = "id")) %>%
  mutate(sex = ifelse(genderid == 4, "M", "F"), 
         rgy = year(regdate),
         dob = year(birthdate),
         age = repyear - dob,
         deathy = year(deathdate),
         trroute = case_when(transferid == '47' ~ "IDU", 
                             transferid == '48' & sex == "M" ~ "MHETERO",
                             transferid == '48' & sex == "F" ~ "FHETERO",
                             transferid == '49' ~ "HOMO", 
                             transferid %in% c('50', '51', '52') ~ "OTHER",
                             is.na(transferid) ~ "OTHER"),
         agrgsex = case_when(age < 15 ~ "<15",
                             age >= 15 & sex == "M" ~ "M15+",
                             age >= 15 & sex == "F" ~ "F15+")) %>%
  filter(!is.na(regnum), rgy <=repyear) %>%
  select(c(2, 91, 92, 93, agrgsex))

NROW(unique(artcascade$regnum))

artcascade %>% count(agrgsex) ## 88,91
artcascade %>% count(trroute) ## 88,91

names(artcascade)
glimpse(artcascade)


## cascade at least one Vl among those on art in 2023
vlarvcascade <- atlstoneVL %>% 
  inner_join(arv23a, by = c("id" = "id"))  %>%
  inner_join(patient, by = c("id" = "regnum")) %>%
  mutate(sex = ifelse(genderid == 4, "M", "F"), 
         rgy = year(regdate),
         dob = year(birthdate),
         age = repyear - dob,
         deathy = year(deathdate),
         trroute = case_when(transferid == '47' ~ "IDU", 
                             transferid == '48' & sex == "M" ~ "MHETERO",
                             transferid == '48' & sex == "F" ~ "FHETERO",
                             transferid == '49' ~ "HOMO", 
                             transferid %in% c('50', '51', '52') ~ "OTHER",
                             is.na(transferid) ~ "OTHER"),
         agrgsex = case_when(age < 15 ~ "<15",
                             age >= 15 & sex == "M" ~ "M15+",
                             age >= 15 & sex == "F" ~ "F15+")) %>%
  filter(rgy <=repyear) %>%
  select(c(1,93, 94, 95, agrgsex))

NROW(unique(vlarvcascade$id))

vlarvcascade %>% count(agrgsex) ## 88,91
vlarvcascade %>% count(trroute) ## 88,91

names(vlarvcascade)
glimpse(vlarvcascade)

## cascade vl 1000 at least one UNDETECTABLE Vl among those on art in 2023
vlarvcascade1000 <- atlstoneVL %>% # adjara cases on art vlunder1000 and vlunder 400
  inner_join(arv23a, by = c("id" = "id"))  %>%
  inner_join(patient, by = c("id" = "regnum")) %>%
  mutate(sex = ifelse(genderid == 4, "M", "F"), 
         rgy = year(regdate),
         dob = year(birthdate),
         age = repyear - dob,
         deathy = year(deathdate),
         trroute = case_when(transferid == '47' ~ "IDU", 
                             transferid == '48' & sex == "M" ~ "MHETERO",
                             transferid == '48' & sex == "F" ~ "FHETERO",
                             transferid == '49' ~ "HOMO", 
                             transferid %in% c('50', '51', '52') ~ "OTHER",
                             is.na(transferid) ~ "OTHER"),
         agrgsex = case_when(age < 15 ~ "<15",
                             age >= 15 & sex == "M" ~ "M15+",
                             age >= 15 & sex == "F" ~ "F15+")) %>%
  filter(rgy <=repyear, lvl < 1000) %>%
  select(c(1,93, 94, 95, agrgsex))

NROW(unique(vlarvcascade1000$id))

vlarvcascade1000 %>% count(agrgsex) ## 88,91
vlarvcascade1000 %>% count(trroute) ## 88,91

names(vlarvcascade1000)
glimpse(vlarvcascade1000)


## cascade vl 500 at least one UNDETECTABLE Vl among those on art in 2023
vlarvcascade500 <- atlstoneVL %>% # adjara cases on art vlunder1000 and vlunder 400
  inner_join(arv23a, by = c("id" = "id"))  %>%
  inner_join(patient, by = c("id" = "regnum")) %>%
  mutate(sex = ifelse(genderid == 4, "M", "F"), 
         rgy = year(regdate),
         dob = year(birthdate),
         age = repyear - dob,
         deathy = year(deathdate),
         trroute = case_when(transferid == '47' ~ "IDU", 
                             transferid == '48' & sex == "M" ~ "MHETERO",
                             transferid == '48' & sex == "F" ~ "FHETERO",
                             transferid == '49' ~ "HOMO", 
                             transferid %in% c('50', '51', '52') ~ "OTHER",
                             is.na(transferid) ~ "OTHER"),
         agrgsex = case_when(age < 15 ~ "<15",
                             age >= 15 & sex == "M" ~ "M15+",
                             age >= 15 & sex == "F" ~ "F15+")) %>%
  filter(rgy <=repyear, lvl < 200) %>%
  select(c(1,93, 94, 95, agrgsex))

NROW(unique(vlarvcascade500$id))

vlarvcascade500 %>% count(agrgsex) ## 88,91
vlarvcascade500 %>% count(trroute) ## 88,91

names(vlarvcascade500)
glimpse(vlarvcascade500)


### form IV-17 for NCDC ####
cascadencdc <- hivdb %>%  count(sex, agect) ## 88,91
names(cascadencdc)
glimpse(cascadencdc)
table(hivdb$agect, hivdb$sex)


### transmissions

hivdbnew <- patient %>% 
  mutate(sex = ifelse(genderid == 4, "M", "F"), 
         rgy = year(regdate),
         dob = year(birthdate),
         age = repyear - dob,
         deathy = year(deathdate),
         agrgsex = case_when(age < 15 ~ "<15",
                             age >= 15 & age <= 24 & sex == "M" ~ "M15-24",
                             age >= 15 & age <= 24 & sex == "F" ~ "F15-24",
                             age >= 25 & sex == "M" ~ "M25+",
                             age >= 25 & sex == "F" ~ "F25+")) %>% 
  filter(rgy == repyear)

table(hivdbnew$regionid, hivdbnew$sex, exclude = F)
table(hivdbnew$transferid, hivdbnew$sex, exclude = F)
table(hivdbnew$agrgsex, hivdbnew$sex, exclude = F)

# filtre, registrarion year before 2023 and exclude left coutries##
lostfromcare <- patient %>%  
  left_join(vlst, by = c("regnum" = "id")) %>%
  anti_join(anonims, by = c("regnum" = "id")) %>% # mutate(vdt = year(lst), ddt = year(deathfixdate))
  mutate(regnum = regnum,
         visstt = visstt,
         vy = year(lastvdate),
         sex = ifelse(genderid == 4, "M", "F"), 
         rgy = year(regdate),
         dob = year(birthdate),
         age = repyear - dob,
         deathy = year(deathdate),
         trroute = case_when(transferid == '47' ~ "IDU", 
                             transferid == '48' & sex == "M" ~ "MHETERO",
                             transferid == '48' & sex == "F" ~ "FHETERO",
                             transferid == '49' ~ "HOMO", 
                             transferid %in% c('50', '51', '52') ~ "OTHER",
                             is.na(transferid) ~ "OTHER"),
         agrgsex = case_when(age < 15 ~ "<15",
                             age >= 15 & age <= 24 & sex == "M" ~ "M15-24",
                             age >= 15 & age <= 24 & sex == "F" ~ "F15-24",
                             age >= 25 & sex == "M" ~ "M15+",
                             age >= 25 & sex == "F" ~ "F15+")) %>%
  filter(!is.na(regnum), rgy <= repyear, (is.na(deathy) | deathy >repyear), vy == repyear, visstt == 255) # %>% # (!is.na(visstt) |
#select(c(2,94, 92, 93, 89, 87, transferid))

NROW(unique(lostfromcare$regnum))

lostfromcare %>% count(agrgsex) ## 88,91
lostfromcare %>% count(trroute) ## 88,91

