## Cascade 2023 done 2024
rm(list=ls()) #remove all clean workspace memory
## load packages
ipak <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(new.pkg))
    install.packages(new.pkg, dependencies = TRUE)
  sapply(pkg, require, character.only = TRUE)
}
# usage
packages <- c("installr", "RPostgres", "dplyr","lubridate","ggplot2","tidyr",
              "readxl","readr", "reshape", "reshape2", "gtsummary", "flextable")
ipak(packages)


### import from postgreslq
### import from postgreslq #########
#con<-dbConnect(PostgreSQL(), user="oto", password="oto2213", dbname="aidshis250420")
con <-  dbConnect( 
  RPostgres::Postgres(),
  dbname = "aidshis060624", 
  #server = "localhost", 
  port = "5432", 
  user = "oto",
  password = "oto2213")

dbGetQuery(con, "SHOW CLIENT_ENCODING") # get cline encoding
### set wirking directory
workwd <- "D:/OTO/DB/scripts/NIKA/2020/cascade"
homewd <-"D:/OTO/DB/script/NIKA/2020/cascade"
if(isTRUE(file.info(workwd) [1,"isdir"])){
  setwd(workwd);dir() # dir.create is used to creatge not existing directory
}else{
  (isTRUE(file.info(homewd)[1,"isdir"]))
  setwd(homewd);dir()
}
getwd() # check working directory

### create final date variable#
frdt <- '2023-04-01'
agerpy <- '2023-04-01'
labrt <- '2023-04-01'

### get data ###
##husdb <- read.xlsx("data/hussurv.xls", header=T, sheetName = "hus") ar mushaobs office 2016 excell
##husdb <- read.xlsx("data/hussurvfnl13122017.xls", header=T, sheetName = "hus")
####
patient<-dbGetQuery(con, "select * from patient
                         where 
                        regdate < '2023-04-01' and regnum is not null;")  # select patient table
visit <- dbGetQuery(con, "select distinct p.regnum as id,
                         date(v.visitdate) as vdate,
                          v.patstatustid as vstatus
                    from
                    patient as p
                    inner JOIN visit as v ON(p.patientid=v.patientid)
                    where
                    --  v.patstatustid != 251
                    v.visitdate < '2021-02-17'  and p.regdate < '2021-01-01'
                    ORDER BY
                    vdate desc;")

dball<-dbGetQuery(con, "select distinct p.regnum as id,
                        date(vst.startdate) as labdate,
                        vst.result_num as rslt,
                        vst.investigationid as vid --,
                        --CURRENT_DATE as d_date
                      from
                        patient as p
                        LEFT JOIN visit as v ON(p.patientid=v.patientid)
                        LEFT JOIN visitstandard as vst ON(v.visitid=vst.visitid)
                        INNER JOIN investigation as inv ON(vst.investigationid=inv.investigationid)
                       -- INNER JOIN code as cod ON(p.genderid=cod.codeid)
                       -- INNER JOIN center as c  ON(vst.centerid=c.centerid)
                      where
                        vst.investigationid in(151, 157) and vst.result_num is not null and 
                        p.regnum is not null and vst.startdate < '2021-02-17' and p.regdate < '2021-01-01'
                      ORDER BY
                        labdate desc;")
dbDisconnect(con)  # clode connection to posgreslq
#### left country ####
glimpse(visit)
glimpse(patient)


### saqartvelos moqalaqeebis datoveba
patients <- subset(patient, patient$citizenshipid == 76)
dim(patients)

### recode age, gender and trroute ##
patients <- patient %>%
  mutate(sex = ifelse(genderid == 4, "M", "F"),
         mode   = case_when(transferid == '47' ~ "IDU", 
                            transferid == '48' & genderid == 4 ~ "HETM", 
                            transferid == '48' & genderid == 5 ~ "HETF", 
                            transferid == '49' ~ "HOMO",
                            TRUE ~ "Other"),
         age = as.duration(interval(birthdate,agerpy)) %/% as.duration(years(1)),
         agegr = ifelse(age < 15, "<15", "15+"),
         agegrds = cut(age, breaks = c(0, 14, 24, 100), labels = c("<15", "15-24", "25+")))
table(patients$agegrds)
names(patients)
#View(patients[,c(2,14,16,57, 86,87,88, 89)])
## anonimous remove
anonims <- read.csv("data/anonimous.csv", header=T)
anons <- as.data.frame(unique(anonims$id))
names(anons) <- "id"
length(anons$id)

### left countr ###
vislftcntr <- visit %>% 
  inner_join(patients, by = c("id" = "regnum"))%>%
  filter(vdate < frdt) %>%
  mutate(vd = year(vdate)) %>%
  arrange(vdate) %>% 
  group_by(id) %>%
  summarise(leftid = last(id), vldt =  last(vstatus), vld = last(vd), 
            gnd = last(sex), agerdy = last(agegrds)) %>% 
  filter(vldt == 255)
glimpse(vislftcntr)

table(vislftcntr$vld)

### last visit ##
lastvisit <- visit %>% 
  filter(vdate < frdt) %>%
  group_by(id) %>%
  summarise(vlrlat = last(vdate)) %>%
  mutate(vst = ifelse(!is.na(vlrlat), "vy", "not")) %>%
  select(id, vst)
glimpse(lastvisit)

### viral loads ###
vll <- dball %>% filter(vid == 151) %>% 
  arrange(labdate) %>%
  mutate(ry = year(labdate)) %>%
  mutate(undvl1000 = ifelse(rslt < 1000, 1,0), 
         undvl200 = ifelse(rslt < 200, 1,0),
         vlall = ifelse(!is.na(rslt),"vldn", "not")) %>%
  filter(labdate >= '2020-01-01' , labdate <= labrt) %>%
  group_by(id) %>%
  summarise(vlrlat = last(rslt), vdt = max(labdate), vldund1000 = last(undvl1000), 
            vldund200 = last(undvl200), lvvl = last(vlall))
vll


##### import art ###
art <- read.csv("data/art.csv", header=T)
glimpse(art)
##### in care ###
patientscare <- visit %>% 
  right_join(patients, by = c("id"="regnum")) %>%
  anti_join(vislftcntr, by = c("id" = "leftid")) %>%
  anti_join(anonims, by = c("id" = "id")) %>%
  filter( is.na(deathfixdate) | deathfixdate > frdt, !is.na(id) ) %>% #
  group_by(id) %>%
  summarise(n = last(id))
glimpse(patientscare)

# count in care
incr <- patientscare %>% count()
incr

#### join and count ###
cascade2020 <- patientscare %>% 
  left_join(lastvisit, by = "id")%>% 
  left_join(vll, by = "id") %>%
  left_join(art, by = "id")
glimpse(cascade2020)


glimpse(patients)
### add other variables ###
cascade20 <- patients %>% 
  select(2,86,87,88, 89, mode, sex,  agegr) %>%
  filter(!is.na(regnum)) %>%
  inner_join(cascade2020, by = c("regnum" = "id")) %>%
  mutate(artvl = case_when(lvvl == "vldn" & arv2020 == "y" ~ "artvly",
                           T ~ "ndt")) %>%
  arrange(regnum)
glimpse(cascade20)
names(cascade20)

write.csv(cascade20, paste(getwd(), "/data/cascade20v", Sys.Date(), ".csv", sep = ""), row.names=F, na="")

# arte <- cascade20 %>% filter(arv2020 == "y")
# artdf <-setdiff(art$id, arte$regnum)
# setequal(artdf, patients$id)
# vislftcntr$id[vislftcntr$id == 5080]
# 
##  data analisis # 

csc20all <- cascade20 %>% ### gender and agegrp and occupation
  select(mode, sex,  agegr) %>%
  tbl_summary(#by =,
    # statistic = list(
    #   all_continuous() ~ "{mean} ({sd})",
    #   all_categorical() ~ "{n} ({p} %)"
    # ),
    #label = q2 ~ "ასაკ. ჯგ.",
    missing_text = "Unk.")
as_flex_table(csc20all)

csc20incare <- cascade20 %>% ### gender and agegrp and occupation
  select(vst, mode, sex,  agegr) %>%
  tbl_summary(by =vst,
    # statistic = list(
    #   all_continuous() ~ "{mean} ({sd})",
    #   all_categorical() ~ "{n} ({p} %)"
    # ),
    #label = q2 ~ "ასაკ. ჯგ.",
    missing_text = "Unk.")
as_flex_table(csc20incare)


csc20arv <- cascade20 %>% ### gender and agegrp and occupation
  select(arv2020, sex, mode, agegr) %>%
  tbl_summary(by = arv2020,
              # statistic = list(
              #   all_continuous() ~ "{mean} ({sd})",
              #   all_categorical() ~ "{n} ({p} %)"
              # ),
              #label = q2 ~ "ასაკ. ჯგ.",
              missing_text = "Unk.")
as_flex_table(csc20arv)


csc20artvl <- cascade20 %>% ### gender and agegrp and occupation
  select(artvl, sex, mode, agegr) %>%
  tbl_summary(by = artvl,
              # statistic = list(
              #   all_continuous() ~ "{mean} ({sd})",
              #   all_categorical() ~ "{n} ({p} %)"
              # ),
              #label = q2 ~ "ასაკ. ჯგ.",
              missing_text = "Unk.")
as_flex_table(csc20artvl)

csc20vl1000 <- cascade20 %>% ### gender and agegrp and occupation
  select(vldund1000, sex, mode, agegr) %>%
  tbl_summary(by = vldund1000,
              # statistic = list(
              #   all_continuous() ~ "{mean} ({sd})",
              #   all_categorical() ~ "{n} ({p} %)"
              # ),
              #label = q2 ~ "ასაკ. ჯგ.",
              missing_text = "Unk.")
as_flex_table(csc20vl1000)


csc20vl200 <- cascade20 %>% ### gender and agegrp and occupation
  select(vldund200, sex, mode, agegr) %>%
  tbl_summary(by = vldund200,
              # statistic = list(
              #   all_continuous() ~ "{mean} ({sd})",
              #   all_categorical() ~ "{n} ({p} %)"
              # ),
              #label = q2 ~ "ასაკ. ჯგ.",
              missing_text = "Unk.")
as_flex_table(csc20vl200)


csc20allleft <- vislftcntr %>% ### gender and agegrp and occupation
  filter(vld == 2023) %>%
  select(gnd, agerdy) %>%
  tbl_summary(by = gnd,
    statistic = list(
      all_continuous() ~ "{mean} ({sd})",
      all_categorical() ~ "{n} ({p} %)"
    ),
    #label = q2 ~ "ასაკ. ჯგ.",
    missing_text = "Unk.")
as_flex_table(csc20allleft)
