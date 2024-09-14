## cascade code 
rm(list=ls())  # remove all data
dev.off()
print(sessionInfo(), l=F)
## packages
## load packages
ipak <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(new.pkg))
    install.packages(new.pkg, dependencies = TRUE)
  sapply(pkg, require, character.only = TRUE)
}
# usage
packages <- c("installr", "RPostgres", "dplyr","lubridate",
              "ggplot2","tidyr", "DBI", "gtsummary", "openxlsx",
              "readxl","readr", "pdftools", "janitor",
              "stringr", "gtools", "emayili", "bit64")
ipak(packages)

### set wirking directory
workwd <- "~/OTO/Oficial/DB/HIVcascade"
homewd <- "/Users/oto/Documents/OTO/DB/HIVcascade"
if(isTRUE(file.info(workwd) [1,"isdir"])){
  setwd(workwd);dir() # dir.create is used to creatge not existing directory
}else{
  (isTRUE(file.info(homewd)[1,"isdir"]))
  setwd(homewd);dir()
}
getwd()

### import from postgreslq
source("code/Connect_to_PGSQL.R") # load data from postgresql, you should enter database name

# Connect to postgres database in case Connect_to_PGSQL.R not working ####
# con <- DBI::dbConnect(RPostgres::Postgres(), 
#                       dbname = "aidshis02052024", 
#                       host = "localhost", 
#                       port = 5432, 
#                       user = "postgres", 
#                       password = rstudioapi::askForSecret("dbpassword")) # "ototata"

##
### create final date variable#
frdt <- '2023-01-01'
repyear <- '2023-12-31'
labrt <- '2023-12-31'

## dada
glimpse(visit)
glimpse(patient)

## anonimous remove
anonims <- read.csv("data/anonimous.csv", header=T)
anons <- as.data.frame(unique(anonims$id))
names(anons) <- "id"
length(anons$id)

### detect last visit
vlst <- visit %>% 
  arrange(vdate, vstatus) %>% 
  #  filter(vdate <= maxvdate) %>%
  group_by(id) %>% 
  summarize(lastvdate = max(vdate), visstt = last(vstatus)) 
glimpse(vlst)
## save in csv
write.csv(vlst,  paste0(getwd(), "/data/vlst", Sys.Date(), ".csv", sep = ""), row.names=F, na="")

## prepare personells database for connection 
prsns <- personells %>% 
  select(personellid, fname, lname) %>% 
  unite(doctorname, fname:lname, sep = " ")


# filter, registrarion year before 2023 and exclude anonimous and left coutries##
hivdb <- patient %>%  
  left_join(vlst, by = c("regnum" = "id")) %>%
  anti_join(anonims, by = c("regnum" = "id")) %>% # mutate(vdt = year(lst), ddt = year(deathfixdate))
  left_join(prsns, by = c("doctorid" = "personellid")) %>% ## connect to personell db
  mutate(sex = ifelse(genderid == 4, "M", "F"), 
         rgy = year(regdate),
         dob = year(birthdate),
         age = as.duration(interval(birthdate,repyear)) %/% as.duration(years(1)),
         deathy = year(deathdate),
         trroute = case_when(transferid == '47' ~ "IDU", 
                             transferid == '48' & sex == "M" ~ "MHETERO",
                             transferid == '48' & sex == "F" ~ "FHETERO",
                             transferid == '49' ~ "MSM", 
                             transferid %in% c('50', '51', '52') ~ "OTHER",
                             is.na(transferid) ~ "OTHER"),
         agrgsex = case_when(age < 15 ~ "<15",
                             age >= 15 & sex == "M" ~ "M15+",
                             age >= 15 & sex == "F" ~ "F15+"),
         agect = cut(age, c(-1,14,24,1000), c("0-14","15-24", "25+"))) %>%
  filter(!is.na(regnum), rgy <= repyear, (is.na(deathy) | deathy >repyear), (is.na(visstt) | visstt != 255)) %>% # (!is.na(visstt) |
  add_count(doctorname, name = "doctncount") %>% 
  add_tally(name = "totaln") %>% 
  mutate( prcntdctnm = round(doctncount/totaln * 100, 2)) %>% 
  select(c(regnum, lastvdate, visstt, deathy, regdate, age, agrgsex, sex, agect, rgy, arvdate, trroute, doctorname, doctncount, totaln, prcntdctnm))
names(hivdb)
glimpse(hivdb)

## patients by dotctors after cleaning
doctors <- hivdb %>%  
  count(doctorname) %>% 
  arrange(desc(n)) 

doctors


## select doctors with <1% of patients from total
noflwpdoct <- hivdb %>% 
  filter(prcntdctnm < 1) %>% 
  select(regnum, doctorname)

## selected list of patients with less then 1% from total
noflwpdoct

## write small number of patient to check by doctors.
write.xlsx(noflwpdoct,  paste0(getwd(), "/data/noflwpdoct", Sys.Date(), ".xlsx", sep = ""), rowNames=F, na="")


### viral loads ###
vll <- dball %>% filter(vid == 151) %>% 
  arrange(labdate) %>%
  mutate(ry = year(labdate)) %>%
  mutate(undvl1000 = ifelse(rslt < 1000, "Und1000", "NonUndVL"), 
         undvl200 = ifelse(rslt < 200, "Und200", "NonUndVL"),
         vlall = ifelse(!is.na(rslt),"vldn", "not")) %>%
  filter(labdate >= frdt , labdate <= labrt) %>%
  group_by(id) %>%
  summarise(vlrlat = last(rslt), vdt = max(labdate), vldund1000 = last(undvl1000), 
            vldund200 = last(undvl200), lvvl = last(vlall))
vll

### plot by hiv transmision routes
hivdb %>% count(rgy, trroute) %>% 
  filter(rgy > 2010, trroute != "OTHER") %>% 
  ggplot(aes(x = rgy, y = n, color = trroute)) + 
  geom_line(lwd = 3)+
  scale_x_continuous("Years", labels = as.character(hivdb$rgy), breaks = hivdb$rgy)+
  scale_y_continuous("Count", limits = c(0, 300), breaks = seq(0,300,50))+
  #xlim(min(db$ry), max(db$ry), by = 1)+
  theme_bw() +
  labs(color = "Transmission route" )+
  # scale_fill_manual()+
  theme(axis.text.x = element_text(angle = 90, colour = "black", size = 10), 
        axis.text.y = element_text(colour = "black", size = 10),
        strip.text.x = element_text(colour = "black", size = 10),
        legend.title = element_text(size = 20), #, face = "bold"
        title = element_text(size = 20),
        legend.text = element_text(size = 20),
        axis.title.x = element_text(size = 15,  face = "bold"),
        axis.title.y = element_text(size = 15,  face = "bold"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
  ) 

### Late diagnosis CD4<350 or CD4<200 ##
latediangosis <- hivdb %>% 
  inner_join(dball, by = c("regnum"="id")) %>%
  mutate(ry = year(regdate), cd4y = year(labdate)) %>%
  filter(vid == 157, !is.na(rslt)) %>%
  arrange(regnum, regdate, labdate, rslt) %>%
  group_by(regnum) %>%
  summarise(cd4fst = first(rslt), cd4fbtd = min(labdate), lsex = first(sex), 
            lregdate = min(regdate), lage = min(age), ry = last(ry), trt = last(trroute)) %>%
  mutate(
         lry = year(lregdate),
         lcd4y = year(cd4fbtd), 
         agrgsex = case_when(lage < 15 ~ "<15",
                             lage > 15 & lsex == "M" ~ "M15+",
                             lage > 15 & lsex == "F" ~ "F15+"),
         cd4200 =  ifelse(cd4fst < 200, "<200", "200+"),
         cd4350 =  ifelse(cd4fst < 350, "<350", "350+")
  ) %>%
  select(regnum, lry, lcd4y, lsex, lage, agrgsex, cd4fst, cd4200, cd4350, trt)

#write.csv(latediangosis,  paste0(getwd(), "/data/cd4results", Sys.Date(), ".csv", sep = ""), row.names=F, na="")

## write plot into file
latediangosisn <- latediangosis %>% 
  filter(lry > 2004) %>% #, , trt == "IDU"
  group_by(lry) %>% 
  count(cd4200) %>% 
  group_by(lry) %>% 
  mutate(per = n / sum(n) * 100)


latediangosisn %>% ggplot(aes(x= lry, y = per, col = as.factor(cd4200))) + 
                          geom_line(position = "jitter", lwd = 2) +
  scale_x_continuous("Year", labels = as.character(latediangosisn$lry), 
                     breaks = latediangosisn$lry)+
  scale_y_continuous("Percent", limits = c(0, 100), breaks = seq(0,100,10))+
                          theme_bw() +
  labs(color = "CD4") +
  scale_color_manual(values = c(2:6)) +
  theme(axis.text.x = element_text(angle = 90, colour = "black", size = 10), 
        axis.text.y = element_text(colour = "black", size = 10),
        strip.text.x = element_text(colour = "black", size = 10),
        legend.title = element_text(size = 20), #, face = "bold"
        title = element_text(size = 20),
        legend.text = element_text(size = 20),
        axis.title.x = element_text(size = 15,  face = "bold"),
        axis.title.y = element_text(size = 15,  face = "bold"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()
  ) 

## stats late diagnosis
table(latediangosis$lry, latediangosis$cd4200) 
table(latediangosis$lry, latediangosis$cd4350) 

### difference between HIV diagnosis date and ART start date
arvdiffs <- hivdb %>% 
    mutate(arvdiffdays = as.duration(interval(regdate, arvdate)) %/% as.duration(days(1)),
           ary = year(arvdate)) %>% 
  filter(is.na(deathy), regdate > '2015-01-01', !(trroute %in% c("OTHER")), 
         arvdiffdays >= 0) %>% 
  group_by(rgy, trroute) %>% 
  summarise(arvdfd = median(arvdiffdays)) %>% 
  ungroup()
  
arvdiffs %>% 
  ggplot(aes(x = rgy, y = arvdfd, color = trroute)) + 
  geom_line(lwd = 2) +
  scale_x_continuous("Year", labels = as.character(latediangosisn$lry), 
                     breaks = latediangosisn$lry)+
  scale_y_continuous("Median (days)")+
  theme_bw() +
  labs(color = "Transmission") +
  scale_color_manual(values = c(2:6)) +
  theme(axis.text.x = element_text(angle = 90, colour = "black", size = 10), 
        axis.text.y = element_text(colour = "black", size = 10),
        strip.text.x = element_text(colour = "black", size = 10),
        legend.title = element_text(size = 20), #, face = "bold"
        title = element_text(size = 20),
        legend.text = element_text(size = 20),
        axis.title.x = element_text(size = 15,  face = "bold"),
        axis.title.y = element_text(size = 15,  face = "bold"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()
  ) 

##### import art ###
art <- read_excel("data/art2023.xlsx", sheet = "art2023") %>% 
  mutate(art2023d = paste0("y"), id = as.integer64(id))
glimpse(art)

artn <-dbarv %>% 
  filter(arvgdate >= '2023-12-01', arvgdate <= '2023-12-31', !is.na(id),
         !(arvst %in% c(268, 302, 354, 357, 364, 539, 598, 626, 627))) %>% 
  mutate(arv23 = paste0("y")) %>% 
  arrange(id) %>% 
  select(id, arv23) %>%   unique() 

setdiff(art$id, artn$id)

##### cascade coding ###
cascadedb <- hivdb %>% 
  left_join(vll, by = c("regnum" = "id")) %>% # 
 # mutate(id = as.numeric(regnum)) %>% 
  left_join(artn, by =  c("regnum" = "id")) %>%  # arv patient data
  mutate(vstcare = ifelse(is.na(lastvdate), "NotIncare", "Incare"), 
         onart = ifelse(is.na(arv23), "NotART", "ART"),
         VlonDone = ifelse(is.na(lvvl), "NonVL", "VLDone"))
glimpse(cascadedb)
names(cascadedb)

write.csv(cascadedb, paste(getwd(), "/data/cascadedb", Sys.Date(), ".csv", sep = ""), row.names=F, na="")

# arte <- cascade20 %>% filter(arv2020 == "y")
# artdf <-setdiff(art$id, arte$regnum)
# setequal(artdf, patients$id)
# vislftcntr$id[vislftcntr$id == 5080]
# 
##  data analisis all # 
cscall <- cascadedb %>% ### gender and agegrp and occupation
  select(-regnum, -lastvdate, -visstt, -deathy, - regdate, -age, -rgy, -arvdate,
         -vlrlat, -vdt, -lvvl, -arv23) %>% 
  #select(trroute, sex,  agect, VlonArt, vl) %>%
  tbl_summary(#by =,
    statistic = list(
      all_continuous() ~ "{mean} ({sd})",
      all_categorical() ~ "{n} ({p} %)"
    ),
    #label = q2 ~ "ასაკ. ჯგ.",
    missing_text = "Unk.")
as_flex_table(cscall)

# incare 
cscincare <- cascadedb %>% ### gender and agegrp and occupation
  select(vstcare, trroute, sex,  agect, onart, VlonDone, vldund1000, vldund200) %>%
  tbl_summary(by =vstcare,
              # statistic = list(
              #   all_continuous() ~ "{mean} ({sd})",
              #   all_categorical() ~ "{n} ({p} %)"
              # ),
              #label = q2 ~ "ასაკ. ჯგ.",
              missing_text = "Unk.")
as_flex_table(cscincare)

## cascade on ART
cscarv <- cascadedb %>% ### gender and agegrp and occupation
  select(onart, sex, trroute, agect) %>%
  tbl_summary(by = onart,
              percent = "row",
              statistic = list(
                all_continuous() ~ "{mean} ({sd})",
                all_categorical() ~ "{n} ({p} %)"
              ),
              #label = q2 ~ "ასაკ. ჯგ.",
              missing_text = "Unk.")
as_flex_table(cscarv)

## cascade done
cscvldone <- cascadedb %>% ### gender and agegrp and occupation
  select(VlonDone, sex, trroute, agect, vldund1000, vldund200, onart) %>%
  tbl_summary(by = VlonDone,
              percent = "column",
              statistic = list(
                all_continuous() ~ "{mean} ({sd})",
                all_categorical() ~ "{n} ({p} %)"
              ),
              #label = q2 ~ "ასაკ. ჯგ.",
              missing_text = "Unk.")
as_flex_table(cscvldone)

## cascqade vl under 1000
cscvl1000 <- cascadedb %>% ### gender and agegrp and occupation
  select(vldund1000, sex, trroute, agect) %>%
  tbl_summary(by = vldund1000,
              percent = "row",
              statistic = list(
                all_continuous() ~ "{mean} ({sd})",
                all_categorical() ~ "{n} ({p} %)"
              ),
              #label = q2 ~ "ასაკ. ჯგ.",
              missing_text = "Unk.")
as_flex_table(cscvl1000)

## cascqade vl under 200
cscvl200 <- cascadedb %>% ### gender and agegrp and occupation
  select(vldund200, sex, trroute, agect) %>%
  tbl_summary(by = vldund200,
              percent = "row",
              statistic = list(
                all_continuous() ~ "{mean} ({sd})",
                all_categorical() ~ "{n} ({p} %)"
              ),
              #label = q2 ~ "ასაკ. ჯგ.",
              missing_text = "Unk.")
as_flex_table(cscvl200)