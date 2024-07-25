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
packages <- c("installr", "RPostgres", "dplyr","lubridate","ggplot2","tidyr", "DBI", 
              "readxl","readr", "pdftools", "stringr", "gtools", "emayili")
ipak(packages)

### import from postgreslq
source("Connect_to_PGSQL.R") # load data from postgresql, you should enter database name

# Connect to postgres database
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
packages <- c("installr", "RPostgres", "dplyr","lubridate","ggplot2","tidyr", "DBI", 
              "readxl","readr", "pdftools", "stringr", "gtools", "emayili")
ipak(packages)

### import from postgreslq
source("D:/OTO/Data_Qdb/code/Connect_to_PGSQL.R") # load data from postgresql, you should enter database name

# Connect to postgres database in case Connect_to_PGSQL.R not working ####
# con <- DBI::dbConnect(RPostgres::Postgres(), 
#                       dbname = "aidshis02052024", 
#                       host = "localhost", 
#                       port = 5432, 
#                       user = "oto", 
#                       password = rstudioapi::askForSecret("dbpassword")) # "oto2213"



##
