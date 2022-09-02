library(tidyverse)
library(dbplyr)
library(DBI)


bioinformatician<-read_csv("admin_bioinformatician.csv")
cell<-read_csv("admin_cell.csv")
format<-read_csv("admin_format.csv")
genotype<-read_csv("admin_genotype.csv")
group<-read_csv("admin_group.csv")
method<-read_csv("admin_method.csv")
organism<-read_csv("admin_organism.csv")
researcher<-read_csv("admin_researcher.csv")

data<-read_csv("df_data.csv")
proj<-read_csv("df_proj.csv")
register<-read_csv("register.csv")

db_file <- "db/database.sqlite"
con <- dbConnect(drv=RSQLite::SQLite(), dbname=db_file)

copy_to(con,bioinformatician,temporary=FALSE)
copy_to(con,cell,temporary=FALSE)
copy_to(con,format,temporary=FALSE)
copy_to(con,genotype,temporary=FALSE)
copy_to(con,group,temporary=FALSE)
copy_to(con,method,temporary=FALSE)
copy_to(con,organism,temporary=FALSE)
copy_to(con,researcher,temporary=FALSE)
copy_to(con,data,name = "datas",temporary=FALSE)
copy_to(con,proj,temporary=FALSE)
copy_to(con,register,name = "user",temporary=FALSE)

dbDisconnect(con)
