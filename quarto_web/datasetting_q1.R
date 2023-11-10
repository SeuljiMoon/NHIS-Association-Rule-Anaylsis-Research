
library(arules)
library(arulesViz)
library(visNetwork)
library(igraph) 
library(data.table); library(dplyr)

wide_raw <- fread("C:/Users/cmc/Documents/GitHub/NHIS_AssociationRules/data/0710_como_network_out.csv")

head(wide_raw)

sm.names<-c("mi_yes", "pvd_yes", "cd_yes", "dem_yes", "hp_yes"
            ,"im_yes", "cpd_yes", "chf_yes", "pud_yes", "mld_yes"
            , "rd_yes", "dia_yes", "cancer_yes", "aids_yes")

cci.names <- c("MI", "PVD", "CVD", "Dementia", "Plegia", "IM",
               "CPD", "CHF", "PUD", "Liver",
               "RD","Diabetes", "Cancer", "AIDS")


wide_raw_after <- wide_raw %>% select(INDI_DSCM_NO, c90_yn, sex,
                                      age, ageg4, insu_g2
                                      , paste0(sm.names,2)
                                      , death, surv_years)

colnames(wide_raw_after) <- c("ID","c90_yn","sex","age", "ageg4", "insu_g2",
                              cci.names,
                              "death", "surv_years")

# case
a <- wide_raw_after |> filter(c90_yn==1)
trans <- a
row.id <- a[ , 1]
trans <- a[, c(7:20)]
trans <- as.data.frame(sapply(trans,as.logical)) 

transdf_case <- as(trans, "transactions")

transdf_case
summary(transdf_case)
