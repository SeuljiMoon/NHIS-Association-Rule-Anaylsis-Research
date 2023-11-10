rm(list=ls())
library(arules)
library(arulesViz)
library(visNetwork)
library(igraph) 
library(circlize) 
library(RColorBrewer) 
library(data.table); library(dplyr); library(tidyr)
library(lubridate)

wide_raw <- fread("C:/Users/cmc/Documents/GitHub/NHIS_AssociationRules/data/0710_como_network_out.csv")


sm.names<-c("mi_yes", "pvd_yes", "cd_yes", "dem_yes", "hp_yes"
            ,"im_yes", "cpd_yes", "chf_yes", "pud_yes", "mld_yes"
            , "rd_yes", "dia_yes", "cancer_yes", "aids_yes")

cci.names <- c("MI", "PVD", "CVD", "Dementia", "Plegia", "IM",
               "CPD", "CHF", "PUD", "Liver",
               "RD","Diabetes", "Cancer", "AIDS")

for (i in c("",2,4,6)) {
  wide <- wide_raw |>  select(INDI_DSCM_NO, c90_yn, sex, age, ageg4, insu_g2
                              , paste0(sm.names,i)
                              , death, surv_years)
  
  colnames(wide) <- c("ID","c90_yn","sex","age", "ageg4", "insu_g2",
                                cci.names,
                                "death", "surv_years")
  
  # Case
  assign(paste0("casedf",i), wide |> filter(c90_yn == 1))
  assign(paste0("casedf",i), get(paste0("casedf",i))[, c(7:20)])
           
  

  assign(paste0("trans.case",i), as.data.frame(sapply(get(paste0("casedf",i)),as.logical)))
  
  assign(paste0("trans.df.case",i), as(get(paste0("trans.case",i)), "transactions"))
  
  # Control
  assign(paste0("controldf",i), wide |> filter(c90_yn == 0)) 
  assign(paste0("controldf",i), get(paste0("controldf",i))[, c(7:20)])
           
  
  
  assign(paste0("trans.control",i), as.data.frame(sapply(get(paste0("controldf",i)),as.logical)))
  
  assign(paste0("trans.df.control",i), as(get(paste0("trans.control",i)), "transactions"))
  
  
}
