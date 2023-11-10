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


# wide_raw_after <- wide_raw %>% select(INDI_DSCM_NO, c90_yn, sex, age, ageg4, insu_g2
#                                       , paste0(sm.names)
#                                       , death, surv_years)
# 
# long_raw_after <- wide_raw_after %>% 
#   gather(key="key", value="value", paste0(sm.names)) %>% 
#   separate(key, sep = '[_]', into = c('var','time')) %>% 
#   arrange(INDI_DSCM_NO)
# 
# casedf <- long_raw_after |>
#   dplyr::select(INDI_DSCM_NO , c90_yn, var, value) |> 
#   filter(value == 1 & c90_yn ==1) |> 
#   mutate(comorbity = case_when (var == 'aids' ~ 'AIDS', 
#                                 var == 'cancer' ~ 'Cancer', 
#                                 var == 'cd' ~ 'CVD',
#                                 var == 'chf' ~ 'CHF',
#                                 var == 'cpd' ~ ' CPD',
#                                 var == 'dem' ~ 'Dementia',
#                                 var == 'dia' ~ 'Diabetes',
#                                 var == 'hp' ~ 'Plegia',
#                                 var == 'im' ~ 'IM',
#                                 var == 'mi' ~ 'MI',
#                                 var == 'mld' ~ 'Liver',
#                                 var == 'pud' ~ 'PUD',
#                                 var == 'pvd' ~ 'PVD',
#                                 var == 'rd' ~ 'RD'))
# 
# 
# controldf <- long_raw_after |>
#   dplyr::select(INDI_DSCM_NO , c90_yn, var, value) |> 
#   filter(value == 1 & c90_yn ==0) |> 
#   mutate(comorbity = case_when (var == 'aids' ~ 'AIDS', 
#                                 var == 'cancer' ~ 'Cancer', 
#                                 var == 'cd' ~ 'CVD',
#                                 var == 'chf' ~ 'CHF',
#                                 var == 'cpd' ~ ' CPD',
#                                 var == 'dem' ~ 'Dementia',
#                                 var == 'dia' ~ 'Diabetes',
#                                 var == 'hp' ~ 'Plegia',
#                                 var == 'im' ~ 'IM',
#                                 var == 'mi' ~ 'MI',
#                                 var == 'mld' ~ 'Liver',
#                                 var == 'pud' ~ 'PUD',
#                                 var == 'pvd' ~ 'PVD',
#                                 var == 'rd' ~ 'RD'))


for (i in c("",2,4,6)) {
  wide <- wide_raw |>  select(INDI_DSCM_NO, c90_yn, sex, age, ageg4, insu_g2
                                      , paste0(sm.names,i)
                                      , death, surv_years)
  
  
  long <- wide |>  
    gather(key="key", value="value", paste0(sm.names,i)) |>  
    separate(key, sep = '[_]', into = c('var','time')) |>  
    arrange(INDI_DSCM_NO)
  
  
  assign(paste0("casedf",i), long |>
          dplyr::select(INDI_DSCM_NO , c90_yn, var, value) |> 
          filter(value == 1 & c90_yn ==1) |> 
          mutate(comorbity = case_when (var == 'aids' ~ 'AIDS', 
                                        var == 'cancer' ~ 'Cancer', 
                                        var == 'cd' ~ 'CVD',
                                        var == 'chf' ~ 'CHF',
                                        var == 'cpd' ~ ' CPD',
                                        var == 'dem' ~ 'Dementia',
                                        var == 'dia' ~ 'Diabetes',
                                        var == 'hp' ~ 'Plegia',
                                        var == 'im' ~ 'IM',
                                        var == 'mi' ~ 'MI',
                                        var == 'mld' ~ 'Liver',
                                        var == 'pud' ~ 'PUD',
                                        var == 'pvd' ~ 'PVD',
                                        var == 'rd' ~ 'RD')))
  
  assign(paste0("controldf",i), long |>
         dplyr::select(INDI_DSCM_NO , c90_yn, var, value) |> 
         filter(value == 1 & c90_yn ==0) |> 
         mutate(comorbity = case_when (var == 'aids' ~ 'AIDS', 
                                       var == 'cancer' ~ 'Cancer', 
                                       var == 'cd' ~ 'CVD',
                                       var == 'chf' ~ 'CHF',
                                       var == 'cpd' ~ ' CPD',
                                       var == 'dem' ~ 'Dementia',
                                       var == 'dia' ~ 'Diabetes',
                                       var == 'hp' ~ 'Plegia',
                                       var == 'im' ~ 'IM',
                                       var == 'mi' ~ 'MI',
                                       var == 'mld' ~ 'Liver',
                                       var == 'pud' ~ 'PUD',
                                       var == 'pvd' ~ 'PVD',
                                       var == 'rd' ~ 'RD')))
  


}


case_cross <- crossprod(table(casedf[c(1,5)]))
case_cross2 <- crossprod(table(casedf2[c(1,5)]))
case_cross4 <- crossprod(table(casedf4[c(1,5)]))
case_cross6 <- crossprod(table(casedf6[c(1,5)]))


diag(case_cross) <- 0 
diag(case_cross2) <- 0 
diag(case_cross4) <- 0 
diag(case_cross6) <- 0 


control_cross <- crossprod(table(controldf[c(1,5)]))
control_cross2 <- crossprod(table(controldf2[c(1,5)]))
control_cross4 <- crossprod(table(controldf4[c(1,5)]))
control_cross6 <- crossprod(table(controldf6[c(1,5)]))

diag(control_cross) <- 0
diag(control_cross2) <- 0
diag(control_cross4) <- 0
diag(control_cross6) <- 0


