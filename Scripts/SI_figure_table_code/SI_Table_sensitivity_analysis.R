##########################  ##########################  ##########################
# Current simulations (TB & Squeeze parameters) - This is what we have with assumed parameters
### Overall TB
contemp_ta_tb_australia <- readRDS("Raw_data/contemp_ta_tb_australia.rds") %>% 
  filter(Species %in% c('C. quatt')) %>% 
  filter(Variable == 'Mean Tb') %>% 
  rename(Test = Variable) %>% 
  mutate(Measure = 'OVERALL',
         Analysis = 'CTmin_max_assumed') %>% 
  select(Species,Analysis, Measure, Test, Slope_10_Year, P_value)

### ACT, MRATE, Foraging overall
ACT_MRate_Foraging_contemp <- readRDS("Raw_data/ACT_MRate_Foraging_contemp.rds") %>% 
  filter(Species != "Overall") %>% 
  filter(Species %in% c('C. quatt')) %>% 
  mutate(Measure = 'OVERALL',
         Analysis = 'CTmin_max_assumed') %>% 
  select(Species, Analysis, Measure,Test, Slope_10_Year, P_value)



##########################  ##########################  ##########################
# CTmax -2 (TB & Squeeze parameters) 43C
contemp_ta_tb_australia_CTmax_43 <- readRDS("Raw_data/contemp_ta_tb_australia_C_quatt_CTmax_43.RDS") %>% 
  filter(Species %in% c('C. quatt')) %>% 
  filter(Variable == 'Mean Tb') %>% 
  rename(Test = Variable) %>% 
  mutate(Measure = 'OVERALL',
         Analysis = '-2') %>% 
  select(Species,Analysis, Measure,  Test, Slope_10_Year, P_value)
### ACT, MRATE, Foraging overall
ACT_MRate_Foraging_contemp_CTmax_43 <- readRDS("Raw_data/ACT_MRate_Foraging_contemp_C_quatt_CTmax_43.rds") %>% 
  filter(Species != "Overall") %>% 
  filter(Species %in% c('C. quatt')) %>% 
  mutate(Measure = 'OVERALL',
         Analysis = '-2') %>% 
  select(Species, Analysis, Measure,Test, Slope_10_Year, P_value)

##########################  ##########################  ##########################
# CTmax +2 (TB & Squeeze parameters) 47C
contemp_ta_tb_australia_CTmax_47 <- readRDS("Raw_data/contemp_ta_tb_australia_C_quatt_CTmax_47.RDS") %>% 
  filter(Species %in% c('C. quatt')) %>% 
  filter(Variable == 'Mean Tb') %>% 
  rename(Test = Variable) %>% 
  mutate(Measure = 'OVERALL',
         Analysis = '+2') %>% 
  select(Species,Analysis,Measure,  Test, Slope_10_Year, P_value)

### ACT, MRATE, Foraging overall
ACT_MRate_Foraging_contemp_CTmax_47 <- readRDS("Raw_data/ACT_MRate_Foraging_contemp_C_quatt_CTmax_47.rds") %>% 
  filter(Species != "Overall") %>% 
  filter(Species %in% c('C. quatt')) %>% 
  mutate(Measure = 'OVERALL',
         Analysis = '+2') %>% 
  select(Species, Analysis, Measure,Test, Slope_10_Year, P_value)


##########################  ##########################  ##########################
# CTmin -2 (TB & Squeeze parameters) 6.7C
contemp_ta_tb_australia_CTmin_6_7 <- readRDS("Raw_data/contemp_ta_tb_australia_C_quatt_CTmin_6_7.RDS") %>% 
  filter(Species %in% c('C. quatt')) %>% 
  filter(Variable == 'Mean Tb') %>% 
  rename(Test = Variable) %>% 
  mutate(Measure = 'OVERALL',
         Analysis = '-2') %>% 
  select(Species,Analysis,Measure,  Test, Slope_10_Year, P_value)

### ACT, MRATE, Foraging overall
ACT_MRate_Foraging_contemp_CTmin_6_7 <- readRDS("Raw_data/ACT_MRate_Foraging_contemp_C_quatt_CTmin_6_7.rds") %>% 
  filter(Species != "Overall") %>% 
  filter(Species %in% c('C. quatt')) %>% 
  mutate(Measure = 'OVERALL',
         Analysis = '-2') %>% 
  select(Species, Analysis, Measure,Test, Slope_10_Year, P_value)


##########################  ##########################  ##########################
# CTmin +2 (TB & Squeeze parameters) 10.7C
contemp_ta_tb_australia_CTmin_10_7 <- readRDS("Raw_data/contemp_ta_tb_australia_C_quatt_CTmin_10_7.RDS") %>% 
  filter(Species %in% c('C. quatt')) %>% 
  filter(Variable == 'Mean Tb') %>% 
  rename(Test = Variable) %>% 
  mutate(Measure = 'OVERALL',
         Analysis = '+2') %>% 
  select(Species,Analysis,Measure,  Test, Slope_10_Year, P_value)

### ACT, MRATE, Foraging overall
ACT_MRate_Foraging_contemp_CTmin_10_7 <- readRDS("Raw_data/ACT_MRate_Foraging_contemp_C_quatt_CTmin_10_7.rds") %>% 
  filter(Species != "Overall") %>% 
  filter(Species %in% c('C. quatt')) %>% 
  mutate(Measure = 'OVERALL',
         Analysis = '+2') %>% 
  select(Species, Analysis, Measure,Test, Slope_10_Year, P_value)




############
# Final tables for CTmin & Max
C_quatt_CTmin <-rbind(contemp_ta_tb_australia_CTmin_6_7, 
                      contemp_ta_tb_australia,
                      contemp_ta_tb_australia_CTmin_10_7,
                      ACT_MRate_Foraging_contemp_CTmin_6_7,
                      ACT_MRate_Foraging_contemp,
                      ACT_MRate_Foraging_contemp_CTmin_10_7) %>% 
  mutate(Analysis = ifelse(Analysis == "CTmin_max_assumed", 
                           "CTmin", Analysis)) %>%
  mutate(Parameter = "CTmin") %>% 
  mutate(Test = factor(Test, levels = c("Mean Tb", "ACT", "M_Rate", "Feeding_demand")),
         Analysis = factor(Analysis, levels = c("-2", "CTmin", "+2"))) %>%
  arrange(Test, Analysis) %>% 
  select(Parameter, Test, Analysis, Slope_10_Year, P_value)


C_quatt_CTmax <-rbind(contemp_ta_tb_australia_CTmax_43, 
                      contemp_ta_tb_australia,
                      contemp_ta_tb_australia_CTmax_47,
                      ACT_MRate_Foraging_contemp_CTmax_43,
                      ACT_MRate_Foraging_contemp,
                      ACT_MRate_Foraging_contemp_CTmax_47) %>% 
  mutate(Analysis = ifelse(Analysis == "CTmin_max_assumed", 
                           "CTmax", Analysis)) %>% 
  mutate(Parameter = "CTmax") %>% 
  mutate(Test = factor(Test, levels = c("Mean Tb", "ACT", "M_Rate", "Feeding_demand")),
         Analysis = factor(Analysis, levels = c("-2", "CTmax", "+2"))) %>%
  arrange(Test, Analysis) %>% 
  select(Parameter, Test, Analysis, Slope_10_Year, P_value)


####################
# Final table
TableS10 <- rbind(C_quatt_CTmin,
                  C_quatt_CTmax)
TableS10 <- flextable(TableS10) %>% 
  hline_top(border = fp_border_default(width = 0), 
            part = "header") %>% 
  fontsize(size = 10, part = "all") %>%  
  hline(i =c(12), j = 1:5, part = "body") %>% 
  merge_v(j = c('Parameter', "Test"), part = "header") %>%
  merge_v(j = c('Parameter',"Test"), part = "body") %>%
  hline(i =c(3,6,9,15,18,21), j = 2:5, part = "body") %>% 
  flextable::font(part = "all", fontname = "Times New Roman") %>%
  set_table_properties(layout = "autofit") %>%
  fix_border_issues(part = "body") %>%
  #flextable::align(j = 3:6, align = "center", part = "all") %>%
  set_table_properties(layout = "autofit")

