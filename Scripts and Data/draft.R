
feols(hc2total ~  lag_lawmaker+democra +  gdp + oldAge + youngAge + TB + labPar |year+country,
      joint_data_lagLaw %>%filter(class %in% c("Deficient Democracy", "Working Democracy"  ) ) ) %>% 
  summary()


feols(hc2total ~  lag_lawmaker+democra +  gdp + oldAge + youngAge + TB + labPar |year+country,
      joint_data_lagLaw %>%filter(class %in% c("Hard Autocracy", "Moderate Autocracy", "Hybrid Regime") ) ) %>% 
  summary()



