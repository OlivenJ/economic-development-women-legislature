library(fixest)

feols(hc2total ~ lag_lawmaker+ gdp + oldAge + youngAge + TB+labPar + lag_leader*lag_lawmakerDummy |year+country,
    joint_data_lagLaw) %>% 
  summary()

feols(hc2total ~ lag_law+ gdp + oldAge + youngAge + TB+labPar + lag_leader*lag_law_dummy |year+country,
      HI_lag_data) %>% 
  summary()

feols(hc2total ~ lag_law+ gdp + oldAge + youngAge + TB+labPar + lag_leader*lag_law_dummy |year+country,
      Upper_lag_data) %>% 
  summary()

feols(hc2total ~ lag_law+ gdp + oldAge + youngAge + TB+labPar +ODA+ lag_leader*lag_law_dummy |year+country,
      Lower_lag_data) %>% 
  summary()

feols(hc2total ~ lag_law+ gdp + oldAge + youngAge + TB+labPar +ODA + lag_leader*lag_law_dummy |year+country,
      LI_lag_data) %>% 
  summary()

joint_data %>% filter(income_level == "Upper") %>% 
  summarize(mean(lawmaker))
