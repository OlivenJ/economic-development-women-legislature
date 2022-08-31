library(fixest)

feols(hc2total ~  lag_lawmaker + democra +  gdp + oldAge + youngAge + TB + labPar |year+country,
      joint_data_lagLaw) %>% 
  summary()


feols(hc2total ~ lag_lawmaker+ gdp + oldAge + youngAge + TB +labPar + demo_dummy*lawmaker_dummy |year+country,
      joint_data_lagLaw) %>% 
  summary()


feols(hc2total ~ gdp + oldAge + youngAge + TB +labPar + demo_dummy*lawmaker_dummy |year+country,
      joint_data_lagLaw) %>% 
  summary()

feols(hc2total ~ lag_lawmaker + gdp+oldAge + youngAge + TB + labPar + demo_dummy*lawmaker_dummy|year+country,
      joint_data_lagLaw %>% filter(income_level == "High")) %>% 
  summary()

feols(hc2total ~ lag_lawmaker + gdp+oldAge + youngAge + TB + labPar + demo_dummy*lawmaker_dummy|year+country,
      joint_data_lagLaw %>% filter(income_level == "Upper")) %>% 
  summary()

feols(hc2total ~ lag_lawmaker + gdp+oldAge + youngAge + TB + labPar + demo_dummy*lawmaker_dummy|year+country,
      joint_data_lagLaw %>% filter(income_level == "Lower")) %>% 
  summary()

feols(hc2total ~ lag_lawmaker + gdp+oldAge + youngAge + TB + labPar + demo_dummy*lawmaker_dummy|year+country,
      joint_data_lagLaw %>% filter(income_level == "Low")) %>% 
  summary()




feols(hc2total ~ lag_lawmaker + gdp+oldAge + youngAge + TB + labPar + democra|year+country,
      joint_data_lagLaw %>% filter(class == "Working Democracy")) %>% 
  summary()

feols(hc2total ~ lag_lawmaker + gdp+oldAge + youngAge + TB + labPar + democra|year+country,
      joint_data_lagLaw %>% filter(class == "Deficient Democracy")) %>% 
  summary()

feols(hc2total ~ lag_lawmaker + gdp+oldAge + youngAge + TB + labPar + democra|year+country,
      joint_data_lagLaw %>% filter(class == "Hybrid Regime")) %>% 
  summary()

feols(hc2total ~ lag_lawmaker + gdp+oldAge + youngAge + TB + labPar + democra|year+country,
      joint_data_lagLaw %>% filter(class == "Moderate Autocracy")) %>% 
  summary()

feols(hc2total ~ lag_lawmaker + gdp+oldAge + youngAge + TB + labPar + democra|year+country,
      joint_data_lagLaw %>% filter(class == "Hard Autocracy")) %>% 
  summary()
