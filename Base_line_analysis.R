library(fixest)

feols(hc2total ~ lag_lawmaker + gdp  +oldAge + youngAge  |year,
      joint_data_lagLaw) %>% 
  summary()

feols(hc2total ~ lag_lawmaker + gdp+oldAge + youngAge + TB |year+country,
      joint_data_lagLaw) %>% 
  summary()

feols(hc2total ~ lag_lawmaker + gdp+oldAge + youngAge + TB + labPar|year,
      joint_data_lagLaw) %>% 
  summary()

feols(hc2total ~ lag_lawmaker + gdp+oldAge + youngAge + TB + labPar|year + country,
      joint_data_lagLaw) %>% 
  summary()

feols(hc2total ~ lag_lawmaker + gdp+oldAge  + TB + labPar|year + country,
      joint_data_lagLaw) %>% 
  summary()

##Country Group Modellings
#feols(hc2total ~ lag_lawmaker + gdp+oldAge + youngAge + TB + labPar |year+country,
#      OECD_joint_lag) %>% 
#  summary()
#
#feols(hc2total ~ lag_lawmaker + gdp+oldAge + youngAge + TB + labPar |year+country,
#      HI_joint_lag) %>% 
#  summary()
#
#feols(hc2total ~ lag_lawmaker + gdp+oldAge + youngAge + TB + labPar |year+country,
#      HI_joint_lag %>% filter(lag_lawmaker < 40)) %>% 
#  summary()
#
#feols(hc2total ~ lag_lawmaker + gdp+oldAge + youngAge + TB + labPar    |year+country,
#      Emerging_joint_lag) %>% 
#  summary()
#
#feols(hc2total ~ lag_lawmaker + gdp+oldAge + youngAge + TB + labPar   |year+country,
#      LDC_joint_lag) %>% 
#  summary()
#
#feols(hc2total ~ lag_lawmaker + gdp+oldAge + youngAge + TB + labPar + ODA  |year+country,
#      LDC_joint_lag) %>% 
#  summary()
#
#feols(hc2total ~ lag_lawmaker  |year+country,
#      HI_joint_lag) %>% 
#  summary()
#
#feols(hc2total ~  lag_lawmaker+  gdp+oldAge + youngAge + TB + labPar  +democra+ lawmaker_dummy*lag_leader |year+country,
#      high_joint_lag) %>% 
#  summary()
#
feols(hc2total ~  lag_lawmaker +  gdp + oldAge + youngAge + TB + labPar  |year+country,
      joint_data_lagLaw %>% filter(publicprivate == 1) ) %>% 
  summary()

feols(hc2total ~  lag_law +  gdp + oldAge + youngAge + TB + labPar +ODA  |year+country,
      HI_lag_data ) %>% 
  summary()

(364^96)/(365^149)
