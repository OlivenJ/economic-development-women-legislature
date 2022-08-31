source("helper.R")
library(tidyverse)
library(readxl)
library(magrittr)

health_path   <- "data/domestic-health-expenditure-gdpratio/84fd4a9a-ee5e-4ec2-8081-fbb93fc69f3d_Data.csv"
lawmaker_path <- "data/female-lawmakers-ratio/API_SG.GEN.PARL.ZS_DS2_en_csv_v2_3731396.csv"
GDP_path      <- "data/GDP per capita/API_NY.GDP.PCAP.CD_DS2_en_csv_v2_3731360.csv"
labPar_path   <- "data/labor force participation rate female/API_SL.TLF.CACT.FE.ZS_DS2_en_csv_v2_3731491.csv"
ODA_path      <- "data/ODA amount/API_DT.ODA.ODAT.PC.ZS_DS2_en_csv_v2_3757406.csv"
oldAge_path   <- "data/population ratio higher 64/API_SP.POP.65UP.TO.ZS_DS2_en_csv_v2_3751797.csv"
youngAge_path <- "data/population ratio below 14/API_SP.POP.0014.TO.ZS_DS2_en_csv_v2_3754383.csv"
TB_path       <- "data/TB prevalance/API_SH.TBS.INCD_DS2_en_csv_v2_3735850.csv"
demo_path     <- "data/DemocracyMatrix_v4.csv"
totalExo_path <- "data/total-expenditure-to-gdpratio/NHA indicators.xlsx"
pop_path      <- "data/population/API_SP.POP.TOTL_DS2_en_csv_v2_3852487.csv"
private_path  <- "data/private health expenditure/API_SH.XPD.PVTD.CH.ZS_DS2_en_csv_v2_3761158.csv"
GNI_path      <- "data/GNI/API_NY.GNP.PCAP.CD_DS2_en_csv_v2_3889743.csv"
leader_path   <- "data/leader.xlsx"

non_country <- c("AFE", "AFW", "ARB","CEB","CSS","EAP", "EAR", "EAS","ECA","ECS","EMU","EUU","FCS", "HIC",
                 "HPC","IBD","IBT","IDA","IDB","IDX","INX","LAC","LCN","LDC","LIC","LMC","LTE","MEA","MIC",
                 "MNA","NAC","OED","OSS","PRE","PSS","PST","SAS","SSA","SSF","SST","TEA","TEC","TLA","TMN","TSA",
                 "TSS","UMC","WLD")

OECD_country <- c("AUS", "AUT", "BEL", "CAN", "CHL", "COL", "CRI", "CZE", "DNK", "EST", "FIN", "FRA", "DEU", "GRC",
                  "HUN", "ISL", "IRL", "ISR" ,"ITA", "JPN", "KOR", "LVA", "LTU", "LUX", "MEX", "NLD", "NZL", "NOR",
                  "POL", "PRT", "SVK", "SVN", "ESP", "SWE", "CHE", "TUR", "GBR", "USA")

Developed_country <- c("AUS", "AUT", "BEL", "CAN", "CZE", "DNK", "EST", "FIN", "FRA", "DEU", "GRC",
                  "ISL", "IRL", "ISR" ,"ITA", "JPN", "KOR", "LVA", "LTU", "LUX", "NLD", "NZL", "NOR",
                   "PRT", "SVK", "SVN", "ESP", "SWE", "CHE", "GBR", "USA")

Emerging_country <- c("ARG", "BGD", "BRA", "BGR", "CHL", "CHN", "COL", "HUN", "IND", "IDN", "MYS", "MEX", "MAR",
                      "PAK", "PER", "PHL", "POL", "ROM", "RUS", "ZAF", "THA", "TUR", "UKR" ,"VEN")

LDC_country <- c("AFG", "AGO", "BGD", "BEN", "BTN", "BFA", "BDI", "KHM", "CAF", "TCD", "COM", "ZAR", "DJI", "ERI",
                 "ETH", "ETF", "GMB", "GIN", "GNB", "HTI", "KIR", "LAO", "LSO", "LBR", "MDG", "MWI", "MLI", "MRT", 
                 "MOZ", "MMR", "NPL", "NER", "RWA", "STP", "SEN", "SLE", "SLB", "SOM", "SDN", "TMP", "TGO", "TUV",
                 "UGA", "TZA", "YDR", "YEM", "ZMB")

health_year_series <-  1972:2021 %>% as.character()
health_label <-  c("Type", "Code", "Country", "Country_Code",health_year_series)
health_exp <- read_csv(health_path, col_names = health_label, show_col_types = FALSE,skip = 1) %>% 
  select(3:4, 33:52)
health_exp[3:22] <- convert.magic(health_exp[3:22], "numeric")

country_code_map <- health_exp %>% select(Country, Country_Code)

private_year_series <- 1960:2020 %>% as.character()
private_label <- c("Country", "Country_Code","Name","Code",private_year_series)
private_exp <- read_csv(private_path, col_names = private_label,skip = 5,show_col_types = FALSE) %>% 
  select(1:2, 45:64) %>% 
  filter(!Country_Code %in% non_country)

lawmaker_year_series <- 1960:2020 %>% as.character()
lawmaker_label <- c("Country", "Country_Code","Name","Code", lawmaker_year_series)
lawmaker_ratio <- read_csv(lawmaker_path, col_names = lawmaker_label,skip = 5,show_col_types = FALSE) %>% 
  select(1:2, 45:64) %>% 
  filter(!Country_Code %in% non_country)

gdp_year_series <- 1960:2020 %>% as.character()
gdp_label <- c("Country", "Country_Code","Name","Code", gdp_year_series)
gdp_series <- read_csv(GDP_path,skip = 5,col_names = gdp_label,show_col_types = FALSE) %>% 
  select(1:2, 45:64) %>% 
  filter(!Country_Code %in% non_country)

labPar_year_series <- 1960:2020 %>% as.character()
labPar_label <- c("Country", "Country_Code","Name","Code", labPar_year_series)
labPar_series <- read_csv(labPar_path,skip = 5,col_names = labPar_label,show_col_types = FALSE) %>% 
  select(1:2, 45:64) %>% 
  filter(!Country_Code %in% non_country)

ODA_year_series <- 1960:2020 %>% as.character()
ODA_label <- c("Country", "Country_Code","Name","Code", ODA_year_series)
ODA_series <- read_csv(ODA_path,skip = 5,col_names = ODA_label,show_col_types = FALSE) %>% 
  select(1:2, 45:64) %>% 
  filter(!Country_Code %in% non_country)

oldAge_year_series <- 1960:2020 %>% as.character()
oldAge_label <- c("Country", "Country_Code","Name","Code", oldAge_year_series)
oldAge_series <- read_csv(oldAge_path,skip = 5,col_names = oldAge_label,show_col_types = FALSE) %>% 
  select(1:2, 45:64) %>% 
  filter(!Country_Code %in% non_country)

youngAge_year_series <- 1960:2020 %>% as.character()
youngAge_label <- c("Country", "Country_Code","Name","Code", youngAge_year_series)
youngAge_series <- read_csv(youngAge_path,skip = 5,col_names = youngAge_label,show_col_types = FALSE) %>% 
  select(1:2, 45:64) %>% 
  filter(!Country_Code %in% non_country)

TB_year_series <- 1960:2020 %>% as.character()
TB_label <- c("Country", "Country_Code","Name","Code", oldAge_year_series)
TB_series <- read_csv(TB_path,skip = 5,col_names = oldAge_label,show_col_types = FALSE) %>% 
  select(1:2, 45:64) %>% 
  filter(!Country_Code %in% non_country)

total_exp_series <- 2000:2019 %>% as.character()
total_exp_label <- c("Country","Type","skip",total_exp_series)
total_exp <- read_excel("data/total-expenditure-to-gdpratio/NHA indicators.xlsx", 
                        col_names = total_exp_label) %>% 
             select(-2,-3)
total_exp <- total_exp[-1:-2,]
total_exp[2:21] <- convert.magic(total_exp[2:21], "numeric")

demo_index <- read_csv(demo_path, show_col_types = FALSE) %>% select(country, year, total_index_core) %>% 
  filter(year %in% 2000:2019) %>% 
  mutate(democra = total_index_core) %>% 
  select(-total_index_core)

demo_class <- read_csv(demo_path, show_col_types = FALSE) %>% select(country, year, classification_core) %>% 
  filter(year %in% 2000:2019) %>% 
  mutate(class = classification_core) %>% 
  select(-classification_core)

pop_year_series <- 1960:2020 %>% as.character()
pop_label <- c("Country", "Country_Code","Name","Code", pop_year_series)
pop_series <- read_csv(pop_path,skip = 5,col_names = pop_label,show_col_types = FALSE) %>% 
  select(1:2, 45:64) %>% 
  filter(!Country_Code %in% non_country)

GNI_year_series <- 1960:2020 %>% as.character()
GNI_label <- c("Country", "Country_Code","Name","Code", GNI_year_series)
GNI_series <- read_csv(GNI_path,skip = 5,col_names = pop_label,show_col_types = FALSE) %>% 
  select(1:2, 45:64) %>% 
  filter(!Country_Code %in% non_country)

criteria <- read_excel("data/criteria.xlsx")
leader <- read_excel(leader_path)


#Clean the Data
gdp_clean <- remove.all.na(gdp_series) %>% 
  pivot_longer(3:22, names_to = "year", values_to = "gdp") %>% 
  fill.with.average() %>% mutate(year = 2000:2019) %>% 
  select(year,everything()) %>% 
  pivot_longer(2:204, names_to = "country", values_to = "gdp") %>% 
  select(country,everything())

health_clean <- remove.all.na(health_exp) %>% 
  pivot_longer(3:22, names_to = "year", values_to = "healthExp") %>% 
  fill.with.average() %>% mutate(year = 2000:2019) %>% 
  select(year,everything()) %>% 
  pivot_longer(2:186, names_to = "country", values_to = "healthExp") %>% 
  select(country,everything())

private_clean <- remove.all.na(private_exp) %>% 
  pivot_longer(3:22, names_to = "year", values_to = "private") %>% 
  fill.with.average() %>% mutate(year = 2000:2019) %>% 
  select(year,everything()) %>% 
  pivot_longer(2:187, names_to = "country", values_to = "private") %>% 
  select(country, everything())

lawmaker_clean <- remove.all.na(lawmaker_ratio) %>% 
  pivot_longer(3:22, names_to = "year", values_to = "lawmaker") %>% 
  fill.with.average() %>% mutate(year = 2000:2019) %>% 
  select(year,everything()) %>% 
  pivot_longer(2:182, names_to = "country", values_to = "lawmaker") %>% 
  select(country, everything())

labPar_clean <- remove.all.na(labPar_series) %>% 
  pivot_longer(3:22, names_to = "year", values_to = "labPar") %>% 
  fill.with.average() %>% mutate(year = 2000:2019) %>% 
  select(year,everything()) %>% 
  pivot_longer(2:189, names_to= "country", values_to ="labPar")

ODA_Clean <- ODA_series %>% 
  replace(is.na(.),0) %>% 
  pivot_longer(3:22, names_to = "year", values_to = "ODA") %>% 
  mutate(year = as.numeric(year),country = Country) %>% 
  select(country,year,ODA) 

oldAge_clean <- remove.all.na(oldAge_series) %>% 
  pivot_longer(3:22, names_to = "year", values_to = "oldAge") %>% 
  fill.with.average() %>% mutate(year = 2000:2019) %>% 
  select(year,everything()) %>% 
  pivot_longer(2:195, names_to = "country", values_to = "oldAge") %>% 
  select(country, everything())

youngAge_clean <- remove.all.na(youngAge_series) %>% 
  pivot_longer(3:22, names_to = "year", values_to = "youngAge") %>% 
  fill.with.average() %>% mutate(year = 2000:2019) %>% 
  select(year,everything()) %>% 
  pivot_longer(2:195, names_to = "country", values_to = "youngAge") %>% 
  select(country, everything())

TB_clean <-remove.all.na(TB_series) %>% 
  pivot_longer(3:22, names_to = "year", values_to = "TB") %>% 
  fill.with.average() %>% mutate(year = 2000:2019) %>% 
  select(year,everything()) %>% 
  pivot_longer(2:206, names_to = "country", values_to = "TB") %>% 
  select(country,everything())

total_clean <- remove.all.na.totalExp(total_exp) %>% 
  pivot_longer(2:21, names_to = 'year', values_to = 'totalExp') %>% 
  fill.with.average.totalExp() %>% mutate(year = 2000:2019) %>% 
  select(year,everything()) %>% 
  pivot_longer(2:188, names_to = 'country', values_to = "totalExp") %>% 
  select(country, everything())

pop_clean <- remove.all.na(pop_series) %>% 
  pivot_longer(3:22, names_to = "year", values_to = "pop") %>% 
  fill.with.average() %>% mutate(year = 2000:2019) %>% 
  select(year,everything()) %>% 
  pivot_longer(2:218, names_to = "country", values_to = "pop") %>% 
  select(country, everything())

leader_clean <- leader %>% pivot_longer(2:21,names_to = "year") %>% 
  rename( "country" = "...1") %>% 
  rename("leader_gen" = "value") %>% 
  mutate(leader_gen_dummy = if_else(leader_gen == "M",0,1)) %>% 
  select(-leader_gen)
  leader_clean$year <- leader_clean$year %>% as.numeric()
  #gsub('\\s+', '', leader_clean$country) %>% unique() 
    
GNI_clean <- remove.all.na(GNI_series)%>% 
    pivot_longer(3:22, names_to = "year", values_to = "GNI") %>% 
    fill.with.average() %>% mutate(year = 2000:2019) %>% 
    select(year,everything()) %>% 
    pivot_longer(2:188, names_to = "country", values_to = "GNI") %>% 
    select(country, everything())

criteria <- criteria %>% 
  rename("category" = "...1") %>% 
  pivot_longer(2:21, names_to = "year", values_to = "criteria")
criteria$year <- criteria$year %>% as.numeric()

#Join the data
joint_data <- lawmaker_clean %>% 
  left_join(health_clean,by = c("country", "year")) %>% 
  left_join(private_clean, by = c("country", "year")) %>% 
  left_join(gdp_clean,by = c("country", "year")) %>% 
  left_join(labPar_clean,by = c("country", "year")) %>% 
  left_join(ODA_Clean,by = c("country", "year")) %>% 
  left_join(oldAge_clean,by = c("country", "year")) %>% 
  left_join(youngAge_clean, by = c("country", "year")) %>% 
  left_join(TB_clean,by = c("country", "year")) %>% 
  left_join(demo_index, by = c("country", "year")) %>% 
  left_join(demo_class, by = c("country", "year")) %>% 
  left_join(total_clean, by = c("country", "year")) %>% 
  left_join(pop_clean, by = c("country", "year")) %>% 
  drop_na()

joint_data %<>% group_by(country) %>% 
  nest() %>% 
  mutate(n = map_dbl(data, nrow)) %>% 
  filter(n == 20) %>% 
  select(-n) %>% 
  unnest(cols = c(data)) %>% 
  ungroup()

joint_data <-joint_data %>% add_column(leader_gen = leader_clean$leader_gen_dummy)
joint_data <- joint_data %>% left_join(GNI_clean, by = c("country", "year"))

joint_data %<>%
  mutate(temp = GNI) %>% 
  mutate(GNI = if_else(country == "Greece", 20000, 
                       if_else(country == "Djibouti", 4000, temp))) %>% 
  select(-temp)

joint_data$income_level <- map2_chr(joint_data$year, joint_data$GNI, income.classify)


joint_data <- joint_data %>% group_by(year)  %>% mutate(lawmaker_average =  mean(lawmaker),
                                                        lawmaker_diff = lawmaker - lawmaker_average) #calculate the average lawmaker per year

joint_data <- joint_data %>% mutate()

joint_data <- joint_data %>% mutate(hc2total = healthExp*(1/totalExp)*100, #calculate the pubici to total
                                    class_rough = if_else(class %in% c("Deficient Democracy", "Working Democracy"  ), "Democracy", "Autocracy" ),
                                    demo_dummy = ifelse(democra >= 0.5, 1, 0),
                                    lawmaker_dummy = ifelse(lawmaker > lawmaker_average, 1, 0), #This can be troublesome 
                                    dummyIntersect = lawmaker_dummy * demo_dummy,
                                    publicprivate = ifelse(private > 50, 0, 1)) #Set up the dummies

joint_data <- joint_data %>% mutate(totalGDP = gdp * pop,
                      hcMoney = healthExp/100*totalGDP,
                      hcPerCapita = hcMoney/pop) 
  

joint_data_lagLaw <- joint_data %>% group_by(country) %>% 
  mutate(lag_lawmaker = dplyr::lag(lawmaker, order_by = country),
         lag_lawmakerDummy = dplyr::lag(lawmaker_dummy, order_by = country),
         lag_dummyIntersect = dplyr::lag(dummyIntersect, order_by = country),
         lag_leader = dplyr::lag(leader_gen, order_by = country),
         lag_diff = dplyr::lag(lawmaker_diff, order_by = country)) %>% 
  drop_na() %>% 
  ungroup()

#OECD_joint <- joint_data %>% filter(country %in% (country_code_map %>% filter(Country_Code %in% OECD_country))$Country)
#Developed_joint<- joint_data %>% filter(country %in% (country_code_map %>% filter(Country_Code %in% Developed_country))$Country)
#Emerging_joint <- joint_data %>% filter(country %in% (country_code_map %>% filter(Country_Code %in% Emerging_country))$Country)
#LDC_joint <- joint_data %>% filter(country %in% (country_code_map %>% filter(Country_Code %in% LDC_country))$Country)
#
#
#OECD_joint_lag <- joint_data_lagLaw %>% filter(country %in% (country_code_map %>% filter(Country_Code %in% OECD_country))$Country)
#Developed_joint_lag <- joint_data_lagLaw %>% filter(country %in% (country_code_map %>% filter(Country_Code %in% Developed_country))$Country)
#Emerging_joint_lag <- joint_data_lagLaw %>% filter(country %in% (country_code_map %>% filter(Country_Code %in% Emerging_country))$Country)
#LDC_joint_lag <- joint_data_lagLaw %>% filter(country %in% (country_code_map %>% filter(Country_Code %in% LDC_country))$Country)

HI_data<- joint_data %>% select(c(1:18,21,29)) %>% 
  filter(income_level == "High") %>% group_by(year) %>% 
  mutate(lawmaker_average = mean(lawmaker)) %>% 
  ungroup() %>% 
  mutate(demo_dummy = if_else(class_rough == "Autocracy",0,1),
         lawmaker_dummy = if_else(lawmaker >= lawmaker_average, 1, 0),
         demo_law = demo_dummy*lawmaker_dummy,
         law_leader = leader_gen * lawmaker_dummy)

Upper_data <- joint_data %>% select(c(1:18,21,29)) %>% 
  filter(income_level == "Upper") %>% group_by(year) %>% 
  mutate(lawmaker_average = mean(lawmaker)) %>% 
  ungroup() %>% 
  mutate(demo_dummy = if_else(class_rough == "Autocracy",0,1),
         lawmaker_dummy = if_else(lawmaker >= lawmaker_average, 1, 0),
         demo_law = demo_dummy*lawmaker_dummy,
         law_leader = leader_gen * lawmaker_dummy)

Lower_data <- joint_data %>% select(c(1:18,21,29)) %>% 
  filter(income_level == "Lower") %>% group_by(year) %>% 
  mutate(lawmaker_average = mean(lawmaker)) %>% 
  ungroup() %>% 
  mutate(demo_dummy = if_else(class_rough == "Autocracy",0,1),
         lawmaker_dummy = if_else(lawmaker >= lawmaker_average, 1, 0),
         demo_law = demo_dummy*lawmaker_dummy,
         law_leader = leader_gen * lawmaker_dummy)

LI_data <- joint_data %>% select(c(1:18,21,29)) %>% 
  filter(income_level == "Low") %>% group_by(year) %>% 
  mutate(lawmaker_average = mean(lawmaker)) %>% 
  ungroup() %>% 
  mutate(demo_dummy = if_else(class_rough == "Autocracy",0,1),
         lawmaker_dummy = if_else(lawmaker >= lawmaker_average, 1, 0),
         demo_law = demo_dummy*lawmaker_dummy,
         law_leader = leader_gen * lawmaker_dummy)

HI_lag_data <- HI_data %>% 
  mutate(lag_law = lag(lawmaker),
         lag_leader = lag(leader_gen),
         lag_law_average = lag(lawmaker_average),
         lag_demo = demo_dummy,
         lag_demo_law = lag_law * lag_demo,
         lag_law_leader = lag_law * lag_leader,
         lag_law_dummy = lag(lawmaker_dummy)
         ) %>% 
  drop_na()

Upper_lag_data <- Upper_data %>% 
  mutate(lag_law = lag(lawmaker),
         lag_leader = lag(leader_gen),
         lag_law_average = lag(lawmaker_average),
         lag_demo = demo_dummy,
         lag_demo_law = lag_law * lag_demo,
         lag_law_leader = lag_law * lag_leader,
         lag_law_dummy = lag(lawmaker_dummy)
  ) %>% 
  drop_na()

Lower_lag_data <- Lower_data %>% 
  mutate(lag_law = lag(lawmaker),
         lag_leader = lag(leader_gen),
         lag_law_average = lag(lawmaker_average),
         lag_demo = demo_dummy,
         lag_demo_law = lag_law * lag_demo,
         lag_law_leader = lag_law * lag_leader,
         lag_law_dummy = lag(lawmaker_dummy)
  ) %>% 
  drop_na()

LI_lag_data <- LI_data %>% 
  mutate(lag_law = lag(lawmaker),
         lag_leader = lag(leader_gen),
         lag_law_average = lag(lawmaker_average),
         lag_demo = demo_dummy,
         lag_demo_law = lag_law * lag_demo,
         lag_law_leader = lag_law * lag_leader,
         lag_law_dummy = lag(lawmaker_dummy)
  ) %>% 
  drop_na()
