library(readr)
library(devtools)
library(dplyr)
library(readr)
library(tidyr)
library(openxlsx)
library(data.table)

#--- COnvert data to RDS----

site_im_all <- readRDS("Site_ALL_OU_FY18Q4_clean_12_27.rds")

psnu_im_all <- readRDS("PSNU_All_OU_FY18Q4_clean_12_27.rds")


# psnu_IM raw indicators----

psnu_indi <- c('LAB_PTCQI', 'LAB_PTCQI_MIL', 'HTS_TST', 'HTS_TST_POS', 'PMTCT_STAT_POS', 'PMTCT_EID', 
               'PMTCT_HEI_POS', 'TX_CURR','TX_NEW', 'TX_PVLS', 'TX_TB')

site_ind <-  c('LAB_PTCQI', "LAB_PTCQI_MIL")

## old code ----
# psnu_im <- psnu_im_all %>% select( -FY2017_TARGETS:-FY2017Q3, -MechanismID, - ImplementingMechanismName,-mechanismUID,-AgeFine, -AgeSemiFine,
#                                   -FY2018Q1:-FY2018Q3, -PSNU, -PSNUuid, -indicatorType, -CountryName) %>% 
#   filter(indicator %in% psnu_indi,!(indicator=='TX_NEW' & AgeAsEntered!='<01'),
#          !(indicator %in% c('HTS_TST','HTS_TST_POS', 'TX_CURR')& standardizedDisaggregate!='Total Numerator' & 
#              !grepl('Modality', standardizedDisaggregate))) %>% 
#   group_by_at(vars(Region:isMCAD)) %>% 
#   summarise_all(sum, na.rm=T) %>% 
#   separate(categoryOptionComboName, c('lab-testing', 'category', 'status'), sep='-', remove=F) %>% 
#   separate(category, c('tests.type', 'CQI_PT_POCT'), sep=',', remove=T) %>% 
#   select(-`lab-testing`)

# lab <- psnu_im_all %>% filter(indicator %in% c('LAB_PTCQI', 'LAB_PTCQI_MIL')) %>% 
#   group_by(OperatingUnit, SNU1,SNU1Uid,PrimePartner,FundingAgency,indicator,numeratorDenom, standardizedDisaggregate,
#            otherDisaggregate,categoryOptionComboName) %>% 
#   summarise_at(vars(FY2017Q4:FY2019_TARGETS), sum, na.rm=T) %>% 
#   separate(categoryOptionComboName, c('lab-testing', 'category', 'status'), sep='-', remove=T) %>% 
#   separate(category, c('tests.type', 'CQI_PT_POCT'), sep=',', remove=T) %>% 
#   select(-`lab-testing`)


# filter on each indicator ----
hts <- psnu_im_all %>% filter(indicator %in% c('HTS_TST', 'HTS_TST_POS') & 
                                standardizedDisaggregate %in% c('Total Numerator', 'Modality/Age/Sex/Result')) %>% 
  group_by(OperatingUnit, SNU1,SNU1Uid,PrimePartner,FundingAgency,indicator, standardizedDisaggregate,numeratorDenom,
           otherDisaggregate, modality) %>% 
  summarise_at(vars(FY2017Q4:FY2019_TARGETS), sum, na.rm=T)
  
pmtct <- psnu_im_all %>% 
  filter(indicator %in% c('PMTCT_STAT_POS', 'PMTCT_EID','PMTCT_HEI_POS')) %>% 
  group_by(OperatingUnit, SNU1,SNU1Uid,PrimePartner,FundingAgency,indicator,numeratorDenom, standardizedDisaggregate,
           otherDisaggregate,categoryOptionComboName, AgeAsEntered) %>% 
  summarise_at(vars(FY2017Q4:FY2019_TARGETS), sum, na.rm=T)

new <- psnu_im_all %>% 
  filter(indicator=='TX_NEW' & AgeAsEntered == '<01') %>% 
  group_by(OperatingUnit, SNU1,SNU1Uid,PrimePartner,FundingAgency,indicator,numeratorDenom, standardizedDisaggregate,
           otherDisaggregate,categoryOptionComboName, AgeAsEntered, Sex) %>% 
  summarise_at(vars(FY2017Q4:FY2019_TARGETS), sum, na.rm=T)
  
curr <- psnu_im_all %>% 
  filter(indicator=='TX_CURR' & standardizedDisaggregate=='Total Numerator') %>% 
  group_by(OperatingUnit, SNU1,SNU1Uid,PrimePartner,FundingAgency,indicator,numeratorDenom, standardizedDisaggregate,
           otherDisaggregate,categoryOptionComboName) %>% 
  summarise_at(vars(FY2017Q4:FY2019_TARGETS), sum, na.rm=T)

pvls <- psnu_im_all %>% 
  filter(indicator=='TX_PVLS') %>% 
  group_by(OperatingUnit, SNU1,SNU1Uid,PrimePartner,FundingAgency,indicator,numeratorDenom, standardizedDisaggregate,
           otherDisaggregate, AgeAsEntered,Sex) %>% 
  summarise_at(vars(FY2017Q4:FY2019_TARGETS), sum, na.rm=T)

tb <- psnu_im_all %>% 
  filter(indicator=='TX_TB') %>% 
  group_by(OperatingUnit, SNU1,SNU1Uid,PrimePartner,FundingAgency,indicator,numeratorDenom, standardizedDisaggregate,
           otherDisaggregate) %>% 
  summarise_at(vars(FY2017Q4:FY2019_TARGETS), sum, na.rm=T) %>% 
  ungroup() %>% 
  rowwise() %>% 
  mutate(FY2018APR=sum(FY2018Q2,FY2018Q4,na.rm = T))


snu_im <- bind_rows(hts,lab,new,pmtct,pvls,tb,curr)


# write.xlsx(snu_im,'snu_im_lab_fy18_clean.xlsx')
  

# merge TX_PVLS FY15-16 data
psnu_im15 <- fread("ICPI_MER_Structured_Dataset_PSNU_IM_FY15-16_20180515_v1_1.txt", na.strings = 'NULL')

psnu15.viral <- psnu_im15 %>% filter(indicator %in% c('TX_VIRAL', 'TX_UNDETECT'))

pvls1516 <- psnu15.viral %>%  filter(standardizedDisaggregate %in% c('Total Numerator', 'Total Denominator')) %>% 
  group_by(OperatingUnit, SNU1,SNU1Uid,PrimePartner,FundingAgency,indicator,numeratorDenom, standardizedDisaggregate,
           otherDisaggregate) %>% 
  summarise_at(vars(FY2015Q4:FY2016APR),sum, na.rm=T) %>% 
  ungroup() 

# Append TX_VIRAL Fy15,16 data 

snu_im_15_18 <-  bind_rows(snu_im, pvls1516)


write.xlsx(snu_im_15_18, 'SNU_IM_lab_fy18Q4_clean_1_03.xlsx')

# Create Site-level file ----
site_im <-  df.site %>% filter(indicator %in% site_ind) %>% 
  select( -FY2017_TARGETS:-FY2017Q4,-CountryName,-Region,-RegionUID,
         -FY2018Q1:-FY2018Q4, -resultStatus, -indicatorType) %>% 
  mutate_at(vars(FY2017APR:FY2019_TARGETS), funs(as.numeric)) %>% 
  group_by_at(vars(orgUnitUID:isMCAD)) %>% 
  summarise_all(sum, na.rm=T) %>% 
  separate(categoryOptionComboName, c('lab-testing', 'category', 'status'), sep='-', remove=F) %>% 
  separate(category, c('tests.type', 'CQI_PT_POCT'), sep=',', remove=T) %>% 
  select(-`lab-testing`) %>% 
  filter(tests.type %in% c(" HIV Viral Load"," TB Xpert"," HIV Serology/Diagnostic"," HIV IVT/EID"))


site_im2 <- site_im %>% filter(tests.type %in% c(" HIV Viral Load"," TB Xpert"," HIV Serology/Diagnostic"," HIV IVT/EID")) %>% 
  group_by(OperatingUnit,SNU1, PSNU, SNUPrioritization, PrimePartner, FundingAgency, ImplementingMechanismName, 
           Facility, SiteType,indicator, standardizedDisaggregate, categoryOptionComboName, tests.type,
           CQI_PT_POCT,status,otherDisaggregate) %>% 
  summarise(FY2018APR=sum(FY2018APR,na.rm = T), FY2017APR=sum(FY2017APR,na.rm = T)) %>% 
  ungroup()

site_im3 <- site_im2 %>% filter(SiteType != 'Community')



# write.xlsx(site_im,'site_im_lab.xlsx')

site_hts <- df.site %>% filter(indicator %in% c('HTS_TST', 'HTS_TST_POS'),  
                                   !(indicator %in% c('HTS_TST','HTS_TST_POS')& 
                                       standardizedDisaggregate!='Total Numerator'), 
                                   !(indicator %in% c('HTS_TST','HTS_TST_POS')& SiteType != 'Facility')) %>% 
  group_by_at(vars(OperatingUnit,SNU1, PSNU, SNUPrioritization, PrimePartner, FundingAgency, ImplementingMechanismName, 
                   Facility, SiteType,indicator, standardizedDisaggregate, categoryOptionComboName, modality)) %>% 
  summarise(FY2017APR= sum(FY2017APR, na.rm=T),
            FY2018APR=sum(FY2018APR,na.rm = T)) %>% 
  ungroup() %>% 
  group_by(OperatingUnit) %>% 
  mutate(hts_vol_per = case_when(
    indicator == 'HTS_TST' ~ round(FY2018APR*100/sum(FY2018APR, na.rm=T),2),
    TRUE ~ NA_real_
  )) %>% 
  arrange(desc(hts_vol_per), .by_group=T) %>% 
  mutate(cum_hts_vol=cumsum(hts_vol_per),
         rank_hts=frank(cum_hts_vol, na.last ='keep')) %>% 
  ungroup()

top50_hts_sites <- site_hts %>% 
  filter(indicator=='HTS_TST'& rank_hts<=50) %>% 
  select(Facility, rank_hts)


# filter sites for hts/ht_pos
site_im_hts_top50 <- site_hts %>% 
  filter(Facility %in% top50_hts_sites$Facility)

check1 <- site_hts_lab %>% filter(indicator=='LAB_PTCQI') %>% 
  group_by(OperatingUnit, standardizedDisaggregate) %>%  
  summarise_at(vars(FY2018APR),sum,na.rm=T)

# append hts/hts_pos data to lab data
site_hts_lab <-  bind_rows(site_im3, site_im_hts_top50)

df <-  left_join(site_hts_lab, top50_hts_sites, by="Facility")

df <- df %>% 
  rename(rank_hts_vol=rank_hts.y) %>% select(-rank_hts.x)

# write site_alb file
write.xlsx(df, 'Site_IM_LAB_top50_HTS.xlsx')

 tx_new <- psnu_im1516 %>% 
  # filter(indicator=='PMTCT_EID') %>%
  group_by(indicator,standardizedDisaggregate, otherDisaggregate) %>% 
  summarise()
