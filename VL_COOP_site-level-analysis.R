library(readr)
library(devtools)
library(dplyr)
library(readr)
library(tidyr)
library(openxlsx)
library(data.table)
library(scales)
library(ggplot2)

memory.limit(size=90000000)


#site-level filter ----
site_im <- Site_IM_MSD_FY19Q1_02_27 %>% filter(indicator %in% c('TX_CURR', 'TX_NEW', 'TX_PVLS', 'LAB_PTCQI', 
                                                                'LAB_PTCQI_MIL', 'SC_STOCK')) 

saveRDS(site_im, 'site_im.rds')

site.lab <- site_im %>% filter(indicator=='LAB_PTCQI')
 tab1 <- site_im %>% group_by(indicator, standardizedDisaggregate, otherDisaggregate) %>%  summarise(fy2019=sum(FY2019Q1, na.rm = T))

 
lab.tab <- 
 
poc.sites <- site.lab %>% 
   filter(grepl('POCT', standardizedDisaggregate) , grepl('Viral Load|EID|TB Xpert', otherDisaggregate)) %>%
   group_by_at(vars(orgUnitUID:otherDisaggregate)) %>% 
   summarise(FY2018APR =sum(FY2018APR, na.rm = T)) %>% 
   filter(FY2018APR > 0) %>% 
   ungroup() %>% 
  separate(categoryOptionComboName, c('lab-testing', 'category', 'status'), sep='-', remove=T) %>% 
  separate(category, c('tests.type', 'CQI_PT_POCT'), sep=',', remove=T) %>% 
  select(-`lab-testing`)
  
poc.sites2 <- poc.sites %>% 
  group_by(orgUnitUID, SiteName, Facility, FacilityUID, indicator, standardizedDisaggregate, tests.type) %>% 
  summarise(FY2018APR=max(FY2018APR, na.rm = T)) %>% 
  ungroup()

poc.vol <- poc.sites2 %>% filter(grepl('TestVolume', standardizedDisaggregate))%>% 
  mutate(vol_test_type = paste(standardizedDisaggregate, tests.type, sep="_")) %>% 
  spread(vol_test_type, FY2018APR) 

poc.vol <- poc.vol %>% 
  group_by_at(vars(orgUnitUID:indicator)) %>% 
  summarise_at(vars(`POCT/TestVolume_ HIV IVT/EID`:`POCT/TestVolume_ TB Xpert`), sum, na.rm=T)

poc.sites3 <- poc.sites2 %>% 
  filter(!grepl('TestVolume', standardizedDisaggregate)) %>% 
  group_by(orgUnitUID, SiteName, Facility, FacilityUID, tests.type) %>% 
  summarise(FY2018APR=max(FY2018APR)) %>% 
  spread(tests.type, FY2018APR) %>% 
  ungroup()

poc.sites4 <- poc.sites3 %>% 
  rowwise() %>% 
  mutate(any_POCT= case_when(
    ` HIV IVT/EID` >=1 | ` HIV Viral Load` >= 1 | ` TB Xpert` >=1 ~ 'POCT',
    TRUE ~ 'NO POCT'
  ))


site.wide.lab <- full_join(poc.sites4, poc.vol) %>% select(-indicator)

tab2 <- poc.sites %>% group_by(OperatingUnit, SiteName) %>% summarise()


# tx indicators at site level
site_tx <- site_im_all %>% 
  filter(standardizedDisaggregate %in% c('Total Denominator', 'Total Numerator', 'Age/Sex/Indication/HIVStatus', 
                                         'Age/Sex/HIVStatus', "Age/NewExistingArt/Sex/HIVStatus", 'RoutineTargeted/HIVStatus', 
                                         'PregnantOrBreastfeeding/Indication/HIVStatus', 
                                         'TB Test Type/HIVStatus', 'Specimen Sent/HIVStatus'), 
         indicator %in% c('TX_CURR', 'TX_PVLS', 'PMTCT_ART','TX_TB'), !otherDisaggregate %in% c('Other', 'Smear')) %>%
  filter_at(vars(FY2018Q3:FY2019Q1), any_vars(!is.na(.)))

site_tx <- site_tx %>% filter(!(indicator == 'TX_TB' & standardizedDisaggregate %in% c('Total Denominator', 'Total Numerator')))

site_lng <- site_tx %>%
  gather(period,val, FY2018Q3, FY2018APR,FY2019Q1) %>% 
  mutate(Ind_pd_nd = paste(indicator,period, standardizedDisaggregate,numeratorDenom, sep = "-")) %>% 
  group_by(orgUnitUID, OperatingUnit,OperatingUnitUID, SNU1, PSNU,PSNUuid, SiteName, FacilityUID,Facility,
           FundingAgency, ImplementingMechanismName, PrimePartner, Sex,
           AgeAsEntered, AgeFine, Ind_pd_nd, otherDisaggregate) %>%
  summarise(val=sum(val, na.rm=T)) %>% 
  filter(val !=0) 

site_vl_wide <- site_lng %>% 
  spread(Ind_pd_nd,val) %>% 
  ungroup()

site_vl_wide <- site_vl_wide %>% 
  select(-`TX_CURR-FY2018APR-Age/Sex/HIVStatus-N`, -`TX_CURR-FY2018APR-Total Numerator-N`, 
         -`PMTCT_ART-FY2018APR-Total Denominator-D`, -`PMTCT_ART-FY2018APR-Total Numerator-N`,
         -`PMTCT_ART-FY2018Q3-Total Numerator-N`, -`PMTCT_ART-FY2018Q3-Total Denominator-D`,
         -`TX_PVLS-FY2018APR-Age/Sex/Indication/HIVStatus-D`: - `TX_PVLS-FY2018APR-Total Numerator-N`) %>% 
  filter_at(vars(`PMTCT_ART-FY2019Q1-Age/NewExistingArt/Sex/HIVStatus-N`:`TX_TB-FY2018APR-TB Test Type/HIVStatus-D`),
            any_vars(!is.na(.)))

site_vl_wide.new <- site_vl_wide %>% 
  select(-ImplementingMechanismName, -`TX_TB-FY2018APR-Specimen Sent/HIVStatus-D`, -AgeAsEntered) %>% 
  group_by_at(vars(orgUnitUID:otherDisaggregate)) %>% 
  summarise_at(vars(`PMTCT_ART-FY2019Q1-Age/NewExistingArt/Sex/HIVStatus-N`:`TX_TB-FY2018APR-TB Test Type/HIVStatus-D`),
               sum, na.rm=T)%>% 
  filter_at(vars(`PMTCT_ART-FY2019Q1-Age/NewExistingArt/Sex/HIVStatus-N`:`TX_TB-FY2018APR-TB Test Type/HIVStatus-D`),
            any_vars(.!=0))


site_vl_wide2 <- site_vl_wide %>% 
  group_by(orgUnitUID,PSNUuid,PSNU,FacilityUID,Facility) %>% 
  mutate(VL_Coverage_FY2019Q1= round(sum(`TX_PVLS-FY2019Q1-Total Denominator-D`,na.rm = T)*100/
                                       sum(`TX_CURR-FY2018Q3-Total Numerator-N`,na.rm = T),2),
         VL_Suppression_FY2019Q1 = round(sum(`TX_PVLS-FY2019Q1-Total Numerator-N`,na.rm = T)*100/
                                           sum(`TX_PVLS-FY2019Q1-Total Denominator-D`,na.rm=T),2), 
         VL_Coverage_FY2019Q1=replace(VL_Coverage_FY2019Q1, `TX_PVLS-FY2019Q1-Total Denominator-D`==0|
                                        is.na(`TX_PVLS-FY2019Q1-Total Denominator-D`), NA), 
         VL_Suppression_FY2019Q1=replace(VL_Suppression_FY2019Q1, `TX_PVLS-FY2019Q1-Total Numerator-N`==0|
                                           is.na(`TX_PVLS-FY2019Q1-Total Numerator-N`), NA),
         quadrant = case_when(
           VL_Coverage_FY2019Q1 >= 75 & VL_Suppression_FY2019Q1 >=90 ~ 1,
           VL_Coverage_FY2019Q1 >= 75 & VL_Suppression_FY2019Q1 < 90 ~ 3,
           VL_Coverage_FY2019Q1 < 75 & VL_Suppression_FY2019Q1 >=90 ~ 2,
           VL_Coverage_FY2019Q1 < 75 & VL_Suppression_FY2019Q1 <90 ~ 4,
           TRUE ~ NA_real_)) %>% 
  filter_at(vars(`PMTCT_ART-FY2019Q1-Age/NewExistingArt/Sex/HIVStatus-N`:quadrant), any_vars(!is.na(.)))


site_vl_lab_wide <- left_join(site_vl_wide2, site.wide.lab)

write.xlsx(site_vl_lab_wide, 'site_VL_COOP_Fy19Q1.xlsx')
saveRDS(site_lng, 'VL_COOP_site_long.rds')



# TB data from FY18Q4
site.tb <- site_im_all %>% filter(indicator %in% c('TX_TB'))
saveRDS(site.tb, 'site.tb.03.26.rds')
tb <- site.tb %>% group_by(indicator,standardizedDisaggregate, otherDisaggregate, numeratorDenom) %>% 
  summarise(fy18apr=sum(FY2018APR, na.rm = T))

site.tb2 <- site.tb %>% 
  filter(standardizedDisaggregate %in% c('TB Test Type/HIVStatus', 'Specimen Sent/HIVStatus'), 
         otherDisaggregate %in% c('Xpert', '')) %>% 
  select(-FY2017_TARGETS:-FY2018Q4,-FY2019_TARGETS,-FY2019Q1) %>% 
  filter(!is.na(FY2018APR),FY2018APR != 0) %>% 
  gather(period, val, FY2018APR) %>% 
  mutate(Ind_pd_std = paste(indicator, period, standardizedDisaggregate, numeratorDenom, sep = '_'))

site.tb_wide <- site.tb2 %>% 
  group_by(orgUnitUID, OperatingUnit,SNU1, PSNU, PSNUuid, FundingAgency, ImplementingMechanismName, 
           SiteName, Facility, FacilityUID, otherDisaggregate, Ind_pd_std) %>% 
  summarise(val = sum(val, na.rm = T)) %>% 
  filter(val != 0) %>% 
  spread(Ind_pd_std, val) %>% 
  ungroup() 
