library(readr)
library(devtools)
library(dplyr)
library(readr)
library(tidyr)
library(openxlsx)
library(data.table)
library(scales)
library(ggplot2)


site_im <- readRDS('site_im.rds')
psnu_im <- readRDS('psnu_im.rds')


## --- Std & Other disaggs to keep -----    

lab_std_disagg <- c('Lab/CQI','Lab/PT',"Lab/TestVolume","POCT/CQI", "POCT/PT","POCT/TestVolume",'LabCount','LabCountDoDOnly')

lab_oth_disagg <- c("Lab Testing - HIV Viral Load, LAB_CQI - Audited and fully accredited",
                    "Lab Testing - HIV Viral Load, LAB_CQI - Testing with no participation",
                    "Lab Testing - HIV Viral Load, PT - Participate and passed last round",
                    "Lab Testing - HIV Viral Load, PT - Testing with no participation",
                    "Lab Testing - HIV Viral Load, POCT_CQI - Audited score 4-5",
                    'Lab Testing - HIV Viral Load')

pmtct_std_disagg <- c("NewExistingArt/Sex/HIVStatus","Total Numerator", "Age/NewExistingArt/Sex/HIVStatus")
pmtct_oth_disagg <- c("Life-long ART, Already","Life-long ART, New","")

stock_std_disagg <- c('ObservedCommodity')
stock_oth_disagg <- c("First Line ARV","Second Line ARV")

curr_std_disagg <- c('Age/Sex/HIVStatus', 'Total Numerator')
new_std_disagg <- c('Age/Sex/HIVStatus', 'Total Numerator', "PregnantOrBreastfeeding/HIVStatus")
new_oth_disagg <- c("Breastfeeding","Pregnant","")

pvls_std_disagg <- c( "Age/Sex/Indication/HIVStatus", 
                      "PregnantOrBreastfeeding/Indication/HIVStatus",
                      "RoutineTargeted/HIVStatus", 
                      "Total Denominator", "Total Numerator")
pvls_oth_disagg <- c("Routine","Targeted","Undocumented Test Indication","Breastfeeding, Routine",
                     "Breastfeeding, Targeted","Breastfeeding, Undocumented Test Indication","Pregnant, Routine",
                     "Pregnant, Targeted","Pregnant, Undocumented Test Indication","")



# Filter psnu-im data ----

pvls <- psnu_im %>% filter(indicator=='TX_PVLS', standardizedDisaggregate %in% pvls_std_disagg,
                               otherDisaggregate %in% pvls_oth_disagg) %>% 
  select(-Region, -RegionUID, -CountryName, -typeMilitary, -DREAMS, -coarseDisaggregate, -modality, -FY2018_TARGETS,
         -indicatorType, -AgeCoarse, -mech_code:-award_number, -CountryName, -SNU1Uid, -StatusHIV:-population,
         -FY2019_TARGETS) %>% 
  group_by_at(vars(OperatingUnit:otherDisaggregate)) %>% 
  summarise_at(vars(FY2018APR:FY2019Q3), sum, na.rm=T) %>% 
  ungroup() %>% 
  filter_at(vars(FY2018APR:FY2019Q3), any_vars(.!=0))

new <- psnu_im %>% filter(indicator == 'TX_NEW', standardizedDisaggregate %in% new_std_disagg,
                              otherDisaggregate %in% new_oth_disagg) %>%
  select(-Region, -RegionUID, -CountryName, -typeMilitary, -DREAMS, -coarseDisaggregate, -modality, -FY2018_TARGETS,
         -indicatorType, -AgeCoarse, -mech_code:-award_number, -CountryName, -SNU1Uid, -StatusHIV:-population,
         -FY2019_TARGETS) %>% 
  group_by_at(vars(OperatingUnit:otherDisaggregate)) %>% 
  summarise_at(vars(FY2017Q3:FY2019Q3), sum, na.rm=T) %>% 
  ungroup() %>% 
  filter_at(vars(FY2017Q3:FY2019Q3), any_vars(.!=0))

curr <- psnu_im %>% filter(indicator == 'TX_CURR', standardizedDisaggregate %in% curr_std_disagg) %>%
  select(-Region, -RegionUID, -CountryName, -typeMilitary, -DREAMS, -coarseDisaggregate, -modality, -FY2018_TARGETS,
         -indicatorType, -AgeCoarse, -mech_code:-award_number, -CountryName, -SNU1Uid, -StatusHIV:-population,
         -FY2019_TARGETS) %>% 
  group_by_at(vars(OperatingUnit:otherDisaggregate)) %>% 
  summarise_at(vars(FY2018Q2:FY2019Q3), sum, na.rm=T) %>% 
  ungroup() %>% 
  filter_at(vars(FY2018Q2:FY2019Q3), any_vars(.!=0))

stock <- psnu_im %>% filter(indicator == 'SC_STOCK', standardizedDisaggregate %in% stock_std_disagg, 
                                otherDisaggregate %in% stock_oth_disagg) %>%
  select(-Region, -RegionUID, -CountryName, -typeMilitary, -DREAMS, -coarseDisaggregate, -modality, -FY2018_TARGETS,
         -indicatorType, -AgeCoarse, -mech_code:-award_number, -CountryName, -SNU1Uid, -StatusHIV:-population,
         -FY2019_TARGETS) %>% 
  group_by_at(vars(OperatingUnit:otherDisaggregate)) %>% 
  summarise_at(vars(FY2018Q4:FY2019Q3), sum, na.rm=T) %>% 
  ungroup() %>% 
  filter_at(vars(FY2018Q4:FY2019Q3), any_vars(.!=0))

pmtct <- psnu_im %>% filter(indicator == 'PMTCT_ART', standardizedDisaggregate %in% pmtct_std_disagg, 
                                otherDisaggregate %in% pmtct_oth_disagg) %>%
  select(-Region, -RegionUID, -CountryName, -typeMilitary, -DREAMS, -coarseDisaggregate, -modality, -FY2018_TARGETS,
         -indicatorType, -AgeCoarse, -mech_code:-award_number, -CountryName, -SNU1Uid, -StatusHIV:-population,
         -FY2019_TARGETS) %>% 
  group_by_at(vars(OperatingUnit:otherDisaggregate)) %>% 
  summarise_at(vars(FY2017Q3:FY2019Q3), sum, na.rm=T) %>% 
  ungroup() %>% 
  filter_at(vars(FY2017Q3:FY2019Q3), any_vars(.!=0))


lab <- psnu_im %>% filter(indicator %in% c('LAB_PTCQI', 'LAB_PTCQI_MIL'), standardizedDisaggregate %in% lab_std_disagg, 
                              otherDisaggregate %in% lab_oth_disagg) %>%
  select(-Region, -RegionUID, -CountryName, -typeMilitary, -DREAMS, -coarseDisaggregate, -modality, -FY2018_TARGETS,
         -indicatorType, -AgeCoarse, -mech_code:-award_number, -CountryName, -SNU1Uid, -StatusHIV:-population,
         -FY2019_TARGETS) %>% 
  group_by_at(vars(OperatingUnit:otherDisaggregate)) %>% 
  summarise_at(vars(FY2018Q4:FY2018APR), sum, na.rm=T) %>% 
  ungroup() %>% 
  filter_at(vars(FY2018Q4:FY2018APR), any_vars(.!=0))

psnu_im_vl <- bind_rows(pmtct,stock,curr,new,pvls,lab)

saveRDS(psnu_im_vl, 'psnu_im_VL.rds')
write.xlsx(psnu_im_vl, 'psnu_im_vl.xlsx')

#site-level filter ----
site_im <- site_im %>% filter(indicator %in% c('TX_CURR', 'TX_NEW', 'TX_PVLS', 'LAB_PTCQI', 
                                                   'LAB_PTCQI_MIL', 'SC_STOCK')) 

saveRDS(site_im, 'site_im.rds')


 # create count of sites----

site_cal_indi <- site_im %>% 
   filter(standardizedDisaggregate %in% c('Total Denominator', 'Total Numerator'), 
          indicator %in% c('TX_CURR', 'TX_PVLS')) 

# convert to wide
site_cal_indi2 <- site_cal_indi %>%
  gather(period,val, FY2019Q3) %>% 
  mutate(Ind_pd_nd = paste(indicator,period, standardizedDisaggregate, sep = "-")) %>% 
  group_by(OperatingUnit,OperatingUnitUID, SNU1, SNU1Uid, PSNU,PSNUuid, FacilityUID,Facility, Ind_pd_nd) %>%
  summarise(val=sum(val, na.rm=T)) %>% 
  mutate(val = replace(val, val==0, NA)) %>% 
  spread(Ind_pd_nd,val) %>% 
  ungroup()

# count sites that report on tx-curr etc
site_cal_indi3 <- site_cal_indi2 %>% 
  mutate(site_curr_reporting = case_when(
    !is.na(`TX_CURR-FY2019Q3-Total Numerator`) ~1,
    TRUE ~ 0), 
    site_vl_testing = case_when(
      !is.na(`TX_PVLS-FY2019Q3-Total Denominator`) ~ 1,
      TRUE ~ 0),
    site_vl_supp_reporting = case_when(
      !is.na(`TX_PVLS-FY2019Q3-Total Numerator`) ~ 1,
      TRUE ~ 0))

# convert to long
site_counts_lng <- site_cal_indi3 %>% 
  gather(indicator, FY2019Q3, site_curr_reporting:site_vl_supp_reporting) %>% 
  group_by_at(vars(OperatingUnit,OperatingUnitUID, SNU1, SNU1Uid, PSNU,PSNUuid, indicator)) %>% 
  summarise(FY2019Q3=sum(FY2019Q3, na.rm=T)) %>% 
  filter(FY2019Q3!= 0) %>% 
  ungroup()

#count total number of sites
tot.sites <- site_im %>% 
  group_by(OperatingUnit,OperatingUnitUID, SNU1, SNU1Uid, PSNU,PSNUuid) %>% 
  summarise(site_count = n_distinct(FacilityUID)) %>% 
  ungroup()

# conver to long
tot.sites_lng <- tot.sites %>% 
  gather(indicator, FY2019Q3, site_count)

# Calculate stockout sites

stock_site <- site_im %>% 
  filter(indicator == 'SC_STOCK', standardizedDisaggregate %in% stock_std_disagg, otherDisaggregate %in% stock_oth_disagg) %>% 
  gather(period, val, FY2019Q3) %>% 
  mutate(Ind_pd_nd = paste(indicator, period, numeratorDenom, sep = '_')) %>% 
  group_by(OperatingUnit, orgUnitUID, SNU1, SNU1Uid, PSNU, PSNUuid, Facility, FacilityUID, Ind_pd_nd) %>% 
  summarise(val=sum(val, na.rm = T)) %>% 
  spread(Ind_pd_nd, val) %>% 
  ungroup() 

stock_site2 <- stock_site %>% 
  mutate(site_stockout = case_when(
    SC_STOCK_FY2019Q3_N / SC_STOCK_FY2019Q3_D < 1 ~ 1,
    SC_STOCK_FY2019Q3_N / SC_STOCK_FY2019Q3_D >= 1 ~0
  ))

psnu_stock_site <-  stock_site2 %>% 
  group_by(OperatingUnit, SNU1, SNU1Uid, PSNU, PSNUuid) %>% 
  summarise(site_stockout = sum(site_stockout, na.rm = T)) %>% 
  gather(indicator, FY2019Q3, site_stockout) %>% 
  ungroup()

# rbind PSNU_IM & psnu_site data sets
psnuim_vl_cov_supp <- bind_rows(psnu_im_vl,site_counts_lng, tot.sites_lng, psnu_stock_site)


# write final psnu-im raw data ----
write.xlsx(psnuim_vl_cov_supp,'psnuim_vl_raw_09_30.xlsx')


# adding Age/Sex data to site-level wide data ----

site_cal_indi_new <- site_im %>% 
  filter(standardizedDisaggregate %in% c('Total Denominator', 'Total Numerator', 
                                         'Age/Sex/Indication/HIVStatus', 'Age/Sex/HIVStatus'), 
         indicator %in% c('TX_CURR', 'TX_PVLS')) %>%
  filter_at(vars(FY2019Q1:FY2019Q3), any_vars(!is.na(.)))

site_cal_indi <- site_cal_indi_new %>%
  gather(period,val, FY2019Q1,FY2019Q3) %>% 
  mutate(Ind_pd_nd = paste(indicator,period, standardizedDisaggregate,numeratorDenom, sep = "-")) %>% 
  group_by(orgUnitUID, OperatingUnit, SNU1, PSNU,PSNUuid, FacilityUID,Facility, 
           AgeFine, Sex, Ind_pd_nd) %>%
  summarise(val=sum(val, na.rm=T)) %>% 
  filter(val !=0) 

site_vl_wide <- site_cal_indi %>% 
  spread(Ind_pd_nd,val) %>% 
  ungroup()


site_patient_vol_sup_cov <- site_vl_wide %>% 
  select(-`TX_PVLS-FY2019Q1-Total Denominator-D`,-`TX_PVLS-FY2019Q1-Total Numerator-N`, 
         -`TX_PVLS-FY2019Q1-Age/Sex/Indication/HIVStatus-D`, -`TX_PVLS-FY2019Q1-Age/Sex/Indication/HIVStatus-N`) %>% 
  group_by(PSNUuid,PSNU,FacilityUID,Facility) %>% 
  mutate(VL_Coverage_FY2019Q3= round(sum(`TX_PVLS-FY2019Q3-Total Denominator-D`,na.rm = T)*100/
                                        sum(`TX_CURR-FY2019Q1-Total Numerator-N`,na.rm = T),2),
         VL_Suppression_FY2019Q3 = round(sum(`TX_PVLS-FY2019Q3-Total Numerator-N`,na.rm = T)*100/
                                            sum(`TX_PVLS-FY2019Q3-Total Denominator-D`,na.rm=T),2), 
         VL_Coverage_FY2019Q3=replace(VL_Coverage_FY2019Q3, `TX_PVLS-FY2019Q3-Total Denominator-D`==0|
                                         is.na(`TX_PVLS-FY2019Q3-Total Denominator-D`), NA), 
         VL_Suppression_FY2019Q3=replace(VL_Suppression_FY2019Q3, `TX_PVLS-FY2019Q3-Total Numerator-N`==0|
                                         is.na(`TX_PVLS-FY2019Q3-Total Numerator-N`), NA),
         quadrant = case_when(
           VL_Coverage_FY2019Q3 >= 80 & VL_Suppression_FY2019Q3 >=90 ~ 1,
           VL_Coverage_FY2019Q3 >= 80 & VL_Suppression_FY2019Q3 < 90 ~ 3,
           VL_Coverage_FY2019Q3 < 80 & VL_Suppression_FY2019Q3 >=90 ~ 2,
           VL_Coverage_FY2019Q3 < 80 & VL_Suppression_FY2019Q3 <90 ~ 4,
           TRUE ~ NA_real_)) %>% 
  filter_at(vars(`TX_CURR-FY2019Q1-Age/Sex/HIVStatus-N`:quadrant), any_vars(!is.na(.)))

# remove NaN from the data
# site_patient_vol_sup_cov[is.na(site_patient_vol_sup_cov)] <- NA
# site_patient_vol_sup_cov[,7:46] <- sapply(site_patient_vol_sup_cov[,7:44],as.numeric)


write.xlsx(site_patient_vol_sup_cov, 'site_vl_raw_09_30.xlsx')


site.lab <- site_im %>% filter(indicator == 'LAB_PTCQI')

