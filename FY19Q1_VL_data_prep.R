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

site_im_all <- readRDS("~/Documents/SiteLevel/site_im-18Q4/Site_IM_All_OUs_Genie_FY18Q4_11_16.rds")
psnu_im_all <- readRDS('PSNU_IM_All_OUs_18Q4_Genie_11_16.rds')
psnu_im_all <- fread('PSNU_IM/MER_Structured_Dataset_PSNU_IM_FY17-19_20190322_v2_1.txt')

site_im <- readRDS('site_im.rds')
psnu_im <- readRDS('psnu_im.rds')

nat_sub <- read_delim("MER_Structured_Dataset_NAT_SUBNAT_FY15-18_20180921_v2_3.txt", "\t", escape_double = FALSE, trim_ws = TRUE)

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

ret_std_disagg <- c("PregnantOrBreastfeeding/HIVStatus", 'Age/Sex/HIVStatus', 'Total Numerator', "Total Denominator")
ret_oth_disagg <- c("Breastfeeding","Pregnant", "")

# TX_RET- create Tot Num /Tot Denominator data
# filter 12 mo/HIV Status and replace with Tot Num/Tot Denom

ret <- psnu_im_all %>% filter(indicator=='TX_RET', standardizedDisaggregate %in% ret_std_disagg, 
                              otherDisaggregate %in% ret_oth_disagg) %>% 
  select(-coarseDisaggregate:-isMCAD, -indicatorType, -AgeCoarse, -Region, -RegionUID, -MechanismUID, -MechanismID, -CountryName, 
         -SNU1Uid, -categoryOptionComboName, -typeMilitary, -resultStatus, -OperatingUnitUID) %>% 
  group_by_at(vars(OperatingUnit:otherDisaggregate)) %>% 
  summarise_at(vars(FY2018Q4:FY2018APR), sum, na.rm=T) %>% 
  ungroup() %>% 
  filter_at(vars(FY2018Q4:FY2018APR), any_vars(. != 0))

pvls <- psnu_im_all %>% filter(indicator=='TX_PVLS', standardizedDisaggregate %in% pvls_std_disagg,
                               otherDisaggregate %in% pvls_oth_disagg) %>% 
  select(-coarseDisaggregate:-isMCAD, -indicatorType, -AgeCoarse, -Region, -RegionUID, -MechanismUID, -MechanismID, -CountryName, -SNU1Uid) %>% 
  group_by_at(vars(OperatingUnit:otherDisaggregate)) %>% 
  summarise_at(vars(FY2018APR:FY2019Q1), sum, na.rm=T) %>% 
  ungroup() %>% 
  filter_at(vars(FY2018APR:FY2019Q1), any_vars(.!=0))

new <- psnu_im_all %>% filter(indicator == 'TX_NEW', standardizedDisaggregate %in% new_std_disagg,
                              otherDisaggregate %in% new_oth_disagg) %>%
  select(-coarseDisaggregate:-isMCAD, -indicatorType, -AgeCoarse, -Region, -RegionUID, -MechanismUID, -MechanismID, -CountryName, -SNU1Uid) %>% 
  group_by_at(vars(OperatingUnit:otherDisaggregate)) %>% 
  summarise_at(vars(FY2017Q3:FY2019Q1), sum, na.rm=T) %>% 
  ungroup() %>% 
  filter_at(vars(FY2017Q3:FY2019Q1), any_vars(.!=0))

curr <- psnu_im_all %>% filter(indicator == 'TX_CURR', standardizedDisaggregate %in% curr_std_disagg) %>%
  select(-coarseDisaggregate:-isMCAD, -indicatorType, -AgeCoarse, -Region, -RegionUID, -MechanismUID, -MechanismID, -CountryName, -SNU1Uid) %>% 
  group_by_at(vars(OperatingUnit:otherDisaggregate)) %>% 
  summarise_at(vars(FY2018Q2:FY2019Q1), sum, na.rm=T) %>% 
  ungroup() %>% 
  filter_at(vars(FY2018Q2:FY2019Q1), any_vars(.!=0))

stock <- psnu_im_all %>% filter(indicator == 'SC_STOCK', standardizedDisaggregate %in% stock_std_disagg, 
                                otherDisaggregate %in% stock_oth_disagg) %>%
  select(-coarseDisaggregate:-isMCAD, -indicatorType, -AgeCoarse, -Region, -RegionUID, -MechanismUID, -MechanismID, -CountryName, -SNU1Uid) %>% 
  group_by_at(vars(OperatingUnit:otherDisaggregate)) %>% 
  summarise_at(vars(FY2018Q4:FY2018APR), sum, na.rm=T) %>% 
  ungroup() %>% 
  filter_at(vars(FY2018Q4:FY2018APR), any_vars(.!=0))

pmtct <- psnu_im_all %>% filter(indicator == 'PMTCT_ART', standardizedDisaggregate %in% pmtct_std_disagg, 
                                otherDisaggregate %in% pmtct_oth_disagg) %>%
  select(-coarseDisaggregate:-isMCAD, -indicatorType, -Region,-RegionUID, -OperatingUnitUID, -CountryName, -AgeCoarse,
         -SNU1Uid, -typeMilitary, -resultStatus, -FY2018_TARGETS, -categoryOptionComboName, -MechanismUID, -MechanismID) %>% 
  group_by_at(vars(OperatingUnit:otherDisaggregate)) %>% 
  summarise_at(vars(FY2017Q3:FY2019Q1), sum, na.rm=T) %>% 
  ungroup() %>% 
  filter_at(vars(FY2017Q3:FY2019Q1), any_vars(.!=0))


lab <- psnu_im_all %>% filter(indicator %in% c('LAB_PTCQI', 'LAB_PTCQI_MIL'), standardizedDisaggregate %in% lab_std_disagg, 
                              otherDisaggregate %in% lab_oth_disagg) %>%
  select(-coarseDisaggregate:-isMCAD, -indicatorType, -AgeCoarse, -Region, -RegionUID, -MechanismUID, -MechanismID, -CountryName, -SNU1Uid) %>% 
  group_by_at(vars(OperatingUnit:otherDisaggregate)) %>% 
  summarise_at(vars(FY2018Q4:FY2018APR), sum, na.rm=T) %>% 
  ungroup() %>% 
  filter_at(vars(FY2018Q4:FY2018APR), any_vars(.!=0))

psnu_im_vl <- bind_rows(pmtct,stock,curr,new,pvls, ret,lab)

saveRDS(psnu_im.vl, 'psnu_im_VL.rds')
write.xlsx(psnu_im.vl, 'psnu_im_vl.xlsx')

#site-level filter ----
site_im <- site_im_all %>% filter(indicator %in% c('TX_CURR', 'TX_NEW', 'TX_PVLS', 'LAB_PTCQI', 
                                                   'LAB_PTCQI_MIL', 'SC_STOCK')) 

saveRDS(site_im, 'site_im.rds')


 # create count of sites----

site_cal_indi <- site_im %>% 
   filter(standardizedDisaggregate %in% c('Total Denominator', 'Total Numerator'), indicator %in% c('TX_CURR', 'TX_PVLS')) 

# convert to wide
site_cal_indi2 <- site_cal_indi %>%
  gather(period,val, FY2019Q1) %>% 
  mutate(Ind_pd_nd = paste(indicator,period, standardizedDisaggregate, sep = "-")) %>% 
  group_by(OperatingUnit,OperatingUnitUID, SNU1, SNU1Uid, PSNU,PSNUuid, FacilityUID,Facility, Ind_pd_nd) %>%
  summarise(val=sum(val, na.rm=T)) %>% 
  mutate(val = replace(val, val==0, NA)) %>% 
  spread(Ind_pd_nd,val) %>% 
  ungroup()

# count sites that report on tx-curr etc
site_cal_indi3 <- site_cal_indi2 %>% 
  mutate(site_curr_reporting = case_when(
    !is.na(`TX_CURR-FY2019Q1-Total Numerator`) ~1,
    TRUE ~ 0), 
    site_vl_testing = case_when(
      !is.na(`TX_PVLS-FY2019Q1-Total Denominator`) ~ 1,
      TRUE ~ 0),
    site_vl_supp_reporting = case_when(
      !is.na(`TX_PVLS-FY2019Q1-Total Numerator`) ~ 1,
      TRUE ~ 0))

# convert to long
site_counts_lng <- site_cal_indi3 %>% 
  gather(indicator, FY2019Q1, site_curr_reporting:site_vl_supp_reporting) %>% 
  group_by_at(vars(OperatingUnit,OperatingUnitUID, SNU1, SNU1Uid, PSNU,PSNUuid, indicator)) %>% 
  summarise(FY2019Q1=sum(FY2019Q1, na.rm=T)) %>% 
  filter(FY2019Q1!= 0) %>% 
  ungroup()

#count total number of sites
tot.sites <- site_im_all %>% 
  group_by(OperatingUnit,OperatingUnitUID, SNU1, SNU1Uid, PSNU,PSNUuid) %>% 
  summarise(site_count = n_distinct(FacilityUID)) %>% 
  ungroup()

# conver to long
tot.sites_lng <- tot.sites %>% 
  gather(indicator, FY2019Q1, site_count)

# Calculate stockout sites

stock_site <- site_im %>% 
  filter(indicator == 'SC_STOCK', standardizedDisaggregate %in% stock_std_disagg, otherDisaggregate %in% stock_oth_disagg) %>% 
  gather(period, val, FY2018APR) %>% 
  mutate(Ind_pd_nd = paste(indicator, period, numeratorDenom, sep = '_')) %>% 
  group_by(OperatingUnit, orgUnitUID, SNU1, SNU1Uid, PSNU, PSNUuid, Facility, FacilityUID, Ind_pd_nd) %>% 
  summarise(val=sum(val, na.rm = T)) %>% 
  spread(Ind_pd_nd, val) %>% 
  ungroup() 

stock_site2 <- stock_site %>% 
  mutate(site_stockout = case_when(
    SC_STOCK_FY2018APR_N / SC_STOCK_FY2018APR_D < 1 ~ 1,
    SC_STOCK_FY2018APR_N / SC_STOCK_FY2018APR_D >= 1 ~0
  ))

psnu_stock_site <-  stock_site2 %>% 
  group_by(OperatingUnit, SNU1, SNU1Uid, PSNU, PSNUuid) %>% 
  summarise(site_stockout = sum(site_stockout, na.rm = T)) %>% 
  gather(indicator, FY2018APR, site_stockout) %>% 
  ungroup()

# rbind PSNU_IM & psnu_site data sets
psnuim_vl_cov_supp <- bind_rows(psnu_im_vl,site_counts_lng, tot.sites_lng, psnu_stock_site)
psnuim_vl_cov_supp <- psnuim_vl_cov_supp %>% 
  select(-typeMilitary, -resultStatus, -FY2018_TARGETS)

# write final psnu-im raw data ----
write.xlsx(psnuim_vl_cov_supp,'psnuim_vl_cov_supp_stockout_03_31.xlsx')


# adding Age/Sex data to site-level wide data ----

site_cal_indi_new <- site_im %>% 
  filter(standardizedDisaggregate %in% c('Total Denominator', 'Total Numerator', 'Age/Sex/Indication/HIVStatus', 'Age/Sex/HIVStatus'), 
         indicator %in% c('TX_CURR', 'TX_PVLS')) %>%
  filter_at(vars(FY2018Q4:FY2019Q1), any_vars(!is.na(.)))

site_cal_indi <- site_cal_indi_new %>%
  gather(period,val, FY2018Q3,FY2019Q1) %>% 
  mutate(Ind_pd_nd = paste(indicator,period, standardizedDisaggregate,numeratorDenom, sep = "-")) %>% 
  group_by(orgUnitUID, OperatingUnit,OperatingUnitUID, SNU1, PSNU,PSNUuid, FacilityUID,Facility, AgeAsEntered, AgeFine, Ind_pd_nd) %>%
  summarise(val=sum(val, na.rm=T)) %>% 
  filter(val >0) 

site_vl_wide <- site_cal_indi %>% 
  spread(Ind_pd_nd,val) %>% 
  ungroup()


site_patient_vol_sup_cov <- site_vl_wide %>% 
  group_by(PSNUuid,PSNU,FacilityUID,Facility) %>% 
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
  filter_at(vars(`TX_CURR-FY2018Q3-Age/Sex/HIVStatus-N`:quadrant), any_vars(!is.na(.)))

# remove NaN from the data
# site_patient_vol_sup_cov[is.na(site_patient_vol_sup_cov)] <- NA
# site_patient_vol_sup_cov[,7:46] <- sapply(site_patient_vol_sup_cov[,7:44],as.numeric)


write.xlsx(site_patient_vol_sup_cov, 'site_vl_age_fine.xlsx')


site.lab <- site_im %>% filter(indicator == 'LAB_PTCQI')

