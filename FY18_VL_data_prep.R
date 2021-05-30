library(readr)
library(devtools)
library(dplyr)
library(readr)
library(tidyr)
library(openxlsx)
library(data.table)
library(scales)
library(ggplot2)

site_im_all <- readRDS("Site_ALL_OU_FY18Q4_clean_12_27.rds")
psnu_im_all <- readRDS('PSNU_ALL_OU_FY18Q4_clean_12_27.rds')

psnu_im_all <- fread('MER_Structured_Dataset_PSNU_IM_FY17-18_20181115_v1_2.txt')
saveRDS(psnu_im_all,'psnu_IM_MSD.rds')

site_im <- readRDS('site_im.rds')
psnu_im <- readRDS('psnu_im.rds')


nat_sub <- read_delim("MER_Structured_Dataset_NAT_SUBNAT_FY15-18_20180921_v2_3.txt", "\t", escape_double = FALSE, trim_ws = TRUE)

## --- Std & Other disaggs to keep -----   
std_disagg_keep <- c('Lab/CQI',
                     'Lab/PT',
                     "Lab/TestVolume",
                     "POCT/CQI", 
                     "POCT/PT",
                     "POCT/TestVolume",
                     'LabCount',
                     'LabCountDoDOnly',
                     "NewExistingArt/Sex/HIVStatus",
                     "Total Numerator",
                     "ObservedCommodity",
                     "Age/Sex/HIVStatus",
                     "PregnantOrBreastfeeding/HIVStatus",
                     "Age/Sex/Indication/HIVStatus", 
                     "PregnantOrBreastfeeding/Indication/HIVStatus",
                     "RoutineTargeted/HIVStatus", 
                     "Total Denominator")

other_disagg_keep <- c("Lab Testing - HIV Viral Load, LAB_CQI - Audited and fully accredited",
                       "Lab Testing - HIV Viral Load, LAB_CQI - Testing with no participation",
                       "Lab Testing - HIV Viral Load, PT - Participate and passed last round",
                       "Lab Testing - HIV Viral Load, PT - Testing with no participation",
                       "Lab Testing - HIV Viral Load, POCT_CQI - Audited score 4-5",
                       'Lab Testing - HIV Viral Load',
                       "Life-long ART, Already",
                       "Life-long ART, New",
                       "First Line ARV",
                       "Second Line ARV",
                       "Breastfeeding",
                       "Pregnant",
                       "Routine",
                       "Targeted",
                       "Undocumented Test Indication",
                       "Breastfeeding, Routine",
                       "Breastfeeding, Targeted",
                       "Breastfeeding, Undocumented Test Indication",
                       "Pregnant, Routine",
                       "Pregnant, Targeted",
                       "Pregnant, Undocumented Test Indication",
                       "")

# --- Disaggs for each indicator ----

lab_std_disagg <- c('Lab/CQI','Lab/PT',"Lab/TestVolume","POCT/CQI", "POCT/PT","POCT/TestVolume",'LabCount','LabCountDoDOnly')

lab_oth_disagg <- c("Lab Testing - HIV Viral Load, LAB_CQI - Audited and fully accredited",
                    "Lab Testing - HIV Viral Load, LAB_CQI - Testing with no participation",
                    "Lab Testing - HIV Viral Load, PT - Participate and passed last round",
                    "Lab Testing - HIV Viral Load, PT - Testing with no participation",
                    "Lab Testing - HIV Viral Load, POCT_CQI - Audited score 4-5",
                    'Lab Testing - HIV Viral Load')

pmtct_std_disagg <- c("NewExistingArt/Sex/HIVStatus","Total Numerator")
pmtct_oth_disagg <- c("Life-long ART, Already","Life-long ART, New", "")

stock_std_disagg <- c('ObservedCommodity')
stock_oth_disagg <- c("First Line ARV","Second Line ARV")

curr_std_disagg <- c('Age/Sex/HIVStatus', 'Total Numerator')
new_std_disagg <- c('Age/Sex/HIVStatus', 'Total Numerator', "PregnantOrBreastfeeding/HIVStatus")
new_oth_disagg <- c("Breastfeeding","Pregnant", "")

pvls_std_disagg <- c( "Age/Sex/Indication/HIVStatus", 
                      "PregnantOrBreastfeeding/Indication/HIVStatus",
                      "RoutineTargeted/HIVStatus", 
                      "Total Denominator", "Total Numerator")
pvls_oth_disagg <- c("Routine","Targeted","Undocumented Test Indication","Breastfeeding, Routine",
                     "Breastfeeding, Targeted","Breastfeeding, Undocumented Test Indication","Pregnant, Routine",
                     "Pregnant, Targeted","Pregnant, Undocumented Test Indication", "")

ret_std_disagg <- c("PregnantOrBreastfeeding/HIVStatus", 'Age/Sex/HIVStatus')
ret_oth_disagg <- c("Breastfeeding","Pregnant", "")

# Check retention data ---- for one-time use - Ignore -----
ret2 <- psnu_im_all %>% filter(indicator=='TX_RET', standardizedDisaggregate=='12mo/HIVStatus') %>% 
  select(-Region,-RegionUID,-dataElementUID, -CountryName, -indicatorType, -coarseDisaggregate:-isMCAD, -typeMilitary) %>% 
  group_by_at(vars(OperatingUnit:otherDisaggregate)) %>% 
  summarise_all(sum, na.rm=T)

write.xlsx(ret2, 'ret_age_sex.xlsx')


# Filter for each indicator and rbind ---- 

#TX_RET- create Tot Num /Tot Denominator data 
# filter 12 mo/HIV Status and replace with Tot Num/Tot Denom
# ret <- psnu_im_all %>% filter(indicator=='TX_RET', standardizedDisaggregate %in% ret_std_disagg, 
#                               otherDisaggregate %in% ret_oth_disagg) %>% 
#   mutate(standardizedDisaggregate=case_when(
#     standardizedDisaggregate== '12mo/HIVStatus' & numeratorDenom=='N' ~ 'Total Numerator',
#     standardizedDisaggregate== '12mo/HIVStatus' & numeratorDenom=='D' ~ 'Total Denominator',
#     TRUE~ as.character(standardizedDisaggregate)
#   )) %>% select(-coarseDisaggregate:-isMCAD, -indicatorType, -AgeFine,-AgeCoarse) %>% 
#   group_by_at(vars(Region:otherDisaggregate)) %>% 
#   summarise_at(vars(FY2017Q1:FY2019_TARGETS), sum, na.rm=T) %>% 
#   ungroup()


ret <- psnu_im_all %>% filter(indicator=='TX_RET', standardizedDisaggregate %in% ret_std_disagg, 
                              otherDisaggregate %in% ret_oth_disagg) %>% 
  select(-coarseDisaggregate:-isMCAD, -indicatorType, -AgeFine,-AgeCoarse, -dataElementUID,
         -MechanismID, -mechanismUID) %>% 
  group_by_at(vars(Region:otherDisaggregate)) %>% 
  summarise_at(vars(FY2017Q1:FY2019_TARGETS), sum, na.rm=T) %>% 
  ungroup()

pvls <- psnu_im_all %>% filter(indicator=='TX_PVLS', standardizedDisaggregate %in% pvls_std_disagg,
                               otherDisaggregate %in% pvls_oth_disagg) %>% 
  select(-coarseDisaggregate:-isMCAD, -indicatorType, -AgeFine,-AgeCoarse, -dataElementUID,
         -MechanismID, -mechanismUID) %>% 
  group_by_at(vars(Region:otherDisaggregate)) %>% 
  summarise_at(vars(FY2017Q1:FY2019_TARGETS), sum, na.rm=T) %>% 
  ungroup()

new <- psnu_im_all %>% filter(indicator == 'TX_NEW', standardizedDisaggregate %in% new_std_disagg,
                              otherDisaggregate %in% new_oth_disagg) %>%
  select(-coarseDisaggregate:-isMCAD, -indicatorType, -AgeFine,-AgeCoarse,-dataElementUID,
         -MechanismID, -mechanismUID) %>% 
  group_by_at(vars(Region:otherDisaggregate)) %>% 
  summarise_at(vars(FY2017Q1:FY2019_TARGETS), sum, na.rm=T) %>% 
  ungroup()

curr <- psnu_im_all %>% filter(indicator == 'TX_CURR', standardizedDisaggregate %in% curr_std_disagg) %>%
  select(-coarseDisaggregate:-isMCAD, -indicatorType, -AgeFine,-AgeCoarse,-otherDisaggregate,-dataElementUID,
         -MechanismID, -mechanismUID) %>% 
  group_by_at(vars(Region:Sex)) %>% 
  summarise_at(vars(FY2017_TARGETS:FY2019_TARGETS), sum, na.rm=T) %>% 
  ungroup()

stock <- psnu_im_all %>% filter(indicator == 'SC_STOCK', standardizedDisaggregate %in% stock_std_disagg, 
                                otherDisaggregate %in% stock_oth_disagg) %>%
  select(-coarseDisaggregate:-isMCAD, -indicatorType, -AgeFine,-AgeCoarse,-dataElementUID,
         -MechanismID, -mechanismUID) %>% 
  group_by_at(vars(Region:otherDisaggregate)) %>% 
  summarise_at(vars(FY2017_TARGETS:FY2019_TARGETS), sum, na.rm=T) %>% 
  ungroup()

pmtct <- psnu_im_all %>% filter(indicator == 'PMTCT_ART', standardizedDisaggregate %in% pmtct_std_disagg, 
                                otherDisaggregate %in% pmtct_oth_disagg) %>%
  select(-coarseDisaggregate:-isMCAD, -indicatorType, -AgeFine,-AgeCoarse,-dataElementUID,
         -MechanismID, -mechanismUID) %>% 
  group_by_at(vars(Region:otherDisaggregate)) %>% 
  summarise_at(vars(FY2017_TARGETS:FY2019_TARGETS), sum, na.rm=T) %>% 
  ungroup()

lab <- psnu_im_all %>% filter(indicator %in% c('LAB_PTCQI', 'LAB_PTCQI_MIL'), standardizedDisaggregate %in% lab_std_disagg, 
                              otherDisaggregate %in% lab_oth_disagg) %>%
  select(-coarseDisaggregate:-isMCAD, -indicatorType, -AgeFine,-AgeCoarse,-dataElementUID,
         -MechanismID, -mechanismUID) %>% 
  group_by_at(vars(Region:otherDisaggregate)) %>% 
  summarise_at(vars(FY2017_TARGETS:FY2019_TARGETS), sum, na.rm=T) %>% 
  ungroup()

psnu_im.vl <- bind_rows(lab,pmtct,stock,curr,new,pvls, ret)

saveRDS(psnu_im.vl, 'psnu_im_VL.rds')

#site-level filter ----
site_im <- df.site %>% filter(indicator %in% c('TX_CURR', 'TX_NEW', 'TX_PVLS', 'LAB_PTCQI', 
                                                   'LAB_PTCQI_MIL', 'SC_STOCK')) %>%
  select(-otherDisaggregate:-FY2018_TARGETS, -FY2019_TARGETS) 

saveRDS(site_im, 'site_im.rds')

# Filter indicators & disaggs---- older code - ignore -----
psnu_im <- psnu_im_all %>% 
  filter(indicator %in% c('LAB_PTCQI', 'LAB_PTCQI_MIL', 'PMTCT_ART', 'TX_CURR',
                          'TX_NEW', 'TX_PVLS', 'TX_RET', 'SC_STOCK'), 
         standardizedDisaggregate %in% std_disagg_keep,
         otherDisaggregate %in% other_disagg_keep)
  
saveRDS(psnu_im, 'psnu_im.rds') 

# psnu_im2 <- psnu_im %>% filter_at(.vars = vars(FY2017Q1:FY2017APR, FY2018Q1:FY2018APR), .vars_predicate = any_vars(!is.na(.)))


# Create VL_Coverage & VL_Suppression indicators ----
# psnu_im2 <- psnu_im.vl %>% 
#   gather(period, val, FY2017_TARGETS:FY2019_TARGETS) %>% 
#   mutate(Ind_pd_ND = paste(indicator,period,numeratorDenom, sep="-")) %>% 
#   group_by_at(vars( -val)) %>% 
#   summarise(val=sum(val,na.rm = T)) %>% 
#   spread(Ind_pd_ND, val) %>% 
#   mutate(VL_COVERAGE_FY2018APR = `TX_PVLS-FY2018APR-D`/`TX_CURR-FY2018Q2-N`,
#          VL_SUPP_FY2018APR= `TX_PVLS-FY2018APR-N`/`TX_PVLS-FY2018APR-D`)


 
# Code to find Centrally supported site----
# sites that report nothing on TX_CURR & TX_NEW between FY18Q1-FY18Q3, but report TX_CURR only for Q4----
cent_site <- site_im %>% 
  filter(indicator %in% c('TX_CURR', 'TX_NEW'), standardizedDisaggregate=='Total Numerator')  %>% 
  gather(period,val, FY2018Q1:FY2018APR) %>% 
  mutate(Ind_pd = paste(indicator,period, sep = "_")) %>% 
  group_by(FacilityUID,Facility, Ind_pd) %>%
  summarise(val=sum(val, na.rm=T)) %>% 
  spread(Ind_pd,val) %>% 
  mutate(Centrally_Supported_site= case_when(
    !is.na(TX_CURR_FY2018APR) & is.na(TX_CURR_FY2018Q1) & is.na(TX_CURR_FY2018Q2) & is.na(TX_CURR_FY2018Q3) & 
      !is.na(TX_CURR_FY2018Q4) & is.na(TX_NEW_FY2018APR) & is.na(TX_NEW_FY2018Q1) & is.na(TX_NEW_FY2018Q2) & 
      is.na(TX_NEW_FY2018Q3) & is.na(TX_NEW_FY2018Q4) ~ 1,
    TRUE~ 0))




# create bar graph for patient volume by psnu----

ggplot(patient_vol2 %>% filter(PSNU %in% c('Bamenda', 'Buea','Bonassama', 'Biyem Assi')), 
       aes(Facility,patient_vol_per),fill=Facility)+
  geom_bar(stat = "identity")+
  facet_wrap("PSNU")


# create VL_coverage & vl_supp at site level -> then calculate avg at PSNU level----
site_cal_indi <- site_im %>% 
  group_by(PSNU, PSNUuid) %>% 
  mutate(site_count=n_distinct(Facility)) %>% 
  ungroup() %>% 
  gather(period,val, FY2018Q2:FY2018APR) %>% 
  filter(standardizedDisaggregate %in% c('Total Denominator', 'Total Numerator'), 
         indicator %in% c('TX_CURR', 'TX_PVLS')) %>% 
  mutate(Ind_pd_nd = paste(indicator,period, standardizedDisaggregate, sep = "-")) %>% 
  group_by(OperatingUnit,OperatingUnitUID, SNU1, SNU1Uid, PSNU,PSNUuid, FacilityUID,Facility, 
           SNUPrioritization, ImplementingMechanismName, PrimePartner, FundingAgency, Ind_pd_nd, site_count) %>%
  summarise(val=sum(val, na.rm=T)) %>% 
  spread(Ind_pd_nd,val) %>% 
  ungroup()

#site level aggrgegates for PSNU_IM file -----
site_cal_indi2 <- site_cal_indi %>% 
  group_by(OperatingUnit,OperatingUnitUID, SNU1, SNU1Uid, PSNU,PSNUuid, FacilityUID,Facility, 
           SNUPrioritization, ImplementingMechanismName, PrimePartner, FundingAgency) %>% 
  mutate(VL_Coverage_FY2018APR= round(sum(`TX_PVLS-FY2018APR-Total Denominator`,na.rm = T)*100/sum(`TX_CURR-FY2018Q2-Total Numerator`,na.rm = T),2),
         VL_Suppression_FY2018APR = round(sum(`TX_PVLS-FY2018APR-Total Numerator`,na.rm = T)*100/sum(`TX_PVLS-FY2018APR-Total Denominator`,na.rm=T),2), 
         VL_Coverage_FY2018APR=replace(VL_Coverage_FY2018APR, `TX_PVLS-FY2018APR-Total Denominator`==0|is.na(`TX_PVLS-FY2018APR-Total Denominator`), NA))%>% 
  ungroup() 

# remove NaN from the data
site_cal_indi2[is.na(site_cal_indi2)] <- NA
site_cal_indi2[site_cal_indi2==0] <- NA

# indicators for site count - sites reporting on TX_CURR & TX_OVLS, N & D 
site_cal_indi2 <- site_cal_indi2 %>% 
  mutate(site_curr_reporting=case_when(
    !is.na(`TX_CURR-FY2018APR-Total Numerator`) ~ 1,
    TRUE ~ 0),
    site_vl_testing = case_when(
      !is.na(`TX_PVLS-FY2018APR-Total Denominator`) ~ 1,
      TRUE ~ 0),
    site_vl_supp_report = case_when(
      !is.na(`TX_PVLS-FY2018APR-Total Numerator`) ~ 1,
      TRUE ~ 0)) 

# site_cal_indi2[,7:44] <- sapply(site_cal_indi2[,7:44],as.numeric)

# aggregate file to PSNU level with mean & range

psnu_site <- site_cal_indi2 %>% 
  group_by(OperatingUnit,OperatingUnitUID, SNU1, SNU1Uid, PSNU,PSNUuid,
           SNUPrioritization, ImplementingMechanismName, PrimePartner, FundingAgency,site_count) %>% 
  summarise(VL_cov_mean = mean(VL_Coverage_FY2018APR,na.rm = T),
            Vl_cov_min = min(VL_Coverage_FY2018APR,na.rm = T),
            VL_cov_max = max(VL_Coverage_FY2018APR,na.rm = T),
            VL_Supp_mean = mean(VL_Suppression_FY2018APR,na.rm = T),
            VL_Supp_min = min(VL_Suppression_FY2018APR,na.rm = T),
            VL_Supp_max = max(VL_Suppression_FY2018APR, na.rm = T),
            site_curr_reporting = sum(site_curr_reporting, na.rm = T),
            site_vl_testing = sum(site_vl_testing, na.rm = T),
            site_vl_supp_report = sum(site_vl_supp_report,na.rm = T)) %>% 
  gather(indicator, FY2018APR, site_count:site_vl_supp_report) %>%  # 0 = no stockout; 1= stockout, blank = no data
  ungroup() %>% 
  mutate(FY2018APR= round(FY2018APR,2)) %>% 
  filter(indicator!='site_count') # remove site_count, needs to be calculated seperately

# creare Site_stockout variable

stock_site <-  site_im %>% 
  filter(indicator=='SC_STOCK', standardizedDisaggregate %in% stock_std_disagg, categoryOptionComboName %in% stock_oth_disagg) %>% 
  select(-FY2018Q1:-FY2018Q4) %>% 
  gather(period,val, FY2018APR) %>% 
  mutate(Ind_pd_nd = paste(indicator,period, numeratorDenom, sep = "_")) %>% 
  group_by(OperatingUnit,OperatingUnitUID, SNU1, SNU1Uid, PSNU,PSNUuid, 
           SNUPrioritization, ImplementingMechanismName, PrimePartner, FundingAgency, Ind_pd_nd) %>%
  summarise(val=sum(val, na.rm=T)) %>% 
  spread(Ind_pd_nd,val) %>% 
  ungroup() %>% 
  mutate(site_stockout=case_when(
    SC_STOCK_FY2018APR_N/SC_STOCK_FY2018APR_D < 1 ~ 1,
    SC_STOCK_FY2018APR_N/SC_STOCK_FY2018APR_D >= 1 ~ 0
  ))

psnu_stock_site <- stock_site %>% 
  group_by(OperatingUnit,OperatingUnitUID, SNU1, SNU1Uid, PSNU,PSNUuid, 
           SNUPrioritization, ImplementingMechanismName, PrimePartner, FundingAgency) %>% 
  summarise(site_stockout=sum(site_stockout, na.rm = T)) %>% 
  gather(indicator, FY2018APR, site_stockout) %>% 
  ungroup()


# is.na(psnu_site) <- sapply(psnu_site, is.infinite)

# remove all Inf & NaN from FY2018APR column
psnu_site$FY2018APR[!is.finite(psnu_site$FY2018APR)] <- NA

site.count <- site_im %>% group_by(OperatingUnit, SNU1, PSNU,PSNUuid,
                                   SNUPrioritization, ImplementingMechanismName, PrimePartner, FundingAgency) %>% 
  summarise(facility_count=n_distinct(Facility)) %>% 
  gather(indicator, FY2018APR, facility_count) %>% 
  filter(ImplementingMechanismName!='Dedup')

# rbind PSNU_IM & psnu_site data sets
psnuim_vl_coc_supp <- bind_rows(psnu_im.vl,psnu_site, psnu_stock_site, site.count)

# write final psnu-im raw data ----
write.xlsx(psnuim_vl_coc_supp,'psnuim_vl_cov_supp_stockout_Q4Clean_1_03.xlsx')



# calculate % of tot patient vol on each site by PSNU----
# Change FY2018Q3 to FY2018APR when all the data is out
# Use TX_CURR FY18APR for patientr volume calculations

site_patient_vol_sup_cov <- site_cal_indi %>% 
  group_by(PSNUuid,PSNU,FacilityUID,Facility) %>% 
  mutate(VL_Coverage_FY2018APR= round(sum(`TX_PVLS-FY2018APR-Total Denominator`,na.rm = T)*100/
                                        sum(`TX_CURR-FY2018Q2-Total Numerator`,na.rm = T),2),
         VL_Suppression_FY2018APR = round(sum(`TX_PVLS-FY2018APR-Total Numerator`,na.rm = T)*100/
                                            sum(`TX_PVLS-FY2018APR-Total Denominator`,na.rm=T),2), 
         VL_Coverage_FY2018APR=replace(VL_Coverage_FY2018APR, `TX_PVLS-FY2018APR-Total Denominator`==0|
                                         is.na(`TX_PVLS-FY2018APR-Total Denominator`), NA)) %>% 
  group_by(OperatingUnit,PSNUuid, PSNU) %>%
  filter(`TX_CURR-FY2018Q2-Total Numerator`>0) %>% 
  mutate(patient_vol_per=round(`TX_CURR-FY2018APR-Total Numerator`*100/sum(`TX_CURR-FY2018APR-Total Numerator`,na.rm = T),2), 
         patient_vol_per=case_when(
           patient_vol_per==0 ~ NA_real_,
            TRUE~ as.numeric(patient_vol_per)))

# remove NaN from the data
site_patient_vol_sup_cov[is.na(site_patient_vol_sup_cov)] <- NA

site_patient_vol_sup_cov2 <- site_patient_vol_sup_cov %>% 
  filter(!is.na(patient_vol_per)) %>% 
  arrange(desc(patient_vol_per), .by_group=T) %>% 
  mutate(cum_per_patient_vol=cumsum(coalesce(patient_vol_per,0)),
         pat_vol_rank = frank(cum_per_patient_vol))

site_pat_vol_na <- site_patient_vol_sup_cov %>% filter(is.na(patient_vol_per))

site_patient_vol_sup_cov3 <- bind_rows(site_patient_vol_sup_cov2,site_pat_vol_na)

site_patient_vol_sup_cov4 <- site_patient_vol_sup_cov3 %>% 
  group_by(OperatingUnit,SNU1,PSNU,PSNUuid,Facility,Facility) %>% 
  summarise_at(vars(`TX_CURR-FY2018APR-Total Numerator`:pat_vol_rank),sum,na.rm=T)
    
# percentile calculation
site_patient_vol_sup_cov <- site_patient_vol_sup_cov %>% 
  group_by(OperatingUnit,PSNUuid, PSNU) %>% 
  mutate(pat_vol_percentile= quantile(cum_per_patient_vol, probs = 0.9, na.rm = T))

mwi <- site_im %>% filter(OperatingUnit =='Malawi')

# site_patient_vol_sup_cov[,7:46] <- sapply(site_patient_vol_sup_cov[,7:44],as.numeric)

#write site level file
write.xlsx(site_patient_vol_sup_cov4, 'site_patient_vol_sup_cov_12_18.xlsx')

# code to check indicators & their disaggs -----
tab1 <- psnu.im.all %>% 
  filter(indicator=='TX_RET') %>% 
  group_by(indicator, standardizedDisaggregate,categoryOptionComboName) %>% 
  summarise()

tab2 <- psnuim_vl_coc_supp %>% 
  group_by(indicator,standardizedDisaggregate,otherDisaggregate,AgeAsEntered) %>% summarise()

tab3 <- psnu_im_all %>% filter(indicator %in% c('LAB_PTCQI', 'LAB_PTCQI_MIL', 'PMTCT_ART', 'TX_CURR',
                                                'TX_NEW', 'TX_PVLS', 'TX_RET', 'SC_STOCK')) %>% 
  group_by(indicator, standardizedDisaggregate,otherDisaggregate) %>% 
  summarise()


# -- Nat/SubNat data ------

indi_Nat_Sub <- c('PLHIV', 'TX_CURR_NAT', 'TX_CURR_SUBNAT', "VL_SUPPRESSION_NAT", "VL_SUPPRESSION_SUBNAT","PMTCT_ARV_NAT", 
                  "PMTCT_ARV_SUBNAT", "PMTCT_STAT_NAT", "PMTCT_STAT_SUBNAT")


nat_sub_fy18 <- nat_sub %>% filter(indicator %in% indi_Nat_Sub)

nat_sub_snu <- nat_sub_fy18 %>% group_by(OperatingUnit, SNU1,SNU1Uid,SNUprioritization, indicator, numeratorDenom,
                                         disaggregate,categoryOptionComboName, otherDisaggregate, age_as_entered, coarse_age, Sex) %>% 
  summarise_at(vars(FY2015:FY2019), sum, na.rm=T)

write.xlsx(nat_sub_fy18, "Nat_SubNat_Fy18_SNU_level_11_07.xlsx")

# replace incorrect site-count data with this
site.count <- site_im %>% group_by(OperatingUnit, SNU1, PSNU,PSNUuid,
                                   SNUPrioritization, ImplementingMechanismName, PrimePartner, FundingAgency) %>% 
  summarise(facility_count=n_distinct(Facility)) %>% 
  gather(indicator, FY2018APR, facility_count) %>% 
  filter(ImplementingMechanismName!='Dedup')

site.count <- bind_rows(psnuim_vl_coc_supp, site.count)
site.count <- site.count %>% filter(indicator=='facility_count') %>% 
  select(-MechanismID,-OperatingUnitUID,-SNU1Uid,-typeMilitary, -resultStatus, -Region,-RegionUID,-CountryName)

write.xlsx(site.count, 'facility_count.xlsx')

