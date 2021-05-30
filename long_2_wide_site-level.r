
memory.limit(size = 90000)

#load required packages
library(tidyverse)
library(data.table)
library(reshape2)
library(readr)

#set working directory
setwd("C:/Users/xxz4/ICPI Factviews")

# read factview file 
dffv <- fread("file:///C:/Users/xxz4/ICPI Factviews/ICPI_MER_Structured_Dataset_SITE_IM_FY17-18_Mozambique_20180515_v1_1.txt", sep = 'auto', header = T, stringsAsFactors = F)

indi <- c('HTS_TST','HTS_TST_POS','HTS_TST_NEG','TX_NEW','TX_CURR',"PMTCT_STAT","PMTCT_ART","PMTCT_EID","PMTCT_EID_POS")

columns <- c("orgUnitUID", "Region", "RegionUID", "OperatingUnit", "OperatingUnitUID", "CountryName", "SNU1","SNU1Uid",
             "PSNU", "PSNUuid","CurrentSNUPrioritization","typeMilitary", "mechanismUID","PrimePartner","FundingAgency",
             "MechanismID","ImplementingMechanismName","CommunityUID", "Community",  "CurrentCommunityPrioritization",
             "typeCommunity", "FacilityUID","Facility",   "CurrentFacilityPrioritization", "typeFacility","dataElementUID",
             "numeratorDenom","indicatorType","disaggregate", "standardizedDisaggregate","categoryOptionComboUID",
             "categoryOptionComboName", "Age","Sex","resultStatus", "otherDisaggregate","coarseDisaggregate","modality",
             "tieredSiteCounts", "typeTieredSupport", "isMCAD", "period", "indicator")

#reshape periods to long data
dffv.long <- asia %>%
  gather(period, values, -1:-42) %>%
  filter(!is.na(values) & indicator %in% indi) %>%
  spread(indicator, values)

# dffv.long <- asia %>% 
#   gather(period, values, -1:-42) %>% 
#   filter(!is.na(values) & indicator %in% indi) %>% 
#   group_by_at(vars(one_of(columns))) %>% 
#   summarise(values=sum(values, na.rm=T)) %>% 
#   ungroup() %>% spread(indicator, values)


#pick one
write_csv(dffv.wide, 'mozambique_fy18q1.tab.csv')
write.table(dffv.wide, 'malawi.tab.txt', sep="\t")
