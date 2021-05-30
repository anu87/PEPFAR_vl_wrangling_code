library(tidyverse)
library(data.table)
library(readr)
library(openxlsx)

# create column vector for psnu & site
site <- fread('MER_Structured_Datasets_SITE_IM_FY17-19_20190322_v2_1_Angola.txt', '\t')
cols <- names(site)
colsite <- as.vector(ifelse(grepl("FY20", cols),"numeric","character"))

# read PSNU_IM MSD file
psnu_im_all <- fread('PSNU_IM/MER_Structured_Dataset_PSNU_IM_FY17-19_20190215_v1_1.txt')

zip.files <- list.files(path = 'Site_IM/', pattern = '*.zip')

zip.temp <- lapply(paste0('Site_IM/', zip.files), unzip)

#replace with .txt files - 4 zip files are corrupt

zip.temp <- list.files( pattern = '*.txt', full.names = T)

txt.temp <- lapply(zip.temp, fread, sep='\t')

new.site <- rbindlist(txt.temp)

df.site <- rbindlist(txt.temp)
  #check structure 
str(df.site)

df.site <- df.site %>% select(-ApprovalLevel,-ApprovalLevelDescription)
# df.site <- rbind(site.im.all, png_site)

site_im_all <- rbind(site_im, new.site)

unique(site_im_all$OperatingUnit)

saveRDS(site_im_all, 'Site_IM_MSD_FY19Q1_Clean_03_30.rds')


site.lab <- df.site %>% filter(indicator %in% c('LAB_PTCQI', 'LAB_PTCQI_MIL')) %>% filter(SiteType=='Facility') 

site.lab <- site.im.all %>% filter(indicator %in% c('LAB_PTCQI', 'LAB_PTCQI_MIL'))

tab2 <- df.site %>% 
  group_by(OperatingUnit, indicator) %>% 
  summarise(fy18apr=sum(FY2018APR, na.rm = T)) %>% 
  spread(indicator, fy18apr)


# PSNU IM files

zip.files <- list.files(path = 'PSNU_IM/', pattern = '*.zip')

zip.temp <- lapply(paste0('PSNU_IM/', zip.files), unzip)

psnu2 <- read_delim("DHIS2_7e6cc17584904fb8b84f500a48020990.txt", 
                    +     "\t", escape_double = FALSE, trim_ws = TRUE)
cols2 <- names(psnu2)
colpsnu <- as.vector(ifelse(grepl("FY20", cols2),'numeric','character'))

#replace with .txt files - 4 zip files are corrupt

zip.temp <- list.files(pattern = '*.txt')

txt.temp <- lapply(zip.temp, fread, '\t', colClasses=colpsnu)
psnu.im <- rbindlist(txt.temp)
str(psnu.im)


# read Kenya separately
ken.psnu <- fread('ken-psnu.txt', '\t', colClasses = colpsnu)

# remove & replace Kenya data
psnu.im <- psnu.im %>% filter(!OperatingUnit=='Kenya')

psnu.im <- bind_rows(psnu.im, ken.psnu)

psnu.tab <- psnu.im %>% 
  group_by(OperatingUnit, indicator) %>% 
  summarise(fy18apr=sum(FY2018APR, na.rm = T)) %>% 
  spread(indicator, fy18apr)

write.xlsx(psnu.tab, 'psnu.tab.xlsx')
write.xlsx(tab2, 'site.tab.xlsx')

psnu.im <- psnu.im %>% select(-ApprovalLevel,-ApprovalLevelDescription)

saveRDS(psnu.im, 'PSNU_ALL_OU_FY18Q4_clean_12_27.rds')

unique(psnu.im$OperatingUnit)

eid.pos <- df.site %>% filter(OperatingUnit %in% c('Tanzania', 'India'), indicator %in% c('PMTCT_EID', 'PMTCT_EID_POS')) 

eid.pos2 <- df.site %>% filter(indicator %in% c('PMTCT_EID_POS'))

site2 <- site_im %>% filter(indicator == 'LAB_PTCQI' | 
                              (indicator == 'TX_CURR' & standardizedDisaggregate == 'Total Numerator') |
                              (indicator == 'TX_PVLS' & standardizedDisaggregate %in% c('Total Numerator', 'Total Denominator')))
tab1 <- site2 %>% group_by(indicator, standardizedDisaggregate) %>% summarise()
saveRDS(site2, 'site_im_labs_Fy19Q1.rds')                    
