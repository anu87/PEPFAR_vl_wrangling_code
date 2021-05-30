library(dplyr)
library(readr)
library(tidyr)
library(openxlsx)
library(data.table)

# load all files
memory.limit(size = 900000)

# create a list of names of all the site-im files (ideally keep all the txt files in the proj folder)
files <- list.files(path = '/Users/Bhuti/Documents/SiteLevel/fy17-lab/SiteLevel 20180215 v1_3')
temp <- lapply(files, read_delim, '\t') # read all the files into another list
df <- rbindlist(temp) # combine them to one dataframe
 


# Fiter desired indicators
df.lab <-  df %>% filter(indicator %in% c("TX_CURR", 'HTS_TST', 'HTS_TST_POS', 'PMTCT_STAT_POS', 'PMTCT_EID', 'PMTCT_EID_POS', 
                                                     'TX_PVLS', 'TX_TB', "LAB_PTCQI", 'LAB_PTCQI_MIL', 'PMTCT_EID_Two_Twelve_Months', 'PMTCT_EID_Less_Equal_Two_Months'))

# list of indicators for which we only want Total_Numerator under standardized_disagg
tot_num_indi <- c('HTS_TST', 'HTS_TST_POS', 'PMTCT_STAT_POS', 'PMTCT_EID', 'PMTCT_EID_POS','PMTCT_EID_Two_Twelve_Months', 'PMTCT_EID_Less_Equal_Two_Months')

# filter on standardized_disagg
df.lab <- df.lab %>% filter(!indicator %in% tot_num_indi | indicator %in% tot_num_indi & standardizedDisaggregate == 'Total Numerator')

# Split category option combo name to obtain test-type and status into separate columns
df.lab <- df.lab %>% separate(categoryOptionComboName, c('lab-testing', 'category', 'status'), sep='-', remove=F)
df.lab <- df.lab %>% separate(category, c('tests.type', 'CQI_PT_POCT'), sep=',', remove=T)

# Aggregate upto PSNU level
df.lab2 <- df.lab %>% group_by(OperatingUnit, PSNU, SNU1, Facility, indicator, PrimePartner, ImplementingMechanismName, numeratorDenom,standardizedDisaggregate, categoryOptionComboName, 
                                  Age, Sex, `lab-testing`, status, tests.type) %>% 
  summarise(Fy17Q4=sum(FY2017Q4, na.rm=T), FY17_Targets= sum(FY2017_TARGETS, na.rm = T), FY17Q1=sum(FY2017Q1, na.rm=T), FY17Q2=sum(FY2017Q2, na.rm=T), 
            FY17Q3=sum(FY2017Q3, na.rm=T), FY17APR = sum(FY2017APR, na.rm=T), FY16Q4=sum(FY2016Q4, na.rm=T), FY16APR=sum(FY2016APR, na.rm=T))

# Extract data for large countries to reduce no. of rows
kenya <- df.lab2 %>% filter(OperatingUnit=='Kenya')
ethiopia <- df.lab2 %>% filter(OperatingUnit=='Ethiopia')
nigeria <- df.lab2 %>% filter(OperatingUnit=='Nigeria')
sa <- df.lab2 %>% filter(OperatingUnit=='South Africa')
tnz <- df.lab2 %>% filter(OperatingUnit=='Tanzania')
uganda <- df.lab2 %>% filter(OperatingUnit=='Uganda')
zam <- df.lab2 %>% filter(OperatingUnit=='Zambia')
cam <- df.lab2 %>% filter(OperatingUnit=='Cameroon')
civ <- df.lab2 %>% filter(OperatingUnit=="Cote d'Ivoire")
zim <- df.lab2 %>% filter(OperatingUnit=='Zimbabwe')
drc <- df.lab2 %>% filter(OperatingUnit=='Democratic Republic of the Congo')

#remove the above countries from the originial data
df.lab.others <- df.lab2 %>% filter(!OperatingUnit %in% c('Kenya', 'Ethiopia', 'Nigeria', 'South Africa', 'Tanzania', 'Uganda',
                                                    'Zambia', 'Cameroon', "Cote d'Ivoire", "Zimbabwe", 'Democratic Republic of the Congo'))

write.xlsx(df.lab.others, 'Other-OUs.xlsx', asTable = F)

# write excel file for all the above countries
df.list <- list('Kenya'=kenya, 'Ethiopia'=ethiopia, 'Nigeria'=nigeria, 'South Africa'=sa, 'Tanzania'=tnz, 'Uganda'=uganda,
                'Zambia'=zam, 'Cameroon'=cam, "Cote d'Ivoire"=civ, "Zimbabwe"=zim, 'Democratic Republic of the Congo'=drc)

mapply(write.xlsx, df.list,file=paste0(names(df.list),'.xlsx'))

dfnetnew <- `FY18_FV_Site-IM_All_OU` %>% filter(indicator %in% c('TX_CURR','TX_NEW') & disaggregate=='Total Numerator')
