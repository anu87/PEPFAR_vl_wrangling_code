library(readr)
library(tidyverse)


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# ============= Required indicators for each file type ~~~~~================

indi_siteIM <- c('EMR_SITE', 'EMR_SITE_MIL','LAB_PTCQI','LAB_PTCQI_MIL','PMTCT_ART','TX_CURR','TX_NEW','TX_PVLS','TX_RET')

indi_ouIM <- c('EMR_SITE', 'EMR_SITE_MIL', 'LAB_PTCQI', 'LAB_PTCQI_HIV_Viral_Load_CQI', 'LAB_PTCQI_HIV_Viral_Load_POCT_CQI',
               'LAB_PTCQI_HIV_Viral_Load_POCT_PT', 'LAB_PTCQI_HIV_Viral_Load_PT', 'LAB_PTCQI_LAB', 'LAB_PTCQI_MIL', 
               'PMTCT_ART', 'TX_CURR', 'TX_NEW', 'TX_PVLS', 'TX_RET')

indi_Nat_Sub <- c('PLHIV', 'TX_CURR_NAT', 'TX_CURR_SUBNAT', "VL_SUPPRESSION_NAT", "VL_SUPPRESSION_SUBNAT","PMTCT_ARV_NAT", 
                  "PMTCT_ARV_SUBNAT", "PMTCT_STAT_NAT", "PMTCT_STAT_SUBNAT")



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# ============= Sub-Regions for OU_IM data  ~~~~~================

asia <- c('Asia Regional Program','Burma', 'Cambodia','Central Asia Region', 'India','Indonesia','Ukraine','Vietnam')

southern_africa <- c('Angola','Botswana', 'Lesotho', 'Malawi','Namibia','South Africa', 'Swaziland','Zambia', 'Zimbabwe')

east_africa <- c('Burundi',"Democratic Republic of the Congo",'Ethiopia', 
                      'Kenya',  'Rwanda', 'South Sudan', 'Tanzania', 'Uganda')

west_africa <- c('Cameroon',"Cote d'Ivoire",'Nigeria','Ghana')

lac <- c("Caribbean Region","Central America Region","Dominican Republic", "Haiti") 


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# ============= list of countries  ~~~~~================

countries <- c('Asia Regional Program','Burma', 'Cambodia','Central Asia Region', 'India','Indonesia','Ukraine','Vietnam',
               'Angola','Botswana', 'Lesotho', 'Malawi','Namibia','South Africa', 'Swaziland','Zambia', 'Zimbabwe',
               'Burundi',"Democratic Republic of the Congo",'Ethiopia', 'Kenya',  'Rwanda', 'South Sudan', 'Tanzania', 
               'Uganda', 'Cameroon',"Cote d'Ivoire",'Nigeria','Ghana', "Caribbean Region","Central America Region","Dominican Republic", "Haiti")


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# ============= # loop for site_im datasets  ~~~~~================

# download all site_im files and save them in site_im_files folder inside the working directory

ou_sites <- list.files(path='SiteLevel/')
files <- array(ou_sites)
path <- paste0('SiteLevel', '/',files)

for(i in 1:length(ou_sites)){
  df <- read_delim(file = path[i],"\t", escape_double = FALSE, trim_ws = TRUE)
  
  df1 <- df %>% filter(indicator %in% indi_siteIM) %>%
    rename(FY17SNUPrioritization=CurrentSNUPrioritization, FY17CommunityPrioritization=CurrentCommunityPrioritization,
           FY17FacilityPrioritization=CurrentFacilityPrioritization) %>%
    select(-c(FY2015Q2,FY2015Q3,FY2015Q4))
  
  name1 <- as.character(files[i])
  name1 <- gsub('.txt','',name1)
  name2 <- paste0(substr(name1, start = 15,stop=199)) # extract Ou and site_im part from file name
  write_tsv(df1,paste0(name2,'.txt'),na='')
}


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# ============= # loop for creating OU_IM files  ~~~~~================

fy17_ouIM <- read_delim("ICPI_FactView_OU_IM_20171222_v2_1.txt", 
                        "\t", escape_double = FALSE, trim_ws = TRUE)
for(i in countries){
 ifelse(i %in% asia,
   df <- fy17_ouIM %>% filter(OperatingUnit %in% asia & indicator %in% indi_ouIM),
   ifelse(i %in% southern_africa, df <- fy17_ouIM %>% filter(OperatingUnit %in% southern_africa & indicator %in% indi_ouIM), 
          ifelse(i %in% east_africa, df <- fy17_ouIM %>% filter(OperatingUnit %in% east_africa & indicator %in% indi_ouIM),
                 ifelse(i %in% west_africa, df <- fy17_ouIM %>% filter(OperatingUnit %in% west_africa & indicator %in% indi_ouIM),
                        ifelse(i %in% lac, df <- fy17_ouIM %>% filter(OperatingUnit %in% lac & indicator %in% indi_ouIM),df)))))
  
   write_tsv(df,paste0(i,'_ouIM','.txt'), na='')
}


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# =============# filter Nat_SubNat data   ~~~~~================

fy17_NatSubNat <- read_delim("ICPI_FactView_NAT_SUBNAT_20171222_v2_1.txt", 
                             "\t", escape_double = FALSE, trim_ws = TRUE)

for(i in countries){
  df <- fy17_NatSubNat %>% filter(OperatingUnit==i & indicator %in% indi_Nat_Sub)
  write_tsv(df, paste0(i, '_NatSubnat','.txt'), na='')
}


