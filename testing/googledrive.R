library(googledrive)

drive_find(n_max = 50)

drive_find(pattern = "chicken")

drive_find(pattern = "Syria Group", type = "folder")


syria_group_data <- drive_get("~/Fall2017/Development_Economics/Syria Group/data")

drive_reveal(what = syria_group_data)

drive_download(file = "2016_Data_Extract_From_World_Development_Indicators",
               path = "~/Development-Economics/data/2016_WBD",
               type = "xlsx")

drive_download(file = "2015_Data_Extract_From_World_Development_Indicators",
               path = "~/Development-Economics/data/2015_WBD",
               type = "xlsx")

drive_download(file = "WBD_WDI_Main.xlsx",
               path = "~/Development-Economics/data/WBD_WDI_Main",
               type = "xlsx")

# library(readxl)

library(lubridate)

WBD_2016 <- read_excel("data/2016_WBD.xlsx", sheet = "Data")# %>%
  # select(-`Time Code`) %>%
  # select(-`Series Code`) %>%
  # mutate(Time = year(as.Date(Time, format = "%Y"))) %>%
  # drop_na()
  
WBD_2015 <- read_excel("data/2015_WBD.xlsx", sheet = "Data") %>%
  select(-`Time Code`) %>%
  select(-`Series Code`) %>%
  mutate(Time = year(as.Date(Time, format = "%Y"))) %>%
  drop_na()

WBD_2014 <- read_excel("data/2014_WBD.xlsx", sheet = "Data") %>%
  select(-`Time Code`) %>%
  select(-`Series Code`) %>%
  mutate(Time = year(as.Date(Time, format = "%Y"))) %>%
  drop_na()

WBD_2016 %>%
  rename(Cuba = `Cuba [CUB]`) %>%
  rename(Syria = `Syrian Arab Republic [SYR]`) %>%
  rename(Russia = `Russian Federation [RUS]`) %>%
  rename(Thailand = `Thailand [THA]`) %>%
  rename(Ghana = `Ghana [GHA]`) %>%
  rename(Mexico = `Mexico [MEX]`) %>%
  rename(India = `India [IND]`) %>%
  rename(`South Africa` = `South Africa [ZAF]`)

WBD_cleaner <- function(x) {
  new_df <- x %>%
    rename(Indicator = `Series Name`) %>%
    rename(Cuba = `Cuba [CUB]`) %>%
    rename(Syria = `Syrian Arab Republic [SYR]`) %>%
    rename(Russia = `Russian Federation [RUS]`) %>%
    rename(Thailand = `Thailand [THA]`) %>%
    rename(Ghana = `Ghana [GHA]`) %>%
    rename(Mexico = `Mexico [MEX]`) %>%
    rename(India = `India [IND]`) %>%
    rename(`South Africa` = `South Africa [ZAF]`)%>%
    select(-`Time Code`) %>%
    select(-`Series Code`) %>%
    mutate(Time = year(as.Date(Time, format = "%Y"))) %>%
    drop_na()
  
  return(new_df)
}
  
WBD_2016_clean <- WBD_2016 %>%
  WBD_cleaner()

# 
# world_bank_data <- read_excel("data/World_Bank_Data.xlsx", sheet = "Data", ) %>%
#   select(-`Poverty headcount ratio at national poverty lines (% of population) [SI.POV.NAHC]`) %>%
#   select(-`Poverty headcount ratio at $1.90 a day (2011 PPP) (% of population) [SI.POV.DDAY]`) %>%
#   select(-`Net migration [SM.POP.NETM]`) %>%
#   select(-`Terrestrial and marine protected areas (% of total territorial area) [ER.PTD.TOTL.ZS]`)
#   # filter(Time > 2014)
# 
# syria_data <- world_bank_data %>%
#   filter(`Country Name` %in% "Syrian Arab Republic") %>%
#   select(-`GNI, Atlas method (current US$) [NY.GNP.ATLS.CD]`) %>%
#   select(-`GNI per capita, Atlas method (current US$) [NY.GNP.PCAP.CD]`) %>%
#   select(-`GNI, PPP (current international $) [NY.GNP.MKTP.PP.CD]`) %>%
#   select(-`GNI per capita, PPP (current international $) [NY.GNP.PCAP.PP.CD]`) %>%
#   select(-`Income share held by lowest 20% [SI.DST.FRST.20]`) %>%
#   select(-`Contraceptive prevalence, any methods (% of women ages 15-49) [SP.DYN.CONU.ZS]`) %>%
#   select(-`Births attended by skilled health staff (% of total) [SH.STA.BRTC.ZS]`) %>%
#   select(-`Prevalence of underweight, weight for age (% of children under 5) [SH.STA.MALN.ZS]`) %>%
#   select(-`Prevalence of HIV, total (% of population ages 15-49) [SH.DYN.AIDS.ZS]`)
# 
# 
# 
# 
# GDP growth (annual %) [NY.GDP.MKTP.KD.ZG]

normalize <- function(x){
  return((x- mean(x))/(max(x)-min(x)))
}

# WBD_2016_clean %>%
  # mutate(Cuba = normalize(Cuba))
  # group_by(Indicator) %>%
  # rowwise() %>%
  # mutate_each(funs = normalize_1_to_0)
# mutate_if(is.numeric, funs(normalize_1_to_0))
  # ungroup()


WBD_2016_clean %>% 
  t() %>% 
  # data.frame() %>% 
  as.tibble()
  mutate_if(is.numeric, funs(normalize_1_to_0)) %>%
  # unique() %>% 
  t()



