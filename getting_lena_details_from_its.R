# some  code for reading in LENA variables
# this script goes through each its library and creates the file-level
#summaries for our speaker vars
# 

#note: this can take a LONG TIME to run over lots of files


# read in its folders -----------------------------------------------------
source("functions.R")

# corpusname ---------------------------------------------------------------------
VIHI_participant_IDs <- read_csv("/Volumes/pn-opus/VIHI/SubjectInformation/General/VIHI_participant_IDs.csv") %>%
  rename(sub_id=new_name) %>%
  select(sub_id, match, match_type) 

folder <- "/Volumes/pn-opus/VIHI/SubjectFiles/LENA/"
da <- read.csv("data/VIHITD_its_files.csv")
da <- da %>%
  mutate(its_path = paste(folder, as.character(sigroup), "/", as.character(sub), "/",
                          as.character(id), "/", as.character(id), ".its", sep=''))


VIHI_allspkr <- da %>% 
  group_by(its_path) %>% 
  do(get_its_tier_times(.$its_path)) %>% 
  mutate(corpus = "VIHI") 

VIHI_chndetails <- da %>% 
  group_by(its_path) %>% 
  do(get_its_tier_times_CHN_details(.$its_path)) %>% 
  mutate(corpus = "VIHI")

VIHI_its_details <- left_join(VIHI_allspkr, VIHI_chndetails) %>% 
  mutate(TV_dur = (TVN_dur + TVF_dur), 
         adult_dur = (FAF_dur + FAN_dur + MAF_dur + MAN_dur), 
         sub_id = str_sub(its_path, (nchar(its_path) - 13), (nchar(its_path)-4)),
         group = str_sub(sub_id, 1, 2)) %>% 
  left_join(VIHI_participant_IDs)

write_csv(VIHI_its_details, "data/VIHI_its_details.csv")
