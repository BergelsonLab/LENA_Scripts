# some  code for reading in LENA variables
# this script goes through each its library and creates the file-level
#summaries for our speaker vars

#note: this can take a LONG TIME to run over lots of files

# this script expects a folder of its files, and a functions script that's in this LENA_script repo
#  you  probably will need to change  path names and change corpusname to VIHI or whatever
# throughouth  and in the writeout step

# read in its folders -----------------------------------------------------
source("functions.R")

# corpusname ---------------------------------------------------------------------

folder <- "../corpusname/its/"
da <- as.data.frame(list.files(folder))
names(da) <- c("its_file")
da <- da %>%
  mutate(its_path = paste(folder, as.character(its_file), sep=''))

corpusname_allspkr <- da %>% 
  group_by(its_path) %>% 
  do(get_its_tier_times(.$its_path)) %>% 
  mutate(corpus = "corpusname")

corpusname_chndetails <- da %>% 
  group_by(its_path) %>% 
  do(get_its_tier_times_CHN_details(.$its_path)) %>% 
  mutate(corpus = "corpusname")

corpusname <- left_join(corpusname_allspkr  , corpusname_chndetails )


write_csv(corpusname, "../project_folder_name/its_info_bycorpus/corpusname_itslengths.csv")