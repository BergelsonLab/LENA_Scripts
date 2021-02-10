#here we have two useful functions
#get_its_tier_times: dur and n for all speakers
#get_its_tier_times_CHN_details: vfx/cry/utt dur and n for CHN in particular
# these should be fed back into homebank/rlena on github in case useful to others!

#let's grab the libraries we need first
library(rlena)
#fyi to install rlena, uncomment and run two lines below
#if(!require(devtools) install.packages("devtools")
#   devtools::install_github("HomeBankCode/rlena", dependencies = TRUE)
library(tidyverse)
library(lubridate)
library(skimr)


#updated get_its_tier_times function thnx to gladys baudet 7/3/19
# further tinkering by elika fall 2019 [now all vfx stuff is in a sep function]

# Psst: i left some helpful notes at the bottom about some idiosyncracies here
#its <- read_its_file("../bergelson/its/123435-0291.its") #typical file
#its <- read_its_file("../kidd/ANU_LENA_its_files/e20170519_140247_012129.its")#weird file
get_its_tier_times <-  function(its_file, speakers = NA){
  print(its_file)
  # read the its file in the right format to apply package functions
  its <- read_its_file(its_file)
  
  # read recording(s) details (several recordings if paused)
  rec <- gather_recordings(its)
  
  # get the start and end time(s) of recording(s)
  rec_start <- rec$startTime
  rec_end <- rec$endTime
  
  # get total length of recording
  total_time <- sum(rec_end-rec_start)
  
  # get segments information
  segments_orig <- gather_segments(its)
  
  # filter down to only the asked for speakers (default is all)
  if (!is.na(speakers)) {
    segments <- segments_orig %>%
      filter(spkr %in% speakers)
  } else {
    segments <- segments_orig
  }
  
  # get duration of speech for each speaker
  duration_by_spkr <- segments %>%
    # compute duration of each segment
    rowwise() %>% 
    mutate(duration = endTime-startTime) %>% 
    # group all segments by speaker
    group_by(spkr) %>%
    # sum all the durations of segments by speaker
    summarize(duration = sum(duration)) %>%
    # get total duration of recording (to compare with rec_start-rec_end)
    mutate(total_time = sum(duration)) %>% 
    # spread on spkr
    spread(key=spkr, value=duration) %>% 
    rename_all(function(x) paste0(x, "_dur"))
  
  segments_by_spkr <- segments %>% 
    group_by(spkr) %>% 
    count() %>% 
    spread(key=spkr, value=n) %>% 
    rename_all(function(x) paste0(x, "_n"))

  #these are for that rare special flower of a file where there are no fan/man/CHN 
  #segments or convos and thus none of these
  
  if(!"femaleAdultNonSpeechLen" %in% colnames(segments)){
    segments$femaleAdultNonSpeechLen <- NA
  }
  if(!"maleAdultNonSpeechLen" %in% colnames(segments)){
    segments$maleAdultNonSpeechLen <- NA
  }
  if(!"femaleAdultUttCnt" %in% colnames(segments)){
    segments$femaleAdultUttCnt <- NA
  }
  if(!"maleAdultUttCnt" %in% colnames(segments)){
    segments$maleAdultUttCnt <- NA
  }
  if(!"femaleAdultUttLen" %in% colnames(segments)){
    segments$femaleAdultUttLen <- NA
  }
  if(!"maleAdultUttLen" %in% colnames(segments)){
    segments$maleAdultUttLen <- NA
  }
  if(!"maleAdultWordCnt" %in% colnames(segments)){
    segments$maleAdultWordCnt <- NA
  }
  if(!"femaleAdultWordCnt" %in% colnames(segments)){
    segments$femaleAdultWordCnt <- NA
  }
  if(!"childUttCnt" %in% colnames(segments)){
    segments$childUttCnt <- NA
  }
  if(!"convTurnCount" %in% colnames(segments)){
    segments$convTurnCount <- NA
  }
  
  AWC_CTC_CVC <- segments %>% 
    #test case "../kidd/ANU_LENA_its_files/e20170519_140247_012129.its" missing cols
    
     summarise(AWC = sum(maleAdultWordCnt, na.rm=T) +
                    sum(femaleAdultWordCnt, na.rm=T),
              CVC = sum(childUttCnt, na.rm=T),# note this includes CHF utts!!! 
              CTC = ifelse(max(convTurnCount, na.rm=T)== -Inf, 0, max(convTurnCount, na.rm=T)),
              FAN_AWC = sum(femaleAdultWordCnt, na.rm=T),#these appear to only have vals when femaleAdultNonSpeechLen=0
              FAN_ns_dur = sum(femaleAdultNonSpeechLen, na.rm=T),#these appear to only have vals when femaleAdultWordCnt=0
              FAN_utt_n = sum(femaleAdultUttCnt, na.rm=T),#these appear to always be 0
              FAN_utt_dur = sum(femaleAdultUttLen, na.rm=T),#these appear to always be 0
              MAN_AWC = sum(maleAdultWordCnt, na.rm=T),#these appear to only have vals when maleAdultNonSpeechLen=0
              MAN_ns_dur = sum(maleAdultNonSpeechLen, na.rm=T),#these appear to only have vals when maleAdultWordCnt=0
              MAN_utt_n = sum(maleAdultUttCnt, na.rm=T),#these appear to always be 0
              MAN_utt_dur = sum(maleAdultUttLen, na.rm=T))#these appear to always be 0
    
    
  res <- cbind(duration_by_spkr, segments_by_spkr, AWC_CTC_CVC)
  
  res
}


#this function gives us all the dur and count info for CHN in particular

 get_its_tier_times_CHN_details <-  function(its_file){
  # read the its file in the right format to apply package functions
  its <- read_its_file(its_file)
  #its <- read_its_file("/Users/eb255/Box Sync/data_broad_strokes_lena/bergelson/its/123417-0008.its")
  

  # get segments information
  segments_orig <- gather_segments(its)
  if (!"CHN" %in% segments_orig$spkr) {
    CHN_segment_dur = 0
    CHN_utt_dur=0
    CHN_cryvfx_dur = 0 
    CHN_cry_dur = 0
    CHN_vfx_dur = 0 
    CHN_segment_n = 0 
    CHN_utt_n = 0 
    CHN_cry_n = 0 
    CHN_vfx_n = 0
    CHN_utt_segs_n = 0 
    CHN_cry_segs_n = 0
    res <- data.frame(CHN_segment_dur, CHN_utt_dur, CHN_cryvfx_dur, CHN_cry_dur,
                      CHN_vfx_dur, CHN_segment_n, CHN_utt_n, CHN_cry_n, CHN_vfx_n,
                      CHN_utt_segs_n, CHN_cry_segs_n) 
  } 
  else {
    
  segments <- filter(segments_orig, spkr=="CHN")
  
   # turns out files can have infinty Utts and Cries but only 1 vfx
  # need to add those col's as NAs for those cases; in our dataset we never
  # found more than 8 cries and 14 Utts so going up to 10 and 15
  # there is a much less brittle way to code this , sorry for the hack job
  
  segments <- segments %>% 
    rowwise() %>% 
    mutate(startCry10 = ifelse(!"startCry10" %in% colnames(segments), NA, startCry10),
             endCry10 =   ifelse(!"endCry10" %in% colnames(segments), NA,   endCry10),
           startCry9 = ifelse(!"startCry9" %in% colnames(segments), NA, startCry9),
             endCry9 =   ifelse(!"endCry9" %in% colnames(segments), NA,   endCry9),
           startCry8 = ifelse(!"startCry8" %in% colnames(segments), NA, startCry8),
             endCry8 =   ifelse(!"endCry8" %in% colnames(segments), NA,   endCry8),
           startCry7 = ifelse(!"startCry7" %in% colnames(segments), NA, startCry7),
             endCry7 =   ifelse(!"endCry7" %in% colnames(segments), NA,   endCry7),
           startCry6 = ifelse(!"startCry6" %in% colnames(segments), NA, startCry6),
             endCry6 =   ifelse(!"endCry6" %in% colnames(segments), NA,   endCry6),
           startCry5 = ifelse(!"startCry5" %in% colnames(segments), NA, startCry5),
             endCry5 =   ifelse(!"endCry5" %in% colnames(segments), NA,   endCry5),
           startCry4 = ifelse(!"startCry4" %in% colnames(segments), NA, startCry4),
             endCry4 =   ifelse(!"endCry4" %in% colnames(segments), NA,   endCry4),
           startCry3 = ifelse(!"startCry3" %in% colnames(segments), NA, startCry3),
             endCry3 =   ifelse(!"endCry3" %in% colnames(segments), NA,   endCry3),
           startCry2 = ifelse(!"startCry2" %in% colnames(segments), NA, startCry2),
             endCry2 =   ifelse(!"endCry2" %in% colnames(segments), NA,   endCry2),
           startCry1 = ifelse(!"startCry1" %in% colnames(segments), NA, startCry1),
             endCry1 =   ifelse(!"endCry1" %in% colnames(segments), NA,   endCry1),
           
           startUtt15 = ifelse(!"startUtt15" %in% colnames(segments), NA, startUtt15),
             endUtt15 =   ifelse(!"endUtt15" %in% colnames(segments), NA,   endUtt15),
           startUtt14 = ifelse(!"startUtt14" %in% colnames(segments), NA, startUtt14),
             endUtt14 =   ifelse(!"endUtt14" %in% colnames(segments), NA,   endUtt14),
           startUtt13 = ifelse(!"startUtt13" %in% colnames(segments), NA, startUtt13),
             endUtt13 =   ifelse(!"endUtt13" %in% colnames(segments), NA,   endUtt13),
           startUtt12 = ifelse(!"startUtt12" %in% colnames(segments), NA, startUtt12),
             endUtt12 =   ifelse(!"endUtt12" %in% colnames(segments), NA,   endUtt12),
           startUtt11 = ifelse(!"startUtt11" %in% colnames(segments), NA, startUtt11),
             endUtt11 =   ifelse(!"endUtt11" %in% colnames(segments), NA,   endUtt11),
           startUtt10 = ifelse(!"startUtt10" %in% colnames(segments), NA, startUtt10),
             endUtt10 =   ifelse(!"endUtt10" %in% colnames(segments), NA,   endUtt10),
           startUtt9 = ifelse(!"startUtt9" %in% colnames(segments), NA, startUtt9),
             endUtt9 =   ifelse(!"endUtt9" %in% colnames(segments), NA,   endUtt9),
           startUtt8 = ifelse(!"startUtt8" %in% colnames(segments), NA, startUtt8),
             endUtt8 =   ifelse(!"endUtt8" %in% colnames(segments), NA,   endUtt8),
           startUtt7 = ifelse(!"startUtt7" %in% colnames(segments), NA, startUtt7),
             endUtt7 =   ifelse(!"endUtt7" %in% colnames(segments), NA,   endUtt7),
           startUtt6 = ifelse(!"startUtt6" %in% colnames(segments), NA, startUtt6),
             endUtt6 =   ifelse(!"endUtt6" %in% colnames(segments), NA,   endUtt6),
           startUtt5 = ifelse(!"startUtt5" %in% colnames(segments), NA, startUtt5),
             endUtt5 =   ifelse(!"endUtt5" %in% colnames(segments), NA,   endUtt5),
           startUtt4 = ifelse(!"startUtt4" %in% colnames(segments), NA, startUtt4),
             endUtt4 =   ifelse(!"endUtt4" %in% colnames(segments), NA,   endUtt4),
           startUtt3 = ifelse(!"startUtt3" %in% colnames(segments), NA, startUtt3),
             endUtt3 =   ifelse(!"endUtt3" %in% colnames(segments), NA,   endUtt3),
           startUtt2 = ifelse(!"startUtt2" %in% colnames(segments), NA, startUtt2),
             endUtt2 =   ifelse(!"endUtt2" %in% colnames(segments), NA,   endUtt2),
           startUtt1 = ifelse(!"startUtt1" %in% colnames(segments), NA, startUtt1),
             endUtt1 =   ifelse(!"endUtt1" %in% colnames(segments), NA,   endUtt1),
           
           startVfx1 = ifelse(!"startVfx1" %in% colnames(segments), NA, startVfx1),
           endVfx1 = ifelse(!"endVfx1" %in% colnames(segments), NA, endVfx1))
  
  # get duration and count of all the possible chi segs
  dur_count_CHN <- segments %>% ungroup() %>% 
    # give each cry and utt a tally
    mutate(cry1 = ifelse(!is.na(startCry1), 1, 0),
           cry2 = ifelse(!is.na(startCry2), 1, 0),
           cry3 = ifelse(!is.na(startCry3), 1, 0),
           cry4 = ifelse(!is.na(startCry4), 1, 0),
           cry5 = ifelse(!is.na(startCry5), 1, 0),
           cry6 = ifelse(!is.na(startCry6), 1, 0),
           cry7 = ifelse(!is.na(startCry7), 1, 0),
           cry8 = ifelse(!is.na(startCry8), 1, 0),
           cry9 = ifelse(!is.na(startCry9), 1, 0),
           cry10 = ifelse(!is.na(startCry10), 1, 0),

           utt1 = ifelse(!is.na(startUtt1), 1, 0),
           utt2 = ifelse(!is.na(startUtt2), 1, 0),
           utt3 = ifelse(!is.na(startUtt3), 1, 0),
           utt4 = ifelse(!is.na(startUtt4), 1, 0),
           utt5 = ifelse(!is.na(startUtt5), 1, 0),
           utt6 = ifelse(!is.na(startUtt6), 1, 0),
           utt7 = ifelse(!is.na(startUtt7), 1, 0),
           utt8 = ifelse(!is.na(startUtt8), 1, 0),
           utt9 = ifelse(!is.na(startUtt9), 1, 0),
           utt10 = ifelse(!is.na(startUtt10), 1, 0),
           utt11 = ifelse(!is.na(startUtt11), 1, 0),
           utt12 = ifelse(!is.na(startUtt12), 1, 0),
           utt13 = ifelse(!is.na(startUtt13), 1, 0),
           utt14 = ifelse(!is.na(startUtt14), 1, 0),
           utt15 = ifelse(!is.na(startUtt15), 1, 0),
           
           #this is embarassingly coded but it works and i hate regex and just can't
           #pull out start and stop of each cry
           cry1_start = str_replace(startCry1,"PT", ""),
           cry1_start = as.numeric(str_replace(cry1_start,"S", "")),
           cry2_start = str_replace(startCry2,"PT", ""), 
           cry2_start = as.numeric(str_replace(cry2_start,"S", "")),
           cry3_start = str_replace(startCry3,"PT", ""), 
           cry3_start = as.numeric(str_replace(cry3_start,"S", "")),
           cry4_start = str_replace(startCry4,"PT", ""),
           cry4_start = as.numeric(str_replace(cry4_start,"S", "")),
           cry5_start = str_replace(startCry5,"PT", ""), 
           cry5_start = as.numeric(str_replace(cry5_start,"S", "")),
           cry6_start = str_replace(startCry6,"PT", ""), 
           cry6_start = as.numeric(str_replace(cry6_start,"S", "")),
           cry7_start = str_replace(startCry7,"PT", ""),
           cry7_start = as.numeric(str_replace(cry7_start,"S", "")),
           cry8_start = str_replace(startCry8,"PT", ""), 
           cry8_start = as.numeric(str_replace(cry8_start,"S", "")),
           cry9_start = str_replace(startCry9,"PT", ""), 
           cry9_start = as.numeric(str_replace(cry9_start,"S", "")),
           cry10_start = str_replace(startCry10,"PT", ""), 
           cry10_start = as.numeric(str_replace(cry10_start,"S", "")),
           
           cry1_end = str_replace(endCry1,"PT", ""),
           cry1_end = as.numeric(str_replace(cry1_end,"S", "")),
           cry2_end = str_replace(endCry2,"PT", ""), 
           cry2_end = as.numeric(str_replace(cry2_end,"S", "")),
           cry3_end = str_replace(endCry3,"PT", ""), 
           cry3_end = as.numeric(str_replace(cry3_end,"S", "")),
           cry4_end = str_replace(endCry4,"PT", ""),
           cry4_end = as.numeric(str_replace(cry4_end,"S", "")),
           cry5_end = str_replace(endCry5,"PT", ""), 
           cry5_end = as.numeric(str_replace(cry5_end,"S", "")),
           cry6_end = str_replace(endCry6,"PT", ""), 
           cry6_end = as.numeric(str_replace(cry6_end,"S", "")),
           cry7_end = str_replace(endCry7,"PT", ""),
           cry7_end = as.numeric(str_replace(cry7_end,"S", "")),
           cry8_end = str_replace(endCry8,"PT", ""), 
           cry8_end = as.numeric(str_replace(cry8_end,"S", "")),
           cry9_end = str_replace(endCry9,"PT", ""), 
           cry9_end = as.numeric(str_replace(cry9_end,"S", "")),
           cry10_end = str_replace(endCry10,"PT", ""), 
           cry10_end = as.numeric(str_replace(cry10_end,"S", "")),
           # how long is each dur
           cry1_dur = ifelse(!is.na(cry1_start), cry1_end - cry1_start,0),
           cry2_dur = ifelse(!is.na(cry2_start), cry2_end - cry2_start,0),
           cry3_dur = ifelse(!is.na(cry3_start), cry3_end - cry3_start,0),
           cry4_dur = ifelse(!is.na(cry4_start), cry4_end - cry4_start,0),
           cry5_dur = ifelse(!is.na(cry5_start), cry5_end - cry5_start,0),
           cry6_dur = ifelse(!is.na(cry6_start), cry6_end - cry6_start,0),
           cry7_dur = ifelse(!is.na(cry7_start), cry7_end - cry7_start,0),
           cry8_dur = ifelse(!is.na(cry8_start), cry8_end - cry8_start,0),
           cry9_dur = ifelse(!is.na(cry9_start), cry9_end - cry9_start,0),
           cry10_dur = ifelse(!is.na(cry10_start), cry10_end - cry10_start,0),
           
           #pull out start and stop of each utt
           utt1_start = str_replace(startUtt1,"PT", ""),
           utt1_start = as.numeric(str_replace(utt1_start,"S", "")),
           utt2_start = str_replace(startUtt2,"PT", ""), 
           utt2_start = as.numeric(str_replace(utt2_start,"S", "")),
           utt3_start = str_replace(startUtt3,"PT", ""), 
           utt3_start = as.numeric(str_replace(utt3_start,"S", "")),
           utt4_start = str_replace(startUtt4,"PT", ""),
           utt4_start = as.numeric(str_replace(utt4_start,"S", "")),
           utt5_start = str_replace(startUtt5,"PT", ""), 
           utt5_start = as.numeric(str_replace(utt5_start,"S", "")),
           utt6_start = str_replace(startUtt6,"PT", ""), 
           utt6_start = as.numeric(str_replace(utt6_start,"S", "")),
           utt7_start = str_replace(startUtt7,"PT", ""),
           utt7_start = as.numeric(str_replace(utt7_start,"S", "")),
           utt8_start = str_replace(startUtt8,"PT", ""), 
           utt8_start = as.numeric(str_replace(utt8_start,"S", "")),
           utt9_start = str_replace(startUtt9,"PT", ""), 
           utt9_start = as.numeric(str_replace(utt9_start,"S", "")),
           utt10_start = str_replace(startUtt10,"PT", ""), 
           utt10_start = as.numeric(str_replace(utt10_start,"S", "")),
           utt11_start = str_replace(startUtt11,"PT", ""), 
           utt11_start = as.numeric(str_replace(utt11_start,"S", "")),
           utt12_start = str_replace(startUtt12,"PT", ""),
           utt12_start = as.numeric(str_replace(utt12_start,"S", "")),
           utt13_start = str_replace(startUtt13,"PT", ""), 
           utt13_start = as.numeric(str_replace(utt13_start,"S", "")),
           utt14_start = str_replace(startUtt14,"PT", ""), 
           utt14_start = as.numeric(str_replace(utt14_start,"S", "")),
           utt15_start = str_replace(startUtt15,"PT", ""), 
           utt15_start = as.numeric(str_replace(utt15_start,"S", "")),
           
           
           utt1_end = str_replace(endUtt1,"PT", ""),
           utt1_end = as.numeric(str_replace(utt1_end,"S", "")),
           utt2_end = str_replace(endUtt2,"PT", ""), 
           utt2_end = as.numeric(str_replace(utt2_end,"S", "")),
           utt3_end = str_replace(endUtt3,"PT", ""), 
           utt3_end = as.numeric(str_replace(utt3_end,"S", "")),
           utt4_end = str_replace(endUtt4,"PT", ""),
           utt4_end = as.numeric(str_replace(utt4_end,"S", "")),
           utt5_end = str_replace(endUtt5,"PT", ""), 
           utt5_end = as.numeric(str_replace(utt5_end,"S", "")),
           utt6_end = str_replace(endUtt6,"PT", ""), 
           utt6_end = as.numeric(str_replace(utt6_end,"S", "")),
           utt7_end = str_replace(endUtt7,"PT", ""),
           utt7_end = as.numeric(str_replace(utt7_end,"S", "")),
           utt8_end = str_replace(endUtt8,"PT", ""), 
           utt8_end = as.numeric(str_replace(utt8_end,"S", "")),
           utt9_end = str_replace(endUtt9,"PT", ""), 
           utt9_end = as.numeric(str_replace(utt9_end,"S", "")),
           utt10_end = str_replace(endUtt10,"PT", ""), 
           utt10_end = as.numeric(str_replace(utt10_end,"S", "")),
           utt11_end = str_replace(endUtt11,"PT", ""), 
           utt11_end = as.numeric(str_replace(utt11_end,"S", "")),
           utt12_end = str_replace(endUtt12,"PT", ""),
           utt12_end = as.numeric(str_replace(utt12_end,"S", "")),
           utt13_end = str_replace(endUtt13,"PT", ""), 
           utt13_end = as.numeric(str_replace(utt13_end,"S", "")),
           utt14_end = str_replace(endUtt14,"PT", ""), 
           utt14_end = as.numeric(str_replace(utt14_end,"S", "")),
           utt15_end = str_replace(endUtt15,"PT", ""), 
           utt15_end = as.numeric(str_replace(utt15_end,"S", "")),
           
           # how long is each dur
           utt1_dur = ifelse(!is.na(utt1_start), utt1_end - utt1_start,0),
           utt2_dur = ifelse(!is.na(utt2_start), utt2_end - utt2_start,0),
           utt3_dur = ifelse(!is.na(utt3_start), utt3_end - utt3_start,0),
           utt4_dur = ifelse(!is.na(utt4_start), utt4_end - utt4_start,0),
           utt5_dur = ifelse(!is.na(utt5_start), utt5_end - utt5_start,0),
           utt6_dur = ifelse(!is.na(utt6_start), utt6_end - utt6_start,0),
           utt7_dur = ifelse(!is.na(utt7_start), utt7_end - utt7_start,0),
           utt8_dur = ifelse(!is.na(utt8_start), utt8_end - utt8_start,0),
           utt9_dur = ifelse(!is.na(utt9_start), utt9_end - utt9_start,0),
           utt10_dur = ifelse(!is.na(utt10_start), utt10_end - utt10_start,0),
           utt11_dur = ifelse(!is.na(utt11_start), utt11_end - utt11_start,0),
           utt12_dur = ifelse(!is.na(utt12_start), utt12_end - utt12_start,0),
           utt13_dur = ifelse(!is.na(utt13_start), utt13_end - utt13_start,0),
           utt14_dur = ifelse(!is.na(utt14_start), utt14_end - utt14_start,0),
           utt15_dur = ifelse(!is.na(utt15_start), utt15_end - utt15_start,0),
           
           
           #count up the cries and utts and their means and durs
           cry_intra_segment_count = (cry1+cry2+cry3+cry4+cry5+cry6+cry7+cry8+cry9+cry10),
           utt_intra_segment_count = childUttCnt,
           utt_intra_segment_dur = (utt1_dur + utt2_dur + utt3_dur + utt4_dur + 
                                      utt5_dur + utt6_dur + utt7_dur + utt8_dur +
                                      utt9_dur + utt10_dur + utt11_dur + utt12_dur +
                                      utt13_dur + utt14_dur + utt15_dur),
           utt_intra_seg_mean = (utt1_dur + utt2_dur + utt3_dur + utt4_dur + 
                                   utt5_dur + utt6_dur + utt7_dur + utt8_dur +
                                   utt9_dur + utt10_dur + utt11_dur + utt12_dur +
                                   utt13_dur + utt14_dur + utt15_dur)/utt_intra_segment_count,
           cry_intra_segment_dur = (cry1_dur+cry2_dur+cry3_dur+cry4_dur+cry5_dur+cry6_dur+cry7_dur+cry8_dur+cry9_dur+cry10_dur),
           cry_intra_seg_mean = (cry1_dur + cry2_dur + cry3_dur + cry4_dur + 
                                   cry5_dur + cry6_dur + cry7_dur + cry8_dur +
                                   cry9_dur + cry10_dur)/cry_intra_segment_count,

           #tally up the (maximum of) 1 vfx (without cries) and its dur
           vfx1 = ifelse(!is.na(startVfx1),1,0), #no option for 2 or 3
           vfx_segment_dur = ifelse(!is.na(endVfx1), endVfx1 - startVfx1, 0),
           
           # duration of CHn segment
           duration_CHN_segment = endTime - startTime) 
 
    # group all segments by speaker
    
    res <- dur_count_CHN %>% ungroup() %>% #group_by(spkr) %>% 
    # sum all the durations of segments by speaker
    summarise(CHN_segment_dur = sum(duration_CHN_segment),#full chn segment
              CHN_utt_dur = sum(utt_intra_segment_dur),#chn segment utts only
              CHN_cryvfx_dur = sum(childCryVfxLen),
              CHN_cry_dur = sum(cry_intra_segment_dur),# chn segment cry only
              CHN_vfx_dur = sum(vfx_segment_dur),  #chn segment vfx only
              CHN_segment_n = n(),
              CHN_utt_n = sum(utt_intra_segment_count),
              CHN_cry_n = sum(cry_intra_segment_count),
              CHN_vfx_n = sum(vfx1),
              CHN_utt_segs_n = sum(!is.na(utt1_start)), #just counts 1 utt per CHN
              CHN_cry_segs_n = sum(!is.na(cry1_start))) # just counts 1 cry per CHN
              #final sanity check
    
    nocry_novfx_noutt_count <- dur_count_CHN  %>% 
      tally(is.na(startCry1) & is.na(startVfx1) & is.na(startUtt1))#4
    cry_and_vfx_count <- dur_count_CHN  %>% tally(!is.na(startCry1) & !is.na(startVfx1)) #15
    cry_and_utt_count <- dur_count_CHN  %>% tally(!is.na(startCry1) & !is.na(startUtt1)) #0
    utt_and_vfx_count <- dur_count_CHN  %>% tally(!is.na(startUtt1) & !is.na(startVfx1)) #0
    utt_count <- dur_count_CHN  %>% tally(!is.na(startUtt1))#773
    cry_count <- dur_count_CHN  %>% tally(!is.na(startCry1))#663
    vfx_count <- dur_count_CHN  %>% tally(!is.na(startVfx1))#63
    
    sanity_check_counts  <- utt_count + cry_count + vfx_count + nocry_novfx_noutt_count - 
      cry_and_vfx_count - cry_and_utt_count - utt_and_vfx_count
    print(its_file)
    print(ifelse(sanity_check_counts==res$CHN_segment_n, 
                 "cries, vfx, & utts (+/-overlaps) add up to nrows, good job!",
                 "uhoh, something's wrong, look into it!"))

  res
  }
}

#N.B. fe/maleAdultUttCnt and fe/maleAdultUttLen appear to always be zero, cool.
 # in contrast, fe/maleAdultWordCnt and fe/maleAdultNonSpeechLen trade off having values; when one is zero the othr isn't
#N.B.: there are unlimited  cries and 1utts in a given CHN (8 and 14 in our dataset)
 #but only up to 1 VFX
#N.B. CHN_utt_dur + CHN_cry_dur + CHN_vfx_only_dur doesn't add up oto CHN_segment dur bc of intra-segment silences

# there can be CHN lines with NO utt NO vfx and NO cry...
#filter(dur_count_CHN, is.na(utt1_start)& is.na(cry1_start) & is.na(startVfx1))

 
 ######built in sanity check that we can remove later (they come from diff functions, just checking math)
 # CHN_dur = CHN_segment_dur and CHN_n = CHN_segment_n and 

 # CVC = childUttCnt != CHN_utt_n bc the firs ttwo include CHF utts (!)
 # but sum(childuttLen) in convo and segs is the same....
 # sum(turnTaking) from gather_conversation is the same as max(convTurnCount) from gather_segments
 # sum(adultWordCnt) from gather_conversation is the same as (sum(femalAdultWordCnt)+sum(maleAdultWordCnt) from gather_segments)
 
