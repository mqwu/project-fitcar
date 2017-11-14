
rm(list=ls(all=TRUE))  # clean memory


########################################################
# Working dir
########################################################
setwd("Z:/project/FitCar/")
#setwd('C:\\Apps\\projects\\FitCar')

########################################################
# Libs and tools
########################################################
source("./code/tools.R")

# load the required packages and try to install them if they are not available
reqPackages <- c("tidyverse", "corrplot", "stringr", "lubridate", "purrr", "rebus")
load_libs(reqPackages)


########################################################
# Load and processing data
########################################################
d_path = "./data"


#-----------------------------------------------------
# load data
#-----------------------------------------------------
df_veh <- readRDS(paste0(d_path,"/df_veh.rds"))

veh <- df_veh[["vehicles"]]  # veh: make model year
diag <- df_veh[["vehicle_diagnostics"]]  # 19970 x 8
diag_event<t- df_veh[["diagnostic_event_lookup"]]  # 4 x 3
maint <- df_veh[["maintenance"]]


#-----------------------------------------------------
# processing data
#-----------------------------------------------------
# set timeStamp as date var
diag$timeStamp <- ymd_hms(diag$timeStamp)

# check # of distinct veh in diag 
# diag %>% distinct(vehicle_id) %>% count()  # 373 vehs in diag

# remove duplicate records(id is not included)
diag_nodup <- diag %>% select(-id) %>% distinct()  # 17391 (19970)

## join diag + veh 
diag_veh <- diag_nodup %>% 
                left_join(veh, by = c("vehicle_id" = "id")) %>% 
                select(dtcCode, dtcDesc, dtcState, checkEngineState, diagnostic_event_lookup_id, timeStamp, vehicle_id, make, model, engine, year)


veh_miles <- maint %>% 
                filter(lastServiceOdometer!=0) %>%  
                group_by(vehicle_id) %>% 
                arrange( desc(lastServiceOdometer) ) %>% 
                slice(1) %>% 
                select(vehicle_id, lastServiceDate, lastServiceOdometer) %>% 
                mutate(mileage_group = ifelse(lastServiceOdometer < 100000, "Less than 100k", ifelse(lastServiceOdometer < 150000, "100 to 150k", "Greater than 150k")))


########################################################
# Pattern analysis
########################################################

#-----------------------------------------------------
# step 1 create pattern table
#-----------------------------------------------------
# arrange diag_veh 
d <- diag_veh %>% arrange(vehicle_id, timeStamp, dtcCode, diagnostic_event_lookup_id) 

### for each code, calculate the time to its critical code
# find critical code diagnostic_event_lookup_id = 1 or 2 
cr <- d %>% filter(diagnostic_event_lookup_id %in% c(1, 2)) %>% select(vehicle_id, timeStamp) %>% rename(cr_time = timeStamp)  # 11373
# calculate time to critical moment
d2 <- d %>% inner_join(cr, by=c("vehicle_id") ) %>% mutate(t_to_cr = difftime(cr_time, timeStamp, units="hours"))

### add time to next critical moment 
# for those having next critical momment, calculate t_to_cr. For critical code, t_to_cr = 0
d3 <- d2 %>% 
          filter(t_to_cr >- 0.09) %>%  # negative 0.09 means it shows within 5 mins after a critical code showed up
          group_by(vehicle_id, timeStamp, dtcCode, diagnostic_event_lookup_id) %>% 
          arrange(t_to_cr) %>% 
          slice(1) %>% 
          select(vehicle_id, timeStamp, dtcCode, diagnostic_event_lookup_id, t_to_cr)
# add t_to_cr 
d4 <- d %>% 
        left_join(d3, by = c("vehicle_id", "timeStamp", "dtcCode", "diagnostic_event_lookup_id")) %>% 
        mutate(t_2_cr=ifelse(is.na(t_to_cr), 8888, ifelse(diagnostic_event_lookup_id %in% c(1, 2), NA, t_to_cr)))   # if not having next cr_t, set t_to_cr = 8888

### add time to proceeding critical moment 
# for those having preceeding critical momment, calculate t_to_cr. For critical code, t_to_cr = 0
d5 <- d2 %>% 
          filter(t_to_cr <= 0) %>% 
          group_by(vehicle_id, timeStamp, dtcCode, diagnostic_event_lookup_id) %>% 
          arrange(desc(t_to_cr)) %>% 
          slice(1) %>% 
          select(vehicle_id, timeStamp, dtcCode, diagnostic_event_lookup_id, pr_cr_time = cr_time) 
# add t_to_pr_cr 
d6 <- d4 %>% 
        left_join(d5, by = c("vehicle_id", "timeStamp", "dtcCode", "diagnostic_event_lookup_id"))  %>%  # if pr_cr_time is missing, assign an very old time stamp
        mutate(pr_cr_time = ifelse(is.na(pr_cr_time), ymd_hms("1999-09-09 00:00:00"), ifelse(diagnostic_event_lookup_id %in% c(1,2), NA, ymd_hms(pr_cr_time)))) %>% 
        mutate(pr_cr_time = as_datetime(pr_cr_time)) %>% 
        select(-t_to_cr)
#View( d6 %>% select(dtcCode, diagnostic_event_lookup_id, timeStamp, vehicle_id, t_2_cr, t_2_pr_cr ) )

 
code <- d6 %>% 
           mutate(norm = ifelse(diagnostic_event_lookup_id %in% c(1,2), 0, 1)) %>%  # add an indicator for norm code (non-critical) 
           mutate(t2cr_gt1h = ifelse(t_2_cr >= 1, 1, 0)) %>%                         # add an indicator t to next cr >= 1 hr
           mutate(t2cr_in_1wk = ifelse(norm == 0, NA,                                # add an indicator for time to next cr < 1wk
                                     ifelse(t2cr_gt1h == 0, NA, 
                                            ifelse(t_2_cr <= 168, "Yes", "No")))) %>% 
           mutate(timeStamp_pr_wk = as_datetime(ifelse(diagnostic_event_lookup_id %in% c(1, 2), NA, timeStamp - days(7)))) %>% 
           mutate(cut_time = as_datetime(ifelse(pr_cr_time > timeStamp_pr_wk, pr_cr_time, timeStamp_pr_wk))) %>% 
           select(-dtcState, -make, -model, -engine, -year) %>% 
           select(vehicle_id, dtcCode, dtcDesc, checkEngineState, diagnostic_event_lookup_id, timeStamp, cut_time, pr_cr_time, timeStamp_pr_wk, t_2_cr, t2cr_gt1h, t2cr_in_1wk, norm)
  

      
form_pattern <- function(code, lookup_id, timeStamp, cut_time, norm){
    ## for each vehicle form patterns according to timestamps 
    ##
    ## inputs:
    ## code
    ## lookup_id
    ## timestamp
    ## cut_time: timestamp - 1 week
    ## norm
    ##
    ## return pattern data frame
  
    d <- data.frame(code, lookup_id, timeStamp, cut_time, norm)
    
    # find normal code and remove normal code with the same timestamp as critical code
    d2 <- d %>% filter(norm == 1) %>% filter(timeStamp!=cut_time) 
    
    
    if(nrow(d2) >= 1) {
      d2 <- d2 %>% mutate(pattern=NA, start=NA, end_code=NA)
    } else {
      return(out = d %>% mutate(pattern=NA, start=NA, end_code=NA))
    }
  
    for(i in 1:nrow(d2)) {
        t <- d2[i, "timeStamp"]
        cut <- d2[i, "cut_time"]
  
        # for a norm code, identify code that before its timestamp and latter than its 1 week cut
        codes <- d2 %>% filter(timeStamp <= t, timeStamp >= cut) %>% select(code, timeStamp)
  
        # form pattern
        d2[i, "pattern"] <-  str_c(codes$code, collapse = "-")
        d2[i, "start"] <- min(codes$timeStamp) 
        d2[i, "end_code"] <-  tail(as.character(codes$code), n=1)  # when codes have same timestamp, we use the last code as end_code
    }
  
    out <- d %>% left_join(d2, by=c("code", "lookup_id", "timeStamp", "cut_time", "norm")) %>% mutate(start=as_datetime(start))
    return(out)
  
}


# form patterns for each vehicles 
pattern <- code %>% 
              group_by(vehicle_id) %>% 
              do(pattern = form_pattern(code=.$dtcCode, lookup_id =.$diagnostic_event_lookup_id, timeStamp=.$timeStamp, cut_time=.$cut_time, norm=.$norm))

# combine pattern list into data frame
pat <- do.call(rbind.data.frame, pattern$pattern)           
pat <- data.frame(code, pattern=pat$pattern, start=pat$start, end_code=pat$end_code)
#write.csv(pat, file = paste0(d_path, "/pattern.csv"), row.names=F)  

# add fault code index
pat <- rownames_to_column(pat, "code_id")  # note the code_id could be different from the end_code when muliple codes have the same timestamp

pa <- pat %>% 
          filter(!is.na(pattern)) %>% 
          filter(pattern!="NULL") %>% 
          mutate(n_code = str_count(pattern, fixed("-")) + 1) %>% 
          select(end_code_id=code_id, vehicle_id, pattern, n_code, end_code, critical_fault_in_1wk=t2cr_in_1wk, start, end=timeStamp) %>%  
          filter(!is.na(critical_fault_in_1wk))   # for those codes share the same timestamp as critical code, remove them
#write.csv(pa, file = paste0(d_path, "/patterns.csv"), row.names=F)  


#------------------------------------------------------------------------------------------------------------------------
# step 2 detect common pattern
#------------------------------------------------------------------------------------------------------------------------
# identify distinct patterns
uniq_pat <- pa %>% distinct(pattern, n_code)
#write.csv(uniq_pat, file = paste0(d_path, "/uniq_pat.csv"), row.names=F)  


## build pattern basic component data frame
# split each distinct patterns
uniq_pat_split <- lapply(uniq_pat$pattern, str_split, "-")
# find unique components
pat_uniq_comp <- unique(unlist(uniq_pat_split))
# build pattern components data frame
pat_comp <- data_frame(component = pat_uniq_comp)


count_pat_comp <- function(pat, pat_comp){
    ## given a pattern, find # of each component 
    ##
    ## inputs:
    ## pat: a pattern
    ## pat_comp: pattern basic component data frame
    ##
    ## return: pattern components count data frame 
   
    # decompose pattern into components 
    str <- str_split(pat, "-", simplify = T)
    str <- as.list(str)
   
    # count # of occurence of each component in the pattern 
    comp_count <- lapply(unique(str), function(x) str_count(str,x))
    names(comp_count) <- unique(str)
    
    # convert count table to data frame 
    comp_n <- do.call(rbind.data.frame, comp_count)
    
    # sum total of each comp
    comp_sum <- as.data.frame(apply(comp_n, 1, sum))
    names(comp_sum) <- "count"
    comp_sum <- rownames_to_column(comp_sum, "component")
   
    # join with pattern component data frame 
    pat_comp_n <- pat_comp %>% left_join(comp_sum, by=c("component"))
    pat_comp_n[is.na(pat_comp_n$count), "count" ] <- 0

    return(pat_comp_n) 
}

# build unique pattern component count data frame list
uniq_pat_comp_n <- apply(uniq_pat[, "pattern", drop=F], 1, count_pat_comp, pat_comp)


check_a_in_b <- function(a_comp_n, b_comp_n){
    ## given pattern a and b, check if a is in b
    ##
    ## inputs:
    ## a_comp_n: pattern a component count data frame
    ## b_comp_n: pattern b component count data frame
    ##
    ## return:  logic var TRUE(a in b) or FALSE (a not in b)
  
     return( sum(b_comp_n$count >= a_comp_n$count) == nrow(a_comp_n) )
}


find_sub_pat <- function(pat_comp_n, uniq_pat_comp_n, uniq_pat) {
    ## given a pattern, find sub patterns from uniq_pat pools
    ##
    ## inputs:
    ## pat_comp_n: pattern component count data frame
    ## uniq_pat_comp_n: unique pattern component count data frame list
    ## uniq_pat: uniq pattern data frame
    ##
    ## return: sub patterns data frame 
  
    sub_pat_index <- sapply(uniq_pat_comp_n, check_a_in_b, pat_comp_n)
    
    pattern <- uniq_pat[which(sub_pat_index==TRUE),] %>% arrange(n_code, pattern) %>% rename(sub_pattern = pattern, sub_n_code = n_code)
    
    return( pattern )
}


find_end_code_sub_pat <- function(end_code, end_code_pa, pat_comp, uniq_pat_comp_n, uniq_pat){
    ## given an end_code, find its sub patterns from uniq_pat pools
    ##
    ## inputs:
    ## end_code_id: end_code_id
    ## end_code: end_code(pattern)
    ## end_code_pa: end_code pattern data frame
    ## pat_comp: pattern component
    ## uniq_pat_comp_n: unique pattern component count data frame list
    ## uniq_pat: uniq pattern data frame
    ##
    ## return: sub patterns data frame 
  
    pat_comp_n <- count_pat_comp(end_code, pat_comp) 
  
    sub_pat <- find_sub_pat(pat_comp_n, uniq_pat_comp_n, uniq_pat)
    
    return(sub_pat)
}

# find sub patters for each pattern
sub_pat <- tapply(pa$pattern, pa$end_code_id, find_end_code_sub_pat, end_code_pa=pa, pat_comp=pat_comp, uniq_pat_comp_n=uniq_pat_comp_n, uniq_pat=uniq_pat)

# combine sub patterns into df
df_sub_pat <- do.call(rbind.data.frame, sub_pat)           
df_sub_pat <- rownames_to_column(df_sub_pat, "end_code_id") %>% arrange(end_code_id) %>% mutate(end_code_id = str_replace_all(end_code_id, "\\." %R% one_or_more(DGT), ""))

# pattern and its sub pattern tables for further analysis
pat_and_sub <- pa %>% left_join(df_sub_pat, by=c("end_code_id"))
#write.csv(pat_and_sub, file = paste0(d_path, "/pat_and_sub.csv"), row.names=F)  

#save.image(file="./data/pat_and_sub.RData")
#load("Z:/project/FitCar/data/pat_and_sub.RData")  # also need to load libs
#load("./data/pat_and_sub.RData")  # also need to load libs




#------------------------------------------------------------------------------------------------------------------------
# step 3 analysis pattern
#------------------------------------------------------------------------------------------------------------------------
#pat_and_sub <- pat_and_sub %>% mutate(start = as_datetime(start), end = as_datetime(end))

rm_sub_pat_dup <- function(start, end){
    ## for each vehicle form patterns according to timestamps 
    ##
    ## inputs:
    ## start: start time of pattern
    ## end  : end time of pattern
    ##
    ## return a logic vector keep or remove sub patterns
  
    d <- data.frame(start, end)
    
    # sort by end time
    d <- d %>% arrange(end)
    
    if(nrow(d) >= 2) {
      d <- d %>% mutate(keep=0)
    } else {
      return(out <- d %>% mutate(keep=1) %>% select(keep))
    }
 
    N <- nrow(d)  # last pattern index
    
    d[N, "keep"] <- 1  # set last pattern as keep 
    keep_start <- d[N, "start"]
    
    for(i in (N-1):1) {
      
        if(d[i, "end"] < keep_start){
            # if the new pattern's end time is before the keep pattern's start time, we keep it 
            keep_start <- d[i, "start"]  # set new keep pattern starting time
            d[i, "keep"] <- 1
        }
    }
  
    out <- d %>% select(keep)
    return(out)
}


# first order the data for better view
pat_and_sub <- pat_and_sub %>% 
                  arrange(vehicle_id, sub_pattern, end) 

# create keep indicator for non-dup records
pat_and_sub_keep <- pat_and_sub %>% 
                      group_by(vehicle_id, sub_pattern) %>% 
                      do(keep = rm_sub_pat_dup(start=.$start, end=.$end))

# combine results into a df
sub_keep <- do.call(rbind.data.frame, pat_and_sub_keep$keep)           

# bind to raw data
pat_and_sub_keep <- bind_cols(pat_and_sub, sub_keep)

# remove duplicates
pat_and_sub_nodup <- pat_and_sub_keep %>% filter(keep==1)

# add veh characteristic
pat_and_sub_nodup_vehinfo <- pat_and_sub_nodup %>% 
                                    left_join(veh, by = c("vehicle_id" = "id")) %>% 
                                    select(-(createdTime:users_id)) %>% 
                                    left_join(veh_miles, by = c("vehicle_id"))
#write.csv(pat_and_sub_nodup_vehinfo, file = paste0(d_path, "/pat_and_sub_nodup.csv"), row.names=F)  

# add weight
pat_and_sub_nodup_w <- pat_and_sub_nodup_vehinfo %>% 
                                group_by(vehicle_id, sub_pattern) %>% 
                                summarize(n_sub_per_veh = n()) %>% 
                                mutate(w = 1.0/n_sub_per_veh)


# calculate # of veh for each sub_pat
n_veh_of_subpat <- pat_and_sub_nodup_w %>% group_by(sub_pattern) %>% summarise(n_veh = n_distinct(vehicle_id)) 
# filter pattern with at least 5 veh
n_veh_of_subpat_gt5 <- n_veh_of_subpat %>% filter(n_veh>=5)

# calculate prob for each pattern weighted # of appearance with each vehicle 
pat_and_sub_nodup_prob  <- pat_and_sub_nodup_vehinfo %>% 
                                        left_join(pat_and_sub_nodup_w, by = c("vehicle_id", "sub_pattern")) %>% 
                                        group_by(sub_pattern, critical_fault_in_1wk) %>% 
                                        summarise(count=sum(w)) %>% 
                                        spread(key=critical_fault_in_1wk, value=count, fill=0) %>% 
                                        mutate(Total = Yes + No) %>%
                                        mutate(Yes=floor(Yes+0.5), No=Total-Yes) %>% 
                                        mutate(Prob_follow_by_crt_fault = Yes/Total * 1.0) %>% 
                                        mutate(CI_Low=ifelse(Yes>=1, binom.test(Yes, Total)$conf.int[1], NA), CI_Up=ifelse(Yes>=1, binom.test(Yes, Total)$conf.int[2], NA))

# summary of sub-pattern prob.
all_veh_subpat_sum <- n_veh_of_subpat %>% left_join(pat_and_sub_nodup_prob, by=c("sub_pattern")) %>% arrange(desc(Prob_follow_by_crt_fault))
ge5_veh_subpat_sum <- n_veh_of_subpat_gt5 %>% left_join(pat_and_sub_nodup_prob, by=c("sub_pattern")) %>% arrange(desc(Prob_follow_by_crt_fault))

#write.csv(all_veh_subpat_sum, file = paste0(d_path, "/all_veh_subpat_sum.csv"), row.names=F)  
#write.csv(ge5_veh_subpat_sum, file = paste0(d_path, "/ge5_veh_subpat_sum.csv"), row.names=F)  


## add veh info to the prob. results
# find distinct veh set in the pat_and_sub_nodup set
pat_and_sub_nodup_dist_veh <- pat_and_sub_nodup_vehinfo %>% 
                                    distinct(sub_pattern, vehicle_id) %>%
                                    left_join(veh, by = c("vehicle_id" = "id")) %>% 
                                    select(-(createdTime:users_id)) %>%
                                    left_join(veh_miles, by = c("vehicle_id")) %>% 
                                    select(-(lastServiceDate:lastServiceOdometer)) %>% 
                                    arrange(sub_pattern, make, model, year, mileage_group) 

all_veh_subpat_sum_detail <- pat_and_sub_nodup_dist_veh %>% left_join(all_veh_subpat_sum, by = "sub_pattern")
ge5_veh_subpat_sum_detail <- pat_and_sub_nodup_dist_veh %>% 
                                  semi_join(n_veh_of_subpat_gt5, by = "sub_pattern") %>% 
                                  left_join(ge5_veh_subpat_sum, by = "sub_pattern")

#write.csv(all_veh_subpat_sum_detail, file = paste0(d_path, "/all_veh_subpat_sum_details.csv"), row.names=F)  
#write.csv(ge5_veh_subpat_sum_detail, file = paste0(d_path, "/ge5_veh_subpat_sum_details.csv"), row.names=F)  



