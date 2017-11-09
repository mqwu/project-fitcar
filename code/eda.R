
rm(list=ls(all=TRUE))  # clean memory


#--------------------------------------------------------
# Working dir
#-------------------------------------------------------- 
setwd("Z:/project/FitCar/")



#--------------------------------------------------------
# Libs and tools
#-------------------------------------------------------- 
source("./code/tools.R")

# load the required packages and try to install them if they are not available
reqPackages <- c("tidyverse", "corrplot", "stringr", "lubridate", "purrr")
load_libs(reqPackages)



#--------------------------------------------------------
# Load and processing data
#-------------------------------------------------------- 
d_path = "./data"

df_veh <- readRDS(paste0(d_path,"/df_veh.rds"))
#df_trip <- readRDS(paste0(d_path,"/df_trip.rds"))
#df_driver <- readRDS( paste0(d_path,"/df_dirver.rds"))
#df_user <- readRDS( paste0(d_path,"/df_user.rds"))
#df_geof <- readRDS( paste0(d_path,"/df_geof.rds"))



#--------------------------------------------------------
# EDA
#-------------------------------------------------------- 

#########################################################
## Vehicle: make model year
#########################################################
veh <- df_veh[["vehicles"]]


# check duplicates, No dup!
dim(veh) == dim(distinct(veh))

summary(veh)


## group veh
by_make_model_year <- veh %>% 
                          group_by(make, model, year) %>% 
                          summarise(n = n()) %>% 
                          arrange(desc(n)) %>% ungroup() %>% 
                          mutate(prob=n/sum(n),label=paste(make, model, year, sep="_")) %>% 
                          select(label, make, model, year, n, prob)
write.csv(by_make_model_year, file = paste0(d_path, "/by_make_model_year.csv"), row.names=F)


by_make_model <- veh %>% 
                     group_by(make, model)  %>% 
                     summarise(n = n()) %>% 
                     arrange(desc(n)) %>% ungroup() %>% 
                     mutate(prob=n/sum(n), label=paste(make, model, sep="_")) %>% 
                     select(label, make, model, n, prob)
write.csv(by_make_model, file = paste0(d_path, "/by_make_model.csv"), row.names=F)


by_make <- veh %>% 
               group_by(make) %>% 
               summarise(n = n()) %>% 
               arrange(desc(n)) %>% ungroup() %>% 
               mutate(prob=n/sum(n))
write.csv(by_make, file = paste0(d_path, "/by_make.csv"), row.names=F)


by_model <- veh %>% 
               group_by(model) %>% 
               summarise(n = n()) %>% 
               arrange(desc(n)) %>% 
               mutate(prob=n/sum(n))
write.csv(by_model, file = paste0(d_path, "/by_model.csv"), row.names=F)


by_year <- veh %>% 
               group_by(year) %>% 
               summarise(n = n()) %>% 
               arrange(desc(n)) %>% 
               mutate(prob=n/sum(n))
write.csv(by_year, file = paste0(d_path, "/by_year.csv"), row.names=F)



## plot
d <- head(by_make, 20)
d$make <- factor(d$make, levels = d$make)
#(ggplot(d, aes(x=make, y=n )) + 
(ggplot(d, aes(x=make, y=prob)) + 
  geom_bar(stat="identity", position="identity") +  # error if stat = not included.
  coord_flip() + scale_x_discrete(limits = rev(levels(d$make)))) %>% 
  #ggsave(filename=paste0(d_path, "/by_make_count.png"))
  ggsave(filename=paste0(d_path, "/by_make_prob.png"))


d <- head(by_year, 20)
d$year <- factor(d$year, levels = d$year)
(ggplot(d, aes(x=year, y=n )) + 
#(ggplot(d, aes(x=year, y=prob)) + 
  geom_bar(stat="identity", position="identity") +  # error if stat = not included.
  coord_flip() + scale_x_discrete(limits = rev(levels(d$year)))) %>% 
  ggsave(filename=paste0(d_path, "/by_year_count.png"))
  #ggsave(filename=paste0(d_path, "/by_year_prob.png"))


d <- head(by_make_model, 20)
d$label <- factor(d$label, levels = d$label)
(ggplot(d, aes(x=label, y=n )) + 
#(ggplot(d, aes(x=label, y=prob)) + 
  geom_bar(stat="identity", position="identity") +  # error if stat = not included.
  coord_flip() + scale_x_discrete(limits = rev(levels(d$label)))) %>% 
  ggsave(filename=paste0(d_path, "/by_make_model_count.png"))
  #ggsave(filename=paste0(d_path, "/by_make_model_prob.png"))


d <- head(by_make_model_year, 20)
d$label <- factor(d$label, levels = d$label)
#(ggplot(d, aes(x=label, y=n )) + 
(ggplot(d, aes(x=label, y=prob)) + 
  geom_bar(stat="identity", position="identity") +  # error if stat = not included.
  coord_flip() + scale_x_discrete(limits = rev(levels(d$label)))) %>% 
  #ggsave(filename=paste0(d_path, "/by_make_model_year_count.png"))
  ggsave(filename=paste0(d_path, "/by_make_model_year_prob.png"))



#########################################################
## Vehicle: maintenance  srv_update  diag
#########################################################
maint <- df_veh[["maintenance"]]
srv_update <- df_veh[["service_update"]]
diag <- df_veh[["vehicle_diagnostics"]]  # 19970 x 8
diag_clr <- df_veh[["vehicle_diagnostics_clear_history"]]  # 8022 x 8
diag_event<- df_veh[["diagnostic_event_lookup"]]  # 4 x 3


#-------------------------------------------------------
## diag and diag_clr
#-------------------------------------------------------
# set timeStamp as date var
diag$timeStamp <- ymd_hms(diag$timeStamp)
diag_clr$timeStamp <- ymd_hms(diag_clr$timeStamp)


# check # of distinct veh in diag and diag_clr
diag %>% distinct(vehicle_id) %>% count()  # 373 vehs in diag
diag_clr %>% distinct(vehicle_id) %>% count()  # 357 vehs in diag_clear 


# remove duplicate records(id is not included)
diag_noid <- diag %>% select(-id) %>% distinct()  # 17391 (19970)
diag_clr_noid <- diag_clr %>% select(-id) %>% distinct()  # 8008 (8022)  # about half has been taken care of

## join diag + veh + event_lookup
diag_veh <- diag_noid %>% left_join(veh, by = c("vehicle_id" = "id")) %>% left_join(diag_event, by = c("diagnostic_event_lookup_id" = "id"))
diag_clr_veh <- diag_clr_noid %>% left_join(veh, by = c("vehicle_id" = "id")) %>% left_join(diag_event, by = c("diagnostic_event_lookup_id" = "id"))


# group by dtc code then count # of veh in each group
diag_by_dtc_code <- diag_veh %>% select(dtcCode, vehicle_id) %>% distinct() %>% group_by(dtcCode) %>% count() %>% arrange(desc(n))
#write.csv(diag_by_dtc_code, file = paste0(d_path, "/diag_by_dtc_code.csv"), row.names=F)
diag_clr_by_dtc_code <- diag_clr_veh %>% select(dtcCode, vehicle_id) %>% distinct() %>% group_by(dtcCode) %>% count() %>% arrange(desc(n))
#write.csv(diag_clr_by_dtc_code, file = paste0(d_path, "/diag_clr_by_dtc_code.csv"), row.names=F)


# group by dtc code then count # of events in each group
diag_by_dtc_code_n_event <- diag_veh %>% group_by(dtcCode) %>% count() %>% arrange(dtcCode) %>% rename(n_event = n)
#write.csv(diag_by_dtc_code_n_event, file = paste0(d_path, "/diag_by_dtc_code_n_event.csv"), row.names=F)
diag_clr_by_dtc_code_n_event <- diag_clr_veh %>% group_by(dtcCode) %>% count() %>% arrange(dtcCode) %>% rename(n_event = n)
#write.csv(diag_clr_by_dtc_code_n_event, file = paste0(d_path, "/diag_clr_by_dtc_code_n_event.csv"), row.names=F)



# group by dtc code, car_type (make and model),  
# 1. count # of veh in each group
# 2. count the max # of veh_type for each det code group
diag_dtc_code_vtype <- 
  diag_veh %>% select(dtcCode, vehicle_id) %>% distinct() %>% arrange(dtcCode) %>% left_join(veh, by = c("vehicle_id" = "id")) %>% unite(make_model, make, model)
diag_by_dtc_code_vtype <- diag_dtc_code_vtype %>% group_by(dtcCode, make_model) %>% count() %>% arrange(desc(n)) %>% mutate(n_max = max(n)) %>% arrange(dtcCode) 
#write.csv(diag_by_dtc_code_vtype, file = paste0(d_path, "/diag_by_dtc_code_vtype.csv"), row.names=F)
diag_by_dtc_code_vtype_nmax <- diag_by_dtc_code_vtype %>% distinct(dtcCode, n_max, .keep_all=T) %>% arrange(desc(n_max))
#write.csv(diag_by_dtc_code_vtype_nmax, file = paste0(d_path, "/diag_by_dtc_code_vtype_nmax.csv"), row.names=F)

diag_clr_dtc_code_vtype <- 
  diag_clr_veh %>% select(dtcCode, vehicle_id) %>% distinct() %>% arrange(dtcCode) %>% left_join(veh, by = c("vehicle_id" = "id")) %>% unite(make_model, make, model)
diag_clr_by_dtc_code_vtype <- diag_clr_dtc_code_vtype %>% group_by(dtcCode, make_model) %>% count() %>% arrange(desc(n)) %>% mutate(n_max = max(n)) %>% arrange(dtcCode) 
#write.csv(diag_clr_by_dtc_code_vtype, file = paste0(d_path, "/diag_clr_by_dtc_code_vtype.csv"), row.names=F)
diag_clr_by_dtc_code_vtype_nmax <- diag_clr_by_dtc_code_vtype %>% distinct(dtcCode, n_max, .keep_all=T) %>% arrange(desc(n_max))
#write.csv(diag_clr_by_dtc_code_vtype_nmax, file = paste0(d_path, "/diag_clr_by_dtc_code_vtype_nmax.csv"), row.names=F)


## plot
# d <- diag_by_dtc_code %>% filter(n >= 10) 
# d <- d[-1,]  # rm null row (dtcCode=NULL, checkEngine is on)
# 
# d$dtcCode <- factor(d$dtcCode, levels = d$dtcCode)
# (ggplot(d, aes(x=dtcCode, y=n)) + 
#   geom_bar(stat="identity", position="identity") +  # error if stat = not included.
#   coord_flip() + scale_x_discrete(limits = rev(levels(d$dtcCode)))) %>% 
#   #ggsave(filename=paste0(d_path, "/diag_by_dtc_code1.png"))  # with NULL
#   ggsave(filename=paste0(d_path, "/diag_by_dtc_code2.png"))  # w/o NULL

# d <- diag_clr_by_dtc_code %>% filter(n >= 10) 
# d <- d[-1,]  # rm null row (dtcCode=NULL, checkEngine is on)
# 
# d$dtcCode <- factor(d$dtcCode, levels = d$dtcCode)
# (ggplot(d, aes(x=dtcCode, y=n)) + 
#   geom_bar(stat="identity", position="identity") +  # error if stat = not included.
#   coord_flip() + scale_x_discrete(limits = rev(levels(d$dtcCode)))) %>% 
#   #ggsave(filename=paste0(d_path, "/diag_clr_by_dtc_code1.png"))  # with NULL
#   ggsave(filename=paste0(d_path, "/diag_clr_by_dtc_code2.png"))  # w/o NULL


# group by veh then count # of occurence
diag_by_vid <- diag_noid %>% group_by(vehicle_id) %>% count() %>% arrange(desc(n))
diag_clr_by_vid <- diag_clr_noid %>% group_by(vehicle_id) %>% count() %>% arrange(desc(n))
#write.csv(diag_by_vid, file = paste0(d_path, "/diag_by_vid.csv"), row.names=F)
#write.csv(diag_clr_by_vid, file = paste0(d_path, "/diag_clr_by_vid.csv"), row.names=F)




#d <- diag_veh %>% arrange(vehicle_id, timeStamp) %>% mutate(date=date(timeStamp), hr=hour(timeStamp), min=minute(timeStamp))
# d1 <- d %>% group_by(vehicle_id, date, hr, min) %>% count() %>% filter(n >=2)
# d2 <- d %>% semi_join(d1, by = c("vehicle_id", "date", "hr", "min"))

d <- diag_veh %>% arrange(vehicle_id, timeStamp) 

## for each fault, calculate its time to nearest engine light on duration
# find check engine on time stamp
on <- d %>% filter(checkEngineState=="ON") %>% select(vehicle_id, timeStamp) %>% rename(on_time=timeStamp)
# add on time to the data
d2 <- d %>% inner_join(on, by=c("vehicle_id") ) %>% mutate(t_to_on = difftime(on_time, timeStamp, units="hours"))
# only keep time to nearest on 
d3 <- d2 %>% filter(t_to_on>0)
#d4 <- d3 %>% group_by(vehicle_id, timeStamp) %>% summarize(t_to_on=min(t_to_on))
d4 <- d3 %>% group_by(vehicle_id, timeStamp) %>% arrange(t_to_on) %>% slice(1) %>% select(vehicle_id, timeStamp, t_to_on)
d5 <- d %>% left_join(d4, by = c("vehicle_id", "timeStamp")) %>% filter(checkEngineState!="ON") # with col time to on (nearest); exclude Eng on records

# events with time to on > 1hr 
to_on_gt1h <- d5 %>% filter(t_to_on>=1)
# events with time to on > 1hr, group by dtcCode and count 
to_on_gt1h_by_dtc_code_n_event <- to_on_gt1h %>% group_by(dtcCode) %>% count() %>% arrange(dtcCode) %>% rename(n_2on_gt1h=n)
#write.csv(to_on_gt1h_by_dtc_code_n_event, file = paste0(d_path, "/diag_to_on_gt1h_by_dtc_code_n_event.csv"), row.names=F)  
to_on_lt1h <- d5 %>% filter(t_to_on<1)
# events with time to on < 1hr, group by dtcCode and count 
to_on_lt1h_by_dtc_code_n_event <- to_on_lt1h %>% group_by(dtcCode) %>% count() %>% arrange(dtcCode) %>% rename(n_2on_lt1h=n)
#write.csv(to_on_lt1h_by_dtc_code_n_event, file = paste0(d_path, "/diag_to_on_lt1h_by_dtc_code_n_event.csv"), row.names=F)  

# events with time to on < 1wk 
to_on_lt1wk <- d5 %>% filter(t_to_on <= 168)
# events with time to on < 1wk, group by dtcCode and count 
to_on_lt1wk_by_dtc_code_n_event <- to_on_lt1wk %>% group_by(dtcCode) %>% count() %>% arrange(dtcCode) %>% rename(n_2on_lt1wk=n)
#write.csv(to_on_lt1wk_by_dtc_code_n_event, file = paste0(d_path, "/diag_to_on_lt1wk_by_dtc_code_n_event.csv"), row.names=F)  
# events with time to on > 1wk 
to_on_gt1wk <- d5 %>% filter(t_to_on > 168)
# events with time to on > 1wk, group by dtcCode and count 
to_on_gt1wk_by_dtc_code_n_event <- to_on_gt1wk %>% group_by(dtcCode) %>% count() %>% arrange(dtcCode) %>% rename(n_2on_gt1wk=n)
#write.csv(to_on_gt1wk_by_dtc_code_n_event, file = paste0(d_path, "/diag_to_on_gt1wk_by_dtc_code_n_event.csv"), row.names=F)  

# events with time to on == NA 
to_on_na <- d5 %>% filter(is.na(t_to_on))
# events with time to on = NA, group by dtcCode and count 
to_on_na_by_dtc_code_n_event <- to_on_na %>% group_by(dtcCode) %>% count() %>% arrange(dtcCode) %>% rename(n_2on_na=n)
#write.csv(to_on_na_by_dtc_code_n_event, file = paste0(d_path, "/diag_to_on_na_by_dtc_code_n_event.csv"), row.names=F)  

# events with time to on >1hr < 1wk 
to_on_gt1h_lt1wk <- d5 %>% filter(t_to_on>=1, t_to_on<=168)  
to_on_gt1h_lt1wk_by_dtc_code_n_event <- to_on_gt1h_lt1wk %>% group_by(dtcCode) %>% count() %>% arrange(dtcCode) %>% rename(n_2on_gt1h_lt1wk=n)
#write.csv(to_on_gt1h_lt1wk_by_dtc_code_n_event, file = paste0(d_path, "/diag_to_on_gt1h_lt1wk_by_dtc_code_n_event.csv"), row.names=F)  

dfs <- list(
            diag_by_dtc_code_n_event,
            to_on_gt1h_by_dtc_code_n_event, to_on_lt1h_by_dtc_code_n_event,
            to_on_lt1wk_by_dtc_code_n_event, to_on_gt1wk_by_dtc_code_n_event,
            to_on_gt1h_lt1wk_by_dtc_code_n_event,
            to_on_na_by_dtc_code_n_event
          ) 
df <- reduce(dfs, left_join, by = "dtcCode") 
#write.csv(df, file = paste0(d_path, "/diag_to_on_by_dtc_code_n_event_summary.csv"), row.names=F)  

df_prob <- df %>% mutate( prob_gt1h = n_2on_gt1h/n_event,
                          prob_lt1h = n_2on_lt1h/n_event,
                          prob_lt1wk = n_2on_lt1wk/n_event,
                          prob_gt1wk = n_2on_gt1wk/n_event,
                          prob_na = n_2on_na/n_event,
                          prob_gt1h_lt1wk = n_2on_gt1h_lt1wk/n_event
                          )
#write.csv(df_prob, file = paste0(d_path, "/diag_to_on_by_dtc_code_n_event_prob_summary.csv"), row.names=F)  


x <- to_on_gt1h_lt1wk %>% arrange(vehicle_id, timeStamp) %>% select(-vehicleId, -createdTime, -drivers_id, -users_id, -devices_id) %>% select(vehicle_id, everything())
write.csv(x, file = paste0(d_path, "/diag_dtc_code_on_gt1h_lt1wk.csv"), row.names=F)  
x <- to_on_gt1h_lt1wk %>% group_by(dtcCode) %>% count() %>% arrange(desc(n)) %>% rename(n_event=n)
write.csv(x, file = paste0(d_path, "/diag_by_dtc_code_on_gt1h_lt1wk_n_events.csv"), row.names=F)  # not for distinct veh, some veh may have more than 1 records
x <- to_on_gt1h_lt1wk %>% group_by(dtcCode, vehicle_id) %>% count() %>% arrange(dtcCode, desc(n))
y <- x %>% group_by(dtcCode) %>% count() %>% rename(n_veh=nn) %>% arrange(desc(n_veh))
write.csv(x, file = paste0(d_path, "/diag_by_dtc_code_veh_on_gt1h_lt1wk_n_events.csv"), row.names=F)  
write.csv(y, file = paste0(d_path, "/diag_by_dtc_code_on_gt1h_lt1wk_n_veh.csv"), row.names=F)  


x <- to_on_gt1wk %>% group_by(dtcCode) %>% count() %>% arrange(desc(n)) %>% rename(n_event=n)


x <- diag_noid %>% arrange(vehicle_id, timeStamp)
y <- diag_clr_noid %>% arrange(vehicle_id, timeStamp)
z <- maint %>% filter(vehicle_id==38)
u <- maint %>% filter(vehicle_id==241)


xx <- diag_noid %>% semi_join(diag_clr_noid, by = c("dtcCode", "timeStamp",  "diagnostic_event_lookup_id", "vehicle_id"))

## maintenance
#-------------------------------------------------------
# convert different encoding
maint$description <- iconv(maint$description, "UTF-8", "ASCII", sub = "")

# check duplicates, No dup!
dim(maint) == dim(distinct(maint))
summary(maint)

# keep true service
maint <- maint %>% filter(as.character(lastServiceDate) != "NULL")  # 1196 x 14

maint_by_v_id <- maint %>% group_by(vehicle_id) %>% count() %>% arrange(desc(n))
maint_by_srv_id <- maint %>% group_by(serviceId,description) %>% count() %>% arrange(desc(n))

# serviceId                             description        n
# <int>                                   <chr> <int>
# 1          1 Jiffy Lube Signature Service Oil Change   487
# 2          2                              Air Filter   170
# 3          6                           Tire Rotation   167
# 4          5                            Wiper Blades    89
# 5          4                        Cabin Air Filter    85
# 6          3    Automatic Transmission Fluid Service    43
# 7         15                    Brake Fluid Exchange    35
# 8         18                 Radiator Fluid Exchange    23
# 9         16           Power Steering Fluid Exchange    21
# 10        17                             Spark Plugs    21
# 11        11                                   Belts    20
# 12        20               Rear Differential Service    19
# 13        12                             Fuel Filter    11
# 14         7              Front Differential Service     5




uniq_srv_id <- maint %>% distinct(serviceId) %>% arrange(serviceId)
aa = maint %>% distinct(serviceId, description) %>% arrange(serviceId) 
aa$description = str_replace_all(aa$description, "[[:punct:]]", " ")
aa$description = iconv(aa$description, "UTF-8", "ASCII", sub = "")
maint %>% distinct(serviceId) %>% arrange(serviceId) %>% select(serviceId, description)


## group

#veh_maint$serviceName <- str_replace_all(veh_maint$serviceName, "[[:punct:]]", " ")


## by srv id
maint_by_srvid <- veh_maint %>%
                      group_by(serviceId) %>% 
                      summarise(n = n()) %>% 
                      arrange(desc(n)) 
write.csv(maint_by_srvid, file = paste0(d_path, "/maint_by_srvid.csv"), row.names=F)

srv_update_by_srvid <- veh_srv_update %>%
                                    group_by(serviceId) %>% 
                                    summarise(n = n()) %>% 
                                    arrange(desc(n)) 
write.csv(srv_update_by_srvid, file = paste0(d_path, "/srv_udpate_by_srvid.csv"), row.names=F)


## by dtcCode
veh_diag_by_dtccode <- veh_diag %>% 
                              group_by(dtcCode) %>%  
                              summarise(n = n()) %>% 
                              arrange(desc(n)) 
write.csv(veh_diag_by_dtccode, file = paste0(d_path, "/diag_by_dtccode.csv"), row.names=F)
                          







srv_by_veh_id <- veh_maint %>%
                      group_by(serviceId) %>% 
                      summarise(n = n()) %>% 
                      arrange(desc(n)) 
write.csv(srv_by_veh_id, file = paste0(d_path, "/srv_by_veh_id.csv"), row.names=F)
                      
by_srv <- veh_maint %>%
                    group_by(serviceId, vehicle_id) %>% 
                    summarise(n = n()) %>% 
                    arrange(serviceId, desc(n)) 
write.csv(by_srv, file = paste0(d_path, "/by_srv.csv"), row.names=F)

by_srv %>% filter(n>=2)



update_by_srv <- veh_srv_update %>%
                    group_by(serviceId) %>% 
                    summarise(n = n()) %>% 
                    arrange(serviceId, desc(n)) 


update_by_srv %>% filter(n>=2)







by_srvID <- veh_maint %>% 
                     group_by(serviceId)  %>% 
                     summarise(n = n()) %>% 
                     arrange(desc(n)) %>% ungroup() %>% 
                     mutate(prob=n/sum(n)) 
write.csv(by_srvID, file = paste0(d_path, "/by_srvID.csv"), row.names=F)




by_srvName <- veh_maint %>%  group_by(serviceName)  %>% 
                     summarise(n = n()) %>% 
                     arrange(desc(n)) %>% ungroup() %>% 
                     mutate(prob=n/sum(n)) 
write.csv(by_srvName, file = paste0(d_path, "/by_srvName.csv"), row.names=F)


by_srvID <- veh_srv_update %>% 
                     group_by(serviceId)  %>% 
                     summarise(n = n()) %>% 
                     arrange(desc(n)) %>% ungroup() %>% 
                     mutate(prob=n/sum(n)) 
write.csv(by_srvID, file = paste0(d_path, "/by_srvName.csv"), row.names=F)


## plot
d <- by_srvID
d$serviceId <- factor(d$serviceId, levels = d$serviceId)
#(ggplot(d, aes(x=serviceId, y=n )) + 
(ggplot(d, aes(x=serviceId, y=prob)) + 
  geom_bar(stat="identity", position="identity") +  # error if stat = not included.
  coord_flip() + scale_x_discrete(limits = rev(levels(d$serviceId)))) %>% 
  #ggsave(filename=paste0(d_path, "/by_srvID_count.png"))
  ggsave(filename=paste0(d_path, "/by_srvID_prob.png"))




#########################################################
## Vehicle: devices
#########################################################
devc <- df_veh[["devices"]]  # dim = 957 x 8

# correct time vars type 
devc$firstReportedTime <- ymd_hms(devc$firstReportedTime)
devc$lastReportedTime  <- ymd_hms(devc$lastReportedTime)
devc$createdTime       <- ymd_hms(devc$createdTime)

devc %>% distinct(deviceId) %>% count()        # 957
devc %>% distinct(deviceUniqueId) %>% count()  # 957
levels(devc$deviceType)                        # 3 types:  ""          "BLUETOOTH" "GSM" 
devc %>% group_by(deviceType) %>% count()      # "" 1  BLUETOOTH 943  GSM 13

devc <- devc %>% mutate(diff_last_2_1st = lastReportedTime - firstReportedTime)
devc %>% filter(!is.na(diff_last_2_1st)) %>% summarise(max = max(diff_last_2_1st), min = min(diff_last_2_1st))

# max      min
# 1 457888.5 mins 1.2 mins

# records with lastReportedTime < firstReportedTime
devc_last_bf_first <- devc %>% filter(lastReportedTime < firstReportedTime)  # id 950 956

devc <- devc %>% filter(lastReportedTime >= firstReportedTime)  # 952
hist(as.numeric(devc$diff_last_2_1st), nclass=50)


#########################################################
## Vehicle: notification
#########################################################
notif <- df_veh[["notifications"]]  # dim = 53839 x 12


#########################################################
## Vehicle: franchise , stores
#########################################################
franc <- df_veh[["franchise"]]  # dim = 3 x 3
store <- df_veh[["stores"]]  # dim = 12 x 12

store %>% distinct(storeNumber) %>% count()  # 12
store %>% group_by(franchise_id) %>% count()  #  f1:7 f2:1 f3:4


#########################################################
## Vehicle: coupon
#########################################################
cpn <- df_veh[["coupon_details"]]  # dim = 2213 x 9

cpn %>% distinct(description) %>% count()  # 7
cpn %>% group_by(description) %>% count()
cpn_freq_by_veh <- cpn %>% group_by(vehicle_id) %>% count() %>% arrange(desc(n))


#########################################################
## Vehicle: trip
#########################################################
trips <- df_trip[["trips"]]
trips_hist <- df_trip[["trips_history"]]
trips_by_day <- df_trip[["trips_day_based"]]



