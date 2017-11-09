
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
reqPackages <- c("tidyverse", "corrplot", "stringr")
load_libs(reqPackages)


#--------------------------------------------------------
# Load and processing data
#-------------------------------------------------------- 
d_path = "./data"

files = list.files(path=d_path, pattern="*.csv")  # file names
df_names = str_replace(files, ".csv", "")  # df names

dfs = list()
for (i in 1:length(files)){
    # load df 
    path <- paste0(d_path, "/", files[i])
    df <- read.csv(path, header=TRUE)
    
    # concatenate df into dfs list 
    dfs[[df_names[i]]] <- df
}



## group data frames

# vehicles 
veh <- c("vehicles", "devices", "notifications", "notification_type_lookup",  "franchise", 
         "stores", "coupon_details", "maintenance", "maintainance_state_lookup", "service_based_lookup",
         "vehicle_diagnostics", "diagnostic_event_lookup", "vehicle_diagnostics_clear_history", "service_update")
df_veh <- dfs[veh]

# trips 
trip <- c("trips", "trips_history", "trips_day_based", "trips_month_based")
df_trip <- dfs[trip]

# drivers 
driver <- c("driver_trip_score",  "driver_trip_score_history", "driver_daily_score", "drivers")
df_driver <- dfs[driver]

# users 
user <- c("users", "terms_and_condition", "privacy_and_policy")
df_user <- dfs[user]

# geogences 
geof <- c("geofence_visits", "geofence_visits_daily", "geofence_visits_monthly", "geofences")
df_geof <- dfs[geof]


saveRDS(df_veh, paste0(d_path,"/df_veh.rds"))
saveRDS(df_trip, paste0(d_path,"/df_trip.rds"))
saveRDS(df_driver, paste0(d_path,"/df_dirver.rds"))
saveRDS(df_user, paste0(d_path,"/df_user.rds"))
saveRDS(df_geof, paste0(d_path,"/df_geof.rds"))




