# Set vaccinetime root directory full path in VACTIMEPATH environment
datafile <- paste0(Sys.getenv("VACTIME_PATH"), "/data/vactweets.Rds")
graph_datafile <- paste0(Sys.getenv("VACTIME_PATH"), "/data/tweets_for_graph.Rdata")
app_datafile <- paste0(Sys.getenv("VACTIME_PATH"), "/tweets_for_graph.Rdata")

library(rtweet)
library(data.table)
vactweets <- rtweet::get_timeline("vaccinetime", n = 1000)
setDT(vactweets)
if(file.exists(datafile)) {
  # print("file exists!")
  previous_vactweets <- readRDS(datafile)
  previous_ids <- previous_vactweets$status_id
  new_vactweets <- vactweets[!(status_id %chin% previous_ids)]
  if(nrow(new_vactweets) > 0) {
    vactweets <- data.table::rbindlist(list(vactweets, new_vactweets))
    vactweets <- unique(vactweets, by = "status_id")
  }  
  
}
saveRDS(vactweets, datafile)
Sys.sleep(5)
tweets <- readRDS(datafile)

tweets <- tweets[is_retweet==FALSE, .(created_at, text)]
tweets[, Date := as.Date(created_at)]
tweets[, Time := lubridate::ymd_hms(created_at, tz = "UTC")]
tweets[, Time := lubridate::with_tz(created_at, tz = "America/New_York")]
tweets[, Hour := lubridate::hour(Time)][, Weekday := weekdays(Date)]
tweets[, Weekday := factor(Weekday, levels = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday"), ordered = TRUE)]
tweets[, Type := fcase(
  grepl("^CVS", text), "CVS",
  grepl("^\\d", text), "Specific Location"
)]
tweets[, Number := ifelse(Type == "Specific Location", gsub("^(\\d+)\\sappointment.*?$", "\\1", text), NA)][, Number := as.integer(Number)]
tweets[, Location := ifelse(Type == "CVS", "CVS", 
                            gsub(".*?\\sin\\s(.*?\\sMA).*?$", "\\1", text)
                            
)]
tweets[, Location := gsub("^.*?available at\\s(.*?)\\son.*?$", "\\1", Location)]

# check_location <- rio::import("data/locations.xlsx")
# useful_tweets <- merge(tweets, check_location, all.x = TRUE, all.y = FALSE)
# useful_tweets <- useful_tweets[Date >= as.Date("2021-03-01") & NotUseful ==  "depends"]
# useful_locations <- check_location$Location[check_location$NotUseful == "depends"]

cvs_tweets <- tweets[Type == "CVS"]

save(tweets, cvs_tweets, file = graph_datafile)
save(tweets, cvs_tweets, file = app_datafile)
