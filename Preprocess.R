library(dplyr)
library(ggplot2)
library(knitr)
library(readr)
library(tools)
library(R.utils)
library(lubridate)
library(stringr)
library(data.table)
library(plyr)

file.name <- "repdata-data-StormData.csv.bz2"
file.url <- "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2FStormData.csv.bz2"

if(!file.exists(file.name)) {
  download.file(url = file.url, destfile = file.name)
}

storm_data <- as.data.table(read.csv(file.name, stringsAsFactors=FALSE, strip.white=TRUE))

dim(storm_data)

# bgn_date as date
sel_storm_data_date  <- storm_data %>% 
  mutate(BGN_DATE = mdy_hms(BGN_DATE)) %>%
  filter(BGN_DATE >= ymd("1959/01/01"))

# columns and date 
sel_storm_data_fn <- sel_storm_data_date %>% 
  select(BGN_DATE, EVTYPE, FATALITIES, INJURIES, PROPDMG, PROPDMGEXP, CROPDMG, CROPDMGEXP)

rm(sel_storm_data_date)

# create factors
sel_storm_data_fn$PROPDMGEXP <- factor(sel_storm_data_fn$PROPDMGEXP, levels=c("","K", "M", "B"))
sel_storm_data_fn$CROPDMGEXP <- factor(sel_storm_data_fn$CROPDMGEXP, levels=c("","K", "M", "B"))

# start cleaning event type
sel_storm_data_fn <- sel_storm_data_fn %>% 
  mutate(EVTYPE = toupper(EVTYPE)) 

# some events have white space
sel_storm_data_fn$EVTYPE <- str_trim(sel_storm_data_fn$EVTYPE)

# create a factor
sel_storm_data_fn$EVTYPE <- factor(sel_storm_data_fn$EVTYPE)

# dictionary exp
exp_num <- c(1, 1000, 1000000, 1000000000)
names(exp_num) <- c("","K", "M", "B")

# calculate damage
sel_storm_data_fn <- sel_storm_data_fn %>% mutate(PROPTOT = exp_num[PROPDMGEXP] * PROPDMG)
sel_storm_data_fn <- sel_storm_data_fn %>% mutate(CROPTOT = exp_num[CROPDMGEXP] * CROPDMG)
sel_storm_data_fn <- sel_storm_data_fn %>% mutate(DAMAGETOT = PROPTOT + CROPTOT)

# calculate harm
sel_storm_data_fn <- sel_storm_data_fn %>% mutate(FATINJTOT = FATALITIES + INJURIES)

# Cleaning up naming discrepancies in events column
file.name.csv <- "events_raw.csv"
file.name.mapped <- "events_mapped.csv"

# write out event names
if(!file.exists(file.name.csv)) {
  events <- sort(levels(sel_storm_data_fn$EVTYPE))
  write.table(as.data.table(events), file = file.name.csv, row.names = FALSE)
}

# read the mapped values back in
event_types_mapped <- read.table(file.name.mapped, stringsAsFactors = FALSE)
names(event_types_mapped) <- c("event_name", "event_name_mapped")

event_types_mapped <- event_types_mapped %>% 
  mutate(event_name_mapped = toupper(event_name_mapped))

# creaete a replacement map.
replace <- as.character(event_types_mapped$event_name_mapped)
names(replace) <- as.character(event_types_mapped$event_name)

# create a copy of EVTYPE so we can check our work.
sel_storm_data_fn <- sel_storm_data_fn %>% mutate(EVTYPEMAP = EVTYPE)

# map to cleaned up codes using revalue from plyr
sel_storm_data_fn$EVTYPEMAP <- revalue(sel_storm_data_fn$EVTYPEMAP, replace = replace)

View(sel_storm_data_fn)