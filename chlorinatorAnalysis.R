## Adaptation of scripts originally created by Diego Perez for the Aquaya Chlorinator project
## Created by Kate Borden, July 2021


## Load required packages
library(tidyverse)
library(plyr)
library(dplyr)
library(lubridate)
library(ggplot2)
library(chron)
library(patchwork)

## Load datasets 
rawDataPath <- "C:/Users/borde/OneDrive/AquayaChlorinators/2021-05-18-2021-06-28--momo-data--ALL.csv"
rawData <- read_csv(file = rawDataPath)
rawData$'Start Date' <- as.Date(rawData$timestamp, tz = "", format = "%m/%d/%y")
rawData <- rawData %>% mutate(timestamp_correctTz = with_tz(timestamp, tzone = "Africa/Kampala"))
rawData <- rawData %>% mutate(padcumul = `pad1` + `pad2` + `pad3` + `pad4`)
rawData <- rawData %>% mutate("cumulative pad percent" = padcumul / 1020)  # max padcumul reading == 892

startDatesPath <- "C:/Users/borde/OneDrive/AquayaChlorinators/startdates.csv"
startDates <- read_csv(file = startDatesPath)
startDates$'startdate' <- as.Date(startDates$startdate, tz = "", format = "%m/%d/%y")
vecStartDates <- startDates$startdate

## Condition datasets to only contain data for this study's sensors
studyData <- inner_join(rawData, startDates, by="id", copy = FALSE)

##Condition datasets to only contain data for dates after sensor installation
conditionedData <- studyData %>% 
  select(id, timestamp_correctTz, pad1, pad2, pad3, pad4, 'cumulative pad percent', 'Start Date', startdate) %>% 
  filter(as.Date(timestamp_correctTz) >= as.Date(startdate))

## Check for gaps in sensor data greater than the threshold
gap_threshold <- 600 # Set gap threshold, let's say, 10 min = 600 sec
tblGaps <- conditionedData %>% 
  group_by(id) %>% 
  arrange(timestamp_correctTz) %>% 
  mutate(gap = timestamp_correctTz[-1] - timestamp_correctTz[-nrow(conditionedData)]) %>% 
  mutate(over_thresh = gap > gap_threshold)
tblGaps_noNAs <- na.omit(tblGaps)

## Calculate uptime/downtime
# Establish Constants (change `segmentlength` and `breakdownlength` if desired)
segmentlength <- 60  # number of minutes as integer (min: 5, max: 1440)
segments <- 1440 / segmentlength
qthreshold <- .24  # Set threshold for anomaly percentile as decimal (10th percentile == .1)
breakdownlength <- 24  # Enter number of hours
breakdownlength2 <- 3
breakdownthreshold <- breakdownlength * 12
breakdownthreshold2 <- breakdownlength2 * 12

## Weekday summary statistics
# Add columns for weekday and hour of day
tblWday <- conditionedData %>% 
  group_by(id)
tblWday <- tblWday %>% 
  mutate(weekday = wday(timestamp_correctTz)) %>% 
  mutate(time = as.numeric(times(strftime(`timestamp_correctTz`, format = "%T", tz = "Africa/Kampala"))))
tblWday <- tblWday %>% 
  mutate(timestamp_hour = hour(timestamp_correctTz))

### Line plots showing cumulative usage for all sensors for all days
lineplots <- tblWday %>% 
  group_by(id) %>% 
  do(plots=ggplot(data=., aes(x=timestamp_correctTz, y=`cumulative pad percent`)) + 
       geom_line() + 
       #labs(title = "", x = "", y = "% Pads Activated") +
       xlab("") +
       ylab("% TIme In Use") +
       facet_wrap( ~ id))

# Display all plots on 
wrap_plots(lineplots$plots, ncol = 4, nrow = 5) + plot_annotation(title = "Handpump Usage Over Course of Study", theme = theme(plot.title = element_text(hjust = 0.5)))

## Median day: hourly usage by handpump 
dailyMedians <- tblWday %>%
  group_by(id, timestamp_hour) %>%
  dplyr::summarise(med = median(`cumulative pad percent`), count = n())

dailyLineplots <- dailyMedians %>% 
  group_by(id) %>% 
  do(plots=ggplot(data=., aes(x=timestamp_hour, y=med)) + 
       geom_line() + 
       #labs(title = "", x = "", y = "% Pads Activated") +
       xlab("Hour of Day") +
       ylab("% Time In Use") +
       facet_wrap( ~ id))

# Display all plots
wrap_plots(dailyLineplots$plots, ncol = 4, nrow = 5) + plot_annotation(title = "Median Daily Handpump Usage", theme = theme(plot.title = element_text(hjust = 0.5)))


## Calculate average daily hours in use (regardless of weekday)
dailyHoursInUse <- tblWday %>%
  group_by(id, date(timestamp_correctTz)) %>%
  filter(`cumulative pad percent` != 0) %>% 
  dplyr::summarise(hoursInUse = n()/12) #288 = number of readngs in day   

avgDailyHoursInUse <- dailyHoursInUse %>%
  group_by(id) %>%
  dplyr::summarise('Median Daily Hours' = median(hoursInUse))

# Print to table
gridExtra::tableGrob(avgDailyHoursInUse)

################## STOP HERE ########################
tblWday_no0 <- tblWday %>% filter('cumulative pad percent' != 0) %>% filter(id == 4595)

gp <- tblWday_no0 %>% 
  group_by(id) %>%
  do(plots=ggplot(data=., aes(x = timestamp_correctTz, y = 'cumulative pad percent')) +
       geom_line() +
       #xlab("") +
       facet_wrap( ~ id))

plot_grid(gp$plots, plotlist = gp$plots,nrow = 5, ncol = 4)



p <- ggplot(tblWday, aes(x=timestamp_correctTz, y=`cumulative pad percent`)) + geom_line() + xlab("")

tbl4595 <- tblWday %>% filter(id == 4595)

p4595 <- ggplot(tbl4595, aes(x=timestamp_correctTz, y=`cumulative pad percent`)) + geom_line() + xlab("")



test <- dayHour %>% 
  group_by(id) %>% 
  do(plots=ggplot(data=., aes(timestamp_hour, weekday)) + 
       geom_tile(aes(fill = 100*N/sum(D)),colour = "white", na.rm = TRUE) +
       #ggtitle(unique(.$id))
       scale_fill_gradient(low = col1, high = col2) +
       guides(fill=guide_legend(title="% Readings")) +
       theme_bw(base_size = 22) + theme_minimal() + theme(plot.title = element_text(hjust = 0.5)) +
       #  labs(title = "Pump Usage by Hour & Day",
       #       x = "Time of Day", y = "Day of Week") +
       theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
       facet_wrap( ~ id))

wrap_plots(test$plots, ncol = 4, nrow = 5) + plot_annotation(title = "Daily Handpump Usage Heatmaps", theme = theme(plot.title = element_text(hjust = 0.5)))

# Group and summarize pad readings by time of day, day of week
wdaySummary <- tblWday %>% 
  group_by(id, weekday, timestamp_hour) %>% 
  summarize(mean = mean(`cumulative pad percent`), sd = sd(`cumulative pad percent`), median = median(`cumulative pad percent`), quantile = quantile(`cumulative pad percent`, c(qthreshold)))


### Compute usage Statistics
### Create filtered tbl with non-zero pad values
tblData_no0 <- filter(tblData, pad3Percentage != 0)
StartTimes_ByDate <- aggregate(tblData_no0["timestamp_time"], by=tblData_no0["timestamp_date"], min)
EndTimes_ByDate <- aggregate(tblData_no0["timestamp_time"], by=tblData_no0["timestamp_date"], max)
StartEndTimes <- merge(StartTimes_ByDate, EndTimes_ByDate, by="timestamp_date")

medMonthlyStartTimes <- StartEndTimes %>%
  mutate(month = floor_date(timestamp_date, "month")) 
medMonthlyStartTimes <- ddply(medMonthlyStartTimes, "month", summarise,
                              avgStart = median(min(times(timestamp_time.x))),
                              avgEnd = median(max(times(timestamp_time.y)))
)

## Hour of Day/Day of Week Heatmap
# Make dataframe for heatmap
dayHour <- ddply(tblWday, .(timestamp_hour, weekday), summarise,
                 N = length(timestamp_hour),
                 D = length(conditionedData)
)

# Reverse day order for graphing
#dayHour$weekday <- factor(dayHour$weekday, levels=rev(levels(dayHour$weekday)))
attach(dayHour)
# Plot heatmap
col1 = "#d8e1cf"
col2 = "#438484"
ggplot(dayHour, aes(timestamp_hour, weekday)) + geom_tile(aes(fill = 100*N/sum(D)),colour = "white", na.rm = TRUE) +
  scale_fill_gradient(low = col1, high = col2) +
  guides(fill=guide_legend(title="% Readings")) +
  theme_bw(base_size = 22) + theme_minimal() + theme(plot.title = element_text(hjust = 0.5)) +
  labs(title = "Pump Usage by Hour & Day",
       x = "Time of Day", y = "Day of Week") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())

# Make heatmaps for each handpump
for idNum in unique(conditionedData$id){
  tblWday_grouped <- tblWday %>% group_by(idNum) %>% select()
  dayHour <- ddply(tblWday, .(timestamp_hour, weekday, id), summarise,
                   N = length(timestamp_hour),
                   D = length(conditionedData)
  )
  attach(dayHour)
  col1 = "#d8e1cf"
  col2 = "#438484"
  ggplot(dayHour, aes(timestamp_hour, weekday)) + geom_tile(aes(fill = 100*N/sum(D)),colour = "white", na.rm = TRUE) +
    scale_fill_gradient(low = col1, high = col2) +
    guides(fill=guide_legend(title="% Readings")) +
    theme_bw(base_size = 22) + theme_minimal() + theme(plot.title = element_text(hjust = 0.5)) +
    labs(title = "Pump Usage by Hour & Day",
         x = "Time of Day", y = "Day of Week") +
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())
}

## KB TEST ##
test <- dayHour %>% 
  group_by(id) %>% 
  do(plots=ggplot(data=., aes(timestamp_hour, weekday)) + 
  geom_tile(aes(fill = 100*N/sum(D)),colour = "white", na.rm = TRUE) +
  #ggtitle(unique(.$id))
  scale_fill_gradient(low = col1, high = col2) +
  guides(fill=guide_legend(title="% Readings")) +
  theme_bw(base_size = 22) + theme_minimal() + theme(plot.title = element_text(hjust = 0.5)) +
#  labs(title = "Pump Usage by Hour & Day",
#       x = "Time of Day", y = "Day of Week") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  facet_wrap( ~ id))
#plot_grid(test, margin = c(1, 1, 1, 1), tags = NULL)


plot_grid(test$plots, plotlist = test$plots,nrow = 5, ncol = 4)




##### STOP HERE #####
# Create average daily use tibble (regardless of weekday)
avgdailyusetbllist <- wdaysummarytbllist %>% lapply(function(x) {
  wdaytbl <- ungroup(x)
  dailytbl <- wdaytbl %>% group_by(segment) %>%
    summarize(median = mean(median), .groups = "keep")
  daytbl <- dailytbl %>% filter(median > 0.2)
})

# Create average daily use tibble (regardless of weekday)
avgdailyusetbllist <- wdaySummary %>% lapply(function(x) {
  wdaytbl <- ungroup(x)
  dailytbl <- wdaytbl %>% group_by(segment) %>%
    summarize(median = mean(median), .groups = "keep")
  daytbl <- dailytbl %>% filter(median > 0.2)
})

# Create heat-maps (stored as list of ggplot objects)
heatmaps <- wdaysummarytbllist %>% lapply(function(x) {
  wdaysummary <- x
  heatmap <- ggplot(data = wdaysummary, aes(x = segment, y = reorder(weekday, desc(weekday)))) +
    geom_tile(aes(fill = mean)) +
    labs(title = "Weekly Handpump Usage", y = "Day of Week", x = "Time of Day") +
    theme_classic(base_size = 24) + 
    scale_y_discrete(breaks=c("7","6","5", "4", "3", "2", "1"), 
                     labels=c("Sat.", "Fri.", "Thurs.", "Wed.", "Tues.", "Mon.", "Sun.")) +
    scale_x_discrete(breaks=c(5,10, 15, 20), 
                     labels=c("4 am", "9 am", "2 pm", "7 pm")) +
    scale_fill_distiller(palette = 1, direction = 1)
})
# Calculate time-in-use (daily, weekly, monthly, cumulatively) for all sensors and for each sensor



## Visualize/characterize water point usage