library(ggplot2)
library(hms)
library(lubridate)
library(scales)
library(corrplot)

#####################################
#### Sleep Data
#####################################
# Load raw data
sleep <- read.csv("./data/raw_sleepdata.csv")
# switching to military time so i dont have to worry abt am/pm
sleep$Start.Time <- format(strptime(sleep$Start.Time, "%Y-%m-%d %I:%M%p"), format="%Y-%m-%d %H:%M") 
sleep$End.Time <- format(strptime(sleep$End.Time, "%Y-%m-%d %I:%M%p"), format="%Y-%m-%d %H:%M") 
head(sleep)
tail(sleep)
# fix dtypes
sleep$Minutes.REM.Sleep <- as.integer(sleep$Minutes.REM.Sleep)
sleep$Minutes.Light.Sleep <- as.integer(sleep$Minutes.Light.Sleep)
sleep$Minutes.Deep.Sleep <- as.integer(sleep$Minutes.Deep.Sleep)
# add Quality = Minutes Asleep/Minutes in Bed
sleep$Quality <- sleep$Minutes.Asleep/sleep$Time.in.Bed

# Extract times
sleep$Sleeptime <- hms::hms(second(sleep$Start.Time), minute(sleep$Start.Time), hour(sleep$Start.Time))
sleep$Sleeptime <- as.POSIXct(sleep$Sleeptime)
sleep$Waketime <- hms::hms(second(sleep$End.Time), minute(sleep$End.Time), hour(sleep$End.Time))
sleep$Waketime <- as.POSIXct(sleep$Waketime)

# Plot densities
ggplot(sleep, aes(Sleeptime)) + 
  geom_density(fill = "orange", alpha = 0.5) +
  scale_x_datetime(labels=date_format("%H:%M AM")) +
  labs(y='Frequency', x='Sleep Time') + 
  ggtitle('Sleep Times Density Plot') +
  theme(plot.title = element_text(hjust = 0.5))

ggplot(sleep, aes(Waketime)) + 
  geom_density(fill = "blue", alpha = 0.5) +
  scale_x_datetime(labels=date_format("%H:%M AM")) +
  labs(y='Frequency', x='Wake Time') + 
  ggtitle('Wake Times Density Plot') +
  theme(plot.title = element_text(hjust = 0.5))

# Multimodal (aka the weekend effect)?
ggplot(sleep, aes(Time.in.Bed)) + 
  geom_density(fill = "brown", alpha = 0.5) +
  labs(y='Frequency', x='Minutes In Bed') + 
  ggtitle('Minutes in Bed Density Plot') +
  theme(plot.title = element_text(hjust = 0.5))

ggplot(sleep, aes(Minutes.Asleep)) + 
  geom_density(fill = "green", alpha = 0.5) +
  labs(y='Frequency', x='Minutes Asleep') + 
  ggtitle('Minutes Asleep Density Plot') +
  theme(plot.title = element_text(hjust = 0.5))

ggplot(sleep, aes(Minutes.Awake)) + 
  geom_density(fill = "red", alpha = 0.5) +
  labs(y='Frequency', x='Minutes Awake') + 
  ggtitle('Minutes Awake Density Plot') +
  theme(plot.title = element_text(hjust = 0.5))

ggplot(sleep, aes(Minutes.REM.Sleep)) + 
  geom_density(fill = "purple", alpha = 0.5) +
  labs(y='Frequency', x='Minutes REM') + 
  ggtitle('Minutes REM Density Plot') +
  theme(plot.title = element_text(hjust = 0.5))

ggplot(sleep, aes(Minutes.Light.Sleep)) + 
  geom_density(fill = "purple", alpha = 0.5) +
  labs(y='Frequency', x='Minutes Light Sleep') + 
  ggtitle('Minutes Light Sleep Density Plot') +
  theme(plot.title = element_text(hjust = 0.5))

ggplot(sleep, aes(Minutes.Deep.Sleep)) + 
  geom_density(fill = "purple", alpha = 0.5) +
  labs(y='Frequency', x='Minutes Deep Sleep') + 
  ggtitle('Minutes Deep Sleep Density Plot') +
  theme(plot.title = element_text(hjust = 0.5))

ggplot(sleep, aes(Quality)) + 
  geom_density(fill = "yellow", alpha = 0.5) +
  labs(y='Frequency', x='Quality (Minutes Asleep/Time in Bed)') + 
  ggtitle('Quality of Sleep Density Plot') +
  theme(plot.title = element_text(hjust = 0.5))

# Correlation Plot
corrplot(cor(sleep[,c(3, 4, 5, 6, 7, 8, 9)]), type='upper', method="circle")

#####################################
#### Activity Data
#####################################
# Load cleaned data
activity <- read.csv("./data/activitydata.csv")
# Drop floors
activity <- activity[,-5]
# Fix dtypes
activity$Calories.Burned <- as.integer(gsub(",", "", activity$Calories.Burned))
activity$Steps <-  as.integer(gsub(",", "", activity$Steps))
activity$Minutes.Sedentary <- as.integer(gsub(",", "", activity$Minutes.Sedentary))
activity$Minutes.Lightly.Active <- as.integer(activity$Minutes.Lightly.Active)
activity$Minutes.Fairly.Active <- as.integer(activity$Minutes.Fairly.Active)
activity$Minutes.Very.Active <- as.integer(activity$Minutes.Very.Active)
activity$Activity.Calories <- as.integer(activity$Activity.Calories)
# Convert to datetime
activity$Date <- format(strptime(activity$Date, "%m/%d/%y"), format="%Y-%m-%d") 
# View data
head(activity)
tail(activity)

# Plot densities
ggplot(activity, aes(Calories.Burned)) + 
  geom_density(fill = "red", alpha = 0.5) +
  labs(y='Frequency', x='Calories Burned') + 
  ggtitle('Calories Burned Density Plot') +
  theme(plot.title = element_text(hjust = 0.5))

ggplot(activity, aes(Steps)) + 
  geom_density(fill = "orange", alpha = 0.5) +
  labs(y='Frequency', x='Daily Steps') + 
  ggtitle('Daily Steps Density Plot') +
  theme(plot.title = element_text(hjust = 0.5))

ggplot(activity, aes(Distance)) + 
  geom_density(fill = "purple", alpha = 0.5) +
  labs(y='Frequency', x='Daily Distance Travelled') + 
  ggtitle('Daily Distance Density Plot') +
  theme(plot.title = element_text(hjust = 0.5))

ggplot(activity, aes(Minutes.Sedentary)) + 
  geom_density(fill = "yellow", alpha = 0.5) +
  labs(y='Frequency', x='Daily Minutes Sedentary') + 
  ggtitle('Daily Minutes Sedentary Density Plot') +
  theme(plot.title = element_text(hjust = 0.5))

ggplot(activity, aes(Minutes.Lightly.Active)) + 
  geom_density(fill = "yellow", alpha = 0.5) +
  labs(y='Frequency', x='Daily Minutes Lightly Active') + 
  ggtitle('Daily Lightly Active Minutes Density Plot') +
  theme(plot.title = element_text(hjust = 0.5))

ggplot(activity, aes(Minutes.Very.Active)) + 
  geom_density(fill = "yellow", alpha = 0.5) +
  labs(y='Frequency', x='Daily Minutes Very Active') + 
  ggtitle('Daily Very Active Minutes Density Plot') +
  theme(plot.title = element_text(hjust = 0.5))

ggplot(activity, aes(Activity.Calories)) + 
  geom_density(fill = "yellow", alpha = 0.5) +
  labs(y='Frequency', x='Daily Activity Calories Burned') + 
  ggtitle('Daily Activity Calories Burned Density Plot') +
  theme(plot.title = element_text(hjust = 0.5))

# Correlation Plot
corrplot(cor(activity[,-1]), type='upper', method="circle")

# Potential analysis ideas:
# Fit curve to sleep time and wake times
# - how to account for weekend effect
# Run a regression to see if activity affects 
# - duration of sleep
# - quality of sleep (Minutes Asleep/Time in Bed)?