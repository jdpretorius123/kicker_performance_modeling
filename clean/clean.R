# Comments ----------------------------------------------------------------

## 1. The script has been updated to specify the interception variable and
##    runs as expected.

# -------------------------------------------------------------------------
# Installing/Loading Dependencies -----------------------------------------

## updating lock file
renv::snapshot()

packages = c('tidyverse', 'lubridate', 'glue', 'janitor', 'plotly',
             'htmlwidgets', 'nflfastR', 'nflreadr', 'corrplot', 'DescTools')
check_packages = function(packages) {
  for (p in packages) {
    if (!require(p, character.only = TRUE)) {
      install.packages(p)
    }
    library(p, character.only = TRUE)
  }
}

check_packages(packages)

# -------------------------------------------------------------------------
# Clearing Memory ---------------------------------------------------------

rm(list = ls())

# -------------------------------------------------------------------------
# Loading Play-by-Play Data -----------------------------------------------

## disabling nflreadr warnings about clearing the cache
options(nflreadr.verbose=FALSE)

pbp = nflfastR::load_pbp(TRUE)
glimpse(pbp)
colSums(is.na(pbp))

demographics = nflreadr::load_players()
glimpse(demographics)
colSums(is.na(demographics))

stats = nflfastR::load_player_stats(TRUE)
glimpse(stats)
colSums(is.na(stats))

fname = './data/raw/stats.Rdata'
save(stats, file = fname)

# -------------------------------------------------------------------------
# Selecting Variables, and Checking Missingness  --------------------------
pbp_temp = pbp %>%
  group_by(game_id, posteam, drive) %>%
  mutate(current_drive_play_count = row_number()) %>%
  ungroup()
glimpse(pbp_temp)

vars = c('play_id', 'game_id', 'home_team', 'away_team', 'season_type', 'week', 
         'posteam', 'posteam_type', 'defteam', 'side_of_field', 'yardline_100',
         'game_date', 'quarter_seconds_remaining', 'half_seconds_remaining',
         'game_seconds_remaining', 'game_half', 'quarter_end', 'drive', 'sp',
         'qtr', 'down', 'goal_to_go', 'time', 'yrdln', 'ydstogo', 'ydsnet', 
         'desc', 'play_type', 'field_goal_result', 'kick_distance', 
         'extra_point_result', 'home_timeouts_remaining', 'away_timeouts_remaining',
         'timeout', 'timeout_team', 'posteam_timeouts_remaining',
         'defteam_timeouts_remaining', 'score_differential',
         'score_differential_post', 'wp', 'wpa', 'extra_point_attempt',
         'field_goal_attempt', 'kicker_player_name', 'kicker_player_id',
         'penalty_team', 'penalty_player_id', 'penalty_player_name', 
         'penalty_yards', 'penalty_type', 'season', 'series', 'series_result',
         'start_time', 'time_of_day', 'stadium', 'weather', 'play_clock', 
         'special_teams_play', 'end_clock_time', 'end_yard_line',
         'current_drive_play_count', 'drive_play_count', 'drive_time_of_possession',
         'drive_quarter_start', 'drive_quarter_end', 'drive_yards_penalized',
         'drive_start_transition', 'drive_end_transition', 'drive_game_clock_start',
         'drive_game_clock_end', 'drive_start_yard_line', 'drive_end_yard_line',
         'location', 'result', 'div_game', 'roof', 'surface', 'temp', 'wind',
         'home_coach', 'away_coach', 'stadium_id', 'game_stadium', 'success',
         'special', 'home_opening_kickoff')

pbp_temp = pbp_temp %>%
  dplyr::select(all_of(vars))
glimpse(pbp_temp)
pbp_na = colSums(is.na(pbp_temp))
pbp_na

# -------------------------------------------------------------------------
# Visualizing Variables with Missing Values -------------------------------

## ***** Wind *****
table(is.na(pbp_temp$wind),
      pbp_temp$season,
      dnn = c('Wind NA','Season'))
table(is.na(pbp_temp$wind),
      pbp_temp$roof,
      dnn = c('Wind NA', 'Roof Type'))

wind = pbp_temp %>%
  filter(roof == 'outdoors') %>%
  dplyr::select(wind, game_stadium) %>%
  na.omit()
glimpse(wind)
summary(wind$wind)

## histogram of wind in outdoor stadiums
wind_histogram = plot_ly(data = wind,
                         x = ~wind,
                         type = 'histogram',
                         marker = list(color = 'lightgray',
                                       line = list(color = 'black',
                                                   width = 1)),
                         name = 'Frequency',
                         hovertemplate = paste(
                           'Wind (mph): %{x}',
                           '<br>Frequency: %{y}',
                           '<extra></extra>')) %>%
  layout(xaxis = list(title = 'Wind (mph)'),
         yaxis = list(title = 'Frequency'),
         title = 'Wind in Outdoor Stadiums')
wind_histogram
## zero heavy with right skew and several outliers


## boxplot of wind in outdoor stadiums
wind_box = plot_ly(data = wind,
                   x = ~wind,
                   type = 'box',
                   marker = list(color = 'lightgray'),
                   name = '') %>%
  layout(xaxis = list(title = 'Wind (mph)'),
         title = 'Wind in Outdoor Stadiums',
         showlegend = FALSE)
wind_box


## ***** Temperature *****
table(is.na(pbp_temp$temp),
      pbp_temp$season,
      dnn = c('Temp NA', 'Season'))
table(is.na(pbp_temp$temp),
      pbp_temp$roof,
      dnn = c('Temp NA', 'Roof Type'))
temp = pbp_temp %>%
  filter(roof == 'outdoors') %>%
  select(temp, game_stadium) %>%
  na.omit()
glimpse(temp)
summary(temp$temp)


## histogram of temperature in outdoor stadiums
temp_histogram = plot_ly(data = temp,
                         x = ~temp,
                         type = 'histogram',
                         marker = list(color = 'lightgray',
                                       line = list(color = 'black',
                                                   width = 1)),
                         name = 'Frequency',
                         hovertemplate = paste(
                           'Temperature (F): %{x}',
                           '<br>Frequency: %{y}',
                           '<extra></extra>')) %>%
  layout(xaxis = list(title = 'Temperature (F)'),
         yaxis = list(title = 'Frequency'),
         title = 'Temperature in Outdoor Stadiums')
temp_histogram
## normally distributed around 60 degrees Faranheit


## boxplot of temp in outdoor stadiums
temp_box = plot_ly(data = temp,
                   x = ~temp,
                   type = 'box',
                   marker = list(color = 'lightgray'),
                   name = '') %>%
  layout(xaxis = list(title = 'Temp'),
         title = 'Temp in Outdoor Stadiums',
         showlegend = FALSE)
temp_box


## ***** Start Time *****
table(is.na(pbp_temp$start_time),
      pbp_temp$season,
      dnn = c('Start Time NA', 'Season'))


## ***** Time of Day *****
table(is.na(pbp_temp$time_of_day),
      pbp_temp$season,
      dnn = c('Time of Day NA', 'Season'))


## ***** Play Clock *****
table(is.na(pbp_temp$play_clock),
      pbp_temp$season,
      dnn = c('Play Clock NA', 'Season'))
unique(pbp_temp$play_clock)


## ***** Weather *****
table(is.na(pbp_temp$weather),
      pbp_temp$season,
      dnn = c('Weather NA', 'Season'))
table(is.na(pbp_temp$weather),
      pbp_temp$roof,
      dnn = c('Weather NA', 'Roof Type'))
table(is.na(pbp_temp$weather),
      pbp_temp$roof,
      pbp_temp$season,
      dnn = c('Weather NA', 'Roof Type', 'Season'))


## ***** Possession Team *****
table(is.na(pbp_temp$posteam),
      pbp_temp$season,
      dnn = c('Possession Team NA', 'Season'))


## ***** Game Seconds Remaining *****
table(is.na(pbp_temp$game_seconds_remaining),
      pbp_temp$season,
      dnn = c('Game Seconds Remaining NA', 'Season'))


## ***** Goal to Go *****
table(is.na(pbp_temp$goal_to_go),
      pbp_temp$season,
      dnn = c('Goal-to-Go NA', 'Season'))


## ***** Drive Start Yard Line *****
table(is.na(pbp_temp$drive_start_yard_line),
      pbp_temp$season,
      dnn = c('Drive Start Yard Line NA', 'Season'))


## ***** Drive Play Count *****
table(is.na(pbp_temp$drive_play_count),
      pbp_temp$season,
      dnn = c('Drive Play Count NA', 'Season'))


## ***** Drive Game Clock Start *****
table(is.na(pbp_temp$drive_game_clock_start),
      pbp_temp$season,
      dnn = c('Drive Game Clock Start NA', 'Season'))


## ***** Conclusions *****
## 1) start_time and play_clock are missing for the seasons in 1999 and 2000,
##    and there are only two values for play_clock (0 and NA), so I am going to
##    remove play_clock
## 2) time_of_day wasn't recorded until the beginning of the 2011 season, so
##    I am going to remove time_of_day
## 3) I filtered out the seasons in 1999 and 2000 since many variables were not 
##    recorded during these years
## 4) weather is a complicated variable that is mostly missing for the seasons
##    in 1999 and 2000, and for some reason, the 2021-2024 seasons too. The
##    missingness for the 2021-2024 seasons is mostly for stadiums with closed
##    retractable roofs. Therefore, I decided to change weather such that if 
##    there is precipitation, its value is 1 and otherwise 0. I'm assuming there
##    was no precipitation for the 68 missing records for open retractable roof 
##    stadiums in the 2023 season
## 5) There were two games with incorrectly entered start times (start_time).
##    The Cleveland Browns vs. Baltimore Ravens game on 09/27/2009 had an
##    entered time of 24:13, when it should have been 10:13.
##    The Atlanta Falcons vs. Baltimore Ravens game on 10/19/2014 had an
##    entered time of 24:13, when it should have been 10:13.
## 6) quarter, half, and game_seconds_remaining are all missing for the same
##    observations
## 7) goal_to_go is missing for only the 2000 season (yeah)
## 8) Since most data is missing for the 1999 and 2000 seasons, I will be
##    removing these seasons from the dataset
## 9) drive_start_yard_line and drive_end_yard_line are uniform in missingness
## 10) drive_play_count and drive_time_of_possession, drive_quarter_start,
##     drive_quarter_end, drive_yards_penalized, drive_game_clock_start,
##     and drive_game_clock_end are uniform in missingness

# -------------------------------------------------------------------------
# Filtering the Data and Imputing Missing Values --------------------------

median_wind = median(wind$wind)
median_temp = median(temp$temp)
precip = 'rain|snow|rainy|snowy|fog|foggy'

pbp_temp = pbp_temp %>%
  filter(!(season %in% c(1999, 2000))) %>%
  select(!play_clock) %>%
  mutate(wind = case_when(is.na(wind) & (roof == 'closed' | roof == 'dome') ~ 0,
                          is.na(wind) & (roof == 'open' | roof == 'outdoors') ~ 
                            median_wind,
                          .default = wind),
         temp = if_else(is.na(temp), median_temp, temp),
         weather = if_else(grepl(precip, weather, ignore.case = TRUE), 1, 0),
         start_time = str_replace(start_time, '24:', '10:'),
         start_time = case_when(grepl('/', start_time, fixed = TRUE) ~ 
                                  as.POSIXct(start_time,
                                             format = '%m/%d/%y, %H:%M:%S',
                                             tz = 'America/New_York'),
                                TRUE ~ as.POSIXct(start_time,
                                                  format = '%H:%M:%S',
                                                  tz = 'America/New_York')),
         start_time = hms::as_hms(start_time))
glimpse(pbp_temp)
colSums(is.na(pbp_temp))

# -------------------------------------------------------------------------
# Creating Kicker Dataset  ------------------------------------------------

## filtering for Kickers
kickers = stats %>%
  dplyr::select(player_id, position) %>%
  distinct()

pbp_temp = left_join(x = pbp_temp,
                     y = kickers,
                     by = c('kicker_player_id'='player_id')) %>%
  filter(position == 'K')
glimpse(pbp_temp)
colSums(is.na(pbp_temp))

## merging K birth_date
dob = demographics %>%
  dplyr::select(gsis_id, birth_date, display_name) %>%
  filter(!is.na(birth_date)) %>%
  distinct() 
glimpse(dob)
colSums(is.na(dob))


pbp_temp = left_join(x = pbp_temp,
                     y = dob,
                     by = c('kicker_player_id'='gsis_id')) %>%
  mutate(birth_date = ymd(birth_date),
         birth_year = year(birth_date),
         birth_month_day = format(birth_date, '%m-%d'),
         age = season - birth_year,
         season_end_date = ymd(paste0(season, '-02-26')),
         birth_date_season = ymd(paste0(season, '-', birth_month_day)),
         age = ifelse(birth_date_season <= season_end_date, age, age-1)) %>%
  dplyr::select(!c(birth_year, birth_month_day, season_end_date,
                   birth_date_season))
glimpse(pbp_temp)
colSums(is.na(pbp_temp))

# -------------------------------------------------------------------------
# Feature Engineering -----------------------------------------------------

pbp_temp = pbp_temp %>%
  mutate(posteam_type = if_else(posteam_type == 'home', 1, 0),
         game_date = ymd(game_date),
         drive_time_of_possession = paste0('00:', drive_time_of_possession),
         drive_time_of_possession = hms::parse_hms(drive_time_of_possession),
         location = if_else(location == 'Home', 1, 0),
         roof = if_else(roof == 'open' | roof == 'outdoors', 1, 0))
glimpse(pbp_temp)
colSums(is.na(pbp_temp))

# -------------------------------------------------------------------------
# Exploring Binary Variables ----------------------------------------------

factors = sapply(pbp_temp, is.factor)
factors = names(factors[factors != FALSE]) 
factors

table(pbp_temp$season_type, useNA = 'always', dnn = c('Season Type'))


table(pbp_temp$posteam_type, useNA = 'always', dnn = c('Possession Team Type'))


table(pbp_temp$side_of_field, useNA = 'always', dnn = c('Side of Field'))


table(pbp_temp$qtr, useNA = 'always', dnn = 'Quarter')
## has both overtime and double-overtime entries


table(pbp_temp$game_half, useNA = 'always', dnn = 'Game Half')


table(pbp_temp$play_type, useNA = 'always', dnn = 'Play Type')
## there are 38 plays considered 'no_play'


table(pbp_temp$season, useNA = 'always', dnn = 'Season')


table(hour(pbp_temp$start_time), useNA = 'always', dnn = 'Start Time Hour')


table(pbp_temp$weather, useNA = 'always', dnn = 'Weather')


table(pbp_temp$location, useNA = 'always', dnn = 'Location')


table(pbp_temp$div_game, useNA = 'always', dnn = 'Divisional Game')


table(pbp_temp$roof, useNA = 'always', dnn = 'Roof Type')


table(pbp_temp$surface, useNA = 'always', dnn = 'Surface')


table(pbp_temp$stadium_id, useNA = 'always', dnn = 'Stadium ID')


table(pbp_temp$position, useNA = 'always', dnn = 'Position')

## ***** Conclusions *****
## 1) start_time has many issues, like the hour ranging from 7 to 23. The times
##    appear to reflect the local time of the person who recorded the start
##    time. I will mutate the start_time variable, such that games are
##    categorized as having early (<=1:00 PM EST), mid (<=4:15 PM EST), and late
##    (>=8:00 PM EST) start times
## 2) surface needs to be further explored because some categories appear to be
##    duplicated. For instance, there appears to be multiple types of astro-turf
##    recorded.

# -------------------------------------------------------------------------
# Removing and Exploring Problematic Variables ----------------------------

## exploring surface
unique(pbp_temp$surface)
length(unique(pbp_temp$surface))
table(pbp_temp$surface,
      pbp_temp$season,
      dnn = c('Surface', 'Season'),
      useNA = 'always')
DescTools::Mode(pbp_temp$surface)


## exploring start_time
start_times = pbp_temp %>%
  dplyr::select(game_id, game_date, home_team, away_team, start_time) %>%
  mutate(hour = hour(start_time),
         minute = minute(start_time),
         second = second(start_time)) %>%
  filter(!(hour %in% c(13,16,20))) %>%
  distinct()
glimpse(start_times)
filename = './data/start_times/start_times.csv'
write_csv(start_times, file=filename, quote='none')

## incorrect start times
updated_times = read_csv(file = './data/start_times/updated_start_times.csv',
                         col_types = list(col_character(),
                                          col_date(format='%m/%d/%Y'),
                                          col_factor(),
                                          col_factor(),
                                          col_time(format='%H:%M:%S'),
                                          col_integer(),
                                          col_integer(),
                                          col_integer()))
glimpse(updated_times)
wrong_start_times = updated_times %>%
  filter(hour != actual_hour) %>%
  mutate(actual_start_time = hms::parse_hms(paste0(actual_hour,':',
                                                   minute=minute,':',
                                                   second=second)))
glimpse(wrong_start_times)

## ***** Conclusions *****
## 1) surface has 10 "unique" values, including an empty string. I will
##    assume the empty string denotes missing information. Therefore, I will
##    impute with the mode, which is "grass." I will combine all entries
##    referencing grass into one category called "grass," and all entries
##    referencing a type of turf into one category called "turf."
## 2) I identified the games with incorrect start times, and will correct these
##    observations by combining the hour of the correct start time with the
##    minute and seconds from the incorrect start time

# -------------------------------------------------------------------------
# Mutating Problematic Variables ------------------------------------------

surface_type = 'a_turf|astroplay|astroturf|dessograss|fieldturf|matrixturf|sportturf'
pbp_clean = pbp_temp %>%
  mutate(surface = if_else(grepl(surface_type, surface), 'turf', 'grass'),
         surface = if_else(surface == 'turf', 1, 0),
         start_time = if_else(game_id %in% wrong_start_times$game_id,
                              wrong_start_times$actual_start_time[
                                match(game_id, wrong_start_times$game_id)
                              ],
                              start_time))
glimpse(pbp_clean)
colSums(is.na(pbp_clean))
colSums(is.na(pbp_clean)) == 0

## checking surface
table(pbp_clean$surface, dnn = c('Surface'), useNA = 'always')


## checking start_time
table(hour(pbp_clean$start_time), dnn = c('Start Time'), useNA = 'always')

# -------------------------------------------------------------------------
# Identifying Variables with One Value ------------------------------------

pbp_temp %>% 
  map_int(n_distinct) %>%
  keep(~.x == 1)
## position and quarter_end only have one value and will be removed

pbp_temp = pbp_temp %>%
  dplyr::select(!all_of(c('position', 'quarter_end')))
glimpse(pbp_temp)

# -------------------------------------------------------------------------
# Saving Raw Data ---------------------------------------------------------

## one last check before saving the cleaned data
glimpse(pbp_clean)

fname = './data/master/pbp_clean.Rdata'
save(pbp_clean, file = fname)

# -------------------------------------------------------------------------
