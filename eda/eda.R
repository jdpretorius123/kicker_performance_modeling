# Comments ----------------------------------------------------------------

## 1. The script has been updated to specify the interception variable and
##    runs as expected.

# -------------------------------------------------------------------------
# Installing/Loading Dependencies -----------------------------------------

## updating lock file
renv::snapshot()

packages = c('tidyverse', 'lubridate', 'glue', 'janitor', 'plotly',
             'htmlwidgets', 'corrplot', 'DescTools')
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
# Loading Master Data -----------------------------------------------------

fname = './data/master/pbp_clean.Rdata'
load(fname)

# -------------------------------------------------------------------------
# Creating a Temporary Dataset --------------------------------------------

temp = pbp_clean %>%
  filter(play_type != 'no_play', season_type == 'REG') %>%
  mutate(drive_top = hour(drive_time_of_possession) + 
           (minute(drive_time_of_possession)/60) + 
           (second(drive_time_of_possession)/60),
         start_time = hour(start_time) + 
           (minute(start_time)/60) + 
           (second(start_time)/60))
table(temp$play_type, dnn = 'Play Type', useNA = 'always')
table(temp$season_type, dnn = 'Season Type', useNA = 'always')

colSums(is.na(temp))

vars = c('play_id', 'game_id', 'home_team', 'away_team', 'week', 'posteam',
         'posteam_type', 'defteam', 'yardline_100', 'game_date',
         'game_seconds_remaining', 'qtr', 'down', 'goal_to_go', 'ydstogo',
         'play_type', 'field_goal_result', 'kick_distance', 'extra_point_result',
         'posteam_timeouts_remaining', 'defteam_timeouts_remaining', 
         'score_differential', 'wp', 'wpa', 'extra_point_attempt',
         'field_goal_attempt', 'kicker_player_name', 'kicker_player_id',
         'season', 'start_time', 'weather', 'special_teams_play',
         'current_drive_play_count', 'drive_top', 'location', 'div_game',
         'roof', 'surface', 'temp', 'wind', 'age', 'display_name')
temp = temp %>%
  dplyr::select(all_of(vars))
colnames(temp)
colSums(is.na(temp))

# -------------------------------------------------------------------------
# Assessing Interactions --------------------------------------------------

## roof: 1 = open, 0 = closed
roof = temp %>%
  filter(field_goal_attempt == 1) %>%
  dplyr::select(roof, field_goal_result) %>%
  mutate(roof = if_else(roof == 1, 'open', 'closed')) %>%
  group_by(roof, field_goal_result) %>%
  summarise(count = n(), .groups = 'drop')
roof_plot = plot_ly(data = roof,
                    x = ~roof,
                    y = ~count,
                    color = ~field_goal_result,
                    customdata = ~field_goal_result,
                    type = 'bar',
                    marker = list(line = list(color = 'black',
                                              width = 1)),
                    name = ~field_goal_result,
                    hovertemplate = paste(
                      'Roof Type: %{x}',
                      '<br>Count: %{y}<br>',
                      '<br>Field Goal Result: %{customdata}',
                      '<extra></extra>')) %>%
  layout(xaxis = list(title = 'Roof Type'),
         yaxis = list(title = 'Count'),
         title = 'Field Goal Result by Roof Type',
         legend = list(title = list(text = 'Field Goal Result')))
roof_plot


## surface: 1 == turf, 0 == grass
table(pbp_clean$interception, pbp_clean$surface,
      dnn = list('Interception', 'Surface'), useNA = 'always')
int_by_surface = pbp_clean %>%
  mutate(surface = if_else(surface == 0, 'grass', 'turf'),
         interception = if_else(interception == 1, 'yes', 'no')) %>%
  group_by(surface, interception) %>%
  summarise(count = n(), .groups = 'drop')
int_by_surface_plot = plot_ly(data = int_by_surface,
                              x = ~surface,
                              y = ~count,
                              type = 'bar',
                              marker = list(line = list(color = 'black',
                                                        width = 1)),
                              name = ~interception,
                              color = ~interception,
                              hovertemplate = paste(
                                'Surface: %{x}',
                                '<br>Interceptions: %{y}',
                                '<extra></extra>')) %>%
  layout(xaxis = list(title = 'Surface'),
         yaxis = list(title = 'Interceptions'),
         title = 'Interceptions by Surface',
         legend = list(title = list(text = 'Interception')))
int_by_surface_plot

## checking interception by wind
int_by_wind = pbp_clean %>%
  mutate(interception = if_else(interception == 1, 'yes', 'no'))
int_by_wind_plot = plot_ly(data = int_by_wind,
                           x = ~interception,
                           y = ~wind,
                           type = 'box',
                           marker = list(line = list(color = 'black',
                                                     width = 1)),
                           name = ~interception) %>%
  layout(xaxis = list(title = 'Interception'),
         yaxis = list(title = 'Wind (mph)'),
         title = 'Interception by Wind (mph)',
         legend = list(title = list(text = 'Interception')))
int_by_wind_plot

## checking interception by temp
int_by_temp = pbp_clean %>%
  dplyr::select(interception, temp) %>%
  mutate(interception = if_else(interception == 1, 'yes', 'no'))
int_by_temp_plot = plot_ly(data = int_by_temp,
                           x = ~interception,
                           y = ~temp,
                           type = 'box',
                           marker = list(line = list(color = 'black',
                                                     width = 1)),
                           name = ~interception) %>%
  layout(xaxis = list(title = 'Interception'),
         yaxis = list(title = 'Temp'),
         title = 'Interception by Temp',
         legend = list(title = list(text = 'Interception')))
int_by_temp_plot

## checking interception by weather
table(pbp_clean$interception, pbp_clean$weather,
      dnn = list('Interception', 'Weather'), useNA = 'always')
int_by_weather = pbp_clean %>%
  mutate(weather = if_else(weather == 1, 'precipitation', 'clear'),
         interception = if_else(interception == 1, 'yes', 'no')) %>%
  group_by(weather, interception) %>%
  summarise(count = n(), .groups = 'drop')
int_by_weather_plot = plot_ly(data = int_by_weather,
                              x = ~weather,
                              y = ~count,
                              type = 'bar',
                              marker = list(line = list(color = 'black',
                                                        width = 1)),
                              name = ~interception,
                              color = ~interception,
                              hovertemplate = paste(
                                'Weather: %{x}',
                                '<br>Interceptions: %{y}',
                                '<extra></extra>')) %>%
  layout(xaxis = list(title = 'Weather'),
         yaxis = list(title = 'Interceptions'),
         title = 'Interceptions by Weather',
         legend = list(title = list(text = 'Interception')))
int_by_weather_plot

## checking interception by posteam_timeouts_remaining
table(pbp_clean$interception, pbp_clean$posteam_timeouts_remaining,
      dnn = list('Interception', 'Possession Team Timeouts'),
      useNA = 'always')
int_by_posteam_timeouts = pbp_clean %>%
  mutate(posteam_timeouts = case_when(posteam_timeouts_remaining == 0 ~ 'zero',
                                      posteam_timeouts_remaining == 1 ~ 'one',
                                      posteam_timeouts_remaining == 2 ~ 'two',
                                      posteam_timeouts_remaining == 3 ~ 'three'),
         interception = if_else(interception == 1, 'yes', 'no')) %>%
  group_by(posteam_timeouts, interception) %>%
  summarise(count = n(), .groups = 'drop') 

int_by_posteam_to_plot = plot_ly(data = int_by_posteam_timeouts,
                                 x = ~posteam_timeouts,
                                 y = ~count,
                                 type = 'bar',
                                 marker = list(line = list(color = 'black',
                                                           width = 1)),
                                 name = ~interception,
                                 color = ~interception,
                                 hovertemplate = paste(
                                   'Posteam Timeouts: %{x}',
                                   '<br>Interceptions: %{y}',
                                   '<extra></extra>')) %>%
  layout(xaxis = list(title = 'Posteam Timouts'),
         yaxis = list(title = 'Interceptions'),
         title = 'Posteam Timeouts by Weather',
         legend = list(title = list(text = 'Interception')))
int_by_posteam_to_plot

## checking interception by defteam_timeouts_remaining
table(pbp_clean$interception, pbp_clean$defteam_timeouts_remaining,
      dnn = list('Interception', 'Defensive Team Timeouts'),
      useNA = 'always')
int_by_defteam_timeouts = pbp_clean %>%
  mutate(defteam_timeouts = case_when(defteam_timeouts_remaining == 0 ~ 'zero',
                                      defteam_timeouts_remaining == 1 ~ 'one',
                                      defteam_timeouts_remaining == 2 ~ 'two',
                                      defteam_timeouts_remaining == 3 ~ 'three'),
         interception = if_else(interception == 1, 'yes', 'no')) %>%
  group_by(defteam_timeouts, interception) %>%
  summarise(count = n(), .groups = 'drop') 

int_by_defteam_to_plot = plot_ly(data = int_by_defteam_timeouts,
                                 x = ~defteam_timeouts,
                                 y = ~count,
                                 type = 'bar',
                                 marker = list(line = list(color = 'black',
                                                           width = 1)),
                                 name = ~interception,
                                 color = ~interception,
                                 hovertemplate = paste(
                                   'Defteam Timeouts: %{x}',
                                   '<br>Interceptions: %{y}',
                                   '<extra></extra>')) %>%
  layout(xaxis = list(title = 'Defteam Timouts'),
         yaxis = list(title = 'Interceptions'),
         title = 'Defteam Timeouts by Weather',
         legend = list(title = list(text = 'Interception')))
int_by_defteam_to_plot

## checking interception by score differential
int_by_score_diff = pbp_clean %>%
  dplyr::select(interception, score_differential) %>%
  mutate(interception = if_else(interception == 1, 'yes', 'no'))
int_by_score_diff_plot = plot_ly(data = int_by_score_diff,
                                 x = ~interception,
                                 y = ~score_differential,
                                 type = 'box',
                                 marker = list(line = list(color = 'black',
                                                           width = 1)),
                                 name = ~interception) %>%
  layout(xaxis = list(title = 'Interception'),
         yaxis = list(title = 'Score Differential'),
         title = 'Interception by Score Differential',
         legend = list(title = list(text = 'Interception')))
int_by_score_diff_plot

## checking interception by current_drive_play_count
int_by_dpc = pbp_clean %>%
  dplyr::select(interception, current_drive_play_count) %>%
  mutate(interception = if_else(interception == 1, 'yes', 'no'))
int_by_dpc_plot = plot_ly(data = int_by_dpc,
                          x = ~interception,
                          y = ~current_drive_play_count,
                          type = 'box',
                          marker = list(line = list(color = 'black',
                                                    width = 1)),
                          name = ~interception) %>%
  layout(xaxis = list(title = 'Interception'),
         yaxis = list(title = 'Current Drive Play Count'),
         title = 'Interception by Drive Play Count',
         legend = list(title = list(text = 'Interception')))
int_by_dpc_plot

## checking interception by location
table(pbp_clean$interception, pbp_clean$location,
      dnn = list('Interception', 'Location'), useNA = 'always')
int_by_location = pbp_clean %>%
  mutate(location = if_else(location == 1, 'home', 'away'),
         interception = if_else(interception == 1, 'yes', 'no')) %>%
  group_by(location, interception) %>%
  summarise(count = n(), .groups = 'drop')
int_by_location_plot = plot_ly(data = int_by_location,
                               x = ~location,
                               y = ~count,
                               type = 'bar',
                               marker = list(line = list(color = 'black',
                                                         width = 1)),
                               name = ~interception,
                               color = ~interception,
                               hovertemplate = paste(
                                 'Location: %{x}',
                                 '<br>Interceptions: %{y}',
                                 '<extra></extra>')) %>%
  layout(xaxis = list(title = 'Location'),
         yaxis = list(title = 'Interceptions'),
         title = 'Interceptions by Location',
         legend = list(title = list(text = 'Interception')))
int_by_location_plot

# -------------------------------------------------------------------------
# Saving Cleaned Play-By-Play Data ----------------------------------------

int_pbp_clean = pbp_clean
fname = './data/cleaned/interception/int_pbp_clean.Rdata'
save(int_pbp_clean, file = fname)

# -------------------------------------------------------------------------
