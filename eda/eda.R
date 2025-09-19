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
         'posteam_type', 'defteam', 'game_date', 'game_seconds_remaining',
         'play_type', 'field_goal_result', 'kick_distance', 'extra_point_result',
         'posteam_timeouts_remaining', 'defteam_timeouts_remaining', 
         'score_differential', 'extra_point_attempt', 'field_goal_attempt',
         'kicker_player_name', 'kicker_player_id', 'season', 'start_time',
         'weather', 'special_teams_play', 'current_drive_play_count', 
         'drive_top', 'div_game', 'roof', 'surface', 'temp', 'wind',
         'age')
temp = temp %>%
  dplyr::select(all_of(vars))
colnames(temp)
colSums(is.na(temp))

# -------------------------------------------------------------------------
# Assessing Interactions with Field Goal Result ---------------------------

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
surface = temp %>%
  filter(field_goal_attempt == 1) %>%
  dplyr::select(surface, field_goal_result) %>%
  mutate(surface = if_else(surface == 1, 'turf', 'grass')) %>%
  group_by(surface, field_goal_result) %>%
  summarise(count = n(), .groups = 'drop')
surface_plot = plot_ly(data = surface,
                       x = ~surface,
                       y = ~count,
                       color = ~field_goal_result,
                       customdata = ~field_goal_result,
                       type = 'bar',
                       marker = list(line = list(color = 'black',
                                                 width = 1)),
                       name = ~field_goal_result,
                       hovertemplate = paste(
                         'Surface: %{x}',
                         '<br>Count: %{y}<br>',
                         '<br>Field Goal Result: %{customdata}',
                         '<extra></extra>')) %>%
  layout(xaxis = list(title = 'Surface'),
         yaxis = list(title = 'Count'),
         title = 'Field Goal Result by Surface',
         legend = list(title = list(text = 'Field Goal Result')))
surface_plot


## wind
wind = temp %>%
  filter(field_goal_attempt == 1) %>%
  dplyr::select(wind, field_goal_result)
wind_plot = plot_ly(data = wind,
                    x = ~field_goal_result,
                    y = ~wind,
                    color = ~field_goal_result,
                    type = 'box',
                    marker = list(line = list(color = 'black',
                                              width = 1)),
                    name = ~field_goal_result,
                    hovertemplate = paste(
                      'Field Goal Result: %{x}',
                      '<br>Wind (mph): %{y}<br>',
                      '<extra></extra>'
                    )) %>%
  layout(xaxis = list(title = 'Field Goal Result'),
         yaxis = list(title = 'Wind (mph)'),
         title = 'Field Goal Result by Wind (mph)',
         legend = list(title = list(text = 'Field Goal Result')))
wind_plot


## temperature
temperature = temp %>%
  filter(field_goal_attempt == 1) %>%
  dplyr::select(temp, field_goal_result)
temp_plot = plot_ly(data = temperature,
                    x = ~field_goal_result,
                    y = ~temp,
                    color = ~field_goal_result,
                    type = 'box',
                    marker = list(line = list(color = 'black',
                                              width = 1)),
                    name = ~field_goal_result,
                    hovertemplate = paste(
                      'Field Goal Result: %{x}',
                      '<br>Temperature: %{y}<br>',
                      '<extra></extra>'
                    )) %>%
  layout(xaxis = list(title = 'Field Goal Result'),
         yaxis = list(title = 'Temperature'),
         title = 'Field Goal Result by Temperature',
         legend = list(title = list(text = 'Field Goal Result')))
temp_plot


## weather: 1 == precipitation, 0 == clear skies
weather = temp %>%
  filter(field_goal_attempt == 1) %>%
  dplyr::select(weather, field_goal_result) %>%
  mutate(weather = if_else(weather == 1, 'precipitation', 'clear')) %>%
  group_by(weather, field_goal_result) %>%
  summarise(count = n(), .groups = 'drop')
weather_plot = plot_ly(data = weather,
                       x = ~weather,
                       y = ~count,
                       color = ~field_goal_result,
                       customdata = ~field_goal_result,
                       type = 'bar',
                       marker = list(line = list(color = 'black',
                                                 width = 1)),
                       name = ~field_goal_result,
                       hovertemplate = paste(
                         'Weather: %{x}',
                         '<br>Count: %{y}<br>',
                         '<br>Field Goal Result: %{customdata}',
                         '<extra></extra>')) %>%
  layout(xaxis = list(title = 'Weather'),
         yaxis = list(title = 'Count'),
         title = 'Field Goal Result by Weather',
         legend = list(title = list(text = 'Field Goal Result')))
weather_plot


## posteam_timeouts_remaining
posteam_timeouts = temp %>%
  filter(field_goal_attempt == 1) %>%
  dplyr::select(posteam_timeouts_remaining, field_goal_result) %>%
  mutate(posteam_timeouts = case_when(
    posteam_timeouts_remaining == 0 ~ 'zero',
    posteam_timeouts_remaining == 1 ~ 'one',
    posteam_timeouts_remaining == 2 ~ 'two',
    posteam_timeouts_remaining == 3 ~ 'three'
  )) %>%
  group_by(posteam_timeouts, field_goal_result) %>%
  summarise(count = n(), .groups = 'drop')
posteam_timeouts_plot = plot_ly(data = posteam_timeouts,
                                x = ~posteam_timeouts,
                                y = ~count,
                                color = ~field_goal_result,
                                customdata = ~field_goal_result,
                                type = 'bar',
                                marker = list(line = list(color = 'black',
                                                          width = 1)),
                                name = ~field_goal_result,
                                hovertemplate = paste(
                                  'Possession Team TO: %{x}',
                                  '<br>Count: %{y}<br>',
                                  '<br>Field Goal Result: %{customdata}',
                                  '<extra></extra>')) %>%
  layout(xaxis = list(title = 'Possession Team Timeouts'),
         yaxis = list(title = 'Count'),
         title = 'Field Goal Result by Possession Team Timeouts',
         legend = list(title = list(text = 'Field Goal Result')))
posteam_timeouts_plot


## defteam_timeouts_remaining
defteam_timeouts = temp %>%
  filter(field_goal_attempt == 1) %>%
  dplyr::select(defteam_timeouts_remaining, field_goal_result) %>%
  mutate(defteam_timeouts = case_when(
    defteam_timeouts_remaining == 0 ~ 'zero',
    defteam_timeouts_remaining == 1 ~ 'one',
    defteam_timeouts_remaining == 2 ~ 'two',
    defteam_timeouts_remaining == 3 ~ 'three'
  )) %>%
  group_by(defteam_timeouts, field_goal_result) %>%
  summarise(count = n(), .groups = 'drop')
defteam_timeouts_plot = plot_ly(data = defteam_timeouts,
                                x = ~defteam_timeouts,
                                y = ~count,
                                color = ~field_goal_result,
                                customdata = ~field_goal_result,
                                type = 'bar',
                                marker = list(line = list(color = 'black',
                                                          width = 1)),
                                name = ~field_goal_result,
                                hovertemplate = paste(
                                  'Defensive Team TO: %{x}',
                                  '<br>Count: %{y}<br>',
                                  '<br>Field Goal Result: %{customdata}',
                                  '<extra></extra>')) %>%
  layout(xaxis = list(title = 'Defensive Team Timeouts'),
         yaxis = list(title = 'Count'),
         title = 'Field Goal Result by Defensiv Team Timeouts',
         legend = list(title = list(text = 'Field Goal Result')))
defteam_timeouts_plot


## score differential
score_differential = temp %>%
  filter(field_goal_attempt == 1) %>%
  dplyr::select(score_differential, field_goal_result)
score_differential_plot = plot_ly(data = score_differential,
                                  x = ~field_goal_result,
                                  y = ~score_differential,
                                  color = ~field_goal_result,
                                  type = 'box',
                                  marker = list(line = list(color = 'black',
                                                            width = 1)),
                                  name = ~field_goal_result,
                                  hovertemplate = paste(
                                    'Field Goal Result: %{x}',
                                    '<br>Score Differential: %{y}<br>',
                                    '<extra></extra>'
                                  )) %>%
  layout(xaxis = list(title = 'Field Goal Result'),
         yaxis = list(title = 'Score Differential'),
         title = 'Field Goal Result by Score Differential',
         legend = list(title = list(text = 'Field Goal Result')))
score_differential_plot


## current_drive_play_count
current_drive_play_count = temp %>%
  filter(field_goal_attempt == 1) %>%
  dplyr::select(current_drive_play_count, field_goal_result)
current_drive_play_count_plot = plot_ly(data = current_drive_play_count,
                                        x = ~field_goal_result,
                                        y = ~current_drive_play_count,
                                        color = ~field_goal_result,
                                        type = 'box',
                                        marker = list(line = list(color = 'black',
                                                                  width = 1)),
                                        name = ~field_goal_result,
                                        hovertemplate = paste(
                                          'Field Goal Result: %{x}',
                                          '<br>Current Drive Play Count: %{y}<br>',
                                          '<extra></extra>'
                                        )) %>%
  layout(xaxis = list(title = 'Field Goal Result'),
         yaxis = list(title = 'Current Drive Play Count'),
         title = 'Field Goal Result by Current Drive Play Count',
         legend = list(title = list(text = 'Field Goal Result')))
current_drive_play_count_plot


## age
age = temp %>%
  filter(field_goal_attempt == 1) %>%
  dplyr::select(age, field_goal_result)
age_plot = plot_ly(data = age,
                   x = ~field_goal_result,
                   y = ~age,
                   color = ~field_goal_result,
                   type = 'box',
                   marker = list(line = list(color = 'black',
                                             width = 1)),
                   name = ~field_goal_result,
                   hovertemplate = paste(
                     'Field Goal Result: %{x}',
                     '<br>Age: %{y}<br>',
                     '<extra></extra>'
                   )) %>%
  layout(xaxis = list(title = 'Field Goal Result'),
         yaxis = list(title = 'Age'),
         title = 'Field Goal Result by Age',
         legend = list(title = list(text = 'Field Goal Result')))
age_plot


## drive_top
drive_top = temp %>%
  filter(field_goal_attempt == 1) %>%
  dplyr::select(drive_top, field_goal_result)
drive_top_plot = plot_ly(data = drive_top,
                         x = ~field_goal_result,
                         y = ~drive_top,
                         color = ~field_goal_result,
                         type = 'box',
                         marker = list(line = list(color = 'black',
                                                   width = 1)),
                         name = ~field_goal_result,
                         hovertemplate = paste(
                           'Field Goal Result: %{x}',
                           '<br>Drive Time of Possession: %{y}<br>',
                           '<extra></extra>'
                         )) %>%
  layout(xaxis = list(title = 'Field Goal Result'),
         yaxis = list(title = 'Drive Time of Possession'),
         title = 'Field Goal Result by Drive Time of Possession',
         legend = list(title = list(text = 'Field Goal Result')))
drive_top_plot


## kick_distance
kick_distance = temp %>%
  filter(field_goal_attempt == 1) %>%
  dplyr::select(kick_distance, field_goal_result)
kick_distance_plot = plot_ly(data = kick_distance,
                             x = ~field_goal_result,
                             y = ~kick_distance,
                             color = ~field_goal_result,
                             type = 'box',
                             marker = list(line = list(color = 'black',
                                                       width = 1)),
                             name = ~field_goal_result,
                             hovertemplate = paste(
                               'Field Goal Result: %{x}',
                               '<br>Kick Distance: %{y}<br>',
                               '<extra></extra>'
                             )) %>%
  layout(xaxis = list(title = 'Field Goal Result'),
         yaxis = list(title = 'Kick Distance'),
         title = 'Field Goal Result by Kick Distance',
         legend = list(title = list(text = 'Field Goal Result')))
kick_distance_plot
## there is a visible correlation between kick_distance and field_goal_result


## game_seconds_remaining
game_seconds_remaining = temp %>%
  filter(field_goal_attempt == 1) %>%
  dplyr::select(game_seconds_remaining, field_goal_result)
game_seconds_remaining_plot = plot_ly(data = game_seconds_remaining,
                                      x = ~field_goal_result,
                                      y = ~game_seconds_remaining,
                                      color = ~field_goal_result,
                                      type = 'box',
                                      marker = list(line = list(color = 'black',
                                                                width = 1)),
                                      name = ~field_goal_result,
                                      hovertemplate = paste(
                                        'Field Goal Result: %{x}',
                                        '<br>Game Seconds Remaining: %{y}<br>',
                                        '<extra></extra>'
                                      )) %>%
  layout(xaxis = list(title = 'Field Goal Result'),
         yaxis = list(title = 'Game Seconds Remaining'),
         title = 'Field Goal Result by Game Seconds Remaining',
         legend = list(title = list(text = 'Field Goal Result')))
game_seconds_remaining_plot


## correlation plot
field_goal = temp %>%
  filter(field_goal_attempt == 1) %>%
  dplyr::select(all_of(c('age',
                         'wind',
                         'temp',
                         'surface',
                         'roof',
                         'div_game',
                         'drive_top',
                         'current_drive_play_count',
                         'weather',
                         'start_time',
                         'score_differential',
                         'defteam_timeouts_remaining',
                         'posteam_timeouts_remaining',
                         'game_seconds_remaining',
                         'kick_distance',
                         'field_goal_result'))) %>%
  mutate(field_goal_result = case_when(field_goal_result == 'made' ~ 0,
                                       field_goal_result == 'missed' ~ 1,
                                       field_goal_result == 'blocked' ~ 2))
corr_matrix = cor(field_goal)
corrplot(corr_matrix, method='color', type='upper', tl.cex=0.5,
         tl.srt=45, cl.pos='b', mar=c(0, 0, 4, 0))

# -------------------------------------------------------------------------
# Assessing Interactions with Extra Point Result --------------------------

## roof: 1 = open, 0 = closed
roof = temp %>%
  filter(extra_point_attempt == 1) %>%
  dplyr::select(roof, extra_point_result) %>%
  mutate(roof = if_else(roof == 1, 'open', 'closed')) %>%
  group_by(roof, extra_point_result) %>%
  summarise(count = n(), .groups = 'drop')
roof_plot = plot_ly(data = roof,
                    x = ~roof,
                    y = ~count,
                    color = ~extra_point_result,
                    customdata = ~extra_point_result,
                    type = 'bar',
                    marker = list(line = list(color = 'black',
                                              width = 1)),
                    name = ~extra_point_result,
                    hovertemplate = paste(
                      'Roof Type: %{x}',
                      '<br>Count: %{y}<br>',
                      '<br>Extra Point Result: %{customdata}',
                      '<extra></extra>')) %>%
  layout(xaxis = list(title = 'Roof Type'),
         yaxis = list(title = 'Count'),
         title = 'Extra Point Result by Roof Type',
         legend = list(title = list(text = 'Extra Point Result')))
roof_plot


## surface: 1 == turf, 0 == grass
surface = temp %>%
  filter(extra_point_attempt == 1) %>%
  dplyr::select(surface, extra_point_result) %>%
  mutate(surface = if_else(surface == 1, 'turf', 'grass')) %>%
  group_by(surface, extra_point_result) %>%
  summarise(count = n(), .groups = 'drop')
surface_plot = plot_ly(data = surface,
                       x = ~surface,
                       y = ~count,
                       color = ~extra_point_result,
                       customdata = ~extra_point_result,
                       type = 'bar',
                       marker = list(line = list(color = 'black',
                                                 width = 1)),
                       name = ~extra_point_result,
                       hovertemplate = paste(
                         'Surface: %{x}',
                         '<br>Count: %{y}<br>',
                         '<br>Extra Point Result: %{customdata}',
                         '<extra></extra>')) %>%
  layout(xaxis = list(title = 'Surface'),
         yaxis = list(title = 'Count'),
         title = 'Extra Point Result by Surface',
         legend = list(title = list(text = 'Extra Point Result')))
surface_plot


## wind
wind = temp %>%
  filter(extra_point_attempt == 1) %>%
  dplyr::select(wind, extra_point_result)
wind_plot = plot_ly(data = wind,
                    x = ~extra_point_result,
                    y = ~wind,
                    color = ~extra_point_result,
                    type = 'box',
                    marker = list(line = list(color = 'black',
                                              width = 1)),
                    name = ~extra_point_result,
                    hovertemplate = paste(
                      'Extra Point Result: %{x}',
                      '<br>Wind (mph): %{y}<br>',
                      '<extra></extra>'
                    )) %>%
  layout(xaxis = list(title = 'Extra Point Result'),
         yaxis = list(title = 'Wind (mph)'),
         title = 'Extra Point Result by Wind (mph)',
         legend = list(title = list(text = 'Extra Point Result')))
wind_plot


## temperature
temperature = temp %>%
  filter(extra_point_attempt == 1) %>%
  dplyr::select(temp, extra_point_result)
temp_plot = plot_ly(data = temperature,
                    x = ~extra_point_result,
                    y = ~temp,
                    color = ~extra_point_result,
                    type = 'box',
                    marker = list(line = list(color = 'black',
                                              width = 1)),
                    name = ~extra_point_result,
                    hovertemplate = paste(
                      'Extra Point Result: %{x}',
                      '<br>Temperature: %{y}<br>',
                      '<extra></extra>'
                    )) %>%
  layout(xaxis = list(title = 'Extra Point Result'),
         yaxis = list(title = 'Temperature'),
         title = 'Extra Point Result by Temperature',
         legend = list(title = list(text = 'Extra Point Result')))
temp_plot


## weather: 1 == precipitation, 0 == clear skies
weather = temp %>%
  filter(extra_point_attempt == 1) %>%
  dplyr::select(weather, extra_point_result) %>%
  mutate(weather = if_else(weather == 1, 'precipitation', 'clear')) %>%
  group_by(weather, extra_point_result) %>%
  summarise(count = n(), .groups = 'drop')
weather_plot = plot_ly(data = weather,
                       x = ~weather,
                       y = ~count,
                       color = ~extra_point_result,
                       customdata = ~extra_point_result,
                       type = 'bar',
                       marker = list(line = list(color = 'black',
                                                 width = 1)),
                       name = ~extra_point_result,
                       hovertemplate = paste(
                         'Weather: %{x}',
                         '<br>Count: %{y}<br>',
                         '<br>Extra Point Result: %{customdata}',
                         '<extra></extra>')) %>%
  layout(xaxis = list(title = 'Weather'),
         yaxis = list(title = 'Count'),
         title = 'Extra Point Result by Weather',
         legend = list(title = list(text = 'Extra Point Result')))
weather_plot


## score differential
score_differential = temp %>%
  filter(extra_point_attempt == 1) %>%
  dplyr::select(score_differential, extra_point_result)
score_differential_plot = plot_ly(data = score_differential,
                                  x = ~extra_point_result,
                                  y = ~score_differential,
                                  color = ~extra_point_result,
                                  type = 'box',
                                  marker = list(line = list(color = 'black',
                                                            width = 1)),
                                  name = ~extra_point_result,
                                  hovertemplate = paste(
                                    'Extra Point Result: %{x}',
                                    '<br>Score Differential: %{y}<br>',
                                    '<extra></extra>'
                                  )) %>%
  layout(xaxis = list(title = 'Extra Point Result'),
         yaxis = list(title = 'Score Differential'),
         title = 'Extra Point Result by Score Differential',
         legend = list(title = list(text = 'Extra Point Result')))
score_differential_plot


## current_drive_play_count
current_drive_play_count = temp %>%
  filter(extra_point_attempt == 1) %>%
  dplyr::select(current_drive_play_count, extra_point_result)
current_drive_play_count_plot = plot_ly(data = current_drive_play_count,
                                        x = ~extra_point_result,
                                        y = ~current_drive_play_count,
                                        color = ~extra_point_result,
                                        type = 'box',
                                        marker = list(line = list(color = 'black',
                                                                  width = 1)),
                                        name = ~extra_point_result,
                                        hovertemplate = paste(
                                          'Extra Point Result: %{x}',
                                          '<br>Current Drive Play Count: %{y}<br>',
                                          '<extra></extra>'
                                        )) %>%
  layout(xaxis = list(title = 'Extra Point Result'),
         yaxis = list(title = 'Current Drive Play Count'),
         title = 'Extra Point Result by Current Drive Play Count',
         legend = list(title = list(text = 'Extra Point Result')))
current_drive_play_count_plot


## age
age = temp %>%
  filter(extra_point_attempt == 1) %>%
  dplyr::select(age, extra_point_result)
age_plot = plot_ly(data = age,
                   x = ~extra_point_result,
                   y = ~age,
                   color = ~extra_point_result,
                   type = 'box',
                   marker = list(line = list(color = 'black',
                                             width = 1)),
                   name = ~extra_point_result,
                   hovertemplate = paste(
                     'Extra Point Result: %{x}',
                     '<br>Age: %{y}<br>',
                     '<extra></extra>'
                   )) %>%
  layout(xaxis = list(title = 'Extra Point Result'),
         yaxis = list(title = 'Age'),
         title = 'Extra Point Result by Age',
         legend = list(title = list(text = 'Extra Point Result')))
age_plot


## drive_top
drive_top = temp %>%
  filter(extra_point_attempt == 1) %>%
  dplyr::select(drive_top, extra_point_result)
drive_top_plot = plot_ly(data = drive_top,
                         x = ~extra_point_result,
                         y = ~drive_top,
                         color = ~extra_point_result,
                         type = 'box',
                         marker = list(line = list(color = 'black',
                                                   width = 1)),
                         name = ~extra_point_result,
                         hovertemplate = paste(
                           'Extra Point Result: %{x}',
                           '<br>Drive Time of Possession: %{y}<br>',
                           '<extra></extra>'
                         )) %>%
  layout(xaxis = list(title = 'Extra Point Result'),
         yaxis = list(title = 'Drive Time of Possession'),
         title = 'Extra Point Result by Drive Time of Possession',
         legend = list(title = list(text = 'Extra Point Result')))
drive_top_plot


## game_seconds_remaining
game_seconds_remaining = temp %>%
  filter(extra_point_attempt == 1) %>%
  dplyr::select(game_seconds_remaining, extra_point_result)
game_seconds_remaining_plot = plot_ly(data = game_seconds_remaining,
                                      x = ~extra_point_result,
                                      y = ~game_seconds_remaining,
                                      color = ~extra_point_result,
                                      type = 'box',
                                      marker = list(line = list(color = 'black',
                                                                width = 1)),
                                      name = ~extra_point_result,
                                      hovertemplate = paste(
                                        'Extra Point Result: %{x}',
                                        '<br>Game Seconds Remaining: %{y}<br>',
                                        '<extra></extra>'
                                      )) %>%
  layout(xaxis = list(title = 'Extra Point Result'),
         yaxis = list(title = 'Game Seconds Remaining'),
         title = 'Extra Point Result by Game Seconds Remaining',
         legend = list(title = list(text = 'Extra Point Result')))
game_seconds_remaining_plot


## correlation plot
extra_point = temp %>%
  filter(extra_point_attempt == 1) %>%
  dplyr::select(all_of(c('age',
                         'wind',
                         'temp',
                         'surface',
                         'roof',
                         'div_game',
                         'drive_top',
                         'current_drive_play_count',
                         'weather',
                         'start_time',
                         'score_differential',
                         'game_seconds_remaining',
                         'extra_point_result'))) %>%
  mutate(extra_point_result = case_when(extra_point_result == 'good' ~ 0,
                                        extra_point_result == 'failed' ~ 1,
                                        extra_point_result == 'blocked' ~ 2)) %>%
  na.omit()
corr_matrix = cor(extra_point)
corrplot(corr_matrix, method='color', type='upper', tl.cex=0.5,
         tl.srt=45, cl.pos='b', mar=c(0, 0, 4, 0))

# -------------------------------------------------------------------------
