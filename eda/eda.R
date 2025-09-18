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
  filter(play_type != 'no_play', season_type == 'REG')
table(temp$play_type, dnn = 'Play Type', useNA = 'always')
table(temp$season_type, dnn = 'Season Type', useNA = 'always')

# -------------------------------------------------------------------------
# Assessing Interactions --------------------------------------------------

## listing the factors in pbp_clean
factors = sapply(pbp_clean, is.factor)
names(pbp_clean)[factors]

## roof: 1 = open, 0 = closed
roof = pbp_clean %>%
  dplyr::select(roof, interception) %>%
  mutate(roof = if_else(roof == 1, 'open', 'closed'),
         interception = if_else(interception == 1, 'Yes', 'No')) %>%
  group_by(roof) %>%
  summarise(total_ints = n(), .groups = 'drop')
roof_plot = plot_ly(data = roof,
                    x = ~roof,
                    y = ~total_ints,
                    type = 'bar',
                    marker = list(line = list(color = 'black',
                                              width = 1)),
                    name = ~roof,
                    hovertemplate = paste(
                      'Roof Type: %{x}',
                      '<br>Total Interceptions: %{y}',
                      '<extra></extra>')) %>%
  layout(xaxis = list(title = 'Roof Type'),
         yaxis = list(title = 'Total Interceptions'),
         title = 'Total Interceptions by Roof Type',
         legend = list(title = list(text = 'Roof Type')))
roof_plot


## qb_scramble
qb_scramble = pbp_clean %>%
  dplyr::select(qb_scramble, interception) %>%
  mutate(qb_scramble = if_else(qb_scramble == 1, 'Yes', 'No'),
         interception = if_else(interception == 1, 'Yes', 'No')) %>%
  group_by(qb_scramble) %>%
  summarise(total_ints = n(), .groups = 'drop')
qb_scramble_plot = plot_ly(data = qb_scramble,
                    x = ~qb_scramble,
                    y = ~total_ints,
                    type = 'bar',
                    marker = list(line = list(color = 'black',
                                              width = 1)),
                    name = ~qb_scramble,
                    hovertemplate = paste(
                      'QB Scramble: %{x}',
                      '<br>Total Interceptions: %{y}',
                      '<extra></extra>')) %>%
  layout(xaxis = list(title = 'QB Scramble'),
         yaxis = list(title = 'Total Interceptions'),
         title = 'Total Interceptions by QB Scramble',
         legend = list(title = list(text = 'QB Scramble')))
qb_scramble_plot

## qb_scramble, shotgun, no_huddle
table(pbp_clean$no_huddle, useNA = 'always')
table(pbp_clean$shotgun, useNA = 'always')
table(pbp_clean$qb_scramble, useNA = 'always')
no_huddle_shotgun = pbp_clean %>%
  dplyr::select(qb_scramble, shotgun, no_huddle) %>%
  group_by(no_huddle, shotgun) %>%
  summarise(count = n(), .groups = 'drop') %>%
  mutate(no_huddle = factor(if_else(no_huddle == 1, 'yes', 'no')),
         shotgun = factor(if_else(shotgun == 1, 'yes', 'no')))
no_huddle_shotgun_plot = plot_ly(data = no_huddle_shotgun,
                                 x = ~no_huddle,
                                 y = ~count,
                                 color = ~shotgun,
                                 type = 'bar',
                                 marker = list(line = list(color = 'black',
                                                           width = 1)),
                                 name = ~shotgun,
                                 hovertemplate = paste(
                                   'Huddle: %{x}',
                                   '<br>Shotgun: ', no_huddle_shotgun[['shotgun']],
                                   '<br>QB Scramble Count: %{y}',
                                   '<extra></extra>')) %>%
  layout(xaxis = list(title = 'Huddle'),
         yaxis = list(title = 'QB Scramble Count'),
         title = 'QB Scramble Count by Shotgun following Huddle/No Huddle',
         legend = list(title = list(text = 'Shotgun')))
no_huddle_shotgun_plot

## checking interception by roof type
table(pbp_clean$interception, pbp_clean$roof, useNA = 'always')
int_by_roof = pbp_clean %>%
  mutate(interception = if_else(interception == 1, 'yes', 'no')) %>%
  group_by(roof, interception) %>%
  summarise(count = n(), .groups = 'drop')
int_by_roof_plot = plot_ly(data = int_by_roof,
                           x = ~roof,
                           y = ~count,
                           type = 'bar',
                           color = ~interception,
                           marker = list(line = list(color = 'black',
                                                     width = 1)),
                           name = ~interception,
                           hovertemplate = paste(
                             'Roof: %{x}',
                             '<br>Interceptions: %{y}',
                             '<extra></extra>')) %>%
  layout(xaxis = list(title = 'Roof Type'),
         yaxis = list(title = 'Interceptions'),
         title = 'Interceptions by Roof Type',
         legend = list(title = list(text = 'Interception')))
int_by_roof_plot

## considering grouping roof into 2 categories: 1. open and 2. closed


## checking interception by surface
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
