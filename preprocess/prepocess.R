# Comments ----------------------------------------------------------------

## 1. The script has been updated to specify the interception variable and
##    runs as expected.

# -------------------------------------------------------------------------
# Installing/Loading Dependencies -----------------------------------------

## updating lock file
renv::snapshot()

packages = c('tidyverse','lubridate','glue','janitor','plotly',
             'htmlwidgets','corrplot','DescTools')
check_packages = function(packages) {
  for (p in packages) {
    if (!require(p, character.only=TRUE)) {
      install.packages(p)
    }
    library(p, character.only=TRUE)
  }
}

check_packages(packages)

# -------------------------------------------------------------------------
# Clearing Memory ---------------------------------------------------------

rm(list = ls())

# -------------------------------------------------------------------------
# Loading Clean Play-by-Play Data -----------------------------------------

load(file = './data/cleaned/interception/int_pbp_clean.Rdata')
glimpse(int_pbp_clean)

# -------------------------------------------------------------------------
# Selecting Candidate Variables for Analysis  -----------------------------
candidates = c('posteam_type','yardline_100','game_seconds_remaining',
               'ydstogo','goal_to_go','yards_gained','qb_scramble',
               'shotgun','no_huddle','posteam_timeouts_remaining',
               'defteam_timeouts_remaining','score_differential','third_down',
               'interception','weather','drive_play_count',
               'location','div_game','roof','surface','temp','wind',
               'home_opening_kickoff','age','start_hour')

## creating a data set for candidate variables
candidate_set = int_pbp_clean[,candidates]
glimpse(candidate_set)

# -------------------------------------------------------------------------
# Correlation Plot --------------------------------------------------------

factors = sapply(candidate_set, is.factor)
candidates_numeric = candidate_set[,!factors]
glimpse(candidates_numeric)

corr_matrix = cor(candidates_numeric)
corrplot(corr_matrix, method='color', type='upper', tl.cex=0.5,
         tl.srt=45, cl.pos='b', mar=c(0, 0, 4, 0))

# -------------------------------------------------------------------------
# Saving Candidate Set ----------------------------------------------------

int_pbp_preprocess = candidate_set
filename = './data/preprocess/interception/int_pbp_preprocess.Rdata'
save(int_pbp_preprocess, file=filename)

# -------------------------------------------------------------------------


