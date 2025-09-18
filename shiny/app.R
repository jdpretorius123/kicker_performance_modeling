# Comments ----------------------------------------------------------------

## 1. The script has been updated to specify the interception variable and
##    runs as expected.

# -------------------------------------------------------------------------
# Installing/Loading Dependencies -----------------------------------------

## updating lock file
renv::snapshot()

packages = c('tidyverse', 'shiny', 'bslib', 'plotly', 'htmlwidgets', 'shinyWidgets')
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
# Loading Data ------------------------------------------------------------

fname = './data/master/pbp_clean.Rdata'
load(fname)

# -------------------------------------------------------------------------
# Creating the UI ---------------------------------------------------------

about_text = paste('This app uses real-time in game conditions to generate',
                   'probabilties for interceptions, completions, completions',
                   'of a given yardage, and more!')
ui = ui = page_navbar(title = 'Real-Time Game Modeling for QBs',
                      nav_panel(title = 'About',
                                h2('About this App'),
                                p(about_text)),
                      nav_panel(title = 'Game Conditions',
                                layout_column_wrap(
                                  width = 1/3,
                                  h2('Enter Current Game Conditions'),
                                  switchInput(inputId = 'goal_to_go',
                                              label = 'Goal To Go:', 
                                              value = FALSE),
                                  selectInput(inputId = 'posteam_timeouts_remaining',
                                              label = 'Timeouts Remaining',
                                              choices = c(0,1,2,3)),
                                  numericInput(inputId = 'ydstogo',
                                               label = 'Yards To Go:',
                                               10, 
                                               min = 1,
                                               max = 10)
                                )),
                      nav_panel(title = 'Predicted Probabilities',
                                sidebar = sidebar(
                                  sliderInput(inputId = 'bins',
                                              label = 'Number of bins:',
                                              min = 1,
                                              max = 50,
                                              value = 30)
                                ),
                                plotlyOutput(outputId = 'distPlot')))

# -------------------------------------------------------------------------
# Creating the Server -----------------------------------------------------

server = function(input, output) {
  output$distPlot = renderPlotly({
    plot_ly(data = pbp_clean,
            x = ~yards_gained,
            type = 'histogram',
            nbinsx = input$bins,
            marker = list(color = 'lightgray',
                          line = list(color = 'black',
                                      width = 1)),
            name = 'Frequency',
            hovertemplate = paste(
              'Yards Gained per Play: %{x}',
              '<br>Frequency: %{y}',
              '<extra></extra>')) %>%
      layout(xaxis = list(title = 'Yards Gained per Play'),
             yaxis = list(title = 'Frequency'),
             title = 'Yards Gained per Play')
  })
}

# -------------------------------------------------------------------------
# Building the Shiny App --------------------------------------------------

shinyApp(ui=ui, server=server)

# -------------------------------------------------------------------------
# Running the Shiny App ---------------------------------------------------

runApp('./shiny/app.R')

# -------------------------------------------------------------------------
