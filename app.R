library(tidyverse)
library(tigris)
library(leaflet)
library(htmltools)
library(plotly)

# read in full Palma data
palma_full <- readRDS('palma_full.Rda') %>%
  # rename puma to PUMA
  mutate(type = str_replace_all(type, 'puma', 'PUMA'))

# read in individual income data
incomes <- readRDS('incomes.Rda')

# create unique PUMAs and counties from income distribution dataset
# do this here, so it does not have to be recreated after each new click of PUMA or county
unique_county <- incomes %>%
  distinct(group) %>%
  arrange(group) %>%
  .[[1]]

unique_puma <- incomes %>%
  distinct(PUMA) %>%
  arrange(PUMA) %>%
  .[[1]]

# pull out Forsyth County PUMAs, to be used as initially selected
forsyth_puma <- na.omit( str_extract(unique_puma, 'Winston.*|Forsyth.*') )

# dataframe of nc PUMA and county boundaries
nc_puma <- readRDS('nc_puma.Rda')
nc_county <- readRDS('nc_county.Rda')

# create unique

# create color palette
pal <- colorNumeric(
  palette = "Greens",
  domain = 0:4,
  na.color = "transparent")

# function to create vertical lines
vline <- function(x = 0, color = "black") {
  list(
    type = "line", 
    y0 = 0, 
    y1 = 1, 
    yref = "paper",
    x0 = x, 
    x1 = x, 
    line = list(color = color)
  )
}

# create list of margins for line plot
m <- list(
  l = 50,
  r = 50,
  b = 50,
  t = 50,
  pad = 4
)

# create y ticks; needed so all PUMAs are displayed
ytick <- list(
  autotick = FALSE,
  ticks = "outside",
  tick0 = 0,
  dtick = 1,
  ticklen = 5,
  tickwidth = 1,
  tickcolor = toRGB("blue"),
  title = ''
)

ui <- navbarPage('North Carolina income inequality',
                tabPanel('Map of Palmas',
                     # add html and css so that the map spans 100% height
                     bootstrapPage(div(class="outer",
                        tags$style(type = "text/css", ".outer {position: fixed; top: 41px; left: 0; right: 0; bottom: 0; overflow: hidden; padding: 0}"),
                        # map of Palmas by PUMA  
                        leafletOutput('palma_map', height = '100%')),
                    
                     # drop down arrow showing year
                     absolutePanel(id = 'controls', draggable = TRUE, right = 20, top = 50,
                     h4('North Carolina Palma ratios'),
                     selectInput('select_year_map', 'Select a year:',
                                 choices = unique(palma_full$year),
                                 selected = max(palma_full$year)),
                     selectInput('select_area_map', 'Select county or PUMA:',
                                 choices = c('county', 'PUMA'),
                                 selected = 'PUMA'),
                     p('Data source: ', a('US Census Public Use Micro Data',
                                        href = 'https://www.census.gov/programs-surveys/acs/data/pums.html'))
                 ))),
                
                tabPanel('Palma rankings',
                         
                         selectInput('select_year_scatter', 'Select a year:',
                                     choices = unique(palma_full$year),
                                     selected = max(palma_full$year)),
                         selectInput('select_area_scatter', 'Select county or PUMA:',
                                     choices = c('county', 'PUMA'),
                                     selected = 'PUMA'),
                         plotlyOutput("scatterplot")),

                tabPanel('Income distribution | Palma ratios through time',
                         
                         selectInput('select_area_dist', 'Select county or PUMA:',
                                     choices = c('county', 'PUMA'),
                                     selected = 'PUMA'),
                         uiOutput("ui_dist_check"),
                         plotlyOutput("dist_plot"),
                         tags$br(),
                         tags$hr(),
                         tags$br(),
                         plotlyOutput("line_plot"))
)

server <- function(input, output) {
  
  year_puma_map <- reactive({
    # for map
    # filter data for year, based on which yeare is selected  
    palma_full %>%
      filter(year == input$select_year_map,
             type == input$select_area_map) %>%
      # select inputs based on whether county or PUMA is selected
      geo_join(if (input$select_area_map == 'county') nc_county else nc_puma,
               .,
               if (input$select_area_map == 'county') 'NAME' else 'NAME10',
               'geo_description')
    
  })
  
  year_puma_scatter <- reactive({
    # for scatterplot
    # filter data for year, based on which yeare is selected  
    palma_full %>%
      filter(year == input$select_year_scatter,
             type == input$select_area_scatter) %>%
      distinct(geo_description, .keep_all = TRUE)
    
  })
  
  output$ui_dist_check <- renderUI({
    
    # create geographic area checkboxes for distribution plot
    
    unique_areas <- if (input$select_area_dist == 'PUMA') unique_puma else unique_county
    selected_geo <- if (input$select_area_dist == 'PUMA') forsyth_puma else 'Forsyth'
    
    # create unique values for PUMAs or counties

    checkboxGroupInput('dist_check', 'Geography:', unique_areas, selected = selected_geo, inline = TRUE)

  })
  
  year_nc <- reactive({
    
    # NC palma for selected year
    # used for vertical line in Plotly scatterplot
    palma_full %>%
      filter(type == 'state',
             year == input$select_year_scatter) %>%
      select(estimate) %>%
      .[[1]] %>%
      round(., 2)
    
  })
  
  dist_area <- reactive({
    
    # create dataset for use with final plots tab
    
    # rename geographic grouping column so that it is the same for PUMA and county
    # this allows us to use the same columns in the plot
    
    # filter for geographic area based on whether county or PUMA
    if (input$select_area_dist == 'PUMA') {
      
      # drop variable with counhty names and rename variable with PUMA names
      # the same name as the variable with group names
      df <- incomes %>%
        select(-group) %>%
        rename(group = PUMA) %>%
        filter(group %in% !!input$dist_check)
      
      return(df)
      
    } else {
      
      df <- incomes %>%
        filter(group %in% !!input$dist_check)
      
      return(df)
      
    }
    
  })
  
  label_year <- reactive({
    
    # name of geography column depends on whether PUMA or count is selected
    geo_name <- if (input$select_area_map == 'county') 'NAME' else 'NAME10'
    
    # tooltip labels, create based on year
    lapply(seq(nrow(year_puma_map()@data)), function(i) {
      paste0( year_puma_map()@data[i, geo_name], '</br>', 
              'Palma: ', year_puma_map()@data[i, "estimate"], '</br>', 
              'Margin of error: +/- ', year_puma_map()@data[i, "moe"]) 
    })
    
  })
  
  output$palma_map <- renderLeaflet({
    
    leaflet(year_puma_map()) %>%
      # set view to center on Forsyth County
      setView(lng = -80.2442, lat = 36.0999, zoom = 10) %>%
      addProviderTiles(providers$CartoDB.Positron) %>%
      addPolygons(fillOpacity = 0.8,
                  weight = 0.2, 
                  smoothFactor = 0.2, 
                  color = ~ pal(estimate),
                  label = lapply(label_year(), HTML)) %>%
      addLegend("bottomright", pal = pal, values = ~estimate,
                title = "Palma ratio",
                opacity = 1)
    
  })
  
  output$scatterplot <- renderPlotly({
    
    # size of plot depends on whether county or PUMA is selected
    plot_size <- if (input$select_area_scatter == 'county') 600 else 1200
    
    year_puma_scatter() %>%
      # create a column that specifies whether the PUMA is in Forsyth
      # we will then color Forsyth points differently
      mutate(geo_description = fct_reorder(geo_description, estimate),
             category = ifelse(.$subtype == 'Forsyth', TRUE, FALSE)) %>%
      # create plot
      plot_ly(height = plot_size, # increase height so all PUMAs show on y axis
              x = ~estimate, y = ~geo_description, type = "scatter", mode = "markers", 
              color = ~category, colors = c("#1f77b4", "#d62728"), showlegend = FALSE,
              hoverinfo = 'text',
              text = ~paste0(geo_description,
                             "<br>Palma ratio:  ", estimate,
                             "<br>Margin of error:  +/-", moe)) %>%
      layout(title = paste0("North Carolina Palma ratios by ", input$select_area_scatter),
             xaxis = list(title = "Palma ratio (vertical black line represents North Carolina state-wide Palma)"),
             yaxis = ytick,
             # horizontal line representing NC Palma
             shapes = list(vline(year_nc()))) %>%
      # add error bars
      add_segments(x = ~(estimate-moe), xend = ~(estimate+moe), 
                   y = ~geo_description, yend = ~geo_description,
                   opacity = 0.5)
    
  })
  
  output$dist_plot <- renderPlotly({
    
    axis_breaks <- c(1000, 3000, 5000, 10000, 20000, 40000, 60000, 100000, 200000, 400000)
    
    dist_area() %>%
      ggplot(aes(post_tax_income, colour = group, fill = group)) +
      geom_density(alpha = 0.3, adjust = 2) +
      scale_x_continuous('Income on logarithmic scale',
                         breaks = axis_breaks,
                         trans = 'log',
                         label=scales::dollar_format()) +
      ylim(0, .6) +
      labs(title = 'After-tax income distribution',
           subtitle = 'Federal and state income taxes, and federal FICA (payroll) taxes were removed from income') +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 90, hjust = 1)) 
    
  })
  
  output$line_plot <- renderPlotly({
    
    # create function for tool tips
    # easiest to create an empty function instead of object since objects are used in the funtion
    tool_tip <- function() {
      
               ~paste0("Geography: ", geo_description,
                        "<br>Year:  ", year,
                        "<br>Palma:  ", round( estimate, 2 ),
                        "<br>MOE:  +/-", round( moe, 2 ))
    }
    
    palma_full %>%
      # filter for either PUMA or county
      filter(type == if (input$select_area_dist == 'PUMA') 'PUMA' else 'county') %>%
      # filter for specific geographic regions within PUMA or county
      filter(geo_description %in% !!input$dist_check) %>%
      plot_ly(x = ~year, y = ~estimate, 
              color = ~geo_description, 
              mode = 'lines', type = 'scatter',
              # tooltip info
              hoverinfo = 'text',
              text = tool_tip()) %>%
      add_ribbons(ymin = ~estimate - moe,
                  ymax = ~estimate + moe,
                  alpha = 0.15,
                  line = list(width = 0, dash = 'dot'),
                  showlegend = FALSE) %>%
      layout(title = paste0('North Carolina yearly Palma change by ', input$select_area_dist),
             margin = m)
    
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)

