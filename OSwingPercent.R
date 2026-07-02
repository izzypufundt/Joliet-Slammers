library(shiny)
library(tidyverse)
library(ggplot2)
library(DT)

main_folder <- "/Users/issabellapufundt/Desktop/Joliet Slammers/05"

swing_calls <- c("StrikeSwinging", "FoulBall", "FoulBallNFielder",
                 "FoulBallFielded", "FoulBallNotFieldable", "FoulBallFieldable",
                 "InPlay")

count_levels <- c("0-0","0-1","0-2",
                  "1-0","1-1","1-2",
                  "2-0","2-1","2-2",
                  "3-0","3-1","3-2")

# CSV loading
# Read every CSV as all-character first, bind, then convert numeric columns.

csv_files <- list.files(path = main_folder, pattern = "\\.csv$",
                        recursive = TRUE, full.names = TRUE)
csv_files <- csv_files[!grepl("playerpositioning", basename(csv_files), ignore.case = TRUE)]

if (length(csv_files) == 0) stop("No CSV files found in: ", main_folder)

tables <- lapply(csv_files, function(f) {
  read.csv(f, stringsAsFactors = FALSE, colClasses = "character")
})
raw <- dplyr::bind_rows(tables)

# Columns to convert to numeric
numeric_cols <- c("RelSpeed", "VertRelAngle", "HorzRelAngle", "SpinRate", "SpinAxis",
                  "Tilt", "RelHeight", "RelSide", "Extension", "VertBreak",
                  "InducedVertBreak", "HorzBreak", "PlateLocHeight", "PlateLocSide",
                  "ZoneSpeed", "VertApprAngle", "HorzApprAngle", "ZoneTime",
                  "ExitSpeed", "Angle", "Direction", "HitSpinRate",
                  "PositionAt110X", "PositionAt110Y", "PositionAt110Z",
                  "Distance", "Bearing", "HangTime",
                  "pfxx", "pfxz", "x0", "y0", "z0", "vx0", "vy0", "vz0",
                  "ax0", "ay0", "az0",
                  "HomeTeamRuns", "AwayTeamRuns", "Inning", "PAofInning",
                  "PitchofPA", "Balls", "Strikes", "Outs")

for (col in numeric_cols) {
  if (col %in% names(raw)) {
    raw[[col]] <- suppressWarnings(as.numeric(raw[[col]]))
  }
}

# De-duplicate by PitchUID (keep most-complete row)
if ("PitchUID" %in% names(raw)) {
  raw$non_na_count <- rowSums(!is.na(raw))
  raw <- raw[order(raw$PitchUID, -raw$non_na_count), ]
  raw <- raw[!duplicated(raw$PitchUID), ]
  raw$non_na_count <- NULL
}

# Pre-process full dataset (all teams)
all_data <- raw %>%
  filter(!is.na(BatterTeam), BatterTeam != "") %>%
  mutate(
    Count = paste0(Balls, "-", Strikes),
    Swing = PitchCall %in% swing_calls,
    Date  = as.Date(Date)
  )

all_teams <- sort(unique(all_data$BatterTeam))

# HOME PLATE polygon
plate_width <- 0.7083
plate_df <- data.frame(
  x = c(-plate_width, plate_width, plate_width,  0, -plate_width, -plate_width),
  y = c(0.1,          0.1,         0.25,          0,  0.25,         0.1)
)


ui <- fluidPage(
  tags$head(tags$style(HTML("
    body { font-family: 'Helvetica Neue', Helvetica, Arial, sans-serif;
           background-color: #f5f5f5; color: #2c3e50; }
    .well { background-color: #ffffff; border: 1px solid #dce1e7; border-radius: 6px; }
    .btn-default { background-color: #18bc9c; color: #fff; border-color: #18bc9c; }
    .btn-default:hover { background-color: #15a589; border-color: #15a589; color: #fff; }
    .nav-tabs > li.active > a { color: #18bc9c; border-top: 3px solid #18bc9c; font-weight: bold; }
    .nav-tabs > li > a:hover { color: #18bc9c; }
    h4 { color: #18bc9c; }
    hr { border-top: 1px solid #dce1e7; }
    .team-box { background:#18bc9c; color:#fff; border-radius:6px;
                padding:8px 14px; margin-bottom:10px; font-weight:bold; font-size:1.05em; }
  "))),
  
  titlePanel(
    div(
      h2("O-Swing % (Chase Rate) Dashboard", style = "margin-bottom:0;"),
      h5("Trackman Data — All Teams", style = "color:#7f8c8d; margin-top:4px;")
    )
  ),
  
  sidebarLayout(
    sidebarPanel(
      width = 3,
      
      h4("Team & Batter", style = "font-weight:bold;"),
      
      selectInput("selected_team", "Select Team:",
                  choices  = all_teams,
                  selected = all_teams[1]),
      
      selectInput("batters", "Batter(s):",
                  choices  = NULL,
                  selected = NULL,
                  multiple = TRUE),
      
      hr(),
      
      h4("Zone Settings", style = "font-weight:bold;"),
      
      sliderInput("zone_side",
                  "Horizontal Zone (ft):",
                  min = -1.5, max = 1.5,
                  value = c(-0.8333, 0.8333), step = 0.05),
      
      sliderInput("zone_height",
                  "Vertical Zone (ft):",
                  min = 0.5, max = 5,
                  value = c(1.5, 3.5), step = 0.1),
      
      hr(),
      
      numericInput("min_oz", "Min OZ Pitches (per cell):", value = 5, min = 1, max = 50),
      
      hr(),
      
      h4("Count Selection", style = "font-weight:bold;"),
      checkboxGroupInput("counts", "Show Counts:",
                         choices  = count_levels,
                         selected = count_levels),
      
      hr(),
      
      h4("Strike Zone Plot Options", style = "font-weight:bold;"),
      
      selectInput("sz_batter", "Batter (Zone Plot):",
                  choices  = NULL,
                  selected = NULL),
      
      selectInput("sz_count", "Count (Zone Plot):",
                  choices  = c("All Counts" = "ALL", count_levels),
                  selected = "ALL"),
      
      radioButtons("sz_color", "Color Points By:",
                   choices  = c("Swing/Take" = "swing", "Pitch Type" = "pitch"),
                   selected = "swing"),
      
      hr(),
      
      downloadButton("dl_wide",    "Download Wide Table (.csv)"),
      br(), br(),
      downloadButton("dl_overall", "Download Overall Summary (.csv)")
    ),
    
    mainPanel(
      width = 9,
      
      uiOutput("team_badge"),
      
      tabsetPanel(
        id = "tabs",
        
        tabPanel("Heatmap",
                 br(),
                 plotOutput("heatmap", height = "auto"),
                 br(),
                 p(em("Blue = lower chase rate | Red = higher chase rate | Gray = insufficient sample"),
                   style = "color:#7f8c8d; font-size:12px;")
        ),
        
        tabPanel("Strike Zone Plot",
                 br(),
                 fluidRow(
                   column(6,
                          h4("All Pitches — In Zone & Out of Zone", style = "text-align:center;"),
                          plotOutput("sz_all", height = "500px")
                   ),
                   column(6,
                          h4("Chase Pitches Only (OZ Swings vs OZ Takes)", style = "text-align:center;"),
                          plotOutput("sz_chase", height = "500px")
                   )
                 ),
                 br(),
                 p(em("Each point = one pitch at its plate location. Zone box reflects your sidebar slider settings."),
                   style = "color:#7f8c8d; font-size:12px;")
        ),
        
        tabPanel("By Count (Table)",
                 br(),
                 DTOutput("table_by_count")
        ),
        
        tabPanel("Wide Table",
                 br(),
                 DTOutput("table_wide")
        ),
        
        tabPanel("Overall Summary",
                 br(),
                 DTOutput("table_overall"),
                 br(),
                 plotOutput("bar_overall", height = "350px")
        ),
        
        tabPanel("Team by Count",
                 br(),
                 DTOutput("table_team"),
                 br(),
                 plotOutput("bar_team", height = "350px")
        )
      )
    )
  )
)


server <- function(input, output, session) {
  
  output$team_badge <- renderUI({
    div(class = "team-box", paste0("\U0001F4CB  Viewing: ", input$selected_team))
  })
  
  team_batters <- reactive({
    req(input$selected_team)
    all_data %>%
      filter(BatterTeam == input$selected_team) %>%
      pull(Batter) %>% unique() %>% sort()
  })
  
  observe({
    batters <- team_batters()
    updateSelectInput(session, "batters",
                      choices  = c("All Batters" = "ALL", batters),
                      selected = "ALL")
    updateSelectInput(session, "sz_batter",
                      choices  = c("All Batters" = "ALL", batters),
                      selected = "ALL")
  })
  
  tm_hitters <- reactive({
    req(input$selected_team)
    all_data %>% filter(BatterTeam == input$selected_team)
  })
  
  filtered <- reactive({
    req(input$zone_side, input$zone_height)
    
    d <- tm_hitters() %>%
      mutate(
        OutsideZone = !is.na(PlateLocSide) & !is.na(PlateLocHeight) &
          (PlateLocSide   < input$zone_side[1]   | PlateLocSide   > input$zone_side[2]   |
             PlateLocHeight < input$zone_height[1]  | PlateLocHeight > input$zone_height[2])
      )
    
    if (!("ALL" %in% input$batters) && length(input$batters) > 0) {
      d <- d %>% filter(Batter %in% input$batters)
    }
    d
  })
  
  sz_data <- reactive({
    req(input$zone_side, input$zone_height)
    
    d <- tm_hitters() %>%
      filter(!is.na(PlateLocSide), !is.na(PlateLocHeight)) %>%
      mutate(
        OutsideZone = PlateLocSide   < input$zone_side[1]   | PlateLocSide   > input$zone_side[2]   |
          PlateLocHeight < input$zone_height[1]  | PlateLocHeight > input$zone_height[2],
        SwingLabel  = ifelse(Swing, "Swing", "Take"),
        ChaseLabel  = case_when(
          OutsideZone & Swing  ~ "OZ Swing (Chase)",
          OutsideZone & !Swing ~ "OZ Take",
          !OutsideZone & Swing  ~ "IZ Swing",
          TRUE                  ~ "IZ Take"
        )
      )
    
    if (input$sz_batter != "ALL") d <- d %>% filter(Batter == input$sz_batter)
    if (input$sz_count  != "ALL") d <- d %>% filter(Count  == input$sz_count)
    d
  })
  
  # Zone base plot
  zone_base <- function(zone_side, zone_height) {
    ggplot() +
      annotate("rect",
               xmin = zone_side[1], xmax = zone_side[2],
               ymin = zone_height[1], ymax = zone_height[2],
               fill = NA, color = "black", linewidth = 1.2) +
      annotate("segment",
               x = zone_side[1], xend = zone_side[2],
               y = zone_height[1] + (zone_height[2] - zone_height[1]) / 3,
               yend = zone_height[1] + (zone_height[2] - zone_height[1]) / 3,
               color = "grey70", linewidth = 0.4, linetype = "dashed") +
      annotate("segment",
               x = zone_side[1], xend = zone_side[2],
               y = zone_height[1] + 2 * (zone_height[2] - zone_height[1]) / 3,
               yend = zone_height[1] + 2 * (zone_height[2] - zone_height[1]) / 3,
               color = "grey70", linewidth = 0.4, linetype = "dashed") +
      annotate("segment",
               x = zone_side[1] + (zone_side[2] - zone_side[1]) / 3,
               xend = zone_side[1] + (zone_side[2] - zone_side[1]) / 3,
               y = zone_height[1], yend = zone_height[2],
               color = "grey70", linewidth = 0.4, linetype = "dashed") +
      annotate("segment",
               x = zone_side[1] + 2 * (zone_side[2] - zone_side[1]) / 3,
               xend = zone_side[1] + 2 * (zone_side[2] - zone_side[1]) / 3,
               y = zone_height[1], yend = zone_height[2],
               color = "grey70", linewidth = 0.4, linetype = "dashed") +
      geom_polygon(data = plate_df, aes(x = x, y = y),
                   fill = "white", color = "black", linewidth = 1) +
      coord_fixed(xlim = c(-2.5, 2.5), ylim = c(-0.1, 5.5)) +
      labs(x = "Horizontal Location (ft)", y = "Height (ft)") +
      theme_minimal(base_size = 12) +
      theme(
        panel.grid.major = element_line(color = "grey92"),
        panel.grid.minor = element_blank(),
        plot.title       = element_text(face = "bold", size = 13),
        legend.position  = "bottom",
        legend.title     = element_blank()
      )
  }
  
  # Strike Zone: All pitches
  output$sz_all <- renderPlot({
    d <- sz_data()
    validate(need(nrow(d) > 0, "No pitch location data for these filters."))
    
    color_var  <- if (input$sz_color == "swing") "SwingLabel" else "AutoPitchType"
    color_vals <- if (input$sz_color == "swing") c("Swing" = "#e74c3c", "Take" = "#3498db") else NULL
    
    p <- zone_base(input$zone_side, input$zone_height) +
      geom_point(data = d,
                 aes(x = PlateLocSide, y = PlateLocHeight, color = .data[[color_var]]),
                 alpha = 0.65, size = 2.2) +
      labs(title = paste("All Pitches |",
                         if (input$sz_batter == "ALL") "All Batters" else input$sz_batter,
                         "|",
                         if (input$sz_count  == "ALL") "All Counts"  else paste("Count:", input$sz_count)))
    
    if (!is.null(color_vals)) p <- p + scale_color_manual(values = color_vals)
    p
  })
  
  # Strike Zone: Chase pitches
  output$sz_chase <- renderPlot({
    d <- sz_data() %>% filter(OutsideZone)
    validate(need(nrow(d) > 0, "No outside-zone pitches for these filters."))
    
    zone_base(input$zone_side, input$zone_height) +
      geom_point(data = d,
                 aes(x = PlateLocSide, y = PlateLocHeight, color = ChaseLabel),
                 alpha = 0.75, size = 2.5) +
      scale_color_manual(values = c("OZ Swing (Chase)" = "#e74c3c", "OZ Take" = "#3498db")) +
      labs(title = paste("OZ Pitches Only |",
                         if (input$sz_batter == "ALL") "All Batters" else input$sz_batter,
                         "|",
                         if (input$sz_count  == "ALL") "All Counts"  else paste("Count:", input$sz_count)))
  })
  
  # Per-player by count
  player_by_count <- reactive({
    filtered() %>%
      filter(OutsideZone, Count %in% input$counts) %>%
      group_by(Batter, Count) %>%
      summarise(
        OZ_Pitches  = n(),
        OZ_Swings   = sum(Swing, na.rm = TRUE),
        O_Swing_Pct = round(OZ_Swings / OZ_Pitches * 100, 1),
        .groups = "drop"
      ) %>%
      filter(OZ_Pitches >= input$min_oz) %>%
      mutate(Count = factor(Count, levels = count_levels)) %>%
      arrange(Batter, Count)
  })
  
  player_wide <- reactive({
    player_by_count() %>%
      select(Batter, Count, O_Swing_Pct) %>%
      pivot_wider(names_from = Count, values_from = O_Swing_Pct, names_sort = TRUE) %>%
      left_join(
        filtered() %>%
          filter(OutsideZone) %>%
          group_by(Batter) %>%
          summarise(Overall = round(sum(Swing) / n() * 100, 1), .groups = "drop"),
        by = "Batter"
      ) %>%
      arrange(Batter)
  })
  
  overall <- reactive({
    filtered() %>%
      filter(OutsideZone) %>%
      group_by(Batter) %>%
      summarise(
        OZ_Pitches  = n(),
        OZ_Swings   = sum(Swing, na.rm = TRUE),
        O_Swing_Pct = round(OZ_Swings / OZ_Pitches * 100, 1),
        .groups = "drop"
      ) %>%
      arrange(desc(O_Swing_Pct))
  })
  
  team_by_count <- reactive({
    filtered() %>%
      filter(OutsideZone, Count %in% input$counts) %>%
      group_by(Count) %>%
      summarise(
        OZ_Pitches  = n(),
        OZ_Swings   = sum(Swing, na.rm = TRUE),
        O_Swing_Pct = round(OZ_Swings / OZ_Pitches * 100, 1),
        .groups = "drop"
      ) %>%
      mutate(Count = factor(Count, levels = count_levels)) %>%
      arrange(Count)
  })
  
  heatmap_height <- reactive({
    n_batters <- n_distinct(player_by_count()$Batter)
    max(400, n_batters * 55 + 120)
  })
  
  output$heatmap <- renderPlot({
    pd <- player_by_count()
    validate(need(nrow(pd) > 0,
                  "No data meets current filters. Try lowering 'Min OZ Pitches' or selecting more counts."))
    
    ggplot(pd, aes(x = Count, y = Batter, fill = O_Swing_Pct)) +
      geom_tile(color = "white", linewidth = 0.6) +
      geom_text(aes(label = paste0(O_Swing_Pct, "%")),
                size = 3.5, color = "black", fontface = "bold") +
      scale_fill_gradient2(
        low = "#2166ac", mid = "#f7f7f7", high = "#d6604d",
        midpoint = 30, name = "O-Swing %", limits = c(0, 100), na.value = "#d3d3d3"
      ) +
      labs(
        title    = paste("O-Swing % (Chase Rate) by Count —", input$selected_team),
        subtitle = paste("Min", input$min_oz, "OZ pitches per cell | Zone:",
                         input$zone_side[1], "to", input$zone_side[2], "ft wide,",
                         input$zone_height[1], "to", input$zone_height[2], "ft tall"),
        x = "Count (Balls-Strikes)", y = NULL
      ) +
      theme_minimal(base_size = 13) +
      theme(
        plot.title      = element_text(face = "bold", size = 15),
        plot.subtitle   = element_text(color = "#7f8c8d", size = 11),
        axis.text.x     = element_text(size = 11, face = "bold"),
        axis.text.y     = element_text(size = 11),
        panel.grid      = element_blank(),
        legend.position = "right"
      )
  }, height = function() heatmap_height())
  
  dt_opts   <- list(pageLength = 25, scrollX = TRUE, dom = "tip")
  pct_color <- function(x) {
    styleInterval(c(15, 25, 35, 45),
                  c("#2166ac","#6baed6","#f7f7f7","#fc8d59","#d6604d"))
  }
  
  output$table_by_count <- renderDT({
    pd <- player_by_count() %>% arrange(Batter, Count)
    datatable(pd, options = dt_opts, rownames = FALSE) %>%
      formatStyle("O_Swing_Pct", backgroundColor = pct_color(pd$O_Swing_Pct))
  })
  
  output$table_wide <- renderDT({
    datatable(player_wide(), options = dt_opts, rownames = FALSE)
  })
  
  output$table_overall <- renderDT({
    datatable(overall(), options = dt_opts, rownames = FALSE) %>%
      formatStyle("O_Swing_Pct", backgroundColor = pct_color(overall()$O_Swing_Pct))
  })
  
  output$table_team <- renderDT({
    datatable(team_by_count(), options = dt_opts, rownames = FALSE) %>%
      formatStyle("O_Swing_Pct", backgroundColor = pct_color(team_by_count()$O_Swing_Pct))
  })
  
  output$bar_overall <- renderPlot({
    od <- overall()
    validate(need(nrow(od) > 0, "No data to display."))
    ggplot(od, aes(x = reorder(Batter, O_Swing_Pct), y = O_Swing_Pct, fill = O_Swing_Pct)) +
      geom_col(show.legend = FALSE) +
      geom_text(aes(label = paste0(O_Swing_Pct, "%")), hjust = -0.2, size = 3.5) +
      scale_fill_gradient2(low = "#2166ac", mid = "#f7f7f7", high = "#d6604d", midpoint = 30) +
      coord_flip() +
      scale_y_continuous(limits = c(0, 105)) +
      labs(title = paste("Overall O-Swing % by Hitter —", input$selected_team),
           x = NULL, y = "O-Swing %") +
      theme_minimal(base_size = 12) +
      theme(plot.title = element_text(face = "bold"), panel.grid.minor = element_blank())
  })
  
  output$bar_team <- renderPlot({
    td <- team_by_count()
    validate(need(nrow(td) > 0, "No data to display."))
    ggplot(td, aes(x = Count, y = O_Swing_Pct, fill = O_Swing_Pct)) +
      geom_col(show.legend = FALSE) +
      geom_text(aes(label = paste0(O_Swing_Pct, "%")),
                vjust = -0.5, size = 3.5, fontface = "bold") +
      scale_fill_gradient2(low = "#2166ac", mid = "#f7f7f7", high = "#d6604d", midpoint = 30) +
      scale_y_continuous(limits = c(0, 105)) +
      labs(title = paste("Team O-Swing % by Count —", input$selected_team),
           x = "Count", y = "O-Swing %") +
      theme_minimal(base_size = 12) +
      theme(plot.title = element_text(face = "bold"), panel.grid.minor = element_blank())
  })
  
  # Downloads
  output$dl_wide <- downloadHandler(
    filename = function() paste0("o_swing_wide_", input$selected_team, "_", Sys.Date(), ".csv"),
    content  = function(file) write_csv(player_wide(), file)
  )
  output$dl_overall <- downloadHandler(
    filename = function() paste0("o_swing_overall_", input$selected_team, "_", Sys.Date(), ".csv"),
    content  = function(file) write_csv(overall(), file)
  )
}

shinyApp(ui = ui, server = server)