library(shiny)
library(dplyr)
library(tidyr)
library(ggplot2)
library(DT)

# Count ordering
count_levels <- c("0-0","0-1","0-2",
                  "1-0","1-1","1-2",
                  "2-0","2-1","2-2",
                  "3-0","3-1","3-2")

# Pitch type colors
pitch_colors <- c(
  "Four-Seam"  = "#E63946",
  "Sinker"     = "#F4A261",
  "Changeup"   = "#2A9D8F",
  "Slider"     = "#457B9D",
  "Curveball"  = "#6A4C93",
  "Cutter"     = "#E9C46A",
  "Splitter"   = "#264653",
  "Other"      = "#A8DADC",
  "Undefined"  = "#CCCCCC"
)

# CSV loading
main_folder <- "/Users/issabellapufundt/Desktop/Joliet Slammers/05"

csv_files <- list.files(main_folder, pattern = "\\.csv$",
                        full.names = TRUE, recursive = TRUE)
csv_files <- csv_files[!grepl("playerpositioning", basename(csv_files), ignore.case = TRUE)]

if (length(csv_files) == 0) stop("No CSV files found in: ", main_folder)

tables <- lapply(csv_files, function(f) {
  tryCatch(
    read.csv(f, stringsAsFactors = FALSE, colClasses = "character"),
    error = function(e) { message("Skipping: ", f); NULL }
  )
})
tables <- Filter(Negate(is.null), tables)
raw_master <- dplyr::bind_rows(tables)

# Convert numeric columns
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
  if (col %in% names(raw_master)) {
    raw_master[[col]] <- suppressWarnings(as.numeric(raw_master[[col]]))
  }
}

# De-duplicate by PitchUID
if ("PitchUID" %in% names(raw_master)) {
  raw_master$non_na_count <- rowSums(!is.na(raw_master))
  raw_master <- raw_master[order(raw_master$PitchUID, -raw_master$non_na_count), ]
  raw_master <- raw_master[!duplicated(raw_master$PitchUID), ]
  raw_master$non_na_count <- NULL
}

# Pre-process
raw_master <- raw_master %>%
  filter(!is.na(AutoPitchType), AutoPitchType != "") %>%
  mutate(
    AutoPitchType = trimws(AutoPitchType),
    Count         = paste0(Balls, "-", Strikes),
    Inning        = as.numeric(Inning),
    PAofInning    = as.numeric(PAofInning)
  )

# Times Through Order 
game_id_col <- if ("GameUID" %in% names(raw_master)) "GameUID" else
  if ("GameID"  %in% names(raw_master)) "GameID"  else NULL

if (!is.null(game_id_col)) {
  raw_master <- raw_master %>%
    group_by(across(all_of(c(game_id_col, "Pitcher")))) %>%
    arrange(Inning, PAofInning, .by_group = TRUE) %>%
    mutate(
      pa_seq  = as.integer(factor(paste(Inning, PAofInning),
                                  levels = unique(paste(Inning, PAofInning)))),
      tto_num = ceiling(pa_seq / 9),
      tto     = case_when(
        tto_num == 1 ~ "1",
        tto_num == 2 ~ "2",
        tto_num == 3 ~ "3",
        tto_num >= 4 ~ "4plus",
        TRUE         ~ "1"
      )
    ) %>%
    ungroup()
} else {
  raw_master$tto <- "1"
}

# UI
ui <- fluidPage(
  tags$head(tags$style(HTML("
    body { background-color: #0d1117; color: #e6edf3;
           font-family: 'Segoe UI', sans-serif; }
    .well { background-color: #161b22; border: 1px solid #30363d; }
    .selectize-input, .selectize-dropdown {
      background-color: #161b22 !important;
      color: #e6edf3 !important; border-color: #30363d !important; }
    .nav-tabs > li > a { color: #8b949e; background-color: #161b22;
      border-color: #30363d #30363d transparent; }
    .nav-tabs > li.active > a,
    .nav-tabs > li.active > a:focus,
    .nav-tabs > li.active > a:hover {
      background-color: #1f6feb; color: #fff; border-color: #1f6feb; }
    .tab-content { background-color: #0d1117; border: 1px solid #30363d;
      border-top: none; padding: 20px; border-radius: 0 0 6px 6px; }
    h2 { color: #58a6ff; border-bottom: 1px solid #30363d; padding-bottom: 10px; }
    h4 { color: #8b949e; }
    .stat-box { background-color: #161b22; border: 1px solid #30363d;
      border-radius: 8px; padding: 15px; text-align: center; margin-bottom: 15px; }
    .stat-box .val { font-size: 2em; font-weight: bold; color: #58a6ff; }
    .stat-box .lbl { font-size: 0.85em; color: #8b949e; margin-top: 4px; }
    .section-header { color: #58a6ff; font-size: 1.1em; font-weight: 600;
      margin: 18px 0 8px; border-left: 3px solid #1f6feb; padding-left: 10px; }
    hr { border-color: #30363d; }
    .count-matrix { width:100%; border-collapse:collapse; font-size:0.92em;
      background:#fff; color:#111; border-radius:8px; overflow:hidden; }
    .count-matrix th, .count-matrix td {
      border:1px solid #ddd; padding:9px 14px;
      text-align:center; white-space:nowrap; }
    .count-matrix thead th { font-weight:700; background:#f0f0f0;
      border-bottom:2px solid #bbb; font-size:0.95em; }
    .count-matrix .row-label { font-weight:700; text-align:left;
      padding-left:14px; background:#f8f8f8; min-width:110px; }
    .count-matrix .empty-cell { color:#bbb; }
    .table-wrap { overflow-x:auto; border-radius:8px;
      box-shadow:0 2px 12px rgba(0,0,0,0.5); margin-bottom:24px; }
    .legend-box { display:inline-block; width:14px; height:14px;
      border-radius:3px; margin-right:5px; vertical-align:middle; }
  "))),
  
  div(style = "padding: 20px 30px;",
      h2("Pitch Usage by Count"),
      
      fluidRow(
        column(3,
               wellPanel(
                 h4("Filters"),
                 selectInput("team",    "Select Team",    choices = sort(unique(raw_master$PitcherTeam))),
                 selectInput("pitcher", "Select Pitcher", choices = NULL),
                 hr(),
                 checkboxGroupInput("counts_filter", "Show Counts:",
                                    choices  = count_levels,
                                    selected = count_levels),
                 hr(),
                 numericInput("min_pitches", "Min pitches per cell:", value = 1, min = 1),
                 hr(),
                 radioButtons("batter_side", "Batter Handedness:",
                              choices  = c("All" = "ALL", "Right (RHH)" = "Right", "Left (LHH)" = "Left"),
                              selected = "ALL"),
                 hr(),
                 radioButtons("inning_group", "Inning:",
                              choices  = c("All Innings"  = "ALL",
                                           "Early (1-3)"  = "early",
                                           "Middle (4-6)" = "middle",
                                           "Late (7+)"    = "late",
                                           "Custom"       = "custom"),
                              selected = "ALL"),
                 conditionalPanel(
                   condition = "input.inning_group == 'custom'",
                   checkboxGroupInput("inning_custom", "Select Innings:",
                                      choices  = 1:9,
                                      selected = 1:9,
                                      inline   = TRUE)
                 ),
                 hr(),
                 radioButtons("tto", "Times Through Order:",
                              choices  = c("All"       = "ALL",
                                           "1st Time"  = "1",
                                           "2nd Time"  = "2",
                                           "3rd Time"  = "3",
                                           "4th Time+" = "4plus"),
                              selected = "ALL"),
                 hr(),
                 p(style = "color:#8b949e; font-size:0.82em;",
                   "Matrix shows the most-used pitch for every count.
                    Bar chart shows full % breakdown per count.
                    Table shows all pitch % by count.")
               )
        ),
        
        column(9,
               fluidRow(
                 column(4, div(class="stat-box",
                               div(class="val", textOutput("total_pitches")),
                               div(class="lbl", "Total Pitches"))),
                 column(4, div(class="stat-box",
                               div(class="val", textOutput("total_pa")),
                               div(class="lbl", "Plate Appearances"))),
                 column(4, div(class="stat-box",
                               div(class="val", textOutput("pitch_types_n")),
                               div(class="lbl", "Pitch Types Used")))
               ),
               
               tabsetPanel(
                 tabPanel("Count Matrix",
                          br(),
                          div(class="section-header",
                              "Most Common Pitch by Count — colored by pitch type, % shown in cell"),
                          div(class="table-wrap", uiOutput("count_matrix_ui")),
                          br(),
                          uiOutput("legend_ui")
                 ),
                 tabPanel("Usage % by Count",
                          br(),
                          div(class="section-header", "Pitch Type Usage % for Every Count"),
                          plotOutput("bar_chart", height = "480px")
                 ),
                 tabPanel("Detail Table",
                          br(),
                          div(class="section-header", "Pitch % by Count — all pitch types"),
                          DTOutput("detail_table")
                 ),
                 tabPanel("Overall Usage",
                          br(),
                          div(class="section-header", "Overall Pitch Mix"),
                          plotOutput("overall_bar", height = "340px"),
                          br(),
                          DTOutput("overall_table")
                 ),
                 tabPanel("Raw Data",
                          br(),
                          DTOutput("raw_table")
                 )
               )
        )
      )
  )
)

# Server
server <- function(input, output, session) {
  
  # Pitcher dropdown — updates when team changes
  observe({
    req(input$team)
    pitchers <- raw_master %>%
      filter(PitcherTeam == input$team) %>%
      pull(Pitcher) %>% unique() %>% sort()
    updateSelectInput(session, "pitcher", choices = pitchers, selected = pitchers[1])
  })
  
  # Filtered data for selected pitcher
  pitcher_data <- reactive({
    req(input$pitcher, input$team)
    d <- raw_master %>% filter(PitcherTeam == input$team, Pitcher == input$pitcher)
    
    if (!is.null(input$batter_side) && input$batter_side != "ALL")
      d <- d %>% filter(BatterSide == input$batter_side)
    
    if (!is.null(input$inning_group)) {
      if      (input$inning_group == "early")  d <- d %>% filter(Inning %in% 1:3)
      else if (input$inning_group == "middle") d <- d %>% filter(Inning %in% 4:6)
      else if (input$inning_group == "late")   d <- d %>% filter(Inning >= 7)
      else if (input$inning_group == "custom" && !is.null(input$inning_custom))
        d <- d %>% filter(Inning %in% as.numeric(input$inning_custom))
    }
    
    if (!is.null(input$tto) && input$tto != "ALL")
      d <- d %>% filter(tto == input$tto)
    
    d
  })
  
  # Pitch-by-count summary
  pbc <- reactive({
    pitcher_data() %>%
      filter(Count %in% input$counts_filter) %>%
      group_by(Count, AutoPitchType) %>%
      summarise(n = n(), .groups = "drop") %>%
      group_by(Count) %>%
      mutate(total = sum(n), pct = round(n / total * 100, 1)) %>%
      ungroup() %>%
      filter(total >= input$min_pitches) %>%
      mutate(Count = factor(Count, levels = count_levels))
  })
  
  # Most common pitch per count
  top_by_count <- reactive({
    pbc() %>%
      group_by(Count) %>%
      slice_max(order_by = pct, n = 1, with_ties = FALSE) %>%
      ungroup()
  })
  
  # Stat boxes
  output$total_pitches <- renderText(nrow(pitcher_data()))
  output$total_pa <- renderText({
    pa_col <- if ("PAofInning" %in% names(pitcher_data())) "PAofInning" else NULL
    if (is.null(pa_col)) return("N/A")
    pitcher_data() %>%
      distinct(Inning, PAofInning) %>% nrow()
  })
  output$pitch_types_n <- renderText(
    pitcher_data() %>% pull(AutoPitchType) %>% unique() %>% length()
  )
  
  # Count Matrix (HTML)
  output$count_matrix_ui <- renderUI({
    top <- top_by_count()
    if (nrow(top) == 0) return(p("Not enough data.", style="color:#888;"))
    
    balls_vals   <- 0:3
    strikes_vals <- 0:2
    
    rows_html <- paste0(sapply(balls_vals, function(b) {
      cells <- paste0(sapply(strikes_vals, function(s) {
        cnt <- paste0(b, "-", s)
        row <- top %>% filter(as.character(Count) == cnt)
        if (nrow(row) == 0) {
          '<td class="empty-cell">—</td>'
        } else {
          pt  <- row$AutoPitchType[1]
          pct <- row$pct[1]
          n   <- row$n[1]
          tot <- row$total[1]
          col <- pitch_colors[pt]
          if (is.na(col)) col <- "#AAAAAA"
          sprintf(
            '<td style="background:%s22; border-left: 4px solid %s;">
               <strong style="font-size:1em;">%s</strong><br>
               <span style="font-size:0.88em; color:#333;">%s%% (%d/%d)</span>
             </td>',
            col, col, pt, pct, n, tot
          )
        }
      }), collapse="")
      sprintf('<tr><td class="row-label">%d Balls</td>%s</tr>', b, cells)
    }), collapse="")
    
    strike_headers <- paste0(
      sapply(strikes_vals, function(s)
        sprintf('<th>%d Strike%s</th>', s, ifelse(s==1,"","s"))
      ), collapse=""
    )
    
    HTML(sprintf('
      <table class="count-matrix">
        <thead><tr><th></th>%s</tr></thead>
        <tbody>%s</tbody>
      </table>
    ', strike_headers, rows_html))
  })
  
  # Legend
  output$legend_ui <- renderUI({
    pitch_types <- pitcher_data() %>% pull(AutoPitchType) %>% unique() %>% sort()
    items <- lapply(pitch_types, function(pt) {
      col <- pitch_colors[pt]; if (is.na(col)) col <- "#AAAAAA"
      span(style = "margin-right:16px; font-size:0.88em; color:#e6edf3;",
           tags$span(class="legend-box", style=paste0("background:", col, ";")), pt)
    })
    div(style="margin-top:8px;", items)
  })
  
  # Stacked bar chart
  output$bar_chart <- renderPlot({
    d <- pbc()
    validate(need(nrow(d) > 0, "No data for selected filters."))
    types_in_data <- sort(unique(d$AutoPitchType))
    col_scale <- pitch_colors[types_in_data]
    col_scale[is.na(col_scale)] <- "#AAAAAA"
    
    ggplot(d, aes(x = Count, y = pct, fill = AutoPitchType)) +
      geom_col(position = "stack", width = 0.7, color = "white", linewidth = 0.3) +
      geom_text(aes(label = ifelse(pct >= 8, paste0(pct, "%"), "")),
                position = position_stack(vjust = 0.5),
                size = 3.2, color = "white", fontface = "bold") +
      scale_fill_manual(values = col_scale, name = "Pitch Type") +
      scale_y_continuous(labels = scales::percent_format(scale = 1),
                         limits = c(0, 105), expand = c(0, 0)) +
      labs(title    = paste0(input$pitcher, " — Pitch Usage % by Count"),
           subtitle = "AutoPitchType | Labels shown when >= 8%",
           x = "Count (Balls-Strikes)", y = "Usage %") +
      theme_minimal(base_size = 13) +
      theme(
        plot.background    = element_rect(fill = "#0d1117", color = NA),
        panel.background   = element_rect(fill = "#0d1117", color = NA),
        panel.grid.major.x = element_blank(),
        panel.grid.minor   = element_blank(),
        panel.grid.major.y = element_line(color = "#30363d"),
        plot.title         = element_text(color = "#58a6ff", face = "bold", size = 14),
        plot.subtitle      = element_text(color = "#8b949e", size = 11),
        axis.text          = element_text(color = "#e6edf3", size = 11),
        axis.title         = element_text(color = "#8b949e"),
        legend.background  = element_rect(fill = "#161b22", color = NA),
        legend.text        = element_text(color = "#e6edf3"),
        legend.title       = element_text(color = "#8b949e")
      )
  }, bg = "#0d1117")
  
  # Detail table
  output$detail_table <- renderDT({
    d <- pbc() %>%
      select(Count, AutoPitchType, n, total, pct) %>%
      arrange(Count, desc(pct)) %>%
      rename(`Pitch Type` = AutoPitchType,
             `# Pitches` = n,
             `Total in Count` = total,
             `Usage %` = pct)
    datatable(d, options = list(pageLength = 20, scrollX = TRUE, dom = "ftp"),
              rownames = FALSE, class = "display compact") %>%
      formatStyle("Usage %",
                  background = styleColorBar(c(0,100), "#1f6feb"),
                  backgroundSize = "100% 88%",
                  backgroundRepeat = "no-repeat",
                  backgroundPosition = "center") %>%
      formatStyle(columns = 0:4, color = "#e6edf3", backgroundColor = "#0d1117")
  })
  
  # Overall usage
  overall_usage <- reactive({
    pitcher_data() %>%
      group_by(AutoPitchType) %>%
      summarise(n = n(), .groups = "drop") %>%
      mutate(pct = round(n / sum(n) * 100, 1)) %>%
      arrange(desc(pct))
  })
  
  output$overall_bar <- renderPlot({
    d <- overall_usage()
    validate(need(nrow(d) > 0, "No data."))
    col_scale <- pitch_colors[d$AutoPitchType]
    col_scale[is.na(col_scale)] <- "#AAAAAA"
    
    ggplot(d, aes(x = reorder(AutoPitchType, pct), y = pct, fill = AutoPitchType)) +
      geom_col(show.legend = FALSE) +
      geom_text(aes(label = paste0(pct, "%")),
                hjust = -0.2, size = 4, color = "#e6edf3", fontface = "bold") +
      scale_fill_manual(values = col_scale) +
      scale_y_continuous(limits = c(0, max(d$pct) * 1.18)) +
      coord_flip() +
      labs(title = paste0(input$pitcher, " — Overall Pitch Mix"), x = NULL, y = "Usage %") +
      theme_minimal(base_size = 13) +
      theme(
        plot.background    = element_rect(fill = "#0d1117", color = NA),
        panel.background   = element_rect(fill = "#0d1117", color = NA),
        panel.grid.minor   = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.major.x = element_line(color = "#30363d"),
        plot.title  = element_text(color = "#58a6ff", face = "bold"),
        axis.text   = element_text(color = "#e6edf3", size = 12),
        axis.title  = element_text(color = "#8b949e")
      )
  }, bg = "#0d1117")
  
  output$overall_table <- renderDT({
    d <- overall_usage() %>%
      rename(`Pitch Type` = AutoPitchType, `# Pitches` = n, `Usage %` = pct)
    datatable(d, options = list(pageLength = 15, dom = "t"),
              rownames = FALSE, class = "display compact") %>%
      formatStyle("Usage %",
                  background = styleColorBar(c(0,100), "#1f6feb"),
                  backgroundSize = "100% 88%", backgroundRepeat = "no-repeat",
                  backgroundPosition = "center") %>%
      formatStyle(columns = 0:2, color = "#e6edf3", backgroundColor = "#0d1117")
  })
  
  # Raw data
  output$raw_table <- renderDT({
    cols <- c("PitchofPA", "Inning", "PAofInning", "PitchofPA",
              "Balls", "Strikes", "Count", "AutoPitchType", "PitchCall",
              "RelSpeed", "SpinRate", "InducedVertBreak", "HorzBreak")
    cols_exist <- intersect(cols, names(pitcher_data()))
    datatable(pitcher_data()[, cols_exist],
              options = list(pageLength = 20, scrollX = TRUE, dom = "lftp"),
              rownames = FALSE, class = "display compact") %>%
      formatStyle(columns = seq_along(cols_exist) - 1,
                  color = "#e6edf3", backgroundColor = "#0d1117")
  })
}

shinyApp(ui = ui, server = server)