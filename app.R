# =====================================================================
#  Canteen Food Demand & Waste Analysis Using R
#  Interactive dashboard (R Shiny + bslib + plotly + DT)
#  Run:  shiny::runApp("canteen_dashboard")   (or press "Run App" in RStudio)
# =====================================================================

# install.packages(c("shiny","bslib","dplyr","tidyr","plotly","DT","readr","scales"))
library(shiny)
library(bslib)
library(dplyr)
library(tidyr)
library(plotly)
library(DT)
library(readr)
library(scales)

# ---------------------------------------------------------------------
# 1. DATA LOADING & FEATURE ENGINEERING
# ---------------------------------------------------------------------
DATA_FILE  <- "canteen_food_demand_waste_dataset.csv"
day_levels <- c("Monday", "Tuesday", "Wednesday", "Thursday",
                "Friday", "Saturday", "Sunday")

df <- read_csv(DATA_FILE, show_col_types = FALSE) |>
  mutate(
    Date         = as.Date(Date),
    Day          = factor(Day, levels = day_levels),
    Day_Type     = if_else(Day %in% c("Saturday", "Sunday"), "Weekend", "Weekday"),
    Waste_Pct    = Waste_Quantity / Quantity_Prepared * 100,
    Sell_Through = Quantity_Sold  / Quantity_Prepared * 100,
    Waste_Value  = Waste_Quantity * Price_Per_Item_INR
  )

items <- sort(unique(df$Food_Item))

# Colour palette (one fixed colour per food item)
item_cols <- setNames(
  c("#D9480F", "#0B7285", "#F59F00", "#5F3DC4", "#2B8A3E", "#C2255C"),
  items
)
COL_PREP  <- "#94A3B8"
COL_SOLD  <- "#0B7285"
COL_WASTE <- "#D9480F"

# Average demand per item and day-type (used by the what-if simulator)
avg_demand <- df |>
  group_by(Food_Item, Day_Type) |>
  summarise(Avg_Sold = mean(Quantity_Sold), .groups = "drop")

inr  <- function(x) paste0("\u20B9", comma(round(x)))
pct1 <- function(x) paste0(round(x, 1), "%")
inr_html <- function(x) HTML(paste0("&#8377;", comma(round(x))))  # ASCII-safe rupee sign

plot_layout <- function(p, ...) {
  p |>
    layout(
      font          = list(family = "Inter, Segoe UI, sans-serif", size = 12, color = "#334155"),
      paper_bgcolor = "rgba(0,0,0,0)",
      plot_bgcolor  = "rgba(0,0,0,0)",
      margin        = list(l = 50, r = 20, t = 10, b = 40),
      legend        = list(orientation = "h", y = -0.2),
      ...
    ) |>
    config(displaylogo = FALSE,
           modeBarButtonsToRemove = c("lasso2d", "select2d", "autoScale2d"))
}

# ---------------------------------------------------------------------
# 2. USER INTERFACE
# ---------------------------------------------------------------------
theme <- bs_theme(
  version   = 5,
  bg        = "#F4F6FA",
  fg        = "#1E293B",
  primary   = "#D9480F",
  secondary = "#0B7285",
  success   = "#2B8A3E",
  danger    = "#C92A2A",
  info      = "#0B7285",
  warning   = "#F59F00",
  base_font = font_collection("Inter", "Segoe UI", "system-ui", "sans-serif")
)

css <- "
.navbar { box-shadow: 0 2px 10px rgba(0,0,0,.12); }
.bslib-sidebar-layout > .sidebar { background: #ffffff; }
.card { border: none; border-radius: 14px; box-shadow: 0 2px 12px rgba(15,23,42,.07); }
.card-header { background: transparent; font-weight: 600; border-bottom: 1px solid #EEF1F6; }
.bslib-value-box { border-radius: 14px !important; box-shadow: 0 4px 14px rgba(15,23,42,.12); }
.bslib-value-box .value-box-value { font-weight: 700; }
.insight { border-left: 5px solid #D9480F; background: #fff; border-radius: 10px;
           padding: 12px 16px; margin-bottom: 12px; box-shadow: 0 2px 8px rgba(15,23,42,.06); }
.insight.good { border-left-color: #2B8A3E; }
.insight.info { border-left-color: #0B7285; }
.insight h6 { margin: 0 0 4px 0; font-weight: 700; }
.insight p  { margin: 0; color: #475569; }
.hero { background: linear-gradient(120deg,#D9480F 0%,#F59F00 100%); color: #fff;
        border-radius: 14px; padding: 18px 24px; margin-bottom: 16px; }
.hero h3 { margin: 0; font-weight: 700; }
.hero p  { margin: 4px 0 0 0; opacity: .92; }
"

ui <- page_navbar(
  title   = "\U0001F37D\uFE0F Canteen Food Demand & Waste Analysis",
  theme   = theme,
  id      = "tabs",
  fillable = FALSE,
  header  = tags$head(tags$style(HTML(css))),

  sidebar = sidebar(
    width = 290,
    h5("Filters"),
    checkboxGroupInput("f_item", "Food item", choices = items, selected = items),
    checkboxGroupInput("f_day", "Day of week", choices = day_levels, selected = day_levels),
    dateRangeInput("f_date", "Date range",
                   start = min(df$Date), end = max(df$Date),
                   min = min(df$Date), max = max(df$Date)),
    actionButton("reset", "Reset filters", class = "btn btn-outline-secondary w-100"),
    hr(),
    downloadButton("dl", "Download filtered data", class = "btn btn-primary w-100"),
    hr(),
    tags$small(class = "text-muted",
               "Waste % = Waste \u00F7 Prepared. Waste value = Waste units \u00D7 selling price.")
  ),

  # ---------------- OVERVIEW ----------------
  nav_panel(
    "Overview",
    div(class = "hero",
        h3("Canteen performance at a glance"),
        p("Track what is prepared, what sells and what gets wasted \u2014 filter on the left to drill down.")),
    layout_columns(
      col_widths = c(3, 2, 2, 2, 3),
      value_box("Total revenue",       uiOutput("k_rev"),       theme = "primary", height = "130px"),
      value_box("Units sold",          textOutput("k_sold"),      theme = "success", height = "130px"),
      value_box("Units prepared",      textOutput("k_prep"),      theme = "secondary", height = "130px"),
      value_box("Waste %",             textOutput("k_wastepct"),  theme = "danger", height = "130px"),
      value_box("Waste value (\u20B9)", uiOutput("k_wasteval"),  theme = "warning", height = "130px")
    ),
    layout_columns(
      col_widths = c(8, 4),
      card(card_header("Daily trend: prepared vs sold vs wasted"),
           plotlyOutput("p_trend", height = "340px")),
      card(card_header("Revenue share by item"),
           plotlyOutput("p_rev_share", height = "340px"))
    ),
    layout_columns(
      col_widths = c(6, 6),
      card(card_header("Prepared vs sold by item"),
           plotlyOutput("p_item_ps", height = "320px")),
      card(card_header("Revenue by day of week"),
           plotlyOutput("p_rev_day", height = "320px"))
    )
  ),

  # ---------------- WASTE ANALYSIS ----------------
  nav_panel(
    "Waste Analysis",
    layout_columns(
      col_widths = c(6, 6),
      card(card_header("Waste % by item"),
           plotlyOutput("p_waste_item", height = "340px")),
      card(card_header("Waste units by day of week"),
           plotlyOutput("p_waste_day", height = "340px"))
    ),
    layout_columns(
      col_widths = c(7, 5),
      card(card_header("Waste % heatmap (item \u00D7 day)"),
           plotlyOutput("p_heat", height = "360px")),
      card(card_header("Waste value by item (\u20B9)"),
           plotlyOutput("p_waste_val", height = "360px"))
    )
  ),

  # ---------------- DEMAND ----------------
  nav_panel(
    "Demand Patterns",
    layout_columns(
      col_widths = c(6, 6),
      card(card_header("Prepared vs sold (points below the line = over-production)"),
           plotlyOutput("p_scatter", height = "380px")),
      card(card_header("Sell-through rate distribution by item"),
           plotlyOutput("p_box", height = "380px"))
    ),
    layout_columns(
      col_widths = c(12),
      card(card_header("Average units sold per day of week, by item"),
           plotlyOutput("p_demand_day", height = "340px"))
    )
  ),

  # ---------------- WHAT-IF ----------------
  nav_panel(
    "What-If Planner",
    card(
      card_header("Smarter preparation: plan from average demand"),
      p("The planner sets tomorrow's preparation to the item's average sales for that day-type ",
        "(weekday / weekend) plus a safety buffer. Move the slider to see how waste and lost sales change."),
      sliderInput("buffer", "Safety buffer above average demand (%)",
                  min = 0, max = 30, value = 10, step = 1, width = "100%")
    ),
    layout_columns(
      col_widths = c(3, 3, 3, 3),
      value_box("Waste now",        textOutput("w_now"),   theme = "danger", height = "130px"),
      value_box("Waste with plan",  textOutput("w_new"),   theme = "success", height = "130px"),
      value_box("Waste units saved", textOutput("w_saved"), theme = "secondary", height = "130px"),
      value_box("Revenue at risk",  uiOutput("w_lost"),  theme = "warning", height = "130px")
    ),
    layout_columns(
      col_widths = c(7, 5),
      card(card_header("Waste units: current vs planned"),
           plotlyOutput("p_whatif", height = "340px")),
      card(card_header("Recommended preparation (per day)"),
           DTOutput("t_whatif"))
    )
  ),

  # ---------------- INSIGHTS ----------------
  nav_panel(
    "Insights",
    uiOutput("insights")
  ),

  # ---------------- DATA ----------------
  nav_panel(
    "Data",
    card(card_header("Dataset (filtered)"), fill = FALSE, min_height = "820px", full_screen = TRUE,
         DTOutput("tbl"))
  ),

  nav_spacer(),
  nav_item(tags$span(class = "navbar-text small",
                     "Micro Project \u2022 R Shiny"))
)

# ---------------------------------------------------------------------
# 3. SERVER
# ---------------------------------------------------------------------
server <- function(input, output, session) {

  observeEvent(input$reset, {
    updateCheckboxGroupInput(session, "f_item", selected = items)
    updateCheckboxGroupInput(session, "f_day",  selected = day_levels)
    updateDateRangeInput(session, "f_date", start = min(df$Date), end = max(df$Date))
  })

  fd <- reactive({
    d <- df |>
      filter(Food_Item %in% input$f_item,
             Day %in% input$f_day,
             Date >= input$f_date[1], Date <= input$f_date[2])
    validate(need(nrow(d) > 0, "No data for the selected filters."))
    d
  })

  # ---- KPIs ----
  output$k_rev       <- renderUI(inr_html(sum(fd()$Sales_Revenue_INR)))
  output$k_sold      <- renderText(comma(sum(fd()$Quantity_Sold)))
  output$k_prep      <- renderText(comma(sum(fd()$Quantity_Prepared)))
  output$k_wastepct  <- renderText(pct1(sum(fd()$Waste_Quantity) / sum(fd()$Quantity_Prepared) * 100))
  output$k_wasteval  <- renderUI(inr_html(sum(fd()$Waste_Value)))

  # ---- Overview plots ----
  output$p_trend <- renderPlotly({
    d <- fd() |>
      group_by(Date) |>
      summarise(Prepared = sum(Quantity_Prepared), Sold = sum(Quantity_Sold),
                Waste = sum(Waste_Quantity), .groups = "drop")
    plot_ly(d, x = ~Date) |>
      add_lines(y = ~Prepared, name = "Prepared", line = list(color = COL_PREP, dash = "dot")) |>
      add_trace(y = ~Sold, name = "Sold", type = "scatter", mode = "lines+markers",
                line = list(color = COL_SOLD, width = 3), marker = list(color = COL_SOLD)) |>
      add_bars(y = ~Waste,     name = "Waste", marker = list(color = COL_WASTE), opacity = .55) |>
      plot_layout(yaxis = list(title = "Units"), xaxis = list(title = ""),
                  hovermode = "x unified")
  })

  output$p_rev_share <- renderPlotly({
    d <- fd() |> group_by(Food_Item) |> summarise(Rev = sum(Sales_Revenue_INR), .groups = "drop")
    plot_ly(d, labels = ~Food_Item, values = ~Rev, type = "pie", hole = 0.55,
            marker = list(colors = unname(item_cols[d$Food_Item])),
            textinfo = "label+percent",
            hovertemplate = "%{label}<br>\u20B9%{value:,.0f}<extra></extra>") |>
      plot_layout(showlegend = FALSE)
  })

  output$p_item_ps <- renderPlotly({
    d <- fd() |> group_by(Food_Item) |>
      summarise(Prepared = sum(Quantity_Prepared), Sold = sum(Quantity_Sold), .groups = "drop")
    plot_ly(d, x = ~Food_Item) |>
      add_bars(y = ~Prepared, name = "Prepared", marker = list(color = COL_PREP)) |>
      add_bars(y = ~Sold,     name = "Sold",     marker = list(color = COL_SOLD)) |>
      plot_layout(barmode = "group", xaxis = list(title = ""), yaxis = list(title = "Units"))
  })

  output$p_rev_day <- renderPlotly({
    d <- fd() |> group_by(Day) |> summarise(Rev = sum(Sales_Revenue_INR), .groups = "drop")
    plot_ly(d, x = ~Day, y = ~Rev, type = "bar",
            marker = list(color = "#D9480F"),
            hovertemplate = "%{x}<br>\u20B9%{y:,.0f}<extra></extra>") |>
      plot_layout(xaxis = list(title = ""), yaxis = list(title = "Revenue (\u20B9)"))
  })

  # ---- Waste plots ----
  output$p_waste_item <- renderPlotly({
    d <- fd() |> group_by(Food_Item) |>
      summarise(WastePct = sum(Waste_Quantity) / sum(Quantity_Prepared) * 100, .groups = "drop") |>
      arrange(desc(WastePct))
    plot_ly(d, x = ~reorder(Food_Item, -WastePct), y = ~WastePct, type = "bar",
            marker = list(color = unname(item_cols[as.character(d$Food_Item)])),
            text = ~paste0(round(WastePct, 1), "%"), textposition = "outside",
            hovertemplate = "%{x}<br>%{y:.1f}% wasted<extra></extra>") |>
      plot_layout(xaxis = list(title = ""), yaxis = list(title = "Waste %"))
  })

  output$p_waste_day <- renderPlotly({
    d <- fd() |> group_by(Day) |>
      summarise(Waste = sum(Waste_Quantity), Sold = sum(Quantity_Sold), .groups = "drop")
    plot_ly(d, x = ~Day) |>
      add_bars(y = ~Sold,  name = "Sold",  marker = list(color = COL_SOLD)) |>
      add_bars(y = ~Waste, name = "Waste", marker = list(color = COL_WASTE)) |>
      plot_layout(barmode = "stack", xaxis = list(title = ""), yaxis = list(title = "Units"))
  })

  output$p_heat <- renderPlotly({
    m <- fd() |>
      group_by(Food_Item, Day) |>
      summarise(W = sum(Waste_Quantity) / sum(Quantity_Prepared) * 100, .groups = "drop") |>
      complete(Food_Item, Day) |>
      pivot_wider(names_from = Day, values_from = W)
    days_present <- intersect(day_levels, names(m))
    z <- as.matrix(m[, days_present])
    plot_ly(x = days_present, y = m$Food_Item, z = z, type = "heatmap",
            colorscale = list(c(0, "#FFF4E6"), c(0.5, "#F59F00"), c(1, "#C92A2A")),
            hovertemplate = "%{y} on %{x}<br>%{z:.1f}% waste<extra></extra>",
            colorbar = list(title = "Waste %")) |>
      plot_layout(xaxis = list(title = ""), yaxis = list(title = ""))
  })

  output$p_waste_val <- renderPlotly({
    d <- fd() |> group_by(Food_Item) |>
      summarise(V = sum(Waste_Value), .groups = "drop") |> arrange(V)
    plot_ly(d, y = ~reorder(Food_Item, V), x = ~V, type = "bar", orientation = "h",
            marker = list(color = unname(item_cols[as.character(d$Food_Item)])),
            hovertemplate = "%{y}<br>\u20B9%{x:,.0f}<extra></extra>") |>
      plot_layout(xaxis = list(title = "Wasted value (\u20B9)"), yaxis = list(title = ""))
  })

  # ---- Demand plots ----
  output$p_scatter <- renderPlotly({
    d <- fd()
    lim <- max(d$Quantity_Prepared) * 1.05
    plot_ly(d, x = ~Quantity_Prepared, y = ~Quantity_Sold, color = ~Food_Item, colors = item_cols,
            type = "scatter", mode = "markers",
            marker = list(size = ~pmax(Waste_Quantity, 4) / 1.5 + 6, opacity = .8,
                          line = list(color = "white", width = 1)),
            text = ~paste0(Food_Item, " (", format(Date, "%d %b"), ")<br>Waste: ", Waste_Quantity),
            hovertemplate = "%{text}<br>Prepared %{x}<br>Sold %{y}<extra></extra>") |>
      add_lines(x = c(0, lim), y = c(0, lim), inherit = FALSE, showlegend = FALSE,
                line = list(color = "#94A3B8", dash = "dash"), hoverinfo = "skip") |>
      plot_layout(xaxis = list(title = "Quantity prepared"), yaxis = list(title = "Quantity sold"))
  })

  output$p_box <- renderPlotly({
    d <- fd()
    plot_ly(d, x = ~Food_Item, y = ~Sell_Through, color = ~Food_Item, colors = item_cols,
            type = "box", boxpoints = "all", jitter = .4, pointpos = 0) |>
      plot_layout(showlegend = FALSE, xaxis = list(title = ""),
                  yaxis = list(title = "Sell-through (%)"))
  })

  output$p_demand_day <- renderPlotly({
    d <- fd() |> group_by(Food_Item, Day) |>
      summarise(Avg = mean(Quantity_Sold), .groups = "drop")
    plot_ly(d, x = ~Day, y = ~Avg, color = ~Food_Item, colors = item_cols,
            type = "scatter", mode = "lines+markers",
            hovertemplate = "%{x}<br>%{y:.0f} units<extra></extra>") |>
      plot_layout(xaxis = list(title = ""), yaxis = list(title = "Avg units sold"))
  })

  # ---- What-if planner ----
  sim <- reactive({
    fd() |>
      left_join(avg_demand, by = c("Food_Item", "Day_Type")) |>
      mutate(
        New_Prep  = ceiling(Avg_Sold * (1 + input$buffer / 100)),
        New_Sold  = pmin(Quantity_Sold, New_Prep),
        New_Waste = New_Prep - New_Sold,
        Lost_Rev  = (Quantity_Sold - New_Sold) * Price_Per_Item_INR
      )
  })

  output$w_now   <- renderText(paste0(comma(sum(sim()$Waste_Quantity)), " units"))
  output$w_new   <- renderText(paste0(comma(sum(sim()$New_Waste)), " units"))
  output$w_saved <- renderText({
    s <- sum(sim()$Waste_Quantity) - sum(sim()$New_Waste)
    paste0(comma(s), " (", pct1(s / sum(sim()$Waste_Quantity) * 100), ")")
  })
  output$w_lost  <- renderUI(inr_html(sum(sim()$Lost_Rev)))

  output$p_whatif <- renderPlotly({
    d <- sim() |> group_by(Food_Item) |>
      summarise(Current = sum(Waste_Quantity), Planned = sum(New_Waste), .groups = "drop")
    plot_ly(d, x = ~Food_Item) |>
      add_bars(y = ~Current, name = "Current", marker = list(color = COL_WASTE)) |>
      add_bars(y = ~Planned, name = "With plan", marker = list(color = "#2B8A3E")) |>
      plot_layout(barmode = "group", xaxis = list(title = ""), yaxis = list(title = "Waste units"))
  })

  output$t_whatif <- renderDT({
    avg_demand |>
      filter(Food_Item %in% input$f_item) |>
      mutate(Recommended = ceiling(Avg_Sold * (1 + input$buffer / 100)),
             Avg_Sold = round(Avg_Sold, 1)) |>
      rename(Item = Food_Item, `Day type` = Day_Type,
             `Avg sold` = Avg_Sold, `Prepare` = Recommended) |>
      datatable(rownames = FALSE, options = list(dom = "t", pageLength = 12))
  })

  # ---- Insights ----
  output$insights <- renderUI({
    d <- fd()
    by_item <- d |> group_by(Food_Item) |>
      summarise(WP = sum(Waste_Quantity) / sum(Quantity_Prepared) * 100,
                WV = sum(Waste_Value), Rev = sum(Sales_Revenue_INR),
                ST = sum(Quantity_Sold) / sum(Quantity_Prepared) * 100, .groups = "drop")
    by_day  <- d |> group_by(Day) |>
      summarise(WP = sum(Waste_Quantity) / sum(Quantity_Prepared) * 100,
                Rev = sum(Sales_Revenue_INR), .groups = "drop")
    by_type <- d |> group_by(Day_Type) |>
      summarise(WP = sum(Waste_Quantity) / sum(Quantity_Prepared) * 100, .groups = "drop")

    worst_item <- by_item |> slice_max(WP, n = 1, with_ties = FALSE)
    best_item  <- by_item |> slice_min(WP, n = 1, with_ties = FALSE)
    costly     <- by_item |> slice_max(WV, n = 1, with_ties = FALSE)
    top_rev    <- by_item |> slice_max(Rev, n = 1, with_ties = FALSE)
    worst_day  <- by_day  |> slice_max(WP, n = 1, with_ties = FALSE)
    best_day   <- by_day  |> slice_max(Rev, n = 1, with_ties = FALSE)

    card_i <- function(cls, title, txt) div(class = paste("insight", cls), h6(title), p(txt))

    tagList(
      layout_columns(
        col_widths = c(6, 6),
        div(
          card_i("", "Highest waste rate",
                 sprintf("%s wastes %s of what is prepared \u2014 the biggest opportunity to cut over-production.",
                         worst_item$Food_Item, pct1(worst_item$WP))),
          card_i("", "Most expensive waste",
                 sprintf("%s accounts for the highest wasted value (%s) because of its higher selling price.",
                         costly$Food_Item, inr(costly$WV))),
          card_i("", "Weakest day",
                 sprintf("%s has the highest waste rate (%s). Preparation should be reduced for this day.",
                         worst_day$Day, pct1(worst_day$WP)))
        ),
        div(
          card_i("good", "Best-managed item",
                 sprintf("%s has the lowest waste rate (%s) \u2014 its preparation level is a good benchmark.",
                         best_item$Food_Item, pct1(best_item$WP))),
          card_i("good", "Top revenue driver",
                 sprintf("%s generates the most revenue (%s). Protect its availability on peak days.",
                         top_rev$Food_Item, inr(top_rev$Rev))),
          card_i("info", "Best sales day",
                 sprintf("%s brings in the highest revenue (%s).", best_day$Day, inr(best_day$Rev)))
        )
      ),
      if (nrow(by_type) == 2) {
        wk <- by_type$WP[by_type$Day_Type == "Weekend"]
        wd <- by_type$WP[by_type$Day_Type == "Weekday"]
        card(card_header("Weekday vs weekend"),
             p(sprintf("Waste rate is %s on weekends versus %s on weekdays. ", pct1(wk), pct1(wd)),
               "Using the same preparation quantity every day does not match the real demand pattern; ",
               "use the What-If Planner tab to test day-type based preparation."))
      }
    )
  })

  # ---- Table & download ----
  output$tbl <- renderDT({
    fd() |>
      select(Date, Day, Food_Item, Quantity_Prepared, Quantity_Sold, Waste_Quantity,
             Price_Per_Item_INR, Sales_Revenue_INR, Waste_Pct) |>
      mutate(Waste_Pct = round(Waste_Pct, 1)) |>
      datatable(rownames = FALSE, filter = "top",
                options = list(pageLength = 12, scrollX = TRUE)) |>
      formatStyle("Waste_Pct",
                  background = styleColorBar(c(0, 60), "#FFC9A8"),
                  backgroundSize = "100% 70%", backgroundRepeat = "no-repeat",
                  backgroundPosition = "center")
  })

  output$dl <- downloadHandler(
    filename = function() paste0("canteen_filtered_", Sys.Date(), ".csv"),
    content  = function(file) write_csv(fd(), file)
  )
}

shinyApp(ui, server)
