# =============================================================================
# BDA400 - Data Science Tools and Techniques
# Assignment 6: Technical Analysis using R, Visualization Phase
# Author : Balwinder
#
# Portfolio dashboard built with R Shiny. The app:
#   1. Fetches historical stock data (Yahoo Finance via quantmod, with a
#      Stooq fallback and full error handling)
#   2. Visualizes it as a Candlestick, Line or Area chart (ggplot2) on a
#      Daily / Weekly / Monthly time frame
#   3. Overlays technical indicators (Moving Averages, Bollinger Bands, RSI,
#      MACD) that can be switched on and off by the user
#   4. Applies a trading rule (MA crossover, RSI, or MACD crossover) and
#      annotates the chart with Buy / Sell / Hold signals
#
# HOW TO RUN (from R or RStudio, with this file in the working directory):
#     shiny::runApp("Balwinder_BDA400_A06.R")
# =============================================================================


# =============================================================================
# TASK 1 - DATA COLLECTION AND SETUP
# =============================================================================

# ---- 1.1 Choose a data source ------------------------------------------------
# Primary source : Yahoo Finance (through quantmod::getSymbols)
# Backup source  : Stooq (CSV download) - used automatically if Yahoo fails
DATA_START <- "2010-01-01"   # fetch a long history so indicators have a
                             # "warm-up" period before the visible date range

# ---- 1.2 Install and load the necessary packages -----------------------------
# 'shiny', 'ggplot2' and 'quantmod' are required by the assignment.
# 'patchwork' is used to stack the price, RSI and MACD panels on one figure.
required_pkgs <- c("shiny", "ggplot2", "quantmod", "patchwork")
missing_pkgs  <- required_pkgs[!vapply(required_pkgs, requireNamespace,
                                        logical(1), quietly = TRUE)]
if (length(missing_pkgs) > 0) {
  install.packages(missing_pkgs)
}

suppressPackageStartupMessages({
  library(shiny)
  library(ggplot2)
  library(quantmod)    # also loads xts, zoo and TTR (SMA, EMA, RSI, MACD, BBands)
  library(patchwork)
})

# ---- 1.3 Fetch historical stock data -----------------------------------------

# Convert an xts object from quantmod into a tidy data.frame
# with the columns: Date, Open, High, Low, Close, Volume
xts_to_df <- function(x) {
  x  <- na.omit(x[, 1:5])
  df <- data.frame(Date = as.Date(index(x)), coredata(x),
                   stringsAsFactors = FALSE)
  names(df) <- c("Date", "Open", "High", "Low", "Close", "Volume")
  df[order(df$Date), ]
}

# Download data for one symbol. Tries Yahoo Finance first, then Stooq.
# Returns list(data = <data.frame>, source = <string>) or stops with a
# readable error message (handled by the Shiny server).
fetch_stock_data <- function(symbol, from = DATA_START, to = Sys.Date()) {
  symbol <- toupper(trimws(symbol))

  # Input validation
  if (!nzchar(symbol)) stop("Please enter a stock symbol.")
  if (!grepl("^[A-Z0-9.^=-]+$", symbol)) {
    stop(paste0("'", symbol, "' is not a valid stock symbol."))
  }

  errors <- character(0)

  # Source 1: Yahoo Finance via quantmod
  result <- tryCatch({
    raw <- suppressWarnings(
      getSymbols(symbol, src = "yahoo", from = from, to = to,
                 auto.assign = FALSE)
    )
    df <- xts_to_df(raw)
    if (nrow(df) == 0) stop("no rows returned")
    list(data = df, source = "Yahoo Finance")
  }, error = function(e) {
    errors <<- c(errors, paste("Yahoo Finance:", conditionMessage(e)))
    NULL
  })
  if (!is.null(result)) return(result)

  # Source 2 (fallback): Stooq CSV download (US tickers)
  result <- tryCatch({
    url <- sprintf("https://stooq.com/q/d/l/?s=%s.us&i=d", tolower(symbol))
    raw <- read.csv(url, stringsAsFactors = FALSE)
    needed <- c("Date", "Open", "High", "Low", "Close", "Volume")
    if (!all(needed %in% names(raw)) || nrow(raw) == 0) {
      stop("no usable data returned")
    }
    df <- raw[, needed]
    df$Date <- as.Date(df$Date)
    df <- na.omit(df)
    df <- df[df$Date >= as.Date(from) & df$Date <= as.Date(to), ]
    if (nrow(df) == 0) stop("no rows in the requested period")
    list(data = df[order(df$Date), ], source = "Stooq (backup)")
  }, error = function(e) {
    errors <<- c(errors, paste("Stooq:", conditionMessage(e)))
    NULL
  })
  if (!is.null(result)) return(result)

  stop(paste0("Could not download data for '", symbol, "'. ",
              paste(errors, collapse = " | ")))
}

# Aggregate daily OHLCV data to a Weekly or Monthly time frame
aggregate_ohlc <- function(df, time_frame) {
  if (time_frame == "Daily") return(df)

  key <- if (time_frame == "Weekly") {
    as.character(cut(df$Date, "week"))
  } else {
    as.character(cut(df$Date, "month"))
  }
  groups <- split(seq_len(nrow(df)), factor(key, levels = unique(key)))

  out <- do.call(rbind, lapply(groups, function(i) {
    data.frame(Date   = df$Date[max(i)],     # last trading day of the period
               Open   = df$Open[min(i)],     # first open
               High   = max(df$High[i]),     # highest high
               Low    = min(df$Low[i]),      # lowest low
               Close  = df$Close[max(i)],    # last close
               Volume = sum(df$Volume[i]))
  }))
  rownames(out) <- NULL
  out
}


# =============================================================================
# TASK 3 (helpers) - TECHNICAL INDICATORS
# Indicators are computed on the FULL history first and only then cut to the
# selected date range, so the moving averages etc. do not start with gaps.
# =============================================================================

# Simple or exponential moving average (returns NA if there is too little data)
ma_fun <- function(x, n, type) {
  if (n >= length(x)) return(rep(NA_real_, length(x)))
  if (type == "EMA") as.numeric(EMA(x, n = n)) else as.numeric(SMA(x, n = n))
}

# Add all indicator columns to the data frame
add_indicators <- function(df, p) {
  n <- nrow(df)

  # Moving averages (short and long)
  df$MA_short <- ma_fun(df$Close, p$short_n, p$ma_type)
  df$MA_long  <- ma_fun(df$Close, p$long_n,  p$ma_type)

  # Relative Strength Index
  df$RSI <- if (p$rsi_n < n) as.numeric(RSI(df$Close, n = p$rsi_n)) else NA_real_

  # MACD: macd line, signal line and histogram
  if (p$macd_slow + p$macd_sig < n) {
    m <- MACD(df$Close, nFast = p$macd_fast, nSlow = p$macd_slow,
              nSig = p$macd_sig, maType = "EMA")
    df$MACD        <- as.numeric(m[, "macd"])
    df$MACD_signal <- as.numeric(m[, "signal"])
  } else {
    df$MACD        <- NA_real_
    df$MACD_signal <- NA_real_
  }
  df$MACD_hist <- df$MACD - df$MACD_signal

  # Bollinger Bands (20 periods, 2 standard deviations)
  if (p$bb_n < n) {
    bb <- BBands(df$Close, n = p$bb_n, sd = 2)
    df$BB_dn   <- as.numeric(bb[, "dn"])
    df$BB_mavg <- as.numeric(bb[, "mavg"])
    df$BB_up   <- as.numeric(bb[, "up"])
  } else {
    df$BB_dn <- df$BB_mavg <- df$BB_up <- NA_real_
  }
  df
}


# =============================================================================
# TASK 4 (helpers) - TRADING RULES
# =============================================================================

lag1 <- function(x) c(NA, x[-length(x)])

# TRUE on the bar where series 'a' crosses ABOVE series 'b'
crosses_above <- function(a, b) {
  r <- (a > b) & (lag1(a) <= lag1(b))
  r[is.na(r)] <- FALSE
  r
}

# Generate a "Buy" / "Sell" / "Hold" signal for every bar
#   Moving Average Crossover : Buy  = short MA crosses ABOVE long MA
#                              Sell = short MA crosses BELOW long MA
#   RSI Overbought/Oversold  : Buy  = RSI crosses back ABOVE the oversold level
#                              Sell = RSI crosses back BELOW the overbought level
#   MACD Crossover           : Buy  = MACD line crosses ABOVE its signal line
#                              Sell = MACD line crosses BELOW its signal line
#   Every other bar is a "Hold".
generate_signals <- function(df, rule, p) {
  n <- nrow(df)

  if (rule == "Moving Average Crossover") {
    buy  <- crosses_above(df$MA_short, df$MA_long)
    sell <- crosses_above(df$MA_long,  df$MA_short)
  } else if (rule == "RSI Overbought/Oversold") {
    buy  <- crosses_above(df$RSI, rep(p$rsi_os, n))
    sell <- crosses_above(rep(p$rsi_ob, n), df$RSI)
  } else {  # MACD Crossover
    buy  <- crosses_above(df$MACD, df$MACD_signal)
    sell <- crosses_above(df$MACD_signal, df$MACD)
  }

  ifelse(buy, "Buy", ifelse(sell, "Sell", "Hold"))
}


# =============================================================================
# TASK 2 & 3 (helpers) - PLOTTING FUNCTIONS (ggplot2)
# =============================================================================

UP_COL <- "#2e9e6b"   # green: bullish candle / Buy
DN_COL <- "#d64545"   # red  : bearish candle / Sell

# Half-width of a candle (in days) for each time frame
bar_width <- function(time_frame) {
  switch(time_frame, Daily = 0.35, Weekly = 2, Monthly = 9)
}

# ---- Main price panel: chart type + MA/Bollinger overlays + signal labels ----
build_price_plot <- function(d, chart_type, time_frame, indicators, p,
                             show_signals, show_hold, title, xr) {
  w   <- bar_width(time_frame)
  g   <- ggplot(d)
  col_map <- character(0)    # legend colours for the overlay lines

  # ---- Base chart: Candlestick, Line or Area ----
  if (chart_type == "Line") {
    g <- g + geom_line(aes(x = Date, y = Close), colour = "#1f4e9c",
                       linewidth = 0.7)
  } else if (chart_type == "Area") {
    y_floor <- min(d$Low, na.rm = TRUE) * 0.995
    g <- g +
      geom_ribbon(aes(x = Date, ymin = y_floor, ymax = Close),
                  fill = "#1f4e9c", alpha = 0.2) +
      geom_line(aes(x = Date, y = Close), colour = "#1f4e9c", linewidth = 0.7)
  } else {  # Candlestick
    up <- d[d$Close >= d$Open, , drop = FALSE]
    dn <- d[d$Close <  d$Open, , drop = FALSE]
    g <- g +
      geom_segment(data = up, aes(x = Date, xend = Date, y = Low, yend = High),
                   colour = UP_COL) +
      geom_rect(data = up, aes(xmin = Date - w, xmax = Date + w,
                               ymin = Open, ymax = Close),
                fill = UP_COL, colour = UP_COL) +
      geom_segment(data = dn, aes(x = Date, xend = Date, y = Low, yend = High),
                   colour = DN_COL) +
      geom_rect(data = dn, aes(xmin = Date - w, xmax = Date + w,
                               ymin = Close, ymax = Open),
                fill = DN_COL, colour = DN_COL)
  }

  # ---- Overlay: Moving Averages ----
  if ("Moving Averages" %in% indicators) {
    lab_s <- paste(p$ma_type, p$short_n)
    lab_l <- paste(p$ma_type, p$long_n)
    g <- g +
      geom_line(aes(x = Date, y = MA_short, colour = lab_s),
                linewidth = 0.8, na.rm = TRUE) +
      geom_line(aes(x = Date, y = MA_long, colour = lab_l),
                linewidth = 0.8, na.rm = TRUE)
    col_map[lab_s] <- "#f28e2b"
    col_map[lab_l] <- "#9b59b6"
  }

  # ---- Overlay: Bollinger Bands ----
  if ("Bollinger Bands" %in% indicators) {
    lab_b <- paste0("Bollinger (", p$bb_n, ", 2)")
    g <- g +
      geom_ribbon(aes(x = Date, ymin = BB_dn, ymax = BB_up),
                  fill = "grey50", alpha = 0.15, na.rm = TRUE) +
      geom_line(aes(x = Date, y = BB_mavg, colour = lab_b),
                linewidth = 0.5, linetype = "dashed", na.rm = TRUE)
    col_map[lab_b] <- "grey40"
  }

  if (length(col_map) > 0) {
    g <- g + scale_colour_manual(values = col_map, name = NULL)
  }

  # ---- Trading signal annotations ----
  if (show_signals) {
    rng   <- diff(range(c(d$Low, d$High), na.rm = TRUE))
    buys  <- d[d$Signal == "Buy",  , drop = FALSE]
    sells <- d[d$Signal == "Sell", , drop = FALSE]

    buys$y_pt   <- buys$Low   - 0.04 * rng
    buys$y_txt  <- buys$Low   - 0.10 * rng
    sells$y_pt  <- sells$High + 0.04 * rng
    sells$y_txt <- sells$High + 0.10 * rng

    g <- g +
      geom_point(data = buys,  aes(x = Date, y = y_pt),
                 shape = 17, size = 3.5, colour = UP_COL) +
      geom_text(data = buys,   aes(x = Date, y = y_txt, label = Signal),
                colour = UP_COL, fontface = "bold", size = 3.4) +
      geom_point(data = sells, aes(x = Date, y = y_pt),
                 shape = 25, size = 3.5, fill = DN_COL, colour = DN_COL) +
      geom_text(data = sells,  aes(x = Date, y = y_txt, label = Signal),
                colour = DN_COL, fontface = "bold", size = 3.4)

    if (show_hold) {
      holds <- d[d$Signal == "Hold", , drop = FALSE]
      g <- g + geom_text(data = holds,
                         aes(x = Date, y = Close, label = Signal),
                         colour = "grey45", size = 2.2, vjust = -1.2)
    }
  }

  g +
    coord_cartesian(xlim = xr) +
    labs(title = title, x = NULL, y = "Price") +
    theme_minimal(base_size = 13) +
    theme(legend.position = "top",
          plot.title = element_text(face = "bold"))
}

# ---- RSI panel ----
build_rsi_plot <- function(d, p, xr) {
  ggplot(d, aes(x = Date, y = RSI)) +
    annotate("rect", xmin = -Inf, xmax = Inf, ymin = p$rsi_os, ymax = p$rsi_ob,
             fill = "grey50", alpha = 0.08) +
    geom_hline(yintercept = p$rsi_ob, linetype = "dashed", colour = DN_COL) +
    geom_hline(yintercept = p$rsi_os, linetype = "dashed", colour = UP_COL) +
    geom_line(colour = "#8e44ad", linewidth = 0.7, na.rm = TRUE) +
    coord_cartesian(xlim = xr, ylim = c(0, 100)) +
    labs(x = NULL, y = paste0("RSI (", p$rsi_n, ")")) +
    theme_minimal(base_size = 13)
}

# ---- MACD panel ----
build_macd_plot <- function(d, p, time_frame, xr) {
  w <- bar_width(time_frame)
  d$positive <- d$MACD_hist >= 0
  ggplot(d, aes(x = Date)) +
    geom_col(aes(y = MACD_hist, fill = positive), width = w * 1.8,
             alpha = 0.6, na.rm = TRUE) +
    scale_fill_manual(values = c("TRUE" = UP_COL, "FALSE" = DN_COL),
                      guide = "none") +
    geom_line(aes(y = MACD, colour = "MACD"), linewidth = 0.7, na.rm = TRUE) +
    geom_line(aes(y = MACD_signal, colour = "Signal"), linewidth = 0.7,
              na.rm = TRUE) +
    scale_colour_manual(values = c("MACD" = "#1f4e9c", "Signal" = "#f28e2b"),
                        name = NULL) +
    geom_hline(yintercept = 0, colour = "grey50") +
    coord_cartesian(xlim = xr) +
    labs(x = NULL,
         y = paste0("MACD (", p$macd_fast, ",", p$macd_slow, ",", p$macd_sig, ")")) +
    theme_minimal(base_size = 13) +
    theme(legend.position = "top")
}


# =============================================================================
# TASK 2 - SHINY APP: UI (skeleton + interactive widgets)
# =============================================================================

symbol_choices <- c("AAPL", "MSFT", "GOOGL", "AMZN", "TSLA", "NVDA", "META",
                    "SHOP.TO", "RY.TO")

ui <- fluidPage(
  tags$head(tags$style(HTML("
    .summary-box { background:#f4f6fa; border-left:4px solid #1f4e9c;
                   padding:10px 14px; margin-bottom:10px; border-radius:4px; }
    .well h4 { margin-top:0; }
  "))),

  titlePanel("Portfolio Dashboard - Technical Analysis (BDA400, Assignment 6)"),

  sidebarLayout(
    # ---------------------------- Sidebar widgets ----------------------------
    sidebarPanel(
      width = 3,

      h4("Data"),
      selectizeInput("symbol", "Stock symbol:", choices = symbol_choices,
                     selected = "AAPL",
                     options = list(create = TRUE,
                                    placeholder = "Type any ticker, e.g. AAPL")),
      dateRangeInput("date_range", "Select Date Range:",
                     start = Sys.Date() - 365, end = Sys.Date(),
                     max = Sys.Date()),
      selectInput("time_frame", "Select Time Frame:",
                  choices = c("Daily", "Weekly", "Monthly")),
      selectInput("chart_type", "Chart type:",
                  choices = c("Candlestick", "Line", "Area")),

      hr(),
      h4("Technical indicators"),
      checkboxGroupInput("technical_indicators", "Overlay (toggle on/off):",
                         choices = c("Moving Averages", "Bollinger Bands",
                                     "RSI", "MACD"),
                         selected = "Moving Averages"),

      hr(),
      h4("Trading rule"),
      selectInput("trading_rule", "Rule:",
                  choices = c("Moving Average Crossover",
                              "RSI Overbought/Oversold",
                              "MACD Crossover")),
      checkboxInput("show_signals", "Annotate Buy / Sell signals", TRUE),
      checkboxInput("show_hold", "Also label 'Hold' bars (can be crowded)", FALSE),

      # Parameters appear only when the rule or an indicator needs them
      conditionalPanel(
        condition = "input.trading_rule == 'Moving Average Crossover' ||
                     (input.technical_indicators || []).indexOf('Moving Averages') > -1",
        strong("Moving average settings"),
        fluidRow(
          column(6, numericInput("short_ma", "Short MA", 20, min = 2, max = 200, step = 1)),
          column(6, numericInput("long_ma",  "Long MA",  50, min = 3, max = 400, step = 1))
        ),
        selectInput("ma_type", "MA type:", choices = c("SMA", "EMA"))
      ),
      conditionalPanel(
        condition = "input.trading_rule == 'RSI Overbought/Oversold' ||
                     (input.technical_indicators || []).indexOf('RSI') > -1",
        strong("RSI settings"),
        fluidRow(
          column(4, numericInput("rsi_n",  "Period",     14, min = 2,  max = 100, step = 1)),
          column(4, numericInput("rsi_os", "Oversold",   30, min = 1,  max = 49,  step = 1)),
          column(4, numericInput("rsi_ob", "Overbought", 70, min = 51, max = 99,  step = 1))
        )
      ),
      conditionalPanel(
        condition = "input.trading_rule == 'MACD Crossover' ||
                     (input.technical_indicators || []).indexOf('MACD') > -1",
        strong("MACD settings"),
        fluidRow(
          column(4, numericInput("macd_fast", "Fast",   12, min = 2, max = 100, step = 1)),
          column(4, numericInput("macd_slow", "Slow",   26, min = 3, max = 200, step = 1)),
          column(4, numericInput("macd_sig",  "Signal",  9, min = 2, max = 100, step = 1))
        )
      )
    ),

    # ------------------------------ Main panel -------------------------------
    mainPanel(
      width = 9,
      tabsetPanel(
        tabPanel("Stock Chart",
                 br(),
                 uiOutput("summary"),
                 uiOutput("chart_ui"),
                 h4("Latest trading signals"),
                 tableOutput("signal_table")),
        tabPanel("Portfolio Overview",
                 br(),
                 selectizeInput("portfolio_symbols", "Stocks in portfolio:",
                                choices = symbol_choices,
                                selected = c("AAPL", "MSFT", "GOOGL"),
                                multiple = TRUE,
                                options = list(create = TRUE), width = "100%"),
                 p("Prices are rebased to 100 at the start of the selected date range ",
                   "so different stocks can be compared on the same scale."),
                 plotOutput("portfolio_chart", height = "420px"),
                 h4("Performance summary"),
                 tableOutput("portfolio_table"))
      )
    )
  )
)


# =============================================================================
# TASK 2, 3 & 4 - SHINY APP: SERVER LOGIC
# =============================================================================

server <- function(input, output, session) {

  # ---- Data layer: download once per symbol and cache for the session ------
  cache <- new.env()

  get_stock <- function(symbol) {
    key <- toupper(trimws(symbol))
    if (!is.null(cache[[key]])) return(cache[[key]])
    res <- fetch_stock_data(key)      # stops with an error message on failure
    cache[[key]] <- res
    res
  }

  # Safe wrapper: shows a notification instead of crashing the app
  safe_get_stock <- function(symbol) {
    tryCatch(get_stock(symbol), error = function(e) {
      showNotification(conditionMessage(e), type = "error", duration = 8)
      NULL
    })
  }

  stock <- reactive({
    req(input$symbol)
    safe_get_stock(input$symbol)
  })

  # ---- Indicator / rule parameters (customizable by the user) --------------
  params <- reactive({
    req(input$short_ma, input$long_ma, input$rsi_n, input$rsi_os, input$rsi_ob,
        input$macd_fast, input$macd_slow, input$macd_sig)
    list(short_n   = as.integer(input$short_ma),
         long_n    = as.integer(input$long_ma),
         ma_type   = input$ma_type,
         rsi_n     = as.integer(input$rsi_n),
         rsi_os    = input$rsi_os,
         rsi_ob    = input$rsi_ob,
         macd_fast = as.integer(input$macd_fast),
         macd_slow = as.integer(input$macd_slow),
         macd_sig  = as.integer(input$macd_sig),
         bb_n      = 20L)
  })

  # ---- Core analysis: filter -> aggregate -> indicators -> signals ---------
  analysis <- reactive({
    s <- stock()
    validate(need(!is.null(s),
                  "No data available. Check the stock symbol and your internet connection."))
    p <- params()
    validate(
      need(input$date_range[1] < input$date_range[2],
           "The start date must be earlier than the end date."),
      need(p$short_n < p$long_n,
           "The short MA period must be smaller than the long MA period."),
      need(p$macd_fast < p$macd_slow,
           "The MACD fast period must be smaller than the slow period."),
      need(p$rsi_os < p$rsi_ob,
           "The RSI oversold level must be below the overbought level.")
    )

    # Aggregate to the chosen time frame (Daily / Weekly / Monthly)
    df <- aggregate_ohlc(s$data, input$time_frame)
    validate(need(nrow(df) > max(p$long_n, p$macd_slow + p$macd_sig, p$rsi_n) + 5,
                  "Not enough history for this time frame and indicator settings."))

    # Indicators and signals use the full history ...
    df        <- add_indicators(df, p)
    df$Signal <- generate_signals(df, input$trading_rule, p)

    # ... then we keep only the selected date range for display
    view <- df[df$Date >= input$date_range[1] & df$Date <= input$date_range[2], ]
    validate(need(nrow(view) >= 5,
                  "Too few observations in the selected range. Widen the date range or use a shorter time frame."))

    list(view = view, source = s$source,
         symbol = toupper(trimws(input$symbol)), p = p)
  })

  # ---- Summary box ----------------------------------------------------------
  output$summary <- renderUI({
    a <- analysis()
    v <- a$view
    counts  <- table(factor(v$Signal, levels = c("Buy", "Sell", "Hold")))
    actions <- v[v$Signal != "Hold", , drop = FALSE]
    last_txt <- if (nrow(actions) > 0) {
      k <- nrow(actions)
      sprintf("%s on %s at %.2f", actions$Signal[k], format(actions$Date[k]),
              actions$Close[k])
    } else {
      "none in this range"
    }
    tags$div(class = "summary-box",
             tags$strong(a$symbol), " | Last close: ",
             sprintf("%.2f", tail(v$Close, 1)),
             " | Buy: ", counts[["Buy"]],
             " | Sell: ", counts[["Sell"]],
             " | Hold: ", counts[["Hold"]],
             " | Latest signal: ", last_txt,
             " | Data source: ", a$source)
  })

  # ---- Chart: height grows with the number of indicator panels shown -------
  output$chart_ui <- renderUI({
    n_extra <- sum(c("RSI", "MACD") %in% input$technical_indicators)
    plotOutput("stock_chart", height = paste0(470 + 170 * n_extra, "px"))
  })

  output$stock_chart <- renderPlot({
    a   <- analysis()
    d   <- a$view
    p   <- a$p
    ind <- input$technical_indicators
    w   <- bar_width(input$time_frame)
    xr  <- range(d$Date) + c(-w, w)

    title <- sprintf("%s - %s %s chart (%s to %s)", a$symbol, input$time_frame,
                     tolower(input$chart_type), format(min(d$Date)),
                     format(max(d$Date)))

    # Panel 1: price + overlays + signal annotations
    panels  <- list(build_price_plot(d, input$chart_type, input$time_frame, ind,
                                     p, input$show_signals, input$show_hold,
                                     title, xr))
    heights <- 3.2

    # Panel 2 / 3: optional RSI and MACD sub-charts
    if ("RSI" %in% ind) {
      panels[[length(panels) + 1]] <- build_rsi_plot(d, p, xr)
      heights <- c(heights, 1.1)
    }
    if ("MACD" %in% ind) {
      panels[[length(panels) + 1]] <- build_macd_plot(d, p, input$time_frame, xr)
      heights <- c(heights, 1.3)
    }

    print(wrap_plots(panels, ncol = 1, heights = heights))
  }, res = 96)

  # ---- Table of the most recent Buy / Sell signals --------------------------
  output$signal_table <- renderTable({
    v   <- analysis()$view
    sig <- v[v$Signal != "Hold", c("Date", "Signal", "Close"), drop = FALSE]
    if (nrow(sig) == 0) {
      return(data.frame(Message = "No Buy/Sell signals in the selected range."))
    }
    sig <- sig[order(sig$Date, decreasing = TRUE), ]
    sig <- head(sig, 15)
    data.frame(Date = format(sig$Date), Signal = sig$Signal,
               `Close Price` = sprintf("%.2f", sig$Close),
               check.names = FALSE)
  }, striped = TRUE, width = "100%")

  # ---- Portfolio overview tab ------------------------------------------------
  portfolio_data <- reactive({
    syms <- req(input$portfolio_symbols)
    validate(need(input$date_range[1] < input$date_range[2],
                  "The start date must be earlier than the end date."))

    rows <- lapply(syms, function(sym) {
      res <- safe_get_stock(sym)
      if (is.null(res)) return(NULL)
      d <- res$data
      d <- d[d$Date >= input$date_range[1] & d$Date <= input$date_range[2], ]
      if (nrow(d) < 2) return(NULL)
      data.frame(Symbol = toupper(trimws(sym)), Date = d$Date, Close = d$Close,
                 Index = 100 * d$Close / d$Close[1], stringsAsFactors = FALSE)
    })
    all <- do.call(rbind, rows)
    validate(need(!is.null(all) && nrow(all) > 0,
                  "No data available for the selected stocks and date range."))
    all
  })

  output$portfolio_chart <- renderPlot({
    all <- portfolio_data()
    ggplot(all, aes(x = Date, y = Index, colour = Symbol)) +
      geom_hline(yintercept = 100, colour = "grey60", linetype = "dashed") +
      geom_line(linewidth = 0.9) +
      labs(title = "Portfolio performance (rebased to 100)", x = NULL,
           y = "Index (start = 100)", colour = NULL) +
      theme_minimal(base_size = 13) +
      theme(legend.position = "top", plot.title = element_text(face = "bold"))
  }, res = 96)

  output$portfolio_table <- renderTable({
    all <- portfolio_data()
    do.call(rbind, lapply(split(all, all$Symbol), function(d) {
      data.frame(Symbol = d$Symbol[1],
                 `Start Price` = sprintf("%.2f", d$Close[1]),
                 `End Price`   = sprintf("%.2f", d$Close[nrow(d)]),
                 `Return (%)`  = sprintf("%.1f", 100 * (d$Close[nrow(d)] / d$Close[1] - 1)),
                 `Period High` = sprintf("%.2f", max(d$Close)),
                 `Period Low`  = sprintf("%.2f", min(d$Close)),
                 check.names = FALSE, stringsAsFactors = FALSE)
    }))
  }, striped = TRUE, width = "100%")
}


# =============================================================================
# RUN THE APP
# =============================================================================
shinyApp(ui, server)
