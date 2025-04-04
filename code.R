pacman::p_load("shiny", "bs4Dash", "dplyr", "ggplot2", "shinyjs", "DBI", "RMySQL")

con <- dbConnect(RMySQL::MySQL(),
                 dbname = "black_scholes_app",
                 host = "localhost", 
                 port = 3306, 
                 user = "root",
                 password = "launchpad")


ui <- bs4DashPage(
  title = "Black-Scholes Pricing Model",
  header = dashboardHeader(
    title = "Black-Scholes App",
    skin = "light",
    rightUi = tags$li(
      class = "dropdown",
      actionButton(
        inputId = "reset", 
        label = NULL, 
        icon = icon("undo"), 
        style = "margin-right: 15px;" 
      )
    )
  ),
  sidebar = bs4DashSidebar(
    skin = "light",
    collapsed = FALSE,
    sidebarMenu(
      menuItem(
        "Parámetros",
        tabName = "main",
        icon = icon("calculator"),
        startExpanded = FALSE,
        fluidRow(
          column(
            width = 12,
            box(
              title = "Parámetros del Modelo",
              status = "primary",
              solidHeader = TRUE,
              width = NULL, 
              collapsible = TRUE, 
              collapsed = TRUE,
              numericInput("S", "Precio Actual del Activo (S)", 100, min = 0, step = 0.01),
              numericInput("K", "Precio Strike (K)", 100, min = 0, step = 0.01),
              numericInput("T", "Tiempo al Vencimiento (T, en años)", 1, min = 0, step = 0.01),
              numericInput("sigma", "Volatilidad (σ)", 0.2, min = 0, step = 0.01),
              numericInput("r", "Tasa Libre de Riesgo (r)", 0.05, min = 0, step = 0.01),
              tags$hr()
            )
          ),
          column(
            width = 12,
            box(
              title = "Parámetros Heatmaps",
              status = "primary",
              solidHeader = TRUE,
              width = NULL, 
              collapsible = TRUE,
              collapsed = TRUE, 
              numericInput("min_spot", "Precio Spot Mínimo", 50, min = 0, step = 0.01),
              numericInput("max_spot", "Precio Spot Máximo", 150, min = 0, step = 0.01),
              numericInput("min_volatility", "Volatilidad Mínima (σ)", 0.1, min = 0.01, step = 0.01),
              numericInput("max_volatility", "Volatilidad Máxima (σ)", 0.5, min = 0.01, step = 0.01),
              tags$hr()
            )
          )
        )
      ),
      menuItem(
        "Compra/Venta",
        tabName = "main",
        icon = icon("dollar"),
        startExpanded = FALSE,
        fluidRow(
          column(
            width = 12,
            box(
              title = "Montos",
              status = "primary",
              solidHeader = TRUE,
              width = NULL, 
              collapsible = TRUE, 
              collapsed = TRUE,
              numericInput("precio_compra_call", "Precio Call", 10, min = 0, step = 0.01),
              numericInput("precio_compra_put", "Precio Put", 10, min = 0, step = 0.01)
            )
          )
        )  
      ),
      actionButton(
        "calculate", " Calcular",
        icon = icon("play"), 
        style = "margin-top: 15px; width: 90%; margin-left: 5%;"
      )
    )
  ),
  body = bs4DashBody(
    tabItem(
      tabName = "main",
      fluidRow(
        column(
          width = 12,
          fluidRow(
            bs4ValueBoxOutput("call_price_box", width = 6),
            bs4ValueBoxOutput("put_price_box", width = 6)
          ),
          fluidRow(
            column(width = 6, plotOutput("heatmap_call", height = "450px")),
            column(width = 6, plotOutput("heatmap_put", height = "450px"))
          )
        )
      )
    )
  )
)

server <- function(input, output, session) {
  
  observeEvent(input$reset, {
    updateNumericInput(session, "S", value = 100)
    updateNumericInput(session, "sigma", value = 0.2)
    updateNumericInput(session, "K", value = 100)
    updateNumericInput(session, "T", value = 1)
    updateNumericInput(session, "r", value = 0.05)
    updateNumericInput(session, "min_spot", value = 50)
    updateNumericInput(session, "max_spot", value = 150)
    updateNumericInput(session, "min_volatility", value = 0.1)
    updateNumericInput(session, "max_volatility", value = 0.5)
    updateNumericInput(session, "precio_compra_call", value = 10)
    updateNumericInput(session, "precio_compra_put", value = 10)
  })
  
  observeEvent(input$calculate, {
    tryCatch({
      query_input <- sqlInterpolate(
        con,
        "INSERT INTO Input (StockPrice, StrikePrice, InterestRate, Volatility, TimeToExpiry, InvAmCall, InvAmPut) 
             VALUES (?StockPrice, ?StrikePrice, ?InterestRate, ?Volatility, ?TimeToExpiry, ?InvAmCall, ?InvAmPut)",
        StockPrice = input$S,
        StrikePrice = input$K,
        InterestRate = input$r,
        Volatility = input$sigma,
        TimeToExpiry = input$T,
        InvAmCall = input$precio_compra_call,
        InvAmPut = input$precio_compra_put
        
      )
      dbExecute(con, query_input)
      
      calculation_id <- dbGetQuery(con, "SELECT LAST_INSERT_ID() AS id")$id[1]
      if (is.null(calculation_id) || is.na(calculation_id)) {
        stop("Error: No se pudo obtener el ID del cálculo.")
      }
      
      S_values <- seq(input$min_spot, input$max_spot, length.out = 10)
      sigma_values <- seq(input$min_volatility, input$max_volatility, length.out = 10)
      call_prices <- outer(S_values, sigma_values, Vectorize(function(S, sigma) {
        black_scholes(S, input$K, input$T, input$r, sigma, type = "call")
      }))
      put_prices <- outer(S_values, sigma_values, Vectorize(function(S, sigma) {
        black_scholes(S, input$K, input$T, input$r, sigma, type = "put")
      }))
      
      option_types <- c("call", "put") 
      
      for (i in seq_along(S_values)) {
        for (j in seq_along(sigma_values)) {
          shock_stock_price <- ((S_values[i] - input$S) / input$S)
          shock_volatility <- ((sigma_values[j] - input$sigma) / input$sigma)
          
          pnl_call <- call_prices[i, j] - input$precio_compra_call
          pnl_put <- put_prices[i, j] - input$precio_compra_put
          
          for (option_type in option_types) {
            if (option_type == "call") {
              option_price <- call_prices[i, j]
              pnl <- pnl_call
              is_call <- 1
            } else {
              option_price <- put_prices[i, j]
              pnl <- pnl_put
              is_call <- 0
            }
            
            query_output <- sqlInterpolate(
              con,
              "INSERT INTO Output (OptionPrice, IsCallPut, ShockStockPrice, ShockVolatility, PnL, CalculationId) 
                     VALUES (?OptionPrice, ?IsCallPut, ?ShockStockPrice, ?ShockVolatility, ?PnL,  ?CalculationId)",
              OptionPrice = option_price,
              IsCallPut = is_call,
              ShockStockPrice = shock_stock_price,
              ShockVolatility = shock_volatility,
              PnL = pnl,
              CalculationId = calculation_id
            )
            dbExecute(con, query_output)
          }
        }
      }
      
      print(paste("Se generaron", length(S_values) * length(sigma_values), 
                  "filas en la tabla Output para CalculationId =", calculation_id))
    }, error = function(e) {
      print(paste("Error al insertar en la base de datos:", e$message))
    })
  })
  
  
  black_scholes <- function(S, K, T, r, sigma, type = "call") {
    d1 <- (log(S / K) + (r + (sigma^2) / 2) * T) / (sigma * sqrt(T))
    d2 <- d1 - sigma * sqrt(T)
    if (type == "call") {
      return(S * pnorm(d1) - K * exp(-r * T) * pnorm(d2))
    } else if (type == "put") {
      return(K * exp(-r * T) * pnorm(-d2) - S * pnorm(-d1))
    }
  }
  
  create_value_box <- function(price, title, color, icon) {
    subtitle_text <- if (!is.null(price)) {
      tags$div(
        style = "text-align: center; font-weight: bold; font-size: 24px;",
        paste("$", round(price, 2))
      )
    } else {
      NULL
    }
    
    valueBox(
      value = tags$span(style = "font-size: 18px;", title),
      subtitle = subtitle_text,
      color = color,
      icon = icon(icon)
    )
  }
  
  calculated_price_call <- reactive({
    black_scholes(input$S, input$K, input$T, input$r, input$sigma, type = "call")
  })
  
  calculated_price_put <- reactive({
    black_scholes(input$S, input$K, input$T, input$r, input$sigma, type = "put")
  })
  
  output$call_price_box <- renderValueBox({
    create_value_box(
      price = calculated_price_call(),
      title = "Precio Call",
      color = "success",
      icon = "money-bill-wave"
    )
  })
  
  output$put_price_box <- renderValueBox({
    create_value_box(
      price = calculated_price_put(),
      title = "Precio Put",
      color = "danger",
      icon = "money-bill-wave"
    )
  })
  
  calculate_pnl <- function(S_values, sigma_values, type, precio_compra) {
    prices <- outer(S_values, sigma_values, Vectorize(function(S, sigma) {
      black_scholes(S, input$K, input$T, input$r, sigma, type = type)
    }))
    
    prices_inv <- t(prices)
    prices_inverted <- prices_inv[nrow(prices_inv):1, ]
    
    if (type == "call") {
      pnl <- prices_inverted - precio_compra
    } else if (type == "put") {
      pnl <- precio_compra - prices_inverted
    }
    
    list(
      S_values = S_values,
      sigma_values = sigma_values,
      pnl = pnl
    )
  }
  
  render_heatmap <- function(heatmap_data, title, text_color_fn) {
    req(heatmap_data)
    S_values <- heatmap_data$S_values
    sigma_values <- heatmap_data$sigma_values
    pnl <- heatmap_data$pnl
    
    par(family = "sans") 
    gradient_colors <- colorRampPalette(c("#FF0C00", "yellow", "green"))(100)
    max_val <- max(pnl, na.rm = TRUE)
    breaks <- c(min(pnl), seq(0, max_val, length.out = 100), max_val)
    
    image(
      x = S_values, 
      y = sigma_values, 
      z = t(pnl), 
      col = c("red", gradient_colors),
      breaks = breaks,  
      xlab = "Precio Actual (S)",
      ylab = "Volatilidad (σ)",
      main = title,
      axes = FALSE 
    )
    
    axis(1, at = S_values, labels = round(S_values, 2), tick = TRUE, col.axis = "black", cex.axis = 0.9, tck = -0.02, las = 1) 
    axis(2, at = sigma_values, labels = round(rev(sigma_values), 2), tick = TRUE, col.axis = "black", cex.axis = 0.9, tck = -0.02, las = 1) 
    abline(h = sigma_values[1] - (sigma_values[2] - sigma_values[1])/2, col = "black", lwd = 2) 
    abline(v = S_values[1] - (S_values[2] - S_values[1])/2, col = "black", lwd = 2)
    
    for (j in 1:length(sigma_values)) { 
      for (i in 1:length(S_values)) { 
        text_color <- text_color_fn(pnl[j, i])
        text(
          x = S_values[i], 
          y = sigma_values[j],
          labels = round(pnl[j, i], 2), 
          col = text_color, 
          cex = 0.9,  
          font = 2    
        )
      }
    }
  }
  
  calculated_heatmap_call <- reactiveVal(NULL)
  calculated_heatmap_put <- reactiveVal(NULL)
  
  observeEvent(input$calculate, {
    S_values <- seq(input$min_spot, input$max_spot, length.out = 10)
    sigma_values <- seq(input$min_volatility, input$max_volatility, length.out = 10)
    
    heatmap_data_call <- calculate_pnl(S_values, sigma_values, "call", input$precio_compra_call)
    heatmap_data_put <- calculate_pnl(S_values, sigma_values, "put", input$precio_compra_put)
    
    calculated_heatmap_call(heatmap_data_call)
    calculated_heatmap_put(heatmap_data_put)
  })
  
  output$heatmap_call <- renderPlot({
    render_heatmap(
      heatmap_data = calculated_heatmap_call(),
      title = "Mapa de Calor: Opción Call",
      text_color_fn = function(value) ifelse(value > 0, "black", "white")
    )
  })
  
  output$heatmap_put <- renderPlot({
    render_heatmap(
      heatmap_data = calculated_heatmap_put(),
      title = "Mapa de Calor: Opción Put",
      text_color_fn = function(value) ifelse(value <= 0, "white", "black")
    )
  })
  onStop(function() {
    dbDisconnect(con)  
  })
}

shinyApp(ui, server)

