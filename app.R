
library(shiny)
library(bslib)
library(ggplot2)
library(scales)
library(DT)
library(glue)

# ---- helper: vectorised projection -------------------------------------------
project_vec <- function(amount, pct, yrs, annual_rate, freq){
  periods <- c(Daily = 365, Weekly = 52, Monthly = 12)[freq]
  n       <- round(pct/100 * periods)
  r       <- (1 + annual_rate/100)^(1/periods) - 1          # per-period rate
  idx     <- rep(seq_len(periods) <= n, yrs)                # contribution pattern
  grow    <- rev((1 + r)^(seq_along(idx)))                 # growth factors
  fv_each <- amount * idx * grow                           # cashflow × growth
  by_year <- split(fv_each, rep(seq_len(yrs), each = periods))
  data.frame(
    Year        = seq_len(yrs),
    Contributed = cumsum(rep(amount*n, yrs)),
    FV_Base     = cumsum(vapply(by_year, sum, numeric(1)))
  )
}

# ---- UI ----------------------------------------------------------------------
ui <- page_navbar(
  title = "Y’Investa – Savings Growth Calculator",
  theme = bs_theme(preset = "minty", primary = "#ffcb05", font_family = "Inter"),
  
  tabPanel(
    "Calculator",
    layout_columns(
      widths = c(12, 12),
      gap = "1rem",
      
      # --- inputs card ---------------------------------------------------------
      card(
        h4("Plan settings"),
        selectInput("freq", "Contribution frequency", c("Daily","Weekly","Monthly")),
        numericInput("amount", "Amount per period (UGX)", 3000, step = 100, min = 100),
        numericInput("pct", "% of periods saved", 60, min = 1, max = 100),
        sliderInput("yrs", "Projection horizon (years)", 1, 10, 5),
        numericInput("rate", "Annual interest rate (%)", 4.9, step = 0.1),
        actionButton("go", "Calculate", class = "btn-primary w-100")
      ),
      
      # --- outputs card --------------------------------------------------------
      card(
        uiOutput("summary"),
        DTOutput("tbl"),
        br(),
        plotOutput("bar", height = 300)
      )
    )
  )
)

# ---- server ------------------------------------------------------------------
server <- function(input, output, session){
  calc <- eventReactive(input$go, {
    project_vec(input$amount, input$pct, input$yrs, input$rate, input$freq)
  }, ignoreNULL = FALSE)
  
  output$summary <- renderUI({
    req(calc())
    HTML(glue("<b>{comma(input$amount)} UGX</b> every <b>{input$freq}</b>, saving <b>{input$pct}%</b> of periods over <b>{input$yrs} years</b> at <b>{round(input$rate,1)}% p.a.</b>"))
  })
  
  output$tbl <- renderDT({
    datatable(calc(), options = list(pageLength = 10, dom = 't', scrollX = TRUE), rownames = FALSE) %>%
      formatCurrency(columns = 2:3, currency = "UGX ")
  })
  
  output$bar <- renderPlot({
    d <- calc();
    ggplot(d, aes(x = factor(Year), y = FV_Base/1e6)) +
      geom_col(fill = "#ffcb05") +
      scale_y_continuous(labels = label_number(suffix = " M", accuracy = 0.01)) +
      labs(x = "Year", y = "Future Value (Millions UGX)") +
      theme_minimal(base_size = 14)
  })
}

shinyApp(ui, server)