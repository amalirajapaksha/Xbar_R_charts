library(shiny)
library(dplyr)
library(ggplot2)
library(DT)
library(readr)
library(readxl)


set.seed(123)

# -----------------------------
# Sample datasets
# -----------------------------
sample1 <- data.frame(
  Subgroup = 1:15,
  X1 = rnorm(15, 10, 0.3),
  X2 = rnorm(15, 10, 0.3),
  X3 = rnorm(15, 10, 0.3),
  X4 = rnorm(15, 10, 0.3),
  X5 = rnorm(15, 10, 0.3)
)

sample1[5, 2]  <- sample1[5, 2] + 3
sample1[12, 6] <- sample1[12, 6] - 2.2

sample2 <- data.frame(
  Subgroup = 1:20,
  X1 = seq(9.5, 10.8, length.out = 20) + rnorm(20, 0, 0.15),
  X2 = seq(9.6, 10.9, length.out = 20) + rnorm(20, 0, 0.15),
  X3 = seq(9.7, 11.0, length.out = 20) + rnorm(20, 0, 0.15),
  X4 = seq(9.6, 10.8, length.out = 20) + rnorm(20, 0, 0.15),
  X5 = seq(9.5, 10.7, length.out = 20) + rnorm(20, 0, 0.15)
)

sample3 <- data.frame(
  Subgroup = 1:18,
  X1 = rnorm(18, 10, 0.4),
  X2 = rnorm(18, 10, 0.4),
  X3 = rnorm(18, 10, 0.4),
  X4 = rnorm(18, 10, 0.4),
  X5 = rnorm(18, 10, 0.4)
)

sample_list <- list(
  "Sample 1: Out-of-control points" = sample1,
  "Sample 2: Trend pattern" = sample2,
  "Sample 3: Random variation" = sample3
)

# -----------------------------
# UI
# -----------------------------
ui <- fluidPage(
  titlePanel("X̄ & R Chart"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput("sample_choice", "Choose Sample Dataset:",
                  choices = names(sample_list)),
      fileInput(
        "file_upload",
        "Upload CSV or Excel (Wide Format)",
        accept = c(".csv", ".xlsx")
      )
      ,
      br(),
      actionButton("calc_xbar", "Calculate X̄ Chart", class = "btn-primary"),
      br(), br(),
      checkboxInput(
        "highlight_ooc",
        "Highlight out-of-control rows in data",
        value = TRUE
      )
    ),
    
    mainPanel(
      tabsetPanel(
        
        tabPanel("Phase I Charts",
                 wellPanel(
                   h4("R Chart"),
                   plotOutput("r_chart", height = "400px"),
                   textOutput("rbar_text"),
                   textOutput("ucl_text"),
                   textOutput("lcl_text")
                 ),
                 wellPanel(
                   h4("X̄ Chart"),
                   plotOutput("xbar_chart", height = "400px"),
                   textOutput("xbar_text"),
                   textOutput("xbar_ucl_text"),
                   textOutput("xbar_lcl_text")
                 )
        ),
        
        tabPanel("Phase I Data",
                 h4("Original Data"),
                 DTOutput("original_data_table"),
                 br(),
                 h4("R Data"),
                 DTOutput("r_data_table"),
                 br(),
                 h4("X̄ Data"),
                 DTOutput("xbar_data_table"),
                 br(),
                 actionButton("delete_rows", "Delete Selected Rows",
                              class = "btn-danger")
        )
      )
    )
  )
)

# -----------------------------
# SERVER
# -----------------------------
server <- function(input, output, session) {
  
  # ---- Original dataset ----
  original_data <- reactiveVal(sample1)
  
  observeEvent(input$sample_choice, {
    original_data(sample_list[[input$sample_choice]])
  })
  
  observeEvent(input$file_upload, {
    req(input$file_upload)
    
    ext <- tools::file_ext(input$file_upload$name)
    
    if (ext == "csv") {
      df <- read_csv(input$file_upload$datapath, show_col_types = FALSE)
    } else if (ext == "xlsx") {
      df <- read_excel(input$file_upload$datapath)
    } else {
      showNotification("Unsupported file type", type = "error")
      return()
    }
    
    df <- as.data.frame(df)
    
    # ---- FORCE numeric conversion (all except first column) ----
    colnames(df)[1] <- "Subgroup"
    
    df[-1] <- lapply(df[-1], function(x) {
      as.numeric(trimws(as.character(x)))
    })
    
    df$Subgroup <- seq_len(nrow(df))
    
    original_data(df)
  })
  
  
  observeEvent(original_data(), {
    xbar_stats(NULL)
  })
  
  # ---- Delete selected rows ----
  observeEvent(input$delete_rows, {
    sel <- input$original_data_table_rows_selected
    if (length(sel) == 0) return()
    df <- original_data()[-sel, ]
    df$Subgroup <- 1:nrow(df)
    original_data(df)
  })
  
  # ---- R chart stats ----
  r_stats <- reactive({
    df <- original_data()
    values <- df %>% select(where(is.numeric)) %>% select(-Subgroup)
    
    R_vals <- apply(values, 1, function(x) max(x) - min(x))
    Rbar <- mean(R_vals)
    n <- ncol(values)
    
    d3d4 <- data.frame(
      n = 2:10,
      D3 = c(0,0,0,0,0.076,0.136,0.184,0.223,0.256),
      D4 = c(3.267,2.574,2.282,2.114,2.004,1.924,1.864,1.816,1.777)
    )
    
    if (n <= 10) {
      D3 <- d3d4$D3[d3d4$n == n]
      D4 <- d3d4$D4[d3d4$n == n]
    } else {
      D3 <- 0
      D4 <- 1 + 3.267 / sqrt(n)
    }
    
    list(
      R_df = data.frame(
        Subgroup = df$Subgroup,
        R = R_vals,
        OutOfControl = R_vals > D4 * Rbar | R_vals < D3 * Rbar
      ),
      Rbar = Rbar,
      UCL = D4 * Rbar,
      LCL = D3 * Rbar
    )
  })
  
  # ---- Xbar stats (manual) ----
  xbar_stats <- reactiveVal(NULL)
  
  observeEvent(input$calc_xbar, {
    df <- original_data()
    values <- df %>% select(where(is.numeric)) %>% select(-Subgroup)
    
    Xbar_vals <- apply(values, 1, mean)
    Xbar_bar <- mean(Xbar_vals)
    n <- ncol(values)
    
    A2_tab <- data.frame(
      n = 2:25,
      A2 = c(
        1.880, 1.023, 0.729, 0.577, 0.483, 0.419, 0.373, 0.337, 0.308, 0.285,
        0.266, 0.249, 0.235, 0.223, 0.212, 0.203, 0.194, 0.187, 0.180, 0.173,
        0.167, 0.162, 0.157, 0.153
      )
      
    )
    
    A2 <- if (n <= 25) A2_tab$A2[A2_tab$n == n] else 0.1
    Rbar <- r_stats()$Rbar
    
    xbar_stats(list(
      Xbar_df = data.frame(
        Subgroup = df$Subgroup,
        Xbar = Xbar_vals,
        OutOfControl = Xbar_vals > Xbar_bar + A2 * Rbar |
          Xbar_vals < Xbar_bar - A2 * Rbar
      ),
      Xbar_bar = Xbar_bar,
      Xbar_UCL = Xbar_bar + A2 * Rbar,
      Xbar_LCL = Xbar_bar - A2 * Rbar
    ))
  })
  
  # ---- Out-of-control subgroups (R OR X̄) ----
  ooc_subgroups <- reactive({
    df <- original_data()
    ooc <- rep(FALSE, nrow(df))
    
    r_df <- r_stats()$R_df
    ooc[r_df$OutOfControl] <- TRUE
    
    if (!is.null(xbar_stats())) {
      x_df <- xbar_stats()$Xbar_df
      ooc[x_df$OutOfControl] <- TRUE
    }
    
    ooc
  })
  
  # ---- Charts ----
  output$r_chart <- renderPlot({
    s <- r_stats()
    ggplot(s$R_df, aes(Subgroup, R)) +
      geom_line(color = "#1f4e79") +
      geom_point(aes(color = OutOfControl), size = 3) +
      geom_hline(yintercept = c(s$Rbar, s$UCL, s$LCL),
                 linetype = c("dashed","solid","solid"),
                 color = c("black","red","red")) +
      scale_color_manual(values = c("black","red")) +
      labs(
        title = "Phase I R Chart",
        x = "Subgroup",
        y = "Sample Range (R)",
        color = ""
      ) +
      theme_minimal(base_size = 13) +
      theme(plot.title = element_text(face = "bold", hjust = 0.5))
  })
  
  output$xbar_chart <- renderPlot({
    s <- xbar_stats()
    req(s)
    ggplot(s$Xbar_df, aes(Subgroup, Xbar)) +
      geom_line(color = "#1f794e") +
      geom_point(aes(color = OutOfControl), size = 3) +
      geom_hline(yintercept = c(s$Xbar_bar, s$Xbar_UCL, s$Xbar_LCL),
                 linetype = c("dashed","solid","solid"),
                 color = c("black","red","red")) +
      scale_color_manual(values = c("black","red")) +
      labs(
        title = "Phase I X̄ Chart",
        x = "Subgroup",
        y = "Sample Mean (X̄)",
        color = ""
      ) +
      theme_minimal(base_size = 13) +
      theme(plot.title = element_text(face = "bold", hjust = 0.5))
  })
  
  # ---- Text summaries ----
  output$rbar_text <- renderText(paste("R̄ =", round(r_stats()$Rbar, 3)))
  output$ucl_text  <- renderText(paste("R UCL =", round(r_stats()$UCL, 3)))
  output$lcl_text  <- renderText(paste("R LCL =", round(r_stats()$LCL, 3)))
  
  output$xbar_text     <- renderText({ req(xbar_stats()); paste("X̄ =", round(xbar_stats()$Xbar_bar, 3)) })
  output$xbar_ucl_text <- renderText({ req(xbar_stats()); paste("X̄ UCL =", round(xbar_stats()$Xbar_UCL, 3)) })
  output$xbar_lcl_text <- renderText({ req(xbar_stats()); paste("X̄ LCL =", round(xbar_stats()$Xbar_LCL, 3)) })
  
  # ---- Tables ----
  output$original_data_table <- renderDT({
    df <- original_data()
    df$OutOfControl <- ooc_subgroups()
    
    datatable(df, selection = "multiple",options = list(pageLength = 25)) %>%
      formatStyle(
        "OutOfControl",
        target = "row",
        backgroundColor = styleEqual(c(TRUE, FALSE),
                                     c("#ffcccc", "white"))
      ) %>%
      formatStyle("OutOfControl", color = "transparent")
  })
  
  output$r_data_table <- renderDT(r_stats()$R_df)
  output$xbar_data_table <- renderDT({ req(xbar_stats()); xbar_stats()$Xbar_df })
}

shinyApp(ui, server)

