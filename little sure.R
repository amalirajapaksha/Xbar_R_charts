library(shiny)
library(dplyr)
library(ggplot2)
library(DT)
library(readr)

# -----------------------------
# SAMPLE DATASETS (WIDE FORMAT)
# -----------------------------
set.seed(123)

sample1 <- data.frame(
  Subgroup = 1:15,
  X1 = rnorm(15, 10, 0.3),
  X2 = rnorm(15, 10, 0.3),
  X3 = rnorm(15, 10, 0.3),
  X4 = rnorm(15, 10, 0.3),
  X5 = rnorm(15, 10, 0.3)
)

sample1[5, 2] <- sample1[5, 2] + 3
sample1[12, 6] <- sample1[12,6] - 2.2

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
  
  titlePanel("Phase I – R Chart (Wide Format Input)"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput(
        "sample_choice",
        "Choose Sample Dataset:",
        choices = names(sample_list),
        selected = names(sample_list)[1]
      ),
      fileInput(
        "file_upload",
        "Or Upload CSV (Wide Format)",
        accept = ".csv"
      )
    ),
    
    mainPanel(
      # -----------------------------
      # Phase I Summary with R chart
      # -----------------------------
      tabsetPanel(
      tabPanel("R Chart", plotOutput("r_chart", height = "600px"),
                 wellPanel(
                   strong("Phase I Summary"),
                   textOutput("rbar_text"),
                   textOutput("ucl_text"),
                   textOutput("lcl_text")
                 )),
     
      # -----------------------------
      # Delete Subgroups & R data table
      # -----------------------------  
      
      tabPanel("R Data", 
      h4("Delete Subgroups & Recalculate"),
      DTOutput("r_data_table"),
      br(),
      actionButton(
        "delete_rows",
        "Delete Selected Subgroups",
        class = "btn-danger"
      )
    )
  )
)))

# -----------------------------
# SERVER
# -----------------------------
server <- function(input, output, session) {
  
  # Initialize with default dataset
  phase1_data <- reactiveVal(sample1)
  
  # ---- Sample dataset selection ----
  observeEvent(input$sample_choice, {
    req(input$sample_choice)
    phase1_data(sample_list[[input$sample_choice]])
  })
  
  # ---- File upload ----
  observeEvent(input$file_upload, {
    req(input$file_upload)
    uploaded <- read_csv(input$file_upload$datapath, show_col_types = FALSE)
    phase1_data(uploaded)
  })
  
  # ---- R chart calculations ----
  r_stats <- reactive({
    df <- phase1_data()
    req(df)
    
    values <- df %>% select(-Subgroup)
    
    R_vals <- apply(values, 1, function(x) max(x) - min(x))
    Rbar <- mean(R_vals)
    
    n <- ncol(values)
    
    d3d4_table <- data.frame(
      n = 2:10,
      D3 = c(0, 0, 0, 0, 0.076, 0.136, 0.184, 0.223, 0.256),
      D4 = c(3.267, 2.574, 2.282, 2.114, 2.004, 1.924, 1.864, 1.816, 1.777)
    )
    
    if (n <= 10) {
      D3 <- d3d4_table$D3[d3d4_table$n == n]
      D4 <- d3d4_table$D4[d3d4_table$n == n]
    } else {
      D3 <- 1 - 3.267 / sqrt(n)
      D4 <- 1 + 3.267 / sqrt(n)
    }
    
    UCL <- D4 * Rbar
    LCL <- D3 * Rbar
    
    out <- data.frame(
      Subgroup = df$Subgroup,
      R = R_vals,
      OutOfControl = (R_vals > UCL | R_vals < LCL)
    )
    
    list(
      df = out,
      Rbar = Rbar,
      UCL = UCL,
      LCL = LCL
    )
  })
  
  # ---- R Chart ----
  output$r_chart <- renderPlot({
    stats <- r_stats()
    df <- stats$df
    
    ggplot(df, aes(Subgroup, R)) +
      geom_line(color = "#1f4e79") +
      geom_point(aes(color = OutOfControl), size = 3) +
      geom_hline(yintercept = stats$Rbar, linetype = "dashed") +
      geom_hline(yintercept = stats$UCL, color = "red") +
      geom_hline(yintercept = stats$LCL, color = "red") +
      scale_color_manual(
        values = c("FALSE" = "black", "TRUE" = "red")
      ) +
      labs(
        title = "Phase I R Chart",
        x = "Subgroup",
        y = "Range (R)",
        color = ""
      ) +
      theme_minimal(base_size = 13) +
      theme(plot.title = element_text(face = "bold", hjust = 0.5))
  })
  
  # ---- R Data Table ----
  output$r_data_table <- renderDT({
    r_stats()$df
  }, selection = "multiple")
  
  # ---- Summary ----
  output$rbar_text <- renderText({
    paste("R̄ =", round(r_stats()$Rbar, 3))
  })
  
  output$ucl_text <- renderText({
    paste("UCL =", round(r_stats()$UCL, 3))
  })
  
  output$lcl_text <- renderText({
    paste("LCL =", round(r_stats()$LCL, 3))
  })
  
  # ---- Delete selected subgroups ----
  observeEvent(input$delete_rows, {
    selected <- input$r_data_table_rows_selected
    if (length(selected) == 0) return()
    
    remove <- r_stats()$df$Subgroup[selected]
    
    new_df <- phase1_data() %>%
      filter(!Subgroup %in% remove)
    
    phase1_data(new_df)
  })
}

shinyApp(ui, server)

