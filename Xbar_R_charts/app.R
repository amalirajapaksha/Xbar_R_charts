library(shiny)
library(dplyr)
library(ggplot2)
library(DT)
library(readr)
library(readxl)
library(shinyjs)



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
  useShinyjs(),
  titlePanel("X̄ & R Chart"),
  
  tags$head(
    tags$style(HTML("
      /* Sidebar dark green */
.skin-blue .main-sidebar {
  background-color: #006400 !important; /* Dark green */
  color: white !important;
}
.skin-blue .main-sidebar .sidebar a,
.skin-blue .main-sidebar .sidebar h4,
.skin-blue .main-sidebar .sidebar label {
  color: white !important;
}

/* Main panel / whole page light green */
.content-wrapper, .right-side, body {
  background-color: #f2fff2 !important;  /* Very light green */
}

/* Keep boxes/well panels default (white/light gray) */
.box, .well {
  background-color: #f7f7f7 !important;
  color: #000000 !important;  /* default text color */
}
"))
  ),
  
  sidebarLayout(
    sidebarPanel(
      h4(HTML("Phase I: Add Data <small>(20–25 samples recommended)</small>")),
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
      actionButton("lock_phase1", "Lock Phase I",
                   class = "btn-warning"),
      br(), br(),
      checkboxInput(
        "highlight_ooc",
        "Highlight out-of-control rows in data",
        value = TRUE
      ),
      
      hr(),
      h4("Phase II: Add One Subgroup"),
      uiOutput("phase2_inputs"),
      
      actionButton("add_phase2", "Add Phase II Subgroup",
                   class = "btn-success"),
      br(), br(),
      actionButton("reset_all", "Reset Whole Process", class = "btn-danger")
      
    ),
    
    mainPanel(
      tabsetPanel(
        
        tabPanel("Phase I Charts",
                 uiOutput("phase1_warnings"),
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
        ),
        
        tabPanel(
          "Phase II Data",
          h4("Phase II Subgroup Data"),
          DTOutput("phase2_data_table"),
          br(),
          actionButton(
            "delete_phase2_rows",
            "Delete Selected Phase II Rows",
            class = "btn-danger"
          )
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
  
  # Phase I sample size (n)
  phase1_n <- reactive({
    df <- original_data()
    ncol(df) - 1   # exclude Subgroup column
  })
  
  
  
  empty_phase2_df <- function(n) {
    df <- data.frame(Subgroup = integer())
    for (i in 1:n) df[[paste0("X", i)]] <- numeric()
    df
  }
  
  
  phase2_data <- reactiveVal(data.frame())
  
  
  
  phase1_locked <- reactiveVal(FALSE)
  
  observeEvent(input$sample_choice, {
    original_data(sample_list[[input$sample_choice]])
    
    phase2_data(empty_phase2_df(phase1_n()))
    xbar_stats(NULL)
    
    showNotification(
      "Phase II data cleared due to new Phase I dataset",
      type = "message",
      duration = 3
    )
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
    
    phase2_data(empty_phase2_df(phase1_n()))
    xbar_stats(NULL)
    
    showNotification(
      "Phase II data cleared due to new Phase I dataset",
      type = "message",
      duration = 3
    )
    
  })
  
  
  observeEvent(input$lock_phase1, {
    phase1_locked(TRUE)
    
    showNotification(
      "Phase I locked. Phase II monitoring only.",
      type = "warning",
      duration = 4
    )
  })
  
  
  
  observeEvent(original_data(), {
    xbar_stats(NULL)
  })
  
  output$phase2_inputs <- renderUI({
    n <- phase1_n()
    req(n)
    
    lapply(1:n, function(i) {
      numericInput(paste0("p2_x", i), paste0("X", i), value = NA)
    })
  })
  
  
  
  
  
  observeEvent(input$add_phase2, {
    req(xbar_stats())
    
    n <- phase1_n()
    values <- sapply(1:n, function(i) input[[paste0("p2_x", i)]])
    
    
    validate(
      need(!any(is.na(values)), "Enter all Phase II values")
    )
    
    new_row <- data.frame(
      Subgroup = nrow(phase2_data()) + 1,
      t(values)
    )
    
    colnames(new_row)[-1] <- paste0("X", 1:n)
    
    phase2_data(bind_rows(phase2_data(), new_row))
    
    # Reset Phase II inputs
    for (i in 1:n) updateNumericInput(session, paste0("p2_x", i), value = NA)
  })
  
  
  
  # Disable Phase II inputs until X̄ is calculated
  observe({
    can_use <- !is.null(xbar_stats())
    for (i in 1:phase1_n()) {
      shinyjs::toggleState(paste0("p2_x", i), can_use)
    }
    shinyjs::toggleState("add_phase2", can_use)
  })
  
  
  
  observe({
    locked <- phase1_locked()
    
    shinyjs::toggleState("sample_choice", condition = !locked)
    shinyjs::toggleState("file_upload", condition = !locked)
    shinyjs::toggleState("delete_rows", condition = !locked)
    shinyjs::toggleState("calc_xbar", condition = !locked)
  })
  
  
  
  
  # ---- Delete selected rows ----
  observeEvent(input$delete_rows, {
    sel <- input$original_data_table_rows_selected
    if (length(sel) == 0) return()
    df <- original_data()[-sel, ]
    df$Subgroup <- 1:nrow(df)
    original_data(df)
  })
  
  
  observeEvent(input$delete_phase2_rows, {
    sel <- input$phase2_data_table_rows_selected
    if (length(sel) == 0) return()
    
    df <- phase2_data()[-sel, , drop = FALSE]
    
    # Re-index Phase II subgroup numbers
    if (nrow(df) > 0) {
      df$Subgroup <- seq_len(nrow(df))
    }
    
    phase2_data(df)
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
  
  
  
  
  phase2_stats <- reactive({
    req(nrow(phase2_data()) > 0)
    
    values <- phase2_data() %>%
      select(starts_with("X"))
    
    
    list(
      Xbar_df = data.frame(
        Subgroup = phase2_data()$Subgroup,
        Xbar = apply(values, 1, mean)
      ),
      R_df = data.frame(
        Subgroup = phase2_data()$Subgroup,
        R = apply(values, 1, function(x) max(x) - min(x))
      )
    )
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
    
    p1 <- s$R_df %>% mutate(Phase = "Phase I")
    p2 <- if (nrow(phase2_data()) > 0)
      phase2_stats()$R_df %>%
      mutate(
        Subgroup = Subgroup + nrow(p1),
        Phase = "Phase II"
      )
    
    df <- bind_rows(p1, p2)
    
    # Add OutOfControl column if not present
    df$OutOfControl <- FALSE
    df$OutOfControl[df$Subgroup <= nrow(p1)] <- s$R_df$OutOfControl
    if (!is.null(p2)) {
      R_phase2 <- phase2_stats()$R_df$R
      df$OutOfControl[df$Subgroup > nrow(p1)] <- R_phase2 > s$UCL | R_phase2 < s$LCL
    }
    
    # Define PointColor based on Phase and OutOfControl
    df$PointColor <- with(df, ifelse(OutOfControl, "red",
                                     ifelse(Phase == "Phase I", "black", "blue")))
    
    ggplot(df, aes(Subgroup, R)) +
      geom_line(aes(group = 1), color = "#1f4e79") +
      geom_point(aes(color = PointColor), size = 3) +
      scale_color_identity() +
      geom_hline(yintercept = s$Rbar, linetype = "dashed", color = "black") +
      geom_hline(yintercept = s$UCL, linetype = "solid", color = "red") +
      geom_hline(yintercept = s$LCL, linetype = "solid", color = "red") +
      annotate("text", x = 0.5, y = s$Rbar, label = "CL", hjust = 0, vjust = -0.5, color = "black") +
      annotate("text", x = 0.5, y = s$UCL, label = "UCL", hjust = 0, vjust = -0.5, color = "red") +
      annotate("text", x = 0.5, y = s$LCL, label = "LCL", hjust = 0, vjust = 1.5, color = "red") +
      {if (!is.null(p2)) geom_vline(xintercept = nrow(p1) + 0.5, linetype = "dashed", color = "gray40")} +
      labs(
        title = "R Chart (Phase I Limits, Phase II Monitoring)",
        x = "Subgroup",
        y = "Sample Range (R)"
      ) +
      theme_minimal(base_size = 13) +
      theme(plot.title = element_text(face = "bold", hjust = 0.5))
  })
  
  
  
  output$xbar_chart <- renderPlot({
    s <- xbar_stats()
    req(s)
    
    p1 <- s$Xbar_df %>% mutate(Phase = "Phase I")
    p2 <- if (nrow(phase2_data()) > 0)
      phase2_stats()$Xbar_df %>%
      mutate(
        Subgroup = Subgroup + nrow(p1),
        Phase = "Phase II"
      )
    
    df <- bind_rows(p1, p2)
    
    # Add OutOfControl column if not present
    df$OutOfControl <- FALSE
    df$OutOfControl[df$Subgroup <= nrow(p1)] <- s$Xbar_df$OutOfControl
    if (!is.null(p2)) df$OutOfControl[df$Subgroup > nrow(p1)] <- phase2_stats()$Xbar_df$Xbar > s$Xbar_UCL |
      phase2_stats()$Xbar_df$Xbar < s$Xbar_LCL
    
    # Define PointColor based on Phase and OutOfControl
    df$PointColor <- with(df, ifelse(OutOfControl, "red",
                                     ifelse(Phase == "Phase I", "black", "blue")))
    
    ggplot(df, aes(Subgroup, Xbar)) +
      # Lines for each phase
      geom_line(aes(group = 1), color = "#1f794e") +
      # Points with proper color
      geom_point(aes(color = PointColor), size = 3) +
      scale_color_identity() +
      # Horizontal lines for CL, UCL, LCL
      geom_hline(yintercept = s$Xbar_bar, linetype = "dashed", color = "black") +
      geom_hline(yintercept = s$Xbar_UCL, linetype = "solid", color = "red") +
      geom_hline(yintercept = s$Xbar_LCL, linetype = "solid", color = "red") +
      # Labels for CL, UCL, LCL
      annotate("text", x = 0.5, y = s$Xbar_bar, label = "CL", hjust = 0, vjust = -0.5, color = "black") +
      annotate("text", x = 0.5, y = s$Xbar_UCL, label = "UCL", hjust = 0, vjust = -0.5, color = "red") +
      annotate("text", x = 0.5, y = s$Xbar_LCL, label = "LCL", hjust = 0, vjust = 1.5, color = "red") +
      # Vertical line separating phases
      {if (!is.null(p2)) geom_vline(xintercept = nrow(p1) + 0.5, linetype = "dashed", color = "gray40")} +
      labs(
        title = "X̄ Chart (Phase I Limits, Phase II Monitoring)",
        x = "Subgroup",
        y = "Sample Mean (X̄)"
      ) +
      theme_minimal(base_size = 13) +
      theme(plot.title = element_text(face = "bold", hjust = 0.5))
  })
  
  
  
  observeEvent(input$reset_all, {
    
    # Reset Phase I data
    original_data(sample1)
    updateSelectInput(session, "sample_choice",
                      selected = names(sample_list)[1])
    
    # Clear Phase II data
    phase2_data(empty_phase2_df(phase1_n()))
    
    
    # Reset statistics & locks
    xbar_stats(NULL)
    phase1_locked(FALSE)
    
    # Clear Phase II input boxes
    for (i in seq_len(phase1_n())) {
      updateNumericInput(session, paste0("p2_x", i), value = NA)
    }
    
    
    showNotification(
      "Process fully reset. Ready for a new Phase I study.",
      type = "warning",
      duration = 4
    )
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
  
  output$phase2_data_table <- renderDT({
    req(nrow(phase2_data()) > 0)
    
    datatable(
      phase2_data(),
      selection = "multiple",
      options = list(pageLength = 10)
    )
  })
  
}

shinyApp(ui, server)

