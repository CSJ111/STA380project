# Ensure these packages are installed: install.packages(c("shiny", "bslib", "dplyr", "ggplot2"))
library(shiny)
library(bslib)
library(dplyr)
library(ggplot2)

# --- UI Definition ---
ui <- page_sidebar(
  title = "STA380 Project: Permutation Test on ECG Age Gap",
  theme = bs_theme(version = 5, bootswatch = "flatly"),
  
  # Sidebar: Input Controls
  sidebar = sidebar(
    numericInput("seed", "1. Random Seed", value = 123),
    selectInput("B", "2. Number of Permutations (B)", 
                choices = c(500, 1000, 5000), selected = 1000),
    sliderInput("alpha", "3. Significance Level (\u03B1)", 
                min = 0.01, max = 0.10, value = 0.05, step = 0.01),
    selectInput("stat", "4. Test Statistic", 
                choices = c("Mean Difference", "Median Difference", "Kolmogorov-Smirnov (KS)")),
    selectInput("data_handling", "5. Repeated Exams Handling", 
                choices = c("One exam per patient (Random)", "One exam per patient (Earliest)")),
    checkboxGroupInput("display", "6. Display Options", 
                       choices = c("Permutation Plot", "Summary Table"), 
                       selected = c("Permutation Plot", "Summary Table")),
    hr(),
    actionButton("run", "Run Permutation Test", class = "btn-primary")
  ),
  
  # Main Panel: Output Results
  layout_columns(
    col_widths = 12,
    card(
      card_header("Test Results"),
      uiOutput("result_text")
    ),
    card(
      card_header("Permutation Distribution"),
      plotOutput("perm_plot")
    ),
    card(
      card_header("Group Summary Statistics"),
      tableOutput("summary_table")
    )
  )
)

# --- Server Logic ---
server <- function(input, output, session) {
  
  # 1. Read or generate data (Reactive)
  my_data <- reactive({
    if (file.exists("exams.csv")) {
      df <- read.csv("exams.csv")
    } else {
      # Generate mock data if the real dataset is not found
      set.seed(42)
      df <- data.frame(
        patient_id = sample(1:500, 800, replace = TRUE),
        AF = sample(c(TRUE, FALSE), 800, replace = TRUE, prob = c(0.15, 0.85)),
        age = rnorm(800, mean = 60, sd = 15),
        nn_predicted_age = rnorm(800, mean = 62, sd = 16)
      )
    }
    
    # Convert to numeric to prevent parsing issues
    df$age <- as.numeric(df$age)
    df$nn_predicted_age <- as.numeric(df$nn_predicted_age)
    
    # Calculate Age Gap (g)
    df$g <- df$nn_predicted_age - df$age
    
    # CRITICAL FIX: Remove rows with NA in 'g' or 'AF'
    df <- df[!is.na(df$g) & !is.na(df$AF), ]
    
    # Data Handling for repeated exams
    if (input$data_handling == "One exam per patient (Random)") {
      df <- df %>% group_by(patient_id) %>% slice_sample(n = 1) %>% ungroup()
    } else {
      # Keep the earliest record (assuming first row is earliest)
      df <- df %>% group_by(patient_id) %>% slice(1) %>% ungroup()
    }
    return(df)
  })
  
  # 2. Execute permutation test when the Run button is clicked
  perm_results <- eventReactive(input$run, {
    req(my_data())
    df <- my_data()
    set.seed(input$seed)
    B <- as.numeric(input$B)
    
    # Extract data for both groups
    g_af <- df$g[df$AF == TRUE]
    g_nonaf <- df$g[df$AF == FALSE]
    g_all <- df$g
    af_labels <- df$AF
    
    # Function to calculate the observed statistic
    calc_stat <- function(af, nonaf, type) {
      if (type == "Mean Difference") return(mean(af) - mean(nonaf))
      if (type == "Median Difference") return(median(af) - median(nonaf))
      if (type == "Kolmogorov-Smirnov (KS)") {
        suppressWarnings(return(ks.test(af, nonaf)$statistic))
      }
    }
    
    obs_stat <- calc_stat(g_af, g_nonaf, input$stat)
    
    # Run permutation loop
    perm_stats <- numeric(B)
    withProgress(message = 'Running Permutations...', value = 0, {
      for (i in 1:B) {
        shuffled_labels <- sample(af_labels)
        sim_af <- g_all[shuffled_labels == TRUE]
        sim_nonaf <- g_all[shuffled_labels == FALSE]
        perm_stats[i] <- calc_stat(sim_af, sim_nonaf, input$stat)
        
        # Update progress bar every 100 iterations
        if (i %% 100 == 0) incProgress(100/B) 
      }
    })
    
    # Calculate Two-sided P-value
    p_val <- (1 + sum(abs(perm_stats) >= abs(obs_stat))) / (B + 1)
    
    return(list(obs = obs_stat, perms = perm_stats, p_val = p_val, data = df))
  }, ignoreNULL = FALSE) # Run automatically on startup
  
  # 3. Output P-value and conclusion
  output$result_text <- renderUI({
    res <- perm_results()
    decision <- if(res$p_val < input$alpha) {
      tags$span("REJECT the null hypothesis (Statistically Significant)", style="color:red; font-weight:bold;")
    } else {
      tags$span("FAIL TO REJECT the null hypothesis", style="color:green; font-weight:bold;")
    }
    
    HTML(paste0(
      "<h4>P-value: <strong>", round(res$p_val, 4), "</strong></h4>",
      "<h4>Conclusion: ", as.character(decision), "</h4>",
      "<p>Observed Test Statistic (", input$stat, "): ", round(res$obs, 4), "</p>"
    ))
  })
  
  # 4. Output Permutation Distribution Plot
  output$perm_plot <- renderPlot({
    req("Permutation Plot" %in% input$display)
    res <- perm_results()
    
    ggplot(data.frame(x = res$perms), aes(x = x)) +
      geom_histogram(bins = 30, fill = "lightblue", color = "black", alpha = 0.7) +
      geom_vline(xintercept = res$obs, color = "red", linetype = "dashed", linewidth = 1.2) +
      geom_vline(xintercept = -res$obs, color = "red", linetype = "dashed", linewidth = 1.2) +
      theme_minimal() +
      labs(title = paste("Permutation Distribution of", input$stat),
           x = "Test Statistic under H0",
           y = "Frequency",
           caption = "Red dashed lines indicate the absolute observed statistic.")
  })
  
  # 5. Output Summary Table
  output$summary_table <- renderTable({
    req("Summary Table" %in% input$display)
    df <- perm_results()$data
    
    df %>%
      group_by(AF) %>%
      summarise(
        N = n(),
        Mean_AgeGap = mean(g),
        Median_AgeGap = median(g),
        SD_AgeGap = sd(g)
      ) %>%
      rename(`Atrial Fibrillation (AF)` = AF)
  })
}

shinyApp(ui, server)