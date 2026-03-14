# Ensure these packages are installed: install.packages(c("shiny", "bslib", "dplyr", "ggplot2"))
library(shiny)
library(bslib)
library(dplyr)
library(ggplot2)

# --- UI Definition ---
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
                       choices = c("Permutation Plot", "Summary Table", "Caveats & Assumptions"), 
                       selected = c("Permutation Plot", "Summary Table", "Caveats & Assumptions")),
    hr(),
    actionButton("run", "Run Permutation Test", class = "btn-primary")
  ),
  
  # --- Main Panel: Output Results (直接堆叠卡片，自动流式排版不重叠) ---
  
  card(
    card_header("Test Results & Effect Size"),
    uiOutput("result_text")
  ),
  
  card(
    card_header("Permutation Distribution"),
    # 直接在这里设置你需要的高度，卡片会自动被撑大！
    plotOutput("perm_plot", height = "800px"),
    full_screen = TRUE
  ),
  
  card(
    card_header("Group Summary Statistics & Data Flow"),
    tableOutput("summary_table"),
    uiOutput("data_flow_text")
  ),
  
  uiOutput("caveats_panel")
)

# --- Server Logic ---
server <- function(input, output, session) {
  
  # 1. Data Processing Pipeline
  my_data <- reactive({
    # CRITICAL FIX 1: Set seed HERE so that patient-level random sampling is reproducible
    set.seed(input$seed)
    
    if (file.exists("exams.csv")) {
      df <- read.csv("exams.csv")
    } else {
      # Mock data fallback for testing
      df <- data.frame(
        patient_id = sample(1:500, 800, replace = TRUE),
        AF = sample(c(TRUE, FALSE), 800, replace = TRUE, prob = c(0.15, 0.85)),
        age = rnorm(800, mean = 60, sd = 15),
        nn_predicted_age = rnorm(800, mean = 62, sd = 16)
      )
    }
    
    raw_n <- nrow(df)
    
    # Force convert types
    df$age <- as.numeric(as.character(df$age))
    df$nn_predicted_age <- as.numeric(as.character(df$nn_predicted_age))
    
    if (is.numeric(df$AF) || all(na.omit(df$AF) %in% c(0, 1, "0", "1"))) {
      df$AF <- as.logical(as.numeric(as.character(df$AF)))
    } else {
      df$AF <- as.logical(toupper(as.character(df$AF)))
    }
    
    # 2. compute_age_gap logic
    df$g <- df$nn_predicted_age - df$age
    
    # Filter NAs
    df_clean <- df[!is.na(df$g) & !is.na(df$AF), ]
    removed_na <- raw_n - nrow(df_clean)
    
    # 3. filter_to_unique_patient logic (CRITICAL: Done BEFORE permutation)
    if (input$data_handling == "One exam per patient (Random)") {
      df_final <- df_clean %>% group_by(patient_id) %>% slice_sample(n = 1) %>% ungroup()
    } else {
      df_final <- df_clean %>% group_by(patient_id) %>% slice(1) %>% ungroup()
    }
    removed_dups <- nrow(df_clean) - nrow(df_final)
    
    return(list(data = df_final, raw_n = raw_n, removed_na = removed_na, removed_dups = removed_dups))
  })
  
  # 4. Permutation Engine
  perm_results <- eventReactive(input$run, {
    dataset <- my_data()
    df <- dataset$data
    
    validate(
      need(nrow(df) > 0, "Error: Dataset is empty after processing."),
      need(sum(df$AF == TRUE) > 0, "Error: No AF==TRUE patients found."),
      need(sum(df$AF == FALSE) > 0, "Error: No AF==FALSE patients found.")
    )
    
    set.seed(input$seed) # Set seed again for the permutation shuffling
    B <- as.numeric(input$B)
    
    g_af <- df$g[df$AF == TRUE]
    g_nonaf <- df$g[df$AF == FALSE]
    g_all <- df$g
    af_labels <- df$AF
    
    # Calculate Observed Statistic
    calc_stat <- function(af, nonaf, type) {
      if (type == "Mean Difference") return(mean(af, na.rm=TRUE) - mean(nonaf, na.rm=TRUE))
      if (type == "Median Difference") return(median(af, na.rm=TRUE) - median(nonaf, na.rm=TRUE))
      if (type == "Kolmogorov-Smirnov (KS)") {
        suppressWarnings(return(ks.test(af, nonaf)$statistic))
      }
    }
    obs_stat <- calc_stat(g_af, g_nonaf, input$stat)
    
    # Calculate Cohen's d (Effect Size) for Mean Difference
    n1 <- length(g_af); n2 <- length(g_nonaf)
    var1 <- var(g_af, na.rm=TRUE); var2 <- var(g_nonaf, na.rm=TRUE)
    pooled_sd <- sqrt(((n1 - 1) * var1 + (n2 - 1) * var2) / (n1 + n2 - 2))
    cohens_d <- (mean(g_af, na.rm=TRUE) - mean(g_nonaf, na.rm=TRUE)) / pooled_sd
    
    # Permutation Loop
    perm_stats <- numeric(B)
    withProgress(message = 'Running Permutations...', value = 0, {
      for (i in 1:B) {
        shuffled_labels <- sample(af_labels)
        sim_af <- g_all[shuffled_labels == TRUE]
        sim_nonaf <- g_all[shuffled_labels == FALSE]
        perm_stats[i] <- calc_stat(sim_af, sim_nonaf, input$stat)
        if (i %% 100 == 0) incProgress(100/B) 
      }
    })
    
    p_val <- (1 + sum(abs(perm_stats) >= abs(obs_stat), na.rm=TRUE)) / (B + 1)
    
    return(list(obs = obs_stat, perms = perm_stats, p_val = p_val, data = df, 
                cohens_d = cohens_d, B = B, dataset_meta = dataset))
  })
  
  # --- Outputs ---
  
  output$result_text <- renderUI({
    if (input$run == 0) return(HTML("<p style='color: gray; font-style: italic;'>Please click 'Run Permutation Test' to view the results.</p>"))
    
    res <- perm_results()
    
    # CRITICAL FIX 3: Explain p-value better
    p_display <- ifelse(res$p_val < (1/res$B), paste0("< ", 1/res$B), round(res$p_val, 4))
    
    decision <- if(res$p_val < input$alpha) {
      tags$span("REJECT the null hypothesis", style="color:red; font-weight:bold;")
    } else {
      tags$span("FAIL TO REJECT the null hypothesis", style="color:green; font-weight:bold;")
    }
    
    # Format Cohen's d Interpretation
    d_mag <- abs(res$cohens_d)
    d_interp <- if(d_mag < 0.2) "Negligible" else if(d_mag < 0.5) "Small" else if(d_mag < 0.8) "Medium" else "Large"
    
    HTML(paste0(
      "<h4>Estimated Permutation P-value: <strong>", p_display, "</strong></h4>",
      "<p style='font-size: 0.9em; color: gray;'><em>Monte Carlo estimate based on B = ", res$B, " permutations.</em></p>",
      "<h4>Conclusion: ", as.character(decision), "</h4>",
      "<hr>",
      "<p><strong>Observed ", input$stat, ":</strong> ", round(res$obs, 4), " years</p>",
      "<p><strong>Standardized Effect Size (Cohen's d):</strong> ", round(res$cohens_d, 3), 
      " <em>(", d_interp, " effect)</em></p>",
      "<p style='font-size: 0.9em;'><em>Note: In large samples (N > 230,000), even small differences easily become statistically significant. Always interpret the practical magnitude (Cohen's d) alongside the p-value.</em></p>"
    ))
  })
  
  output$perm_plot <- renderPlot({
    req("Permutation Plot" %in% input$display)
    if (input$run == 0) return(NULL)
    res <- perm_results()
    
    ggplot(data.frame(x = res$perms), aes(x = x)) +
      geom_histogram(bins = 30, fill = "#3498db", color = "black", alpha = 0.7) +
      geom_vline(xintercept = res$obs, color = "#e74c3c", linetype = "dashed", linewidth = 1.2) +
      geom_vline(xintercept = -res$obs, color = "#e74c3c", linetype = "dashed", linewidth = 1.2) +
      theme_minimal() +
      labs(title = paste("Permutation Distribution of", input$stat),
           subtitle = paste("Null hypothesis: Exchangeability between AF and non-AF groups"),
           x = "Test Statistic under H0",
           y = "Frequency")
  })
  
  output$summary_table <- renderTable({
    req("Summary Table" %in% input$display)
    if (input$run == 0) return(NULL)
    df <- perm_results()$data
    
    df %>%
      group_by(AF) %>%
      summarise(
        N = n(),
        Mean_Gap = mean(g, na.rm=TRUE),
        Median_Gap = median(g, na.rm=TRUE),
        SD_Gap = sd(g, na.rm=TRUE),
        IQR_Gap = IQR(g, na.rm=TRUE) # CRITICAL FIX 5: Added IQR
      ) %>%
      rename(`Atrial Fibrillation (AF)` = AF)
  })
  
  # Data Flow tracking (Missing/Removed data)
  output$data_flow_text <- renderUI({
    req("Summary Table" %in% input$display)
    if (input$run == 0) return(NULL)
    meta <- perm_results()$dataset_meta
    HTML(paste0(
      "<div style='font-size: 0.85em; color: #555; background-color: #f8f9fa; padding: 10px; border-radius: 5px;'>",
      "<strong>Data Flow:</strong> Started with ", meta$raw_n, " records. ",
      "Removed ", meta$removed_na, " rows due to missing values. ",
      "Removed ", meta$removed_dups, " repeated exams to preserve patient independence. ",
      "Final sample size: ", nrow(meta$data), " unique patients.",
      "</div>"
    ))
  })
  
  # CRITICAL FIX 6: Caveats panel
  output$caveats_panel <- renderUI({
    req("Caveats & Assumptions" %in% input$display)
    card(
      card_header("Statistical Caveats & Assumptions"),
      HTML("
        <ul>
          <li><strong>Independence:</strong> We restrict the dataset to one exam per patient before permutation to satisfy the independence assumption of the test.</li>
          <li><strong>Interpretation of P-value:</strong> The p-value is a Monte Carlo estimate. It will fluctuate slightly if you change the random seed or the number of permutations (B).</li>
          <li><strong>Confounding Factors:</strong> A significant result indicates the age gap distribution differs between the AF and non-AF groups. However, this is an observational finding. The difference may be confounded by actual age, sex, or other comorbidities not accounted for in this simple two-sample comparison.</li>
          <li><strong>Statistic Specificity:</strong> This test specifically evaluates the chosen statistic (e.g., Mean Difference). It does not test if the entire shape of the distributions are identical unless the KS statistic is selected.</li>
        </ul>
      ")
    )
  })
}

shinyApp(ui, server)