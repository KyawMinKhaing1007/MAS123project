# <<<<< ============================================================= >>>>> 
#         Group-1          MAS-19 Kyaw Min Khaing        Group_1
#                               Presentation
#                           Kruskal-Wallis Test
# <<<<< ============================================================= >>>>> 
#Preparing Necessary Packages
library(pacman)
p_load( tidyverse, here, readxl, broom, rstatix, ggprism, ggpubr, gganimate, prettyunits, gifski, av)

here()

# <<<<< ============================================================= >>>>>
# Histogram cyl and hp
bar_cyl <- ggplot(mtcars, aes(x = factor(cyl))) +
  geom_bar(aes(y = after_stat(count)), fill = "steelblue", color = "black") +
  labs(title = "Animated Histogram of Cylinders", x = "Cylinders", y = "Frequency") 
print(bar_cyl)
hist_hp <- ggplot(mtcars, aes(x = hp)) +
  geom_histogram(binwidth = 25, fill = "steelblue", color = "black", boundary = 0) +
  labs(
    title = "Animated Histogram of Horsepower (HP)",
    x = "Horsepower",
    y = "Frequency")
print(hist_hp)
# <<<<< ============================================================= >>>>>
# Animated QQ plot 45line
animated_qq <- ggplot(mtcars) +
  aes(sample = hp, frame = cyl) + # `frame` for animation states based on cyl
  stat_qq() +
  stat_qq_line() +
  labs(
    title = "Animated QQ Plot",
    subtitle = "Cylinder Group: {closest_state}",
    x = "Theoretical Quantiles",
    y = "Sample Quantiles"
  ) +
  transition_states(factor(cyl), transition_length = 1, state_length = 1) +  
  enter_fade() + exit_shrink() +  # Animate by cylinder groups
  ease_aes('cubic-in-out')
# Animate and Save
animate(animated_qq, nframes = 100, fps = 20, width = 600, height = 400, renderer = gifski_renderer())
# <<<<< ============================================================= >>>>>
# Normality Test for Shapiro-Wilk
normal_test_signifi <- mtcars |> 
  select(hp, cyl) |> 
  pivot_longer(cols = everything(), names_to = "variable", values_to = "value") |> 
  group_by(variable) |> 
  summarise(
    shapiro_test = list(shapiro.test(value))
  ) 
print(normal_test_signifi)
# <<<<< ============================================================= >>>>>
# Kruskal-Wallis Test
data <- read_excel(here("mtcar.xlsx")) |> 
  select(cyl, hp) |> 
  mutate(cyl = as.factor(cyl)) |> 
  pivot_longer(cols = hp, names_to = "variable", values_to = "value")  # Pivot longer for hp
print(data)
# Perform Kruskal-Wallis Test
kruskal_result <- data |> 
  kruskal_test(value ~ cyl)
# Print the Kruskal-Wallis Test result
print(kruskal_result)
# <<<<< ============================================================= >>>>>
# Perform pairwise Wilcoxon Rank Sum Test with Bonferroni correction
pairwise_result <- data |> 
  pairwise_wilcox_test(value ~ cyl, p.adjust.method = "bonferroni") |> 
  add_xy_position(x = "cyl", step.increase = 0.1)
# Print the pairwise Wilcoxon test results
print(pairwise_result)
# <<<<< ============================================================= >>>>>
animated_heatmap <- ggplot(pairwise_result, aes(x = group1, y = group2, fill = p)) +
  geom_tile(color = "white") +
  scale_fill_gradient(low = "red", high = "yellow", name = "p-value") +
  geom_text(aes(label = round(p, 4)), color = "black", size = 5) +
  labs(title = "Animated Heatmap of Pairwise Wilcoxon p-values",
       subtitle = "Frame: {closest_state}",
       x = "Group 1", y = "Group 2") +
  theme_minimal() +
  transition_states(group1, transition_length = 2, state_length = 1) +  
  ease_aes('cubic-in-out')
animate(animated_heatmap, nframes = 50, fps = 10, width = 600, height = 500, renderer = gifski_renderer())
# <<<<< ============================================================= >>>>>
# Group data for counts to add geom text
data_count <- data  |> 
  group_by(cyl)  |> 
  summarise(n = n())
# Manually annotate p-values with proper y-position to add geom text
pairwise_annotations <- pairwise_result  |> 
  mutate(
    label = paste0("p = ", format(p, scientific = FALSE, digits = 4)), # Dynamically display p-values based on digit
    y.position = c(150, 170, 190)  # Manually set y.position for the p-values (same y positions for all comparisons)
  )
# Adjust the position of each label for distinct comparisons
pairwise_annotations <- pairwise_annotations  |> 
  mutate(
    label = case_when(
      group1 == "4" & group2 == "6" ~ paste0("p = ", format(p, scientific = FALSE, digits = 4)),
      group1 == "4" & group2 == "8" ~ paste0("p = ", format(p, scientific = FALSE, digits = 4)),
      group1 == "6" & group2 == "8" ~ paste0("p = ", format(p, scientific = FALSE, digits = 4)),
      TRUE ~ label
    ),
    y.position = c(150, 170, 190) # Adjust label position per pairwise comparison
  )
# <<<<< ============================================================= >>>>>
# Create the boxplot with p-values
fig <- ggplot(data, aes(x = cyl, y = value, fill = cyl)) +
  geom_boxplot(outlier.colour = "red", outlier.shape = 8) +
  geom_jitter(
    position = position_jitterdodge(jitter.width = 0.2, dodge.width = 0.75),
    color = "black",
    alpha = 0.6,
    size = 1.5
  ) +
  stat_summary(fun = "mean", geom = "point", shape = 4, size = 3, color = "blue") + # Add mean points
  labs(
    title = "Horsepower (hp) by Cylinder Count (cyl)",
    subtitle = paste0("Kruskal-Wallis p = ", round(kruskal_result$p, 4)),
    x = "Cylinder Count",
    y = "Horsepower (hp)"
  ) +
  geom_text(
    data = data_count,
    aes(x = cyl, y = max(data$value) * 1.05, label = paste0("n=", n)),
    inherit.aes = FALSE,
    size = 4,
    color = "black"
  ) + # Add sample size counts
  geom_text(
    data = pairwise_annotations,
    aes(x = group1, y = y.position, label = label), # Place p-values with the manually set y.position
    inherit.aes = FALSE,
    size = 4,
    color = "blue"
  ) + # Add pairwise p-value labels
  theme_minimal() +
  theme(legend.position = "none") +
  ggprism::theme_prism(base_size = 12)
# Display the plot
print(fig)
# <<<<< ============================================================= >>>>>
# Ensure required libraries are installed
# Render the animation
animated_fig <- fig +
  transition_states(cyl, transition_length = 2, state_length = 1) +
  enter_fade() +
  exit_shrink() +
  ease_aes('linear')
# Animate and save to a file
animate(animated_fig, nframes = 100, width = 900, height = 700, renderer = gifski_renderer())
# Save the animation to a specific file
anim_save("animated_figure.gif", animation = last_animation(), path = getwd())

# <<<<< ============================================================= >>>>>
p_load(shiny)
# Load Data
mtcars_data <- mtcars  |>  mutate(cyl = as.factor(cyl))

# UI
ui <- fluidPage(
  titlePanel("Shiny Web App - Statistical Analysis"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput("selected_cyl", "Select Cylinder:", choices = unique(mtcars_data$cyl), selected = "4"),
      sliderInput("binwidth", "Select Histogram Bin Width:", min = 5, max = 50, value = 25),
      actionButton("run_test", "Run Normality & Kruskal-Wallis Test"),
      downloadButton("download_plot", "Download Boxplot")
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel("Histogram",
                 plotOutput("hist_cyl"),
                 plotOutput("hist_hp")),
        
        tabPanel("QQ Plot Animation",
                 imageOutput("qq_animation")),
        
        tabPanel("Normality Test",
                 verbatimTextOutput("shapiro_result")),
        
        tabPanel("Kruskal-Wallis Test",
                 verbatimTextOutput("kruskal_result")),
        
        tabPanel("Wilcoxon Test",
                 plotOutput("heatmap_plot")),
        
        tabPanel("Boxplot",
                 plotOutput("boxplot"),
                 imageOutput("boxplot_animation"))
      )
    )
  )
)

# Server
server <- function(input, output) {
  
  # Histogram for Cylinders
  output$hist_cyl <- renderPlot({
    ggplot(mtcars_data, aes(x = cyl)) +
      geom_bar(fill = "steelblue", color = "black") +
      labs(title = "Histogram of Cylinders", x = "Cylinders", y = "Frequency")
  })
  
  # Histogram for Horsepower
  output$hist_hp <- renderPlot({
    ggplot(mtcars_data, aes(x = hp)) +
      geom_histogram(binwidth = input$binwidth, fill = "steelblue", color = "black") +
      labs(title = "Histogram of Horsepower (HP)", x = "Horsepower", y = "Frequency")
  })
  
  # QQ Plot Animation
  output$qq_animation <- renderImage({
    animated_qq <- ggplot(mtcars_data, aes(sample = hp)) +
      stat_qq() +
      stat_qq_line() +
      labs(title = "Animated QQ Plot", x = "Theoretical Quantiles", y = "Sample Quantiles") +
      transition_states(cyl, transition_length = 1, state_length = 1) +
      enter_fade() + exit_shrink() + ease_aes('cubic-in-out')
    
    anim_path <- tempfile(fileext = ".gif")
    animate(animated_qq, nframes = 100, fps = 20, width = 600, height = 400, renderer = gifski_renderer())
    anim_save(anim_path, animation = last_animation())
    
    list(src = anim_path, contentType = "image/gif")
  }, deleteFile = TRUE)
  
  # Normality Test (Shapiro-Wilk)
  observeEvent(input$run_test, {
    normal_test <- shapiro.test(mtcars_data$hp)
    output$shapiro_result <- renderPrint({ normal_test })
  })
  
  # Kruskal-Wallis Test
  observeEvent(input$run_test, {
    kruskal_result <- kruskal_test(mtcars_data, hp ~ cyl)
    output$kruskal_result <- renderPrint({ kruskal_result })
  })
  
  # Pairwise Wilcoxon Test Heatmap
  output$heatmap_plot <- renderPlot({
    pairwise_result <- pairwise_wilcox_test(mtcars_data, hp ~ cyl, p.adjust.method = "bonferroni")
    
    ggplot(pairwise_result, aes(x = group1, y = group2, fill = p)) +
      geom_tile(color = "white") +
      scale_fill_gradient(low = "red", high = "yellow", name = "p-value") +
      geom_text(aes(label = round(p, 4)), color = "black", size = 5) +
      labs(title = "Pairwise Wilcoxon p-values", x = "Group 1", y = "Group 2") +
      theme_minimal()
  })
  
  # Boxplot with Kruskal-Wallis Test
  output$boxplot <- renderPlot({
    ggplot(mtcars_data, aes(x = cyl, y = hp, fill = cyl)) +
      geom_boxplot(outlier.color = "red", outlier.shape = 8) +
      geom_jitter(position = position_jitterdodge(jitter.width = 0.2), color = "black", alpha = 0.6, size = 1.5) +
      stat_summary(fun = "mean", geom = "point", shape = 4, size = 3, color = "blue") +
      labs(title = "Horsepower by Cylinder Count", x = "Cylinder", y = "Horsepower") +
      theme_minimal()
  })
  
  # Animated Boxplot
  output$boxplot_animation <- renderImage({
    animated_fig <- ggplot(mtcars_data, aes(x = cyl, y = hp, fill = cyl)) +
      geom_boxplot(outlier.color = "red", outlier.shape = 8) +
      transition_states(cyl, transition_length = 2, state_length = 1) +
      enter_fade() + exit_shrink() + ease_aes('linear')
    
    anim_path <- tempfile(fileext = ".gif")
    animate(animated_fig, nframes = 100, width = 900, height = 700, renderer = gifski_renderer())
    anim_save(anim_path, animation = last_animation())
    
    list(src = anim_path, contentType = "image/gif")
  }, deleteFile = TRUE)
  
  # Download Boxplot
  output$download_plot <- downloadHandler(
    filename = function() { "boxplot.png" },
    content = function(file) {
      ggsave(file, plot = ggplot(mtcars_data, aes(x = cyl, y = hp, fill = cyl)) +
               geom_boxplot(outlier.color = "red", outlier.shape = 8) +
               labs(title = "Horsepower by Cylinder Count", x = "Cylinder", y = "Horsepower") +
               theme_minimal(), width = 6, height = 4, dpi = 300)
    }
  )
}

# Run App
shinyApp(ui = ui, server = server)

# <<<<< ============================================================= >>>>>(imput Excel File)

ui <- fluidPage(
  titlePanel("Kruskal-Wallis Test with Animation"),
  sidebarLayout(
    sidebarPanel(
      fileInput("file", "Upload Excel File", accept = ".xlsx"),
      actionButton("analyze", "Analyze")
    ),
    mainPanel(
      plotOutput("hist_cyl"),
      plotOutput("hist_hp"),
      imageOutput("animated_qq"),
      tableOutput("kruskal_result"),
      tableOutput("pairwise_result"),
      plotOutput("boxplot"),
      imageOutput("animated_boxplot")
    )
  )
)

server <- function(input, output) {
  data <- reactive({
    req(input$file)
    read_excel(input$file$datapath) %>%
      select(cyl, hp) %>%
      mutate(cyl = as.factor(cyl)) %>%
      pivot_longer(cols = hp, names_to = "variable", values_to = "value")
  })
  
  output$hist_cyl <- renderPlot({
    ggplot(data(), aes(x = factor(cyl))) +
      geom_bar(fill = "steelblue", color = "black") +
      labs(title = "Histogram of Cylinders", x = "Cylinders", y = "Frequency")
  })
  
  output$hist_hp <- renderPlot({
    ggplot(data(), aes(x = hp)) +
      geom_histogram(binwidth = 25, fill = "steelblue", color = "black") +
      labs(title = "Histogram of Horsepower", x = "Horsepower", y = "Frequency")
  })
  
  output$animated_qq <- renderImage({
    animated_qq <- ggplot(data()) +
      aes(sample = hp, frame = cyl) +
      stat_qq() +
      stat_qq_line() +
      labs(title = "Animated QQ Plot", x = "Theoretical Quantiles", y = "Sample Quantiles") +
      transition_states(factor(cyl))
    
    anim_file <- tempfile(fileext = ".gif")
    animate(animated_qq, renderer = gifski_renderer(), output = anim_file)
    list(src = anim_file, contentType = "image/gif")
  }, deleteFile = TRUE)
  
  kruskal_test_result <- eventReactive(input$analyze, {
    kruskal_test(data(), value ~ cyl)
  })
  
  output$kruskal_result <- renderTable({
    kruskal_test_result()
  })
  
  pairwise_result <- eventReactive(input$analyze, {
    pairwise_wilcox_test(data(), value ~ cyl, p.adjust.method = "bonferroni")
  })
  
  output$pairwise_result <- renderTable({
    pairwise_result()
  })
  
  output$boxplot <- renderPlot({
    ggplot(data(), aes(x = cyl, y = value, fill = cyl)) +
      geom_boxplot(outlier.colour = "red", outlier.shape = 8) +
      geom_jitter(position = position_jitter(width = 0.2), color = "black") +
      labs(title = "Boxplot of Horsepower by Cylinder", x = "Cylinders", y = "Horsepower")
  })
  
  output$animated_boxplot <- renderImage({
    animated_fig <- ggplot(data(), aes(x = cyl, y = value, fill = cyl)) +
      geom_boxplot() +
      transition_states(cyl)
    
    anim_file <- tempfile(fileext = ".gif")
    animate(animated_fig, renderer = gifski_renderer(), output = anim_file)
    list(src = anim_file, contentType = "image/gif")
  }, deleteFile = TRUE)
}

shinyApp(ui, server)

# <<<<< ============================================================= >>>>>
#please fit this error
# <<<<< ============================================================= >>>>>
#please fit this error
# stat_pvalue_manual(
#pairwise_result,
#label = "p.adj",
#tip.length = 0.01
#)
# <<<<< ============================================================= >>>>>
# # # PowerBi / Excel
#  Rank_Sum = SUMX(FILTER('Table', 'Table'[cyl] = SELECTEDVALUE('Table'[cyl])), 'Table'[Rank_Hp])
# Group_Count = COUNTROWS(FILTER('Table', 'Table'[cyl] = SELECTEDVALUE('Table'[cyl])))

# Ri_Squared_Divided = (Rank_Sum ^ 2) / Group_Count

# Total_N = COUNTROWS('Table')

# Normalization_Factor = 12 / (Total_N * (Total_N + 1))

# H_Statistic = Normalization_Factor * SUMX(VALUES('Table'[cyl]), Ri_Squared_Divided) - 3 * (Total_N + 1)

# "Group: " & [cyl1] & " vs " & [cyl2]

# Wilcoxon_Rank_Sum = ABS(SUMX(FILTER('Table', 'Table'[cyl] = "Group1"), 'Table'[Rank_Hp]) - SUMX(FILTER('Table', 'Table'[cyl] = "Group2"), 'Table'[Rank_Hp]))
# 