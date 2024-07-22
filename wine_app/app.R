# Global.R
library(shiny)
library(shinydashboard)
library(shinythemes)
library(lime)
library(gt)
library(plotly)
library(markdown)
library(wordcloud)
library(RColorBrewer)
library(DT)
library(bslib)
library(dplyr)
library(thematic)
library(bsicons)
library(FNN)
library(caret)
library(randomForest)
thematic_shiny()


model <- readRDS("model.rds")
x <- readRDS("words.rds")
X_reduced <- readRDS("X_reduced.rds")
data <- X_reduced[,-c(1:4)]
data_numeric <- readRDS("knn.rds")

"#990012"
"#890010"
"#7a000e"
"#6b000e"
"#5b000e" 
#bunlar kırmızı kodları
#ffffeb
#ffffcf




x <- x %>% filter(x$Count0 >= 100)

# Separate the target column and features
target <- data_numeric[, 1]
features <- data_numeric[, -1]

# Ensure the column types match those in the training data
factor_columns <- sapply(data, is.factor)
factor_levels <- lapply(data[, factor_columns], levels)

# Remove "Diagnose" column from data for feature selection
feature_columns <- setdiff(colnames(data), "Diagnose")

# Create lime explainer using the training data
explainer <- lime(data, model)

# Define UI
ui <- page_navbar(
  title = "Julide",
  theme = bs_theme(
    "spacer" = "0.56rem",
    font_scale = 0.70,
    bootswatch = "darkly",
    success = "#7a000e",
    primary = "#6b000e",
    secondary = "#6b000e",
    danger = "#ffffcf",
    "table-color" = "#ffffcf",
    base_font = (font_google("Lato")),
    bg = "rgb(34,34,34)",
    fg = "#ffffeb",
    "enable-gradients" = TRUE,
    "enable-shadows" = TRUE
  ),
  nav_panel(
    title = "",
    layout_columns(
      navset_card_tab(
        height = 300,
        sidebar = sidebar(
          class = "bg-secondary",
          "Select maximum 2 main words that describes your wine best.",
          accordion(
            class = "bg-secondary",
            accordion_panel(
              class = "bg-secondary",
              "Features", icon = bsicons::bs_icon("menu-app"),
              selectInput(
                inputId = "selected_columns",
                label = "Select Columns:",
                choices = feature_columns,
                multiple = TRUE
              ),
              actionButton("find_btn", "Find Wine", class = "btn-primary rounded-0"),
              actionButton("predict_btn", "Predict Score", class = "btn-primary rounded-0")
            )
          )
        ),
        nav_panel(
          title = "Find Your Wine", 
          tableOutput("found_similar_cases")
        ),
        nav_panel(
          title = "Predict Wine's Score", 
          tableOutput("prediction_table"),
          markdown(
            "If your wine predicted X1 that means it's above +90 wine score enjoy.
             Otherwise, try to enjoy... For more information about wine scoring please 
             visit this [link](https://www.wine-searcher.com/wine-scores)."
          )
        )
      ),
      card(fill = TRUE,
        full_screen = TRUE,
        card_header("Wordcloud"),
        layout_sidebar(
          sidebar = sidebar(
            sliderInput(
              "freq",
              "Minimum Frequency:",
              min = 100, max = 500, value = 150
            ),
            sliderInput(
              "max",
              "Maximum Number of Words:",
              min = 1, max = 112, value = 15
            ),
            actionButton(
              "plus90", "Most used main words from wine enthusiasts for above 90 score wines",
              class = "btn-primary rounded-0"
            ),
            actionButton(
              "minus89", "Most used main words from wine enthusiasts for below 89 score wines",
              class = "btn-primary rounded-0"
            )
          ),
          height = 300, 
          full_screen = TRUE,
          card_body(fill = TRUE,
            min_height = 150,
            class = "gap-2 container",
            plotOutput("plot")
          ),
          fill = TRUE,
        )
      ),
      card(
        fill = TRUE,
        card_header(
          class = "bg-dark",
          "Most Used Main Words by Critics"
        ),
        value_box(fill = TRUE,
                  title = "Finish", value = 10545, showcase = bsicons::bs_icon("star"),
                  showcase_layout = "top right"),
        value_box(
          title = "Fruit", value = 6015, showcase = bsicons::bs_icon("apple"),
          showcase_layout = c("top right")),
        value_box(title = "Great", value = 5730, showcase = bsicons::bs_icon("hand-thumbs-up"),
                  showcase_layout = "top right")
      ),
      card(
        full_screen = TRUE,
        card_header("Why system choose this?"),
        plotlyOutput("explanation_plot")
      ),
      card(
        max_height = 600,
        full_screen = TRUE,
        card_header("How to understand critics?"),
        includeMarkdown("deneme.Rmd")
      ),
      col_widths = c(5, 4, 3, 7, 5),
      row_heights = c(10000, 8000)
    )
  ),
  nav_spacer(),
  nav_menu(
    title = "Links",
    nav_item(
      markdown("[GitHub](https://github.com/SETOsfk)"),
      class = "bg-danger"
    )
  )
)

# Server.R
server <- function(input, output, session) {
  #bs_themer()
  
  # Word Cloud Reactive Values and Events
  word_data <- reactiveVal(data.frame())
  observeEvent(input$plus90, {
    word_data(data.frame(Word = x$Word1, Count = x$Count1))
  })
  observeEvent(input$minus89, {
    word_data(data.frame(Word = x$Word0, Count = x$Count0))
  })
  
  # Make the wordcloud drawing predictable during a session
  wordcloud_rep <- repeatable(wordcloud)
  output$plot <- renderPlot({
    req(word_data())
    wordcloud_rep(words = word_data()$Word,
                  freq = word_data()$Count,
                  scale = c(3, 2),
                  min.freq = input$freq,
                  max.words = input$max,
                  colors = brewer.pal(8, "Dark2"))
  })
  
  # Reactive value to store the selected columns
  selected_columns <- reactiveVal(character())
  
  observeEvent(input$selected_columns, {
    selected_columns(input$selected_columns)
  })
  
  output$selected_columns <- renderText({
    paste(selected_columns(), collapse = ", ")
  })
  
  data_reactive <- reactiveVal(data.frame(matrix(0, ncol = ncol(data), nrow = 1, 
                                                 dimnames = list(NULL, colnames(data)))))
  
  observeEvent(input$predict_btn, {
    df <- data.frame(matrix(0, ncol = ncol(data), nrow = 1, dimnames = list(NULL, colnames(data))))
    df[1, selected_columns()] <- 1
    
    # Convert columns to factors with the same levels as in the training data
    for (col in names(factor_levels)) {
      df[[col]] <- factor(df[[col]], levels = factor_levels[[col]])
    }
    
    data_reactive(df)
    
    # Make prediction using the selected columns
    prediction <- predict.train(model, newdata = df, type = "prob")
    prediction_probabilities <- as.data.frame(prediction)
    
    # Create table with prediction probabilities
    output$prediction_table <- renderTable({
      prediction_table <- data.frame(
        Score = c("+90", "-89"),
        Probability = c(prediction_probabilities$X1, prediction_probabilities$X0)
      )
      
      prediction_table
    })
    
    # Explain the prediction using lime with dynamic feature selection method
    n_features <- 5  # Default number of features for lime explain
    explanation <- lime::explain(df, explainer, n_labels = 1, n_features = n_features, 
                                 feature_selection = "auto")
    
    output$explanation_plot <- renderPlotly({
      ggplotly(plot_features(explanation))
    })
  })
  
  observeEvent(input$find_btn, {
    df_n <- data.frame(matrix(0, ncol = ncol(data), nrow = 1, dimnames = list(NULL, colnames(data))))
    df_n[1, selected_columns()] <- 1
    
    # Convert columns to factors with the same levels as in the training data
    
    k <- 5
    nn <- get.knnx(data = features, query = df_n[, -1], k = k, algorithm = "brute")
    nn_indices <- nn$nn.index
    
    similar_wines <- X_reduced[nn_indices, c(1:3)]
    output$found_similar_cases <- renderTable({
      similar_wines$Year<-round(similar_wines$Year,0)
      similar_wines[,]
    })
  })
  
}
shinyApp(ui = ui, server = server)