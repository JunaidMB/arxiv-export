library(aRxiv)
library(glue)
library(dplyr)
library(stringi)
library(stringr)
library(gmailr)
library(tableHTML)
library(ggplot2)
library(shiny)

ui <- fluidPage(
  title = 'Selectize examples',
  sidebarLayout(
    sidebarPanel(
      
      selectizeInput(
        inputId = 'subject_select', label = 'Select the Subjects', choices = c("Statistics - Applications" = "stat.AP"
                                                                               , "Statistics - Computation" = "stat.CO"
                                                                               , "Statistics - Machine Learning" = "stat.ML"
                                                                               , "Statistics - Theory" = "math.ST"
                                                                               , "Mathematics - Probability" = "math.PR")
        , selected = "Statistics - Applications", multiple = TRUE),
      
      dateRangeInput('dateRange',
                     label = 'Date range input',
                     start = Sys.Date() - 3, end = Sys.Date() - 2
      ),
      
      numericInput(inputId = "resultlimits", label = "How many results would you like to return?", value = 10, min = 1, max = 500, step = 1)
      
      
    ),
    mainPanel(
      
      strong("Please note: Submission dates may not line up exactly with the arXiv website."),
      
      br(),
      br(),
      br(),
      
      dataTableOutput('table')
      
    )
  )
)

server <- function(input, output) {
  
  output$table <- renderDataTable({
    
    # Input parameters
    ## Select Categories to search
    selected_cats <- c(input$subject_select)
    categories <- paste(selected_cats, collapse = " , ")
    
    ## Select date range
    
    date_range <- input$dateRange
    formatted_date_range <- str_c(str_c(gsub("-", "", date_range[[1]]) , "*"), str_c(gsub("-", "", date_range[[2]]) , "*"), sep = " , ")
    
    ## Limit
    limit <- c(input$resultlimits)
    
    # Obtain arxiv search results and save them to a Tibble
    
    full_results <- as_tibble(arxiv_search(query = glue("cat: ({categories}) AND submittedDate: ({formatted_date_range})"), limit = limit))
    
    ## Restrict to only a few columns
    results <- full_results %>% mutate(submitted = str_sub(submitted, end = 10), authors = str_replace_all(authors, "[|]", " & ")) %>% select(Title = title, Submission_Date = submitted, Authors = authors, PDF_Link = link_pdf, Primary_Category = primary_category)
    
    results
    })
  

  
}

shinyApp(ui = ui, server = server)
