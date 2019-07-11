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
      
      numericInput(inputId = "resultlimits", label = "How many results would you like to return?", value = 10, min = 1, max = 500, step = 1),
      
      textInput(inputId = 'arxiv.email', label = "Enter Email Address Here", placeholder = "jmbutt28@gmail.com"),
      
      br(),
      br(),
      
      actionButton(inputId = "arxiv.get.results", label = "Run"),
      actionButton(inputId = "arxiv.send.email", label = "Send Email")
      
      
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
  

   


  
 observeEvent(input$arxiv.get.results, {
   
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
   
  
   output$table <- renderDataTable(
     expr = results, escape = FALSE)
   
   
   }) 
  
  
  observeEvent(input$arxiv.send.email, {
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
    
    ## Create an HTML table of results
    msg <- tableHTML(results)
    
    ## Add a paragraph before the table
    html_bod <- str_c("<p> This is your arXiv export. </p>", msg)
    
    ## Create a MIME message and send it
    email <- input$arxiv.email
    
    mime() %>% 
      to(email) %>% 
      from("jmbutt28@gmail.com") %>% 
      subject("arXiv Export") %>% 
      html_body(html_bod) %>% 
      send_message()
    
  })

  
}

shinyApp(ui = ui, server = server)
