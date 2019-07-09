install.packages("aRxiv")
install.packages("gmailr")
install.packages("tableHTML")
library(aRxiv)
library(glue)
library(dplyr)
library(stringi)
library(stringr)
library(gmailr)
library(tableHTML)
library(ggplot2)

# Academic categories in arxiv
arxiv_cats

# Input parameters
## Select Categories to search
selected_cats <- c("stat.AP", "stat.ML")

categories <- paste(selected_cats, collapse = " , ")

## Select date range

date_range <- c("2019-07-04", "2019-07-05")
formatted_date_range <- str_c(str_c(gsub("-", "", date_range[[1]]) , "*"), str_c(gsub("-", "", date_range[[2]]) , "*"), sep = " , ")

## Limit
limit <- c(500)

# Obtain arxiv search results and save them to a Tibble

full_results <- as_tibble(arxiv_search(query = glue("cat: ({categories}) AND submittedDate: ({formatted_date_range})"), limit = limit))

## Restrict to only a few columns
results <- full_results %>% select(title, submitted, authors, link_pdf, primary_category)

# Email results 

## Setup Oauth token
# gmail_auth() 

## Clear Oauth token
# clear_token()

## Create an HTML table of results
msg <- tableHTML(results)

## Add a paragraph before the table
html_bod <- str_c("<p> This is your Arxiv export. </p>", msg)

## Create a MIME message and send it
email <- c("jmbutt28@gmail.com", "jmbutt2807@gmail.com")

mime() %>% 
  to(email) %>% 
  from("arxiv.export@gmail.com") %>% 
  subject("Arxiv Export") %>% 
  html_body(html_bod) %>% 
  send_message()
