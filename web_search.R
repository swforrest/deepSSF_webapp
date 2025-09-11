library(httr)
library(jsonlite)

source("perplexity-search-functions.R")

# Load environment variables from .env file
load_dot_env()

openrouter_api_key <- Sys.getenv("OPENROUTER_API_KEY")

# Example of standard web search query

if (FALSE) {
user_message <- "Give me background and sample code for running resource selection functions (RSFs) for animal GPS tracking data in R."

system_message <- "You are a helpful AI assistant. 
        Rules: 
        1 Include the DOI in your report of any paper you reference.   
        2. Produce reports that are less than 5000 words."

}

# Example of generating an R tutorial 

if (TRUE){

user_message <- "Give me background and sample code for running resource selection functions (RSFs) for animal GPS tracking data in R."

system_message <- "You are a helpful AI agent who creates statistical analysis tutorials in R. 
        Rules: 
        1. Include text and examples of code in your responses. 
        2. Produce reports that are less than 5000 words."
}


response <- call_openrouter_api(
  openrouter_api_key,
  model = "perplexity/sonar-deep-research",
  system_message = system_message,
  user_message,
  search_context_size = "medium"
  #Options "low"  "medium", "high"
)

# Example usage:
save_response_as_qmd(response, "results/results.qmd")
