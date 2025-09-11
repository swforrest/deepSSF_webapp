#' Call the OpenRouter API
#'
#' This function sends a request to the OpenRouter API with the specified parameters and returns the API response.
#'
#' @param openrouter_api_key Character string. The API key used for authentication.
#' @param model Character string. The model to use for the API call (default is "perplexity/sonar").
#' @param system_message Character string. The system message to set the context for the AI (default is a message about creating statistical analysis tutorials in R).
#' @param user_message Character string. The user's message or prompt to send to the AI.
#' @param search_context_size Character string. The size of the web search context to include (default is "medium"). Options "low"  "medium", "high"

#'
#' @return A list containing the API response.


call_openrouter_api <- function(
  openrouter_api_key,
  model = "perplexity/sonar",
  system_message = "You are a helpful AI agent who creates statistical analysis tutorials in R. 
        Rules: 
        1. Include text and examples of code in your responses. 
        2. Produce reports that are less than 5000 words.",
  user_message,
  search_context_size = "medium"
) {
  response <- POST(
    url = "https://openrouter.ai/api/v1/chat/completions",
    add_headers(
      "Content-Type" = "application/json",
      "Authorization" = paste("Bearer", openrouter_api_key)
    ),
    body = toJSON(list(
      model = model,
      messages = list(
        list(
          role = "system",
          content = system_message
        ),  
        list(
          role = "user",
          content = user_message
        )
      ),
      web_search_options = list(
        search_context_size = search_context_size
      )
    ), auto_unbox = TRUE),
    encode = "raw"
  )
  return(response)
}

#' Save AI Response as Quarto Markdown (.qmd) File
#'
#' This function takes an AI model response object (`r3`), extracts its content, annotations, and reasoning,
#' and saves it as a Quarto Markdown (.qmd) file. The function processes citations in the content, replacing
#' citation markers (e.g., [1], [2]) with HTML links to the referenced sources, and appends a references section.
#' It also includes a YAML header, a generic warning about AI-generated content, and the model used.
#' It assumes data is structured as returned by the OpenRouter API with the Perplexity models.
#'
#' @param r3 The response object from the AI model, expected to be a JSON HTTP response.
#' @param file The output filename for the Quarto Markdown file. Defaults to "output.qmd".
#'
#' @details
#' - Extracts main content, annotations (citations), and reasoning from the AI response.
#' - Replaces citation markers in the content with HTML links to the sources.
#' - Appends a references section listing all sources with links.
#' - Adds a YAML header for Quarto, a warning about AI-generated content, and the model name.
#'
#' @return No return value. The function writes the processed content to the specified file.
#'
#' @examples
#' \dontrun{
#' save_response_as_qmd(r3, file = "my_report.qmd")
#' }
#'
#' @importFrom jsonlite fromJSON
#' @importFrom httr content
#' @export

save_response_as_qmd <- function(r3, file = "output.qmd") {
  r3 = fromJSON(content(r3, "text"))

  # Extract content and annotations
  content <- tryCatch({
    r3$choices$message$content[[1]]
  }, error = function(e) {
    "Could not extract content from r3."
  })
  annotations <- tryCatch({
    r3$choices$message$annotations[[1]]
  }, error = function(e) {
    NULL
  })
  reasoning <- tryCatch({
    r3$choices$message$reasoning[[1]]
  }, error = function(e) {
    NULL
  })

  # Defensive: if annotations is a data.frame, convert to list
  if (!is.null(annotations) && is.data.frame(annotations)) {
    annotations <- split(annotations, seq(nrow(annotations)))
  }
  # Replace [n] with HTML links if present, but NOT inside code blocks
  content_linked <- content
  if (!is.null(annotations) && length(annotations) > 0) {
    # Find code block positions
    code_pat <- "```[rR]?\\n[\\s\\S]*?```"
    code_matches <- gregexpr(code_pat, content, perl = TRUE)[[1]]
    merged <- character(0)
    last_end <- 1
    # If no code blocks, just one segment
    if (code_matches[1] == -1) {
      merged <- list(content)
    } else {
      for (i in seq_along(code_matches)) {
        start <- code_matches[i]
        end <- start + attr(code_matches, "match.length")[i] - 1
        # Non-code segment before code block
        if (start > last_end) {
          merged <- c(merged, substr(content, last_end, start - 1))
        }
        # Code block itself
        merged <- c(merged, substr(content, start, end))
        last_end <- end + 1
      }
      # Any trailing non-code segment
      if (last_end <= nchar(content)) {
        merged <- c(merged, substr(content, last_end, nchar(content)))
      }
    }
    # Replace [n] only in non-code segments
    for (i in seq_along(annotations)) {
      ann <- annotations[[i]]
      url <- NA; title <- NA
      if (!is.null(ann$url_citation)) {
        url <- ann$url_citation$url
        title <- ann$url_citation$title
      } else {
        url <- ann$url
        title <- ann$title
      }
      if (is.null(url) || is.na(url)) url <- "#"
      if (is.null(title) || is.na(title)) title <- url
      link <- sprintf('<a href="%s" target="_blank">[%d]</a>', url, i)
      merged <- lapply(merged, function(seg) {
        if (!grepl("^```", seg)) {
          gsub(sprintf('\\[%d\\]', i), link, seg, perl = TRUE)
        } else {
          seg
        }
      })
    }
    content_linked <- paste0(merged, collapse = "")
  }
  # Build references section
  references <- ""
  if (!is.null(annotations) && length(annotations) > 0) {
    references <- "\n\n## References\n"
    for (i in seq_along(annotations)) {
      ann <- annotations[[i]]
      url <- NA; title <- NA
      if (!is.null(ann$url_citation)) {
        url <- ann$url_citation$url
        title <- ann$url_citation$title
      } else {
        url <- ann$url
        title <- ann$title
      }
      if (is.null(url) || is.na(url)) url <- "#"
      if (is.null(title) || is.na(title)) title <- url
      references <- paste0(references, sprintf('%d. <a href="%s" target="_blank">%s</a>\n', i, url, title))
    }
  }
  # Add basic Quarto YAML header
  yaml_header <-
    "---\ntitle: 'AI Generated Report'\nformat: html\n---\n\n"

  # Add reasoning section if present
  reasoning_section <- ""
  if (!is.null(reasoning) && nzchar(reasoning)) {
    reasoning_section <- paste0("\n\n## Reasoning\n\n", reasoning, "\n")
  }

  generic_warning <- "AI generated content - please verify accuracy and citations. Links go directly to unverified web content, always inspect the link before clicking."
  model <- paste("The model used was", r3$model)

  # Write to file
  cat(yaml_header, "## Details", generic_warning, model, "## Content", content_linked, reasoning_section, references, file = file, sep = "\n\n")
  message(paste("Saved r3 content to", file))
}
