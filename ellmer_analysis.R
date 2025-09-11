library(dotenv)
library(ellmer)

# Load environment variables from .env file
load_dot_env()

chat <- chat_openrouter(system_prompt = "You are an excellent and renowned improv teacher.",
    model = "anthropic/claude-sonnet-4",
    api_args = list(max_tokens = 1000, 
                    temperature = 2.0, 
                    top_k  = 1),
    api_key = Sys.getenv("OPENROUTER_API_KEY")
    )

chat$chat("Tomorrow I'm going to ")

chat$chat("I was thinking of going to the beach, but ")

chat$chat("Provide me with some starting improv scences")



stats_bot <- readr::read_file(url("https://raw.githubusercontent.com/cbrown5/AI-assistants-for-scientific-coding/refs/heads/main/resources/DIY-stats-bot-system.md"))

chat_stats <- chat_openrouter(
  system_prompt = stats_bot,
  model = "anthropic/claude-sonnet-4",
  api_args = list(max_tokens = 5000)
)

chat_stats$chat("Who are you?")

live_browser(chat_stats)



chat <- chat_openrouter(system_prompt = "You are a helpful assistant",
    # model = "openai/gpt-5",
  model = "anthropic/claude-sonnet-4",
    # api_args = list(max_tokens = 1000, 
    #                 temperature = 2.0, 
    #                 top_k  = 1),
    api_key = Sys.getenv("OPENROUTER_API_KEY")
    )

chat$chat("I have a regression that finds observed data of snapper abundance increases by an average of 2.5 fish per 10% increase in seagrass cover, with an intercept of zero. But the observed snapper count data were biased upwards at high density, with a power relationship that has an exponent of 1.1. What is the expected abundance at a seagrass cover of 55%? You can use R code to solve it.")
