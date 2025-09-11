library(dotenv)
library(ellmer)

# Load environment variables from .env file
load_dot_env()

chat <- chat_openrouter(system_prompt = "Please finish the sentence I start.",
    model = "anthropic/claude-sonnet-4",
    api_args = list(max_tokens = 100, 
                    temperature = 2.0, 
                    top_k  = 1),
    api_key = Sys.getenv("OPENROUTER_API_KEY")
    )

chat$chat("Tomorrow I'm going to ")

chat$chat("I was thinking of going to the beach, but ")
