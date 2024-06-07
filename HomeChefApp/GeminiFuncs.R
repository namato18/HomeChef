library(httr)
library(jsonlite)
library(base64enc)


api_key <- Sys.setenv(GEMINI_API_KEY = "AIzaSyCFCKa8CM0c_khCk8qBtGZWInPO2pFvKak")
gemini <- function(prompt, 
                   temperature=0.5,
                   max_output_tokens=1024,
                   api_key=Sys.getenv("GEMINI_API_KEY"),
                   model = "gemini-1.0-pro") {
  
  if(nchar(api_key)<1) {
    api_key <- readline("Paste your API key here: ")
    Sys.setenv(GEMINI_API_KEY = api_key)
  }
  
  model_query <- paste0(model, ":generateContent")
  
  response <- POST(
    url = paste0("https://generativelanguage.googleapis.com/v1beta/models/", model_query),
    query = list(key = api_key),
    content_type_json(),
    encode = "json",
    body = list(
      contents = list(
        parts = list(
          list(text = prompt)
        )),
      generationConfig = list(
        temperature = temperature,
        maxOutputTokens = max_output_tokens
      )
    )
  )
  
  if(response$status_code>200) {
    stop(paste("Error - ", content(response)$error$message))
  }
  
  candidates <- content(response)$candidates
  outputs <- unlist(lapply(candidates, function(candidate) candidate$content$parts))
  
  return(outputs)
  
}


chat_gemini <- function(prompt, 
                        temperature=0.5,
                        api_key=Sys.getenv("GEMINI_API_KEY"),
                        model="gemini-1.0-pro") {
  
  if(nchar(api_key)<1) {
    api_key <- readline("Paste your API key here: ")
    Sys.setenv(GEMINI_API_KEY = api_key)
  }
  
  model_query <- paste0(model, ":generateContent")
  
  # Add new message
  chatHistory <<- append(chatHistory, list(list(role = 'user', 
                                                parts = list(
                                                  list(text = prompt)
                                                ))))
  
  response <- POST(
    url = paste0("https://generativelanguage.googleapis.com/v1beta/models/", model_query),
    query = list(key = api_key),
    content_type_json(),
    body = toJSON(list(
      contents = chatHistory,
      generationConfig = list(
        temperature = temperature
      )
    ),  auto_unbox = T))
  
  if(response$status_code>200) {
    chatHistory <<- chatHistory[-length(chatHistory)]
    stop(paste("Status Code - ", response$status_code))
  } else {
    answer <- content(response)$candidates[[1]]$content$parts[[1]]$text
    chatHistory <<- append(chatHistory, list(list(role = 'model', 
                                                  parts = list(list(text = answer)))))
  }
  
  return(answer)
  
}

# Function
gemini_vision <- function(prompt, 
                          image,
                          temperature=0.1,
                          max_output_tokens=4096,
                          api_key=Sys.getenv("GEMINI_API_KEY"),
                          model = "gemini-pro-vision") {
  
  if(nchar(api_key)<1) {
    api_key <- readline("Paste your API key here: ")
    Sys.setenv(GEMINI_API_KEY = api_key)
  }
  
  model_query <- paste0(model, ":generateContent")
  
  response <- POST(
    url = paste0("https://generativelanguage.googleapis.com/v1beta/models/", model_query),
    query = list(key = api_key),
    content_type_json(),
    encode = "json",
    body = list(
      contents = list(
        parts = list(
          list(
            text = prompt
          ),
          list(
            inlineData = list(
              mimeType = "image/png",
              data = base64encode(image)
            )
          )
        )
      ),
      generationConfig = list(
        temperature = temperature,
        maxOutputTokens = max_output_tokens
      )
    )
  )
  
  if(response$status_code>200) {
    stop(paste("Error - ", content(response)$error$message))
  }
  
  candidates <- content(response)$candidates
  outputs <- unlist(lapply(candidates, function(candidate) candidate$content$parts))
  
  return(outputs)
  
}

fileInputArea <- function(inputId, label, multiple = FALSE, accept = NULL,
                          width = NULL, buttonLabel = "Browse...", placeholder = "No file selected") {
  restoredValue <- restoreInput(id = inputId, default = NULL)
  
  # Catch potential edge case - ensure that it's either NULL or a data frame.
  if (!is.null(restoredValue) && !is.data.frame(restoredValue)) {
    warning("Restored value for ", inputId, " has incorrect format.")
    restoredValue <- NULL
  }
  
  if (!is.null(restoredValue)) {
    restoredValue <- toJSON(restoredValue, strict_atomic = FALSE)
  }
  
  inputTag <- tags$input(
    id = inputId,
    name = inputId,
    type = "file",
    # Don't use "display: none;" style, which causes keyboard accessibility issue; instead use the following workaround: https://css-tricks.com/places-its-tempting-to-use-display-none-but-dont/
    style = "position: absolute !important; top: -99999px !important; left: -99999px !important;",
    `data-restore` = restoredValue
  )
  
  if (multiple) {
    inputTag$attribs$multiple <- "multiple"
  }
  if (length(accept) > 0) {
    inputTag$attribs$accept <- paste(accept, collapse = ",")
  }
  
  div(
    class = "form-group shiny-input-container w-100",
    style = htmltools::css(width = htmltools::validateCssUnit(width)),
    shiny:::shinyInputLabel(inputId, ""),
    div(
      class = "input-group mb-3",
      # input-group-prepend is for bootstrap 4 compat
      tags$label(
        class = "input-group-btn input-group-prepend w-100",
        span(
          class = "btn btn-area w-100", inputTag,
          div(tags$image(src = icon_encoded, width = "80px;"), style = "margin-top: 2rem;"),
          div(p(label), style = "font-size: 1.2rem; font-weight: 700; padding-top: 2rem;"),
          div(p(buttonLabel), style = "font-size: 1rem; font-weight: 400; margin-bottom: 2rem;")
        )
      )
    ),
    tags$div(
      id = paste(inputId, "_progress", sep = ""),
      class = "progress active shiny-file-input-progress",
      tags$div(class = "progress-bar")
    )
  )
}

saveRecipe <- function(session, name, ingredients, instructions){
  session$sendCustomMessage("saveRecipe",list(name = name, ingredients = ingredients, instructions = instructions))
}

getRecipes <- function(session, message){
  session$sendCustomMessage("getRecipes", list(message = 'hi'))
}

# Function to add a new text input
addIngredient <- function(rv) {
  rv$num_ingredients <- rv$num_ingredients + 1
  insertUI(
    selector = "#submitIngredients", 
    where = "beforeBegin", 
    ui = textInput(
      paste0("ingredient_", rv$num_ingredients), 
      label = paste("Ingredient", rv$num_ingredients), 
      value = ""
    )
  )
}

# Function to remove the last text input
removeIngredient <- function(rv) {
  removeUI(selector = paste0("#ingredient_", rv$num_ingredients))
  rv$num_ingredients <- rv$num_ingredients - 1
}

extract_nutrition <- function(nutrition_response, nutrient) {
  pattern <- paste0(nutrient, ":\\*\\*(.*?)\\\\|", nutrient, ":\\*\\*(.*?)\\n")
  match <- str_match(nutrition_response, pattern)
  value <- ifelse(!is.na(match[,2]), match[,2], match[,3])
  trimws(value)
}

# Nutrition info function
GetNutrition <- function(recipe_ingredients){
  nutrition_prompt = paste0("Based on the ingredients and amount of ingredients used here: ",
                            paste(recipe_ingredients, collapse = ", "),
                            ". Can you please give me an esimate amount of how many calories, ",
                            "protein, carbs and fat are in a serving of this meal.",
                            " Format your response as follows: ",
                            "\\n **TOTAL CALORIES:** (total calories in meal)\\n",
                            "\\n **TOTAL PROTEIN:** (total protein in meal)\\n",
                            "\\n **TOTAL CARBS:** (total carbs in meal)\\n",
                            "\\n **TOTAL FAT:** (total fat in meal)\\n",
                            "** Disclaimer: **",
                            "Please end the disclaimer with '\\nEND")
  
  nutrition_response = gemini(nutrition_prompt)
  
  print(nutrition_response)
  
  
  # nutrition_individual = str_match_all(string = nutrition_response, pattern = "\\* (.*?\\(.*?\\))")[[1]][,2]
  # nutrition_total_calories = trimws(str_match(string = nutrition_response, pattern = "TOTAL CALORIES:\\*\\*(.*?)\\\\")[,2])
  # nutrition_total_protein = trimws(str_match(string = nutrition_response, pattern = "TOTAL PROTEIN:\\*\\*(.*?)\\\\")[,2])
  # nutrition_total_carbs = trimws(str_match(string = nutrition_response, pattern = "TOTAL CARBS:\\*\\*(.*?)\\\\")[,2])
  # nutrition_total_fats = trimws(str_match(string = nutrition_response, pattern = "TOTAL FAT:\\*\\*(.*?)\\\\")[,2])
  
  to.return = list(
    nutrition_total_calories = extract_nutrition(nutrition_response, "TOTAL CALORIES"),
    nutrition_total_protein = extract_nutrition(nutrition_response, "TOTAL PROTEIN"),
    nutrition_total_carbs = extract_nutrition(nutrition_response, "TOTAL CARBS"),
    nutrition_total_fats = extract_nutrition(nutrition_response, "TOTAL FAT")
  )
  
  
  print('exiting function nutrition')
  
  return(to.return)
}















