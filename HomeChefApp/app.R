library(shiny)
library(shinybusy)
library(shiny.pwa)
library(shinyWidgets)
library(bslib)
library(stringr)

source("GeminiFuncs.R")
source('icon.R')

# Define UI for application
ui <- fluidPage(
  theme = bslib::bs_theme(preset = 'flatly'),
  
  # Make app into a downloadable format for mobile/web
  pwa(
    domain = "https://shiny.nick-amato.com/GeminiTesting",
    title = "HomeChef",
    icon = "www/dupescoop.png",
    output = 'www',
  ),
  
  div(h1("Home Chef", class = 'main-title'), align = 'center'),
  
  # ---- Fancy image input ---- 
  
  shiny::fluidRow(
    style = "margin-top: 20px;",
    shiny::column(
      width = 10, offset = 1,
      card(
        title = div("Select an image of a product!", align = 'center'),
        shiny::fluidRow(
          column(
            width = 4, offset = 4,
            fileInputArea(
              "imageInput",
              label = "Tap to upload!",
              buttonLabel = "Upload a clear picture!",
              multiple = FALSE
            ),
            shiny::tableOutput("files")
          )
        )
      )
    )
  ),
  # ---- end fancy ----
  
  div(
    selectInput(inputId = "select1", label = "Select which meal this is for",
                choices = c('Breakfast','Brunch','Lunch','Dinner')),
    selectInput(inputId = "allergiesSelect", label = "Do you have food restrictions?",
                choices = c("None", "Gluten Free", "Peanuts", "Fish","Lactose",
                            "Vegetarian","Vegan"),
                multiple = TRUE,
                selected = "None"),
    actionButton(inputId = "generate", label = "Get Recipe!", class = 'btn-primary'),
    h4("Recipe Name"),
    textOutput('recipeName'),
    h4("Recipe Ingredients"),
    textOutput('recipeIngredients'),
    h4("Recipe Instructions"),
    textOutput('recipeInstructions'),
    align = 'center'
  )
  
  
  
)

# ---- Define server logic ----
server <- function(input, output) {
  
  observeEvent(input$generate, {
    path = input$imageInput$datapath
    
    vision_prompt = paste0("Please list the ingredients in this picture. Your response should be in the format \\n*(ingredient)")
    vision_response = gemini_vision(vision_prompt, path)
    
    # vision_response = gemini_vision(vision_prompt, "C:/Users/xbox/Pictures/testing.jpg")
    
    ingredients = paste(str_match_all(string = vision_response, pattern = "\\*\\s(.*)\\n")[[1]][,2], collapse = ", ")
    
    recipe_prompt = paste0("Using strictly the following ingredients, give me a recipe for ","dinner",".",
                           " The ingredients are: ",ingredients,". ",
                           " Return your response in the format: \\n **(name of recipe)** \\n\\n",
                           " *(recipe item)* \\n\\n",
                           " (Instructions on how to cook numbered like \\n(digit).",
                           " Please end the last step with the words 'END RECIPE'.")
    recipe_response = gemini(recipe_prompt)
    
    recipe_name = str_match(string = recipe_response, pattern = "\\*\\*(.*?)\\*\\*")[,2]
    recipe_ingredients = trimws(str_match_all(string = recipe_response, pattern = "\\n\\*(.*?)\\n")[[1]][,2])
    recipe_instructions = str_match_all(string = recipe_response, pattern = "\\d+\\.\\s(.*)\\n")[[1]][,2]

    print(recipe_response)
    
    output$recipeName = renderText(recipe_name)
    output$recipeIngredients = renderText(recipe_ingredients)
    output$recipeInstructions = renderText(recipe_instructions)
  })
  

  
  # gemini_vision(main_prompt, "C:/Users/xbox/Pictures/ingredients.jpg")
}

# Run the application 
shinyApp(ui = ui, server = server)
