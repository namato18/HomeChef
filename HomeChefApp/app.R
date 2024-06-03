library(shiny)
library(shinybusy)
library(shiny.pwa)
library(shinyWidgets)
library(bslib)
library(stringr)
library(shinybusy)
library(shinyjs)
library(shinyalert)

source("GeminiFuncs.R")
source('icon.R')

# Define UI for application
ui <- fluidPage(
  useShinyjs(),
  
  # shinybusy::add_busy_spinner(spin = 'semipolar', color = 'white', position = 'full-page'),
  
  theme = bslib::bs_theme(preset = 'darkly',
                          primary = 'lightgreen',
                          secondary = 'black') %>%
    bs_add_rules("
      body {
        font-family: 'Roboto', sans-serif;
      }
      .card {
        background-color: rgba(128, 128, 128, 0.5); /* Transparent grey */
        box-shadow: 0 4px 8px rgba(0, 0, 0, 0.2);
        border-radius: 10px;
        padding: 20px;
      }
      .centered-list-container {
        display: flex;
        justify-content: center;
        align-items: center;
        width: 100%;
      }
      .centered-list {
        list-style: none; /* Remove default bullets */
        padding-left: 0; /* Remove default padding */
        text-align: center; /* Center text within the list */
      }
      .centered-list li::before {
        content: '•'; /* Custom bullet */
        color: #3498db; /* Bullet color */
        display: inline-block;
        width: 1em;
        margin-left: -1em;
      }
      .main-title {
        font-size: 3em;
        color: #3498db;
        text-shadow: 1px 1px 2px rgba(0, 0, 0, 0.2);
      }
      .btn-primary {
        background-color: #3498db;
        border-color: #3498db;
        border-radius: 5px;
      }
      .btn-primary:hover {
        background-color: #2980b9;
        border-color: #2980b9;
      }
      .shiny-input-container {
        margin-bottom: 20px;
      }
      
      .input-shadow{
        box-shadow: 0 2px 4px rgba(0,0,0,0.1);
      }
    "),
  setBackgroundImage('darker.jpg'),
  
  # Make app into a downloadable format for mobile/web
  pwa(
    domain = "https://shiny.nick-amato.com/HomeChef",
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
    # selectInput(inputId = "select1", label = "Select which meal this is for",
    #             choices = c('Breakfast','Brunch','Lunch','Dinner')),
    # card(
    #   radioGroupButtons(
    #     inputId = "Id070",
    #     label = "Select Which Meal",
    #     choices = c("Breakfast", 
    #                 "Lunch", "Dinner"),
    #     justified = TRUE,
    #     checkIcon = list(
    #       yes = icon("ok", 
    #                  lib = "glyphicon"))
    #   )
    # ),
    # selectInput(inputId = "allergiesSelect", label = "Do you have food restrictions?",
    #             choices = c("None", "Gluten Free", "Peanuts", "Fish","Lactose",
    #                         "Vegetarian","Vegan"),
    #             multiple = TRUE,
    #             selected = "None"),
    actionButton(inputId = "generate", label = "Get Recipe!", class = 'btn-primary'),
    
    hr(),
    fluidRow(
      column(
        width = 10, offset = 1,
        card(id = 'card1',
             h4("Recipe Name"),
             textOutput('recipeName'),
             align = 'center'
        ),
        card(id = 'card2',
             h4("Recipe Ingredients"),
             div(uiOutput('recipeIngredients'), class = 'centered-list-container'),
             align = 'center'
        ),
        card(id = 'card3',
             h4("Recipe Instructions"),
             div(uiOutput('recipeInstructions'), class = 'centered-list-container'),
             align = 'center'
        )
      )
    )
,

    align = 'center'
  )
  
  
  
)

# ---- Define server logic ----
server <- function(input, output, session) {
  
  shinyjs::hide('card1')
  shinyjs::hide('card2')
  shinyjs::hide('card3')
  
  
  
  observeEvent(input$generate, {
    
    shinybusy::show_modal_spinner(spin = 'semipolar',
                                  color = 'white',
                                  text = 'PREPARE TO FEAST!')
    
    shinyjs::show('card1')
    shinyjs::show('card2')
    shinyjs::show('card3')
    
    path = input$imageInput$datapath
    
    vision_prompt = paste0("Please list the ingredients in this picture. Your response should be in the format \\n*(ingredient)")
    vision_response = gemini_vision(vision_prompt, path)
    
    # vision_response = gemini_vision(vision_prompt, "C:/Users/xbox/Pictures/testing.jpg")
    
    ingredients = paste(str_match_all(string = vision_response, pattern = "\\*\\s(.*)\\n")[[1]][,2], collapse = ", ")
    
    recipe_prompt = paste0("Using strictly the following ingredients, give me a recipe for ",'dinner',".",
                           " The ingredients are: ",ingredients,". ",
                           "Please give the amount of each ingredient to use as well.",
                           " Return your response in the format: \\n **(name of recipe)** \\n\\n",
                           " *(recipe item)* \\n\\n",
                           " (Instructions on how to cook numbered like \\n digit.",
                           " Please end the last step with the words 'END RECIPE'.")
    recipe_response = gemini(recipe_prompt)
    
    recipe_name = str_match(string = recipe_response, pattern = "\\*\\*(.*?)\\*\\*")[,2]
    recipe_ingredients = trimws(str_match_all(string = recipe_response, pattern = "\\n\\*(.*?)\\n")[[1]][,2])
    recipe_instructions = str_match_all(string = recipe_response, pattern = "\\d+\\.\\s(.*)\\n")[[1]][,2]
    
    print(recipe_response)
    
    output$recipeName = renderText(recipe_name)
    output$recipeIngredients = renderUI(
      tags$ul(
        class = 'centered-list',
        lapply(recipe_ingredients, function(ingredient){
          tags$li(ingredient)
        })
      )
    )
    output$recipeInstructions = renderUI(
      tags$ul(
        class = 'centered-list',
        lapply(recipe_instructions, function(instruction){
          tags$li(instruction)
        })
      )
    )
    
    shinybusy::remove_modal_spinner()
    
  })
  
  
  
  
  
  # gemini_vision(main_prompt, "C:/Users/xbox/Pictures/ingredients.jpg")
}

# Run the application 
shinyApp(ui = ui, server = server)
