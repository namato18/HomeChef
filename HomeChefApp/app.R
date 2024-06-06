library(shiny)
library(shinybusy)
library(shiny.pwa)
library(shinyWidgets)
library(bslib)
library(stringr)
library(shinybusy)
library(shinyjs)
library(shinyalert)
library(aws.s3)

access_key = readRDS('access_key.rds')
secret_key = readRDS("secret_key.rds")

Sys.setenv(
  "AWS_ACCESS_KEY_ID" = access_key,
  "AWS_SECRET_ACCESS_KEY" = secret_key,
  "AWS_DEFAULT_REGION" = "us-east-1"
)

source("GeminiFuncs.R")
source('icon.R')



# Define UI for application
ui <- fluidPage(
  useShinyjs(),
  
  tags$script(src = 'recipeFunctions.js'),
  tags$script(src = 'cookie-handler.js'),
  
  tags$head(
    # Include custom CSS for the loading screen
    tags$style(HTML('
    
html, body {
  height: 100%;
  font-family: -apple-system, BlinkMacSystemFont, "Segoe UI", Roboto, "Helvetica Neue", Arial, sans-serif;
  background-color: #f0f0f5;
  margin: 0;
  padding: 0;
  background-image: url("bluechef.jpg");
  background-size: cover;
  background-position: center;
  background-attachment: fixed;
  overflow-y: auto;
  overflow-x: hidden;
}

  #loading-screen {
    position: fixed;
    width: 100%;
    height: 100%;
    background: white;
    z-index: 9999;
    display: flex;
    align-items: center;
    justify-content: center;
    font-size: 24px;
    color: #007aff;
  }
    ')),
    # Include custom JavaScript to hide the loading screen once the app is fully loaded
    tags$script(HTML("
    $(document).on('shiny:connected', function() {
      $('#loading-screen').fadeOut('slow', function() {
        $('html, body').css('overflow', 'auto'); /* Enable scrolling after loading */
      });
    });
    "))
  ),
  
  # shinybusy::add_busy_spinner(spin = 'semipolar', color = 'white', position = 'full-page'),
  
  theme = bslib::bs_theme(preset = 'darkly',
                          primary = 'blue',
                          secondary = 'black') %>%
    bs_add_rules('
    


.card {
  background-color: rgba(80, 80, 80, 0.7); /* Transparent grey */;
  box-shadow: 0 2px 10px rgba(0, 0, 0, 0.1);
  border-radius: 12px;
  padding: 20px;
  margin: 20px;
  transition: box-shadow 0.3s ease;
}

.card:hover {
  box-shadow: 0 4px 15px rgba(0, 0, 0, 0.15);
}

.centered-list-container {
  display: flex;
  justify-content: center;
  align-items: center;
  width: 100%;
}

.centered-list {
  list-style: none;
  padding-left: 0;
  text-align: center;
}

.centered-list li::before {
  content: "•";
  color: #007aff; /* iOS blue */
  display: inline-block;
  width: 1em;
  margin-left: -1em;
}

.btn-primary {
  background-color: #007aff;
  border: none;
  border-radius: 10px;
  color: white;
  padding: 10px 20px;
  font-size: 16px;
  transition: background-color 0.3s ease;
}

.btn-primary:hover {
  background-color: #005bb5;
}

.shiny-input-container {
  margin-bottom: 20px;
}

.input-shadow {
  box-shadow: 0 1px 3px rgba(0, 0, 0, 0.1);
  border-radius: 10px;
}

.tab-content > .tab-pane {
  display: none;
}

.tab-content > .active {
  display: block;
  animation: slideInFromBottom 0.5s forwards;
}

@keyframes slideInFromBottom {
  from {
    transform: translateY(100%);
    opacity: 0;
  }
  to {
    transform: translateY(0);
    opacity: 1;
  }
}

.navbar {
  background-color: #f8f8f8;
  border-bottom: 1px solid #e7e7e7;
  box-shadow: 0 1px 0 rgba(0, 0, 0, 0.1);
  padding: 10px 20px;
  display: flex;
  justify-content: space-between;
  align-items: center;
  position: relative;
}

.navbar-brand {
  color: #007aff !important;
  font-size: 20px;
  font-weight: bold;
}

.navbar-nav {
  display: flex;
  justify-content: center;
  align-items: center;
  flex: 1;
}

.navbar-nav > li {
  text-align: center;
}

.navbar-nav > li > a {
  color: #007aff !important;
  font-size: 18px;
  display: block;
  padding: 14px;
}

.navbar-collapse {
  display: flex;
  justify-content: space-around;
  padding-right: 10px;
  padding-left: 10px;
}

#card1, #card2, #card3, #card4, #saveButton {
  display: none;
}

.logo-container {
  display: flex;
  justify-content: center;
  margin-top: 20px;
}

.logo {
  height: 100px;
  width: auto;
}


    '),
  # setBackgroundImage('bluechef.jpg'),
  
  # Make app into a downloadable format for mobile/web
  pwa(
    domain = "https://shiny.nick-amato.com/HomeChef",
    title = "HomeChef",
    icon = "www/chef-teal.png",
    output = 'www',
  ),
  
  div(id = 'loading-screen', 'Loading...'),
  div(class = 'logo-container', tags$img(src = 'chef-white-ring-removebg.png', class = 'logo')),
  
  hidden(div(id = "loginPage",
             h2("Welcome!"),
             hr(),
             fluidRow(
               column(width = 4, offset = 4,
                      paste0("Since this is your first time here, you just need to create a username!",
                             " This will allow you to save your recipes and re-use them if you'd like to!"),
               ),
               hr(),
               strong('(You only need to do this once)')
             ),
             textInput("username", "Enter your username:"),
             actionButton("loginBtn", "Login"),
             align = 'center'
  )),
  
  hidden(div(id='mainPage',
             # ---- Fancy image input ---- 
             actionButton("clearCookieBtn", "Clear Cookie and Logout"),
             
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
                         buttonLabel = "Upload a clear picture of the ingredients you have available!",
                         multiple = FALSE
                       ),
                       shiny::tableOutput("files")
                     )
                   )
                 )
               )
             ),
             # ---- end fancy ----
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
             # actionButton(inputId = "generate", label = "Get Recipe!", class = 'btn-primary'),
             
             fluidRow(
               column(width = 8, offset = 2,
                      actionBttn(inputId = 'generate',
                                 label = 'Get Recipe!',
                                 color = 'primary',
                                 icon = icon('utensils'),
                                 style = 'jelly',
                                 block = TRUE),
                      br(),
                      br(),
               )
             ),
             
             fluidRow(
               column(
                 width = 10, offset = 1,
                 card(id = 'card1',
                      h4("Recipe Name"),
                      hr(),
                      textOutput('recipeName'),
                      align = 'center'
                 ),
                 card(id = 'card2',
                      h4("Recipe Ingredients"),
                      hr(),
                      div(uiOutput('recipeIngredients'), class = 'centered-list-container'),
                      align = 'center'
                 ),
                 card(id = 'card3',
                      h4("Recipe Instructions"),
                      hr(),
                      div(uiOutput('recipeInstructions'), class = 'centered-list-container'),
                      align = 'center'
                 ),
                 card(id = 'card4',
                      h4('Nutrition Estimates (per serving)'),
                      hr(),
                      div(uiOutput('nutritionEstimates'), class = 'centered-list-container'),
                      align = 'center'
                 )
               )
             ),
             # fluidRow(
             #   column(width = 8, offset = 2,
             #          actionBttn(inputId = 'saveButton',
             #                     label = 'Save Recipe!',
             #                     color = 'primary',
             #                     icon = icon('floppy-disk'),
             #                     style = 'jelly',
             #                     block = TRUE),
             #          actionBttn(inputId = 'loadButton',
             #                     label = 'Load Recipe!',
             #                     color = 'primary',
             #                     icon = icon('floppy-disk'),
             #                     style = 'jelly',
             #                     block = TRUE),
             #          verbatimTextOutput('savedRecipes')
             #   )
             # ),
             
             
             align = 'center'
  )
  
  
  
  
  
  ))

# ---- Define server logic ----
server <- function(input, output, session) {
  
  
  
  rv = reactiveValues(username = character(),
                      updated_ingredients = character(),
                      num_ingredients = numeric())
  
  recipe = reactiveValues(name = character(),
                          ingredients = character(),
                          instructions = character(),
                          estimates = character())
  
  observe({
    
    session$sendCustomMessage(type = 'getCookie', list(name = 'username'))
    
  })
  
  observeEvent(input$loginBtn, {
    username = input$username
    
    if(username != ""){
      
      user_tracker = aws.s3::s3readRDS(object = 'users.rds', bucket = 'homechef-tracker')
      
      if(username %in% user_tracker$username){
        print('user already found')
      }else{
        session$sendCustomMessage(type = 'setCookie', list(name = 'username', value = username, expires = 365))
        shinyjs::hide('loginPage')
        shinyjs::show('mainPage')
        
        tmp.df = data.frame("username" = username,
                            "signup_date" = Sys.Date(),
                            "recipes" = NA)
        print(tmp.df)
        
        user_tracker_tmp = rbind(user_tracker, tmp.df)
        
        saveRDS(user_tracker_tmp, file = paste0(tempdir(), "/tmp.rds"))
        
        put_object(
          file = file.path(tempdir(), "tmp.rds"),
          object = paste0("users.rds"),
          bucket = paste0("homechef-tracker")
        )
      }
      
      
    }
  })
  
  observeEvent(input$cookie, {
    if(input$cookie$name == 'username' && input$cookie$value != ""){
      shinyjs::hide('loginPage')
      shinyjs::show('mainPage')
      rv$username = input$cookie$value
      print(rv$username)
    }else{
      shinyjs::hide('mainPage')
      shinyjs::show('loginPage')
    }
  })
  
  
  
  observeEvent(input$generate, {
    
    if(!is.null(input$imageInput)){
      
      
      shinybusy::show_modal_spinner(spin = 'semipolar',
                                    color = 'white',
                                    text = 'Analyzing Image!')
      
      
      
      path = input$imageInput$datapath
      
      vision_prompt = paste0("Please accurately list the ingredients in this picture. Your response should be in the format: *(ingredient)*")
      vision_response = gemini_vision(vision_prompt, path)
      
      vision_response = gemini_vision(vision_prompt, "C:/Users/xbox/Pictures/food.jpg")
      
      ingredients_ind = str_match_all(string = vision_response, pattern = "\\*\\s(.*)\\n")[[1]][,2]

      rv$num_ingredients = length(ingredients_ind)
      
      
      ingredient_inputs = lapply(seq_along(ingredients_ind), function(i){
        textInput(paste0('ingredient_',i), label = paste("Ingredient",i), value = ingredients_ind[i])
      })
      
      shinybusy::remove_modal_spinner()
      # Show the dynamic inputs in a shinyalert modal
      shinyalert(
        title = "Confirm Ingredients",
        html = TRUE,
        text = tagList(
          div(
            p("Please confirm or correct the ingredients:"),
            do.call(tagList, ingredient_inputs),
            actionButton(inputId = 'submitIngredients', label = 'Submit!'),
            actionButton(inputId = "addIngredientBtn", label = 'add'),
            actionButton(inputId = 'removeIngredient', label = 'remove')
          )
        ),
        size = "l",
        showConfirmButton = FALSE
      )
    }else{
      
    }
    
    
    
  })
  
  observeEvent(input$submitIngredients, {
    # Retrieve the values of all text inputs
    ingredient_values <- lapply(seq_along(ingredients_ind), function(i) {
      input[[paste0('ingredient_', i)]]
    })
    
    # You can now use ingredient_values for further processing
    ingredients = (paste(ingredient_values, collapse = ", "))
    
    shinybusy::show_modal_spinner(spin = 'semipolar',
                                  color = 'white',
                                  text = 'PREPARE TO FEAST!')
    
    recipe_prompt = paste0("Using strictly the following ingredients, give me a recipe for ",'dinner',".",
                           " The ingredients are: ",ingredients,". ",
                           "Please give the amount of each ingredient to use as well.",
                           " Return your response in the format: \\n **(name of recipe)** \\n\\n",
                           "**INGREDIENTS:**\\n\\n",
                           "\\n\\n\\n *(recipe item)* \\n\\n",
                           "\\n\\n **INSTRUCTIONS** \\n\\n",
                           " (Instructions on how to cook numbered like \\n digit.",
                           " Please end the last step with the words 'END RECIPE'.")
    recipe_response = gemini(recipe_prompt)
    
    recipe_name = str_match(string = recipe_response, pattern = "\\*\\*(.*?)\\*\\*")[,2]
    recipe_ingredients = trimws(str_match_all(string = recipe_response, pattern = "\\* ([^\\*]+)")[[1]][,2])
    
    
    
    
    
    
    
    recipe_instructions = str_match_all(string = recipe_response, pattern = "\\d+\\.\\s(.*)\\n")[[1]][,2]
    
    print(recipe_response)
    
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
    nutrition_total_calories = trimws(str_match(string = nutrition_response, pattern = "TOTAL CALORIES:\\*\\*(.*)\\n")[,2])
    nutrition_total_protein = trimws(str_match(string = nutrition_response, pattern = "TOTAL PROTEIN:\\*\\*(.*?)\\n")[,2])
    nutrition_total_carbs = trimws(str_match(string = nutrition_response, pattern = "TOTAL CARBS:\\*\\*(.*?)\\n")[,2])
    nutrition_total_fats = trimws(str_match(string = nutrition_response, pattern = "TOTAL FAT:\\*\\*(.*?)\\n")[,2])
    
    
    total_cal_text = paste0("Calorie Estimate: ", nutrition_total_calories)
    total_protein_text = paste0("Protein Estimate: ", nutrition_total_protein)
    total_carb_text = paste0("Carb Estimate: ", nutrition_total_carbs)
    total_fat_text = paste0('Fats Estimate: ', nutrition_total_fats)
    
    estimates_text = c(total_cal_text, total_protein_text, total_carb_text, total_fat_text)
    
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
    
    output$nutritionEstimates = renderUI(
      tags$ul(
        class = 'centered-list',
        lapply(estimates_text, function(estimate){
          tags$li(estimate)
        })
      )
    )
    
    
    
    shinybusy::remove_modal_spinner()
    
    
    shinyjs::show('card1')
    shinyjs::show('card2')
    shinyjs::show('card3')
    shinyjs::show('card4')
    # shinyjs::show('saveButton')
    
    recipe$name = recipe_name
    recipe$ingredients = recipe_ingredients
    recipe$instructions = recipe_instructions
    recipe$estimates = estimates_text
    
    
    
    
    nutrition_disclaimer = trimws(str_match(string = nutrition_response, pattern = "Disclaimer:\\*\\*(.*?)\\n\\nEND")[,2])
  })
  
  # observeEvent(input$saveButton, {
  #   saveRecipe(session, recipe$name, recipe$ingredients, recipe$instructions)
  #   print('recipe saved')
  # })
  # 
  # observeEvent(input$loadButton, {
  #   getRecipes(session, 'hi')
  # })
  # 
  # output$savedRecipes = renderPrint({
  #   response = input$jsResponse
  #   if(!is.null(response)){
  #     response
  #   }else{
  #     
  #   }
  #   
  # })
  
  observeEvent(input$clearCookieBtn, {
    session$sendCustomMessage(type = "clearCookie", list(name = "username"))
    shinyjs::hide('mainPage')
    shinyjs::show('loginPage')
  })
  
  # Function to add a new ingredient input
  observeEvent(input$addIngredientBtn, {
    new_input_id <- paste0("ingredient_", length(ingredient_inputs) + 1)
    new_input <- textInput(new_input_id, label = paste("Ingredient", length(ingredient_inputs) + 1), value = "")
    ingredient_inputs <<- c(ingredient_inputs, list(new_input))
    shinyalert(
      title = "Confirm Ingredients",
      html = TRUE,
      text = tagList(
        div(
          p("Please confirm or correct the ingredients:"),
          do.call(tagList, ingredient_inputs),
          actionButton(inputId = 'submitIngredients', label = 'Submit!'),
          actionButton(inputId = "addIngredientBtn", label = 'Add'),
          actionButton(inputId = 'removeIngredient', label = 'Remove')
        )
      ),
      size = "l",
      showConfirmButton = FALSE
    )
  })
  
  # Function to remove the last ingredient input
  observeEvent(input$removeIngredient, {
    if (length(ingredient_inputs) > 1) {
      ingredient_inputs <<- ingredient_inputs[-length(ingredient_inputs)]
      shinyalert(
        title = "Confirm Ingredients",
        html = TRUE,
        text = tagList(
          div(
            p("Please confirm or correct the ingredients:"),
            do.call(tagList, ingredient_inputs),
            actionButton(inputId = 'submitIngredients', label = 'Submit!'),
            actionButton(inputId = "addIngredientBtn", label = 'Add'),
            actionButton(inputId = 'removeIngredient', label = 'Remove')
          )
        ),
        size = "l",
        showConfirmButton = FALSE
      )
    }
  })
  
  
  
  # gemini_vision(main_prompt, "C:/Users/xbox/Pictures/ingredients.jpg")
}

# Run the application 
shinyApp(ui = ui, server = server)
