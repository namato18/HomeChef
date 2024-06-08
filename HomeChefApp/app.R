library(shiny)
library(shinybusy)
library(shiny.pwa)
library(shinyWidgets)
library(bslib)
library(stringr)
library(shinyjs)
library(shinyalert)
library(aws.s3)
library(shinydisconnect)

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
  
  disconnectMessage(text = "Connection timed out! Please refresh the page (recipes saved)",
                    refresh = "Refresh",
                    top = 'center'),
  
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

.btn-danger-custom {
  background-color: red !important;
  color: white !important;
  border: none;
  border-radius: 10px;
  padding: 10px 20px;
  font-size: 16px;
  transition: background-color 0.3s ease;
}
.btn-danger-custom:hover {
  background-color: darkred !important;
}

.btn-primary-custom {
  background-color: blue !important;
  color: white !important;
  border: none;
  border-radius: 10px;
  padding: 10px 20px;
  font-size: 16px;
  transition: background-color 0.3s ease;
}
.btn-primary-custom:hover {
  background-color: darkblue !important;
}

.btn-primary-custom-full {
  background-color: #206ef5 !important;
  color: white !important;
  border: none !important;
  border-radius: 25px !important;
  width: 100% !important;
  padding: 15px !important;
  font-size: 18px !important;
  margin-bottom: 10px !important;
}
.btn-primary-custom-full:hover {
  background-color: #144599 !important;
}

/* New styles for bottom bar and buttons */
.navbar-fixed-bottom .navbar-collapse {
  background-color: #007aff;
  border-radius: 10px 10px 0 0;
  box-shadow: 0 -2px 10px rgba(0, 0, 0, 0.1);
  width: 100%;
  padding: 0;
}

.navbar-fixed-bottom .navbar-nav {
  display: flex;
  justify-content: space-around;
  align-items: stretch; /* Ensure items stretch to fill the container height */
  width: 100%;
}

.navbar-fixed-bottom .navbar-nav > li {
  flex: 1;
  text-align: center; /* Center the text within each button */
}

.navbar-fixed-bottom .navbar-nav > li > a {
  color: white !important;
  font-size: 16px;
  padding: 10px 0;
  display: flex;
  align-items: center;
  justify-content: center;
}

.navbar-fixed-bottom .navbar-nav > li > a:hover {
  background-color: #005bb5;
  border-radius: 10px;
}


  
    ')),
    # Include custom JavaScript to hide the loading screen once the app is fully loaded
    tags$script(HTML("
    $(document).on('shiny:connected', function() {
      $('#loading-screen').fadeOut('slow', function() {
        $('html, body').css('overflow-y', 'auto'); /* Enable scrolling in y-direction */
        $('html, body').css('overflow-x', 'hidden'); /* Ensure x-direction scrolling is hidden */
      });
    });
    "))
  ),
  
  # shinybusy::add_busy_spinner(spin = 'semipolar', color = 'white', position = 'full-page'),
  
  theme = bslib::bs_theme(preset = 'darkly',
                          primary = '#007aff',
                          secondary = 'white',
                          success = 'lightgreen',
                          warning = 'orange',
                          danger = 'red') %>%
    bs_add_rules('

        
.card {
  background-color: rgba(80, 80, 80, 0.7); /* Transparent grey */
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
  background-color: #007aff !important;
  border: none !important;
  border-radius: 10px !important;
  color: white !important;
  padding: 10px 20px !important;
  font-size: 16px !important;
  transition: background-color 0.3s ease !important;
}

.btn-primary:hover {
  background-color: #005bb5 !important;
}

.shiny-input-container {
  margin-bottom: 20px !important;
}

.input-shadow {
  box-shadow: 0 1px 3px rgba(0, 0, 0, 0.1) !important;
  border-radius: 10px !important;
}

.tab-content > .tab-pane {
  display: none !important;
}

.tab-content > .active {
  display: block !important;
  animation: slideInFromBottom 0.5s forwards !important;
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

.logo-container {
  display: flex !important;
  justify-content: center !important;
  margin-top: 20px !important;
}

.logo {
  height: 100px !important;
  width: auto !important;
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
  
  navbarPage(
    title = "",
    id = 'tabs',
    position = 'fixed-bottom',
    fluid = TRUE,
    
    tabPanel(
      title = div(icon('home'), "Home"),
      
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
                 # actionButton("clearCookieBtn", "Clear Cookie and Logout"),
                 
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
                   column(width = 4, offset = 4,
                          actionButton(inputId = 'generate',
                                       label = 'Get Recipe!',
                                       icon = icon('utensils'),
                                       class = 'btn-primary-custom-full'),
                          # actionBttn(inputId = 'generate',
                          #            label = 'Get Recipe!',
                          #            color = 'primary',
                          #            icon = icon('utensils'),
                          #            style = 'jelly',
                          #            block = TRUE),
                          actionButton(inputId = 'showRecent',
                                       label = 'Show Your Latest Recipe!',
                                       icon = icon('repeat'),
                                       class = 'btn-primary-custom-full'),
                          # actionBttn(inputId = 'showRecent',
                          #            label = 'Show Your Last Recipe!',
                          #            color = 'primary',
                          #            icon = icon('repeat'),
                          #            style = 'jelly',
                          #            block = TRUE),
                          # actionButton(inputId = 'disconnect',
                          #              label = 'Disconnect',
                          #              icon = icon('repeat'),
                          #              class = 'btn-danger-custom-full'),
                          # actionBttn(inputId = 'disconnect',
                          #            label = 'Disconnect',
                          #            color = 'danger',
                          #            icon = icon('repeat'),
                          #            style = 'jelly',
                          #            block = TRUE),
                          # verbatimTextOutput('debugging'),
                          # textOutput('userLoggedIn'),
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
    )
    
  ),
  
  tabPanel(
    title = div(icon('repeat'), "Past Recipes"),
    h3('past recipes')
    
  )
  

  
  
  
  
  
  ))

# ---- Define server logic ----
server <- function(input, output, session) {
  
  shinyjs::hide("card1")
  shinyjs::hide('card2')
  shinyjs::hide('card3')
  shinyjs::hide('card4')
  shinyjs::hide('showRecent')
  
  
  
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
      
      # output$userLoggedIn = renderText(rv$username)
      
      user_tracker = aws.s3::s3readRDS(object = 'users.rds', bucket = 'homechef-tracker')
      latest_recipe = user_tracker$recipes[user_tracker$username == rv$username][[1]][[length(user_tracker$recipes[user_tracker$username == rv$username][[1]])]]
      # latest_recipe = user_tracker$recipes[user_tracker$username == 'newUser'][[1]][[length(user_tracker$recipes[user_tracker$username == 'newUser'][[1]])]]
      
      # output$debugging = renderText(toString(latest_recipe))
      

      # print(rv$username)
      # print(latest_recipe)

      if(!is.na(latest_recipe[[1]])){
        shinyjs::show('showRecent')
        
        latest_recipe_name = latest_recipe$name
        latest_recipe_ingredients = latest_recipe$ingredients
        latest_recipe_instructions = latest_recipe$instructions
        latest_recipe_estimates = latest_recipe$estimates
        
        output$recipeName = renderText(latest_recipe_name)
        output$recipeIngredients = renderUI(
          tags$ul(
            class = 'centered-list',
            lapply(latest_recipe_ingredients, function(ingredient){
              tags$li(ingredient)
            })
          )
        )
        output$recipeInstructions = renderUI(
          tags$ul(
            class = 'centered-list',
            lapply(latest_recipe_instructions, function(instruction){
              tags$li(instruction)
            })
          )
        )
        
        output$nutritionEstimates = renderUI(
          tags$ul(
            class = 'centered-list',
            lapply(latest_recipe_estimates, function(estimate){
              tags$li(estimate)
            })
          )
        )
      }else{}

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
      
      
      # vision_response = gemini_vision(vision_prompt, "C:/Users/xbox/Pictures/food.jpg")
      
      print(vision_response)
      
      
      ingredients_ind = str_match_all(string = vision_response, pattern = "(?m)^(?:\\*\\s*)?(.*?)(?:\\s*\\*)?$")[[1]][,2]
      ingredients_for_mod = paste(ingredients_ind, collapse = ", ")
      print(ingredients_for_mod)

      rv$num_ingredients = length(ingredients_ind)
      
      
      # ingredient_inputs = lapply(seq_along(ingredients_ind), function(i){
      #   textInput(paste0('ingredient_',i), label = paste("Ingredient",i), value = ingredients_ind[i])
      # })
      
      # rv$ingredient_inputs = ingredient_inputs
      
      shinybusy::remove_modal_spinner()
      # Show the dynamic inputs in a shinyalert modal
      shinyalert(
        title = "Confirm Ingredients",
        html = TRUE,
        text = tagList(
          div(
            style = 'display: flex; flex-direction: column;',
            imageOutput('imgOut'),
            p("Please confirm or correct the ingredients:"),
            textAreaInput(inputId = 'updatedIngredients', label = "", value = ingredients_for_mod, rows = 10),
            actionButton(inputId = 'submitIngredients', label = 'Submit!', class = 'btn-primary-custom'),
            actionButton(inputId = 'cancel', label = "Cancel", class = 'btn-danger-custom')
            # actionButton(inputId = "addIngredientBtn", label = 'add'),
            # actionButton(inputId = 'removeIngredient', label = 'remove')
          )
        ),
        size = "l",
        showConfirmButton = FALSE
      )
      
      output$imgOut = renderImage({
        list(
          src = path,
          width = "300px",
          height = "300px"
        )
      }, deleteFile = FALSE)
    }else{
      
    }
    
    
    
  })
  
  observeEvent(input$submitIngredients, {
    # Retrieve the values of all text inputs
    # ingredient_values <- lapply(seq_along(ingredients_ind), function(i) {
    #   input[[paste0('ingredient_', i)]]
    # })
    
    # You can now use ingredient_values for further processing
    # ingredients = (paste(ingredients_for_mod, collapse = ", "))
    ingredients = input$updatedIngredients
    
    shinybusy::show_modal_spinner(spin = 'semipolar',
                                  color = 'white',
                                  text = 'PREPARE TO FEAST!')
    
    recipe_prompt = paste0("Using strictly the following ingredients, give me a recipe.",
                           " The ingredients are: ",ingredients,". ",
                           " You may only use the ingredients provided, do not use anything else. ",
                           "Please give the amount of each ingredient to use as well.",
                           " Return your response in the format: \\n **(name of recipe)** \\n",
                           "**INGREDIENTS:**\\n",
                           "\\n\\n *(recipe item)* \\n\\n",
                           "\\n\\n **INSTRUCTIONS** \\n\\n",
                           " (Instructions on how to cook numbered like \\n digit.",
                           " Please end the last step with the words 'END RECIPE'.")
    print(recipe_prompt)
    recipe_response = gemini(recipe_prompt)
    
    print(recipe_response)
    
    recipe_name = str_match(string = recipe_response, pattern = "\\*\\*(.*?)\\*\\*")[,2]
    
    ingredients_section = str_match(string = recipe_response, pattern = "(?s)INGREDIENTS(.*)INSTRUCTIONS")[,2]
    recipe_ingredients = trimws(str_match_all(string = ingredients_section, pattern = "\\* ([^\\*]+)")[[1]][,2])
    recipe_ingredients = gsub(pattern = '\n', replacement = '', x = recipe_ingredients, fixed = TRUE)
    recipe_ingredients = gsub(pattern = '\\n', replacement = '', x = recipe_ingredients, fixed = TRUE)
    print(recipe_ingredients)
    
    instructions_section = str_match(string = recipe_response, pattern = "(?s)INSTRUCTIONS(.*)END RECIPE")[,2]
    recipe_instructions = trimws(str_match_all(string = instructions_section, pattern = "\\d+\\.(.*?)\\n")[[1]][,2])
    recipe_instructions <- gsub("\\s*END RECIPE\\s*$", "", recipe_instructions)
    print(recipe_instructions)
    
    
    
    nutrition_info = GetNutrition(recipe_ingredients)
    
    
    if(any(is.na(c(nutrition_info$nutrition_total_calories,
                   nutrition_info$nutrition_total_protein,
                   nutrition_info$nutrition_total_fats,
                   nutrition_info$nutrition_total_carbs)))){

      stop('something went wrong nutrition')
      
    }
    
    print('made it line 589')
    
    total_cal_text = paste0("Calorie Estimate: ", nutrition_info$nutrition_total_calories)
    total_protein_text = paste0("Protein Estimate: ", nutrition_info$nutrition_total_protein)
    total_carb_text = paste0("Carb Estimate: ", nutrition_info$nutrition_total_carbs)
    total_fat_text = paste0('Fats Estimate: ', nutrition_info$nutrition_total_fats)
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
    
    
    
    
    # nutrition_disclaimer = trimws(str_match(string = nutrition_response, pattern = "Disclaimer:\\*\\*(.*?)\\n\\nEND")[,2])
    
    
    user_tracker = aws.s3::s3readRDS(object = 'users.rds', bucket = 'homechef-tracker')
    # user = 'newUser'
    
    recipe_list = list(
      name = recipe_name,
      ingredients = recipe_ingredients,
      instructions = recipe_instructions,
      estimates = estimates_text
    )
    
    # output$debugging = renderText(toString(user_tracker$recipes[user_tracker$username == 'newUser'] == 'NA'))
    # output$debugging = renderText(toString(user_tracker$recipes[user_tracker$username == 'nick']))
    # user_tracker$recipes[user_tracker$username == rv$username] == 'NA'
    
    if(is.na(user_tracker$recipes[user_tracker$username == rv$username])){
      # output$debugging = renderText("We are within the code for starting a new list")
      user_tracker$recipes[user_tracker$username == rv$username] = list(list(recipe_list))

      saveRDS(user_tracker, file = paste0(tempdir(), "/tmp.rds"))

      put_object(
        file = file.path(tempdir(), "tmp.rds"),
        object = paste0("users.rds"),
        bucket = paste0("homechef-tracker")
      )
      # output$debugging = renderText("We started a new entry for user")
    }else{
      # output$debugging = renderText("We are within the code for adding to current list")
      
      # Retrieve the existing recipes list
      existing_recipes <- user_tracker$recipes[user_tracker$username == rv$username][[1]]

      # Append the new recipe to the existing list
      updated_recipes <- append(existing_recipes, list(recipe_list))

      # Update the data frame with the new list of recipes
      user_tracker$recipes[user_tracker$username == rv$username] <- list(updated_recipes)


      saveRDS(user_tracker, file = paste0(tempdir(), "/tmp.rds"))

      put_object(
        file = file.path(tempdir(), "tmp.rds"),
        object = paste0("users.rds"),
        bucket = paste0("homechef-tracker")
      )
      
      # output$debugging = renderText("We added to existing list")
    }
    

    
    
  })
  
  
  # observeEvent(input$clearCookieBtn, {
  #   session$sendCustomMessage(type = "clearCookie", list(name = "username"))
  #   shinyjs::hide('mainPage')
  #   shinyjs::show('loginPage')
  # })
  
  observeEvent(input$showRecent, {
    shinyjs::show('card1')
    shinyjs::show('card2')
    shinyjs::show('card3')
    shinyjs::show('card4')
  })

  # observeEvent(input$disconnect, {
  #   session$close()
  # })
  
  
  
  # gemini_vision(main_prompt, "C:/Users/xbox/Pictures/ingredients.jpg")
}

# Run the application 
shinyApp(ui = ui, server = server)
