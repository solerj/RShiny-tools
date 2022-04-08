#setwd("D:/1_DataScience/rShiny/biWeeklyShoppingTool")

library(tidyverse)
library(shiny)
library(lubridate)
library(fuzzyjoin)
library(shinydashboard)

myMenu <- data.frame(dish = character()
                     , type = character()
                     , item = character()
                     , quantity = numeric()
                     , metric = character()
                     , stringsAsFactors = FALSE)

recipeUDF <- function(dish, items){
  L <- length(items)
  itemsSplit <- str_split(items, '_')
  recipeDf <- data.frame(
    dish       = rep(dish, L)
    , type     = unlist(lapply(itemsSplit, function(x) x[[1]]))
    , item     = unlist(lapply(itemsSplit, function(x) x[[2]]))
    , quantity = as.numeric(unlist(lapply(itemsSplit, function(x) x[[3]])))
    , metric   = unlist(lapply(itemsSplit, function(x) x[[4]]))
    , stringsAsFactors = FALSE)
  recipeDf
}

myMenu <- rbind(myMenu, recipeUDF("nice spice rice (peanut butter)"
               , c("v_kale_100_g"
                   , "v_coriander_20_g"
                   , "v_garlic_2_"
                   , "v_ginger_5_cm"
                   , "v_red chilli_1_"
                   , "v_spring onions_5_"
                   , "v_red pepper_1_"
                   , "v_baby corn_80_g"
                   , "v_asparagus_60_g"
                   , "v_mangetout_80_g"
                   , "v_broccoli_80_g"
                   , "s_coconut oil_1_tbsp"
                   , "s_sesame oil_1_tbsp"
                   , "s_honey_25_ml"
                   , "s_soy sauce_4_tbsp"
                   , "s_peanut butter_120_g"
                   , "s_basmati rice_1_cup")))

myMenu <- rbind(myMenu, recipeUDF("Vietnamese Sticky Tofu"
                                  , c("v_garlic_1_"
                                      , "v_ginger_2.5_cm"
                                      , "v_red chilli_1_"
                                      , "v_spring onions_3_"
                                      , "v_pok choy_2_heads"
                                      , "v_lime_2_"
                                      , "s_olive oil_3_tbsp"
                                      , "s_coconut water_160_ml"
                                      , "s_tofu_1_"
                                      , "s_sesame oil_2_tbsp"
                                      , "s_soy sauce_4_tbsp"
                                      , "s_brown sugar_80_g"
                                      , "s_basmati rice_1_cup"
                                      , "s_corn flour_50_g"
                                      , "s_sesame seeds_2_tsp")))

myMenu <- rbind(myMenu, recipeUDF("eggs al pasanda"
          , c("v_eggs_8_"
              , "v_carrots_500_g"
              , "v_cauliflower_1_"
              , "s_coconut yoghurt_200_g"
              , "s_sliced almonds_50_g"
              , "v_ginger_2_cm"
              , "v_garlic_2_"
              , "v_corriander_50_g"
              , "s_baking poweder_2_tbsp"
              , "s_milk_100_ml")))

myMenu <- rbind(myMenu, recipeUDF("stuffed peppers"
          , c("v_yellow peppers_4_"
              , "v_lentils_200_g"
              , "s_sundried tomatoes_6_"
              , "v_celery sticks_2_"
              , "s_shredded mozzarella_50_g")))

myMenu <- rbind(myMenu, recipeUDF("pizza blue cheese"
          , c("s_blue cheese_150_g"
              , "s_pizza base_2_"
              , "s_sundried tomatoes_4_"
              , "s_honey_2_tbsp"
              , "s_shredded mozzarella_100_g"
              , "v_basil_0.25_bunches"
              , "v_cherry tomatoes_75_g"
              , "v_walnuts_50_g")))

myMenu <- rbind(myMenu, recipeUDF("australian breakfast"
          , c("s_panini_2_"
              , "v_eggs_2_"
              , "s_halloumi_0.5_"
              , "v_avocado_1_"
              , "v_garlic_1_"
              , "v_lemon_0.25_")))

myMenu <- rbind(myMenu, recipeUDF("bolognese"
          , c("s_spaghetti_300_g"
              , "v_carrots_2_"
              , "v_mushrooms_700_g"
              , "v_red onion_2_"
              , "v_garlic_4_"
              , "v_celery_1_"
              , "s_olive oil_2_tbsp"
              , "s_tomato puree_1_tbsp"
              , "s_red wine_300_ml"
              , "s_balsamic vinegar_1_tsp"
              , "s_dried oregano_0.5_tbsp"
              , "s_soy sauce_2_tsp")))

myMenu <- rbind(myMenu, recipeUDF("pineapple crispy tofu"
          , c("s_tofu_1_"
              , "v_ginger_6_cm"
              , "v_red onion_1_"
              , "v_garlic_1_"
              , "v_green pepper_1_"
              , "s_pineapple juice_200_ml"
              , "s_rice vinegar_60_ml"
              , "s_tomato catchup_60_ml"
              , "s_brown sugar_70_g"
              , "s_garlic powder_1_tsp"
              , "s_onion powder_1_tsp"
              , "s_corn flower_4_tbsp"
              , "s_olive oil_2_tbsp"
              , "s_sesame oil_2_tbsp"
              , "s_chilli flakes_0.5_tsp"
              , "s_pineapple chunks_100_g"
              , "s_rice_1_cup")))

myMenu <- rbind(myMenu, recipeUDF("tuna pulpetti x2"
          , c("s_tuna cans_5_"
              , "v_eggs_3_"
              , "s_olives_1_tbsp"
              , "s_capers_1_tbsp"
              , "s_onion_1_"
              , "s_corn flower_4_tbsp"
              , "s_sundried tomatoes_6_"
              , "v_butternut squash_0.5_"
              , "v_courgettes_4_"
              , "v_cauliflower_1_"
              , "v_carrots_200_g")))

myMenu <- rbind(myMenu, recipeUDF("red thai curry (quick)"
          , c("v_red pepper_1_"
              , "v_green pepper_1_"
              , "v_red chilly_1_"
              , "v_mushrooms_200_g"
              , "v_baby corn_60_g"
              , "s_olive oil_2_tbsp"
              , "s_tin coconut milk_400_ml"
              , "s_vegetable stock_150_ml"
              , "s_brown sugar_1_tbsp"
              , "s_honey_2_tbsp"
              , "s_soy sauce_4_tbsp"
              , "v_baby plum tomatoes_160_g"
              , "v_mangetout_50_g"
              , "v_corriander_1_")))

myMenu <- rbind(myMenu, recipeUDF("red thai curry (with sauce)"
          , c("v_red pepper_1.5_"
              , "v_green pepper_1_"
              , "v_red chilly_4_"
              , "v_mushrooms_200_g"
              , "v_baby corn_60_g"
              , "s_olive oil_2_tbsp"
              , "s_tin coconut milk_400_ml"
              , "s_vegetable stock_150_ml"
              , "s_brown sugar_1_tbsp"
              , "s_honey_2_tbsp"
              , "s_soy sauce_4_tbsp"
              , "v_baby plum tomatoes_160_g"
              , "v_mangetout_50_g"
              , "s_cumin seeds_1_tsp"
              , "s_corriander seeds_2_tbsp"
              , "v_fresh ginger_2_cm"
              , "v_shallots_5_"
              , "v_garlic_5_"
              , "v_lemon grass_2_"
              , "s_black pepper corns_1_tsp"
              , "s_tomato puree_2_tbsp"
              , "v_lime_0.5_"
              , "v_corriander_1_")))

myMenu <- rbind(myMenu, recipeUDF("pasta pesto"
          , c("s_spaghetti_300_g"
              , "v_basil_2_bunches"
              , "s_olive oil_0.33_cup"
              , "v_garlic_3_"
              , "s_pine nuts_0.33_cup"
              , "s_parmesan_0.25_cup")))

myMenu <- rbind(myMenu, recipeUDF("quich lorraine x2"
          , c("v_onion_2_"
              , "v_eggs_4_"
              , "s_milk_250_ml"
              , "s_benna cream_125_ml"
              , "s_cheddar_350_g"
              , "s_pastry_1_"
              , "s_spinach_0.5_packet")))

myMenu <- rbind(myMenu, recipeUDF("irresistable risotto"
          , c("s_macadamia nuts_60_g"
              , "v_red onion_1_"
              , "v_garlic_2_"
              , "s_mixed herbs_3_tbsp"
              , "v_french beans_75_g"
              , "v_asparagus_60_g"
              , "v_kale_60_g"
              , "v_lemon_0.5_"
              , "s_vegetable stock_900_ml"
              , "s_olive oil_2_tbsp"
              , "s_risotto_250_g"
              , "s_dry white wine_125_ml"
              , "s_garden peas_75_g"
              , "s_butter_1.5_tbsp")))

myMenu <- rbind(myMenu, recipeUDF("crispy chilli tofu"
          , c("s_tofu_1_"
              , "s_corn flow_4_tbsp"
              , "s_olive oil_2_tbsp"
              , "v_lemon_2_"
              , "s_orange juice_250_ml"
              , "s_sweet chilly sauce_100_g"
              , "s_soy sauce_3_tbsp"
              , "v_spring onion_1_"
              , "s_sesame seeds_1_tbsp")))

myMenu <- rbind(myMenu, recipeUDF("guaca jacks x2"
          , c("v_sweet potatoes_500_g"
              , "s_olive oil_2_tbsp"
              , "s_chilly flakes_1_tbsp"
              , "v_avocado_2_"
              , "v_shallots_1_"
              , "v_garlic_0.5_"
              , "v_chilly_1_"
              , "v_cherry tomatoes_50_g"
              , "v_lime_1_"
              , "v_corriander_1_"
              , "s_black beans_400_g"
              , "s_panini_4_"
              , "s_vegan hagi_4_")))

myMenu <- rbind(myMenu, recipeUDF("cheese night"
          , c("s_cheeses_3_"
              , "s_stuffed olives_1_"
              , "s_galletti_1_"
              , "v_grapes_1_"
              , "s_bigilla_1_"
              , "s_hummus_1_"
              , "s_bread_1_"
              , "s_bottle of wine_1_")))

myMenu <- rbind(myMenu, recipeUDF("brussels sprouts"
                                  , c("v_mushrooms_200_g"
                                      , "v_brussels sprouts_500_g"
                                      , "v_shallots_2_"
                                      , "s_olive oil_4_tbsp"
                                      , "s_honey_1_tbsp"
                                      , "s_smoked salt_1_tsp"
                                      , "s_smoked paprika_0.5_tsp"
                                      , "s_black pepper_0.5_tsp"
                                      , "s_flaked almonds_25_g")))

menuChoices <- unique(myMenu$dish)
menuChoices <- menuChoices[order(menuChoices)]

body <- dashboardBody(
  column(
    width = 6
    , fluidRow(
      column(
        width = 3
        , valueBoxOutput("date_1", width = 12)
      )
      , column(
        width = 3
        , selectizeInput(inputId = "breakfast_1"
                         , label = "Morning"
                         , choices = menuChoices
                         , options = list(placeholder = 'Select action'
                                          , onInitialize = I('function() { this.setValue(""); }'))
                         , selected = NULL)
        
      )
      , column(
        width = 3
        , selectizeInput(inputId = "lunch_1"
                         , label = "Noon"
                         , choices = menuChoices
                         , options = list(placeholder = 'Select action'
                                          , onInitialize = I('function() { this.setValue(""); }'))
                         , selected = NULL)
        
      )
      , column(
        width = 3
        , selectizeInput(inputId = "dinner_1"
                         , label = "Evening"
                         , choices = menuChoices
                         , options = list(placeholder = 'Select action'
                                          , onInitialize = I('function() { this.setValue(""); }'))
                         , selected = NULL)
      )
    )
    , fluidRow(
      column(
        width = 3
        , valueBoxOutput("date_2", width = 12)
        
      )
      , column(
        width = 3
        , selectizeInput(inputId = "breakfast_2"
                         , label = NULL
                         , choices = menuChoices
                         , options = list(placeholder = 'Select action'
                                          , onInitialize = I('function() { this.setValue(""); }'))
                         , selected = NULL)
        
      )
      , column(
        width = 3
        , selectizeInput(inputId = "lunch_2"
                         , label = NULL
                         , choices = menuChoices
                         , options = list(placeholder = 'Select action'
                                          , onInitialize = I('function() { this.setValue(""); }'))
                         , selected = NULL)
        
      )
      , column(
        width = 3
        , selectizeInput(inputId = "dinner_2"
                         , label = NULL
                         , choices = menuChoices
                         , options = list(placeholder = 'Select action'
                                          , onInitialize = I('function() { this.setValue(""); }'))
                         , selected = NULL)
      )
    )
    , fluidRow(
      column(
        width = 3
        , valueBoxOutput("date_3", width = 12)
        
      )
      , column(
        width = 3
        , selectizeInput(inputId = "breakfast_3"
                         , label = NULL
                         , choices = menuChoices
                         , options = list(placeholder = 'Select action'
                                          , onInitialize = I('function() { this.setValue(""); }'))
                         , selected = NULL)
        
      )
      , column(
        width = 3
        , selectizeInput(inputId = "lunch_3"
                         , label = NULL
                         , choices = menuChoices
                         , options = list(placeholder = 'Select action'
                                          , onInitialize = I('function() { this.setValue(""); }'))
                         , selected = NULL)
        
      )
      , column(
        width = 3
        , selectizeInput(inputId = "dinner_3"
                         , label = NULL
                         , choices = menuChoices
                         , options = list(placeholder = 'Select action'
                                          , onInitialize = I('function() { this.setValue(""); }'))
                         , selected = NULL)
      )
    )
    , fluidRow(
      column(
        width = 3
        , valueBoxOutput("date_4", width = 12)
        
      )
      , column(
        width = 3
        , selectizeInput(inputId = "breakfast_4"
                         , label = NULL
                         , choices = menuChoices
                         , options = list(placeholder = 'Select action'
                                          , onInitialize = I('function() { this.setValue(""); }'))
                         , selected = NULL)
        
      )
      , column(
        width = 3
        , selectizeInput(inputId = "lunch_4"
                         , label = NULL
                         , choices = menuChoices
                         , options = list(placeholder = 'Select action'
                                          , onInitialize = I('function() { this.setValue(""); }'))
                         , selected = NULL)
        
      )
      , column(
        width = 3
        , selectizeInput(inputId = "dinner_4"
                         , label = NULL
                         , choices = menuChoices
                         , options = list(placeholder = 'Select action'
                                          , onInitialize = I('function() { this.setValue(""); }'))
                         , selected = NULL)
      )
    )
    , fluidRow(
      column(
        width = 3
        , valueBoxOutput("date_5", width = 12)
        
      )
      , column(
        width = 3
        , selectizeInput(inputId = "breakfast_5"
                         , label = NULL
                         , choices = menuChoices
                         , options = list(placeholder = 'Select action'
                                          , onInitialize = I('function() { this.setValue(""); }'))
                         , selected = NULL)
        
      )
      , column(
        width = 3
        , selectizeInput(inputId = "lunch_5"
                         , label = NULL
                         , choices = menuChoices
                         , options = list(placeholder = 'Select action'
                                          , onInitialize = I('function() { this.setValue(""); }'))
                         , selected = NULL)
        
      )
      , column(
        width = 3
        , selectizeInput(inputId = "dinner_5"
                         , label = NULL
                         , choices = menuChoices
                         , options = list(placeholder = 'Select action'
                                          , onInitialize = I('function() { this.setValue(""); }'))
                         , selected = NULL)
      )
    )
    , fluidRow(
      column(
        width = 3
        , valueBoxOutput("date_6", width = 12)
        
      )
      , column(
        width = 3
        , selectizeInput(inputId = "breakfast_6"
                         , label = NULL
                         , choices = menuChoices
                         , options = list(placeholder = 'Select action'
                                          , onInitialize = I('function() { this.setValue(""); }'))
                         , selected = NULL)
        
      )
      , column(
        width = 3
        , selectizeInput(inputId = "lunch_6"
                         , label = NULL
                         , choices = menuChoices
                         , options = list(placeholder = 'Select action'
                                          , onInitialize = I('function() { this.setValue(""); }'))
                         , selected = NULL)
        
      )
      , column(
        width = 3
        , selectizeInput(inputId = "dinner_6"
                         , label = NULL
                         , choices = menuChoices
                         , options = list(placeholder = 'Select action'
                                          , onInitialize = I('function() { this.setValue(""); }'))
                         , selected = NULL)
      )
    )
    , fluidRow(
      column(
        width = 3
        , valueBoxOutput("date_7", width = 12)
        
      )
      , column(
        width = 3
        , selectizeInput(inputId = "breakfast_7"
                         , label = NULL
                         , choices = menuChoices
                         , options = list(placeholder = 'Select action'
                                          , onInitialize = I('function() { this.setValue(""); }'))
                         , selected = NULL)
        
      )
      , column(
        width = 3
        , selectizeInput(inputId = "lunch_7"
                         , label = NULL
                         , choices = menuChoices
                         , options = list(placeholder = 'Select action'
                                          , onInitialize = I('function() { this.setValue(""); }'))
                         , selected = NULL)
        
      )
      , column(
        width = 3
        , selectizeInput(inputId = "dinner_7"
                         , label = NULL
                         , choices = menuChoices
                         , options = list(placeholder = 'Select action'
                                          , onInitialize = I('function() { this.setValue(""); }'))
                         , selected = NULL)
      )
    )
    , fluidRow(
      column(
        width = 3
        , valueBoxOutput("date_8", width = 12)
        
      )
      , column(
        width = 3
        , selectizeInput(inputId = "breakfast_8"
                         , label = NULL
                         , choices = menuChoices
                         , options = list(placeholder = 'Select action'
                                          , onInitialize = I('function() { this.setValue(""); }'))
                         , selected = NULL)
        
      )
      , column(
        width = 3
        , selectizeInput(inputId = "lunch_8"
                         , label = NULL
                         , choices = menuChoices
                         , options = list(placeholder = 'Select action'
                                          , onInitialize = I('function() { this.setValue(""); }'))
                         , selected = NULL)
        
      )
      , column(
        width = 3
        , selectizeInput(inputId = "dinner_8"
                         , label = NULL
                         , choices = menuChoices
                         , options = list(placeholder = 'Select action'
                                          , onInitialize = I('function() { this.setValue(""); }'))
                         , selected = NULL)
      )
    )
    , fluidRow(
      column(
        width = 3
        , valueBoxOutput("date_9", width = 12)
        
      )
      , column(
        width = 3
        , selectizeInput(inputId = "breakfast_9"
                         , label = NULL
                         , choices = menuChoices
                         , options = list(placeholder = 'Select action'
                                          , onInitialize = I('function() { this.setValue(""); }'))
                         , selected = NULL)
        
      )
      , column(
        width = 3
        , selectizeInput(inputId = "lunch_9"
                         , label = NULL
                         , choices = menuChoices
                         , options = list(placeholder = 'Select action'
                                          , onInitialize = I('function() { this.setValue(""); }'))
                         , selected = NULL)
        
      )
      , column(
        width = 3
        , selectizeInput(inputId = "dinner_9"
                         , label = NULL
                         , choices = menuChoices
                         , options = list(placeholder = 'Select action'
                                          , onInitialize = I('function() { this.setValue(""); }'))
                         , selected = NULL)
      )
    )
    , fluidRow(
      column(
        width = 3
        , valueBoxOutput("date_10", width = 12)
        
      )
      , column(
        width = 3
        , selectizeInput(inputId = "breakfast_10"
                         , label = NULL
                         , choices = menuChoices
                         , options = list(placeholder = 'Select action'
                                          , onInitialize = I('function() { this.setValue(""); }'))
                         , selected = NULL)
        
      )
      , column(
        width = 3
        , selectizeInput(inputId = "lunch_10"
                         , label = NULL
                         , choices = menuChoices
                         , options = list(placeholder = 'Select action'
                                          , onInitialize = I('function() { this.setValue(""); }'))
                         , selected = NULL)
        
      )
      , column(
        width = 3
        , selectizeInput(inputId = "dinner_10"
                         , label = NULL
                         , choices = menuChoices
                         , options = list(placeholder = 'Select action'
                                          , onInitialize = I('function() { this.setValue(""); }'))
                         , selected = NULL)
      )
    )
    , fluidRow(
      column(
        width = 3
        , valueBoxOutput("date_11", width = 12)
        
      )
      , column(
        width = 3
        , selectizeInput(inputId = "breakfast_11"
                         , label = NULL
                         , choices = menuChoices
                         , options = list(placeholder = 'Select action'
                                          , onInitialize = I('function() { this.setValue(""); }'))
                         , selected = NULL)
        
      )
      , column(
        width = 3
        , selectizeInput(inputId = "lunch_11"
                         , label = NULL
                         , choices = menuChoices
                         , options = list(placeholder = 'Select action'
                                          , onInitialize = I('function() { this.setValue(""); }'))
                         , selected = NULL)
        
      )
      , column(
        width = 3
        , selectizeInput(inputId = "dinner_11"
                         , label = NULL
                         , choices = menuChoices
                         , options = list(placeholder = 'Select action'
                                          , onInitialize = I('function() { this.setValue(""); }'))
                         , selected = NULL)
      )
    )
    , fluidRow(
      column(
        width = 3
        , valueBoxOutput("date_12", width = 12)
        
      )
      , column(
        width = 3
        , selectizeInput(inputId = "breakfast_12"
                         , label = NULL
                         , choices = menuChoices
                         , options = list(placeholder = 'Select action'
                                          , onInitialize = I('function() { this.setValue(""); }'))
                         , selected = NULL)
        
      )
      , column(
        width = 3
        , selectizeInput(inputId = "lunch_12"
                         , label = NULL
                         , choices = menuChoices
                         , options = list(placeholder = 'Select action'
                                          , onInitialize = I('function() { this.setValue(""); }'))
                         , selected = NULL)
        
      )
      , column(
        width = 3
        , selectizeInput(inputId = "dinner_12"
                         , label = NULL
                         , choices = menuChoices
                         , options = list(placeholder = 'Select action'
                                          , onInitialize = I('function() { this.setValue(""); }'))
                         , selected = NULL)
      )
    )
    , fluidRow(
      column(
        width = 3
        , valueBoxOutput("date_13", width = 12)
        
      )
      , column(
        width = 3
        , selectizeInput(inputId = "breakfast_13"
                         , label = NULL
                         , choices = menuChoices
                         , options = list(placeholder = 'Select action'
                                          , onInitialize = I('function() { this.setValue(""); }'))
                         , selected = NULL)
        
      )
      , column(
        width = 3
        , selectizeInput(inputId = "lunch_13"
                         , label = NULL
                         , choices = menuChoices
                         , options = list(placeholder = 'Select action'
                                          , onInitialize = I('function() { this.setValue(""); }'))
                         , selected = NULL)
        
      )
      , column(
        width = 3
        , selectizeInput(inputId = "dinner_13"
                         , label = NULL
                         , choices = menuChoices
                         , options = list(placeholder = 'Select action'
                                          , onInitialize = I('function() { this.setValue(""); }'))
                         , selected = NULL)
      )
    )
    , fluidRow(
      column(
        width = 3
        , valueBoxOutput("date_14", width = 12)
        
      )
      , column(
        width = 3
        , selectizeInput(inputId = "breakfast_14"
                         , label = NULL
                         , choices = menuChoices
                         , options = list(placeholder = 'Select action'
                                          , onInitialize = I('function() { this.setValue(""); }'))
                         , selected = NULL)
        
      )
      , column(
        width = 3
        , selectizeInput(inputId = "lunch_14"
                         , label = NULL
                         , choices = menuChoices
                         , options = list(placeholder = 'Select action'
                                          , onInitialize = I('function() { this.setValue(""); }'))
                         , selected = NULL)
        
      )
      , column(
        width = 3
        , selectizeInput(inputId = "dinner_14"
                         , label = NULL
                         , choices = menuChoices
                         , options = list(placeholder = 'Select action'
                                          , onInitialize = I('function() { this.setValue(""); }'))
                         , selected = NULL)
      )
    )
  )
  , column(
    width = 2
    , tableOutput('vegList1')
  )
  , column(
    width = 2
    , tableOutput('vegList2')
  )
  , column(
    width = 2
    , tableOutput('supermarketList')
  )
)

sidebar <- dashboardSidebar(
  textInput(inputId = "daysDiff"
            , label = "Days to Shop"
            , value = 3)
  , textInput(inputId = "daysInFirstWeek"
              , label = "Days in 1st Week"
              , value = 7)
)

ui <- dashboardPage(header = dashboardHeader(title = "Food Plan")
  , sidebar = sidebar
  , body = body
)

server <- function(input, output, session) {
  
  daysDiff <- reactive({
    as.integer(input$daysDiff)
  })
  
  daysInFirstWeek <- reactive({
    as.integer(input$daysInFirstWeek)
  })
  
  output$date_1 <- renderValueBox({
    valueBox(
      value = tags$p(paste0(wday(today()+daysDiff()+0, label = T), ' ', mday(today()+daysDiff()+0), ' ', month(today()+daysDiff()+0, label = TRUE)), style = "font-size: 50%; height: 15px;")
      , subtitle = NULL
      , color = if(daysInFirstWeek() >= 1){"green"}else{"blue"}
    )
  })
  output$date_2 <- renderValueBox({
    valueBox(
      value = tags$p(paste0(wday(today()+daysDiff()+1, label = T), ' ', mday(today()+daysDiff()+1), ' ', month(today()+daysDiff()+1, label = TRUE)), style = "font-size: 50%; height: 15px;")
      , subtitle = NULL
      , color = if(daysInFirstWeek() >= 2){"green"}else{"blue"}
    )
  })
  output$date_3 <- renderValueBox({
    valueBox(
      value = tags$p(paste0(wday(today()+daysDiff()+2, label = T), ' ', mday(today()+daysDiff()+2), ' ', month(today()+daysDiff()+2, label = TRUE)), style = "font-size: 50%; height: 15px;")
      , subtitle = NULL
      , color = if(daysInFirstWeek() >= 3){"green"}else{"blue"}
    )
  })
  output$date_4 <- renderValueBox({
    valueBox(
      value = tags$p(paste0(wday(today()+daysDiff()+3, label = T), ' ', mday(today()+daysDiff()+3), ' ', month(today()+daysDiff()+3, label = TRUE)), style = "font-size: 50%; height: 15px;")
      , subtitle = NULL
      , color = if(daysInFirstWeek() >= 4){"green"}else{"blue"}
    )
  })
  output$date_5 <- renderValueBox({
    valueBox(
      value = tags$p(paste0(wday(today()+daysDiff()+4, label = T), ' ', mday(today()+daysDiff()+4), ' ', month(today()+daysDiff()+4, label = TRUE)), style = "font-size: 50%; height: 15px;")
      , subtitle = NULL
      , color = if(daysInFirstWeek() >= 5){"green"}else{"blue"}
    )
  })
  output$date_6 <- renderValueBox({
    valueBox(
      value = tags$p(paste0(wday(today()+daysDiff()+5, label = T), ' ', mday(today()+daysDiff()+5), ' ', month(today()+daysDiff()+5, label = TRUE)), style = "font-size: 50%; height: 15px;")
      , subtitle = NULL
      , color = if(daysInFirstWeek() >= 6){"green"}else{"blue"}
    )
  })
  output$date_7 <- renderValueBox({
    valueBox(
      value = tags$p(paste0(wday(today()+daysDiff()+6, label = T), ' ', mday(today()+daysDiff()+6), ' ', month(today()+daysDiff()+6, label = TRUE)), style = "font-size: 50%; height: 15px;")
      , subtitle = NULL
      , color = if(daysInFirstWeek() >= 7){"green"}else{"blue"}
    )
  })
  output$date_8 <- renderValueBox({
    valueBox(
      value = tags$p(paste0(wday(today()+daysDiff()+7, label = T), ' ', mday(today()+daysDiff()+7), ' ', month(today()+daysDiff()+7, label = TRUE)), style = "font-size: 50%; height: 15px;")
      , subtitle = NULL
      , color = if(daysInFirstWeek() >= 8){"green"}else{"blue"}
    )
  })
  output$date_9 <- renderValueBox({
    valueBox(
      value = tags$p(paste0(wday(today()+daysDiff()+8, label = T), ' ', mday(today()+daysDiff()+8), ' ', month(today()+daysDiff()+8, label = TRUE)), style = "font-size: 50%; height: 15px;")
      , subtitle = NULL
      , color = if(daysInFirstWeek() >= 9){"green"}else{"blue"}
    )
  })
  output$date_10 <- renderValueBox({
    valueBox(
      value = tags$p(paste0(wday(today()+daysDiff()+9, label = T), ' ', mday(today()+daysDiff()+9), ' ', month(today()+daysDiff()+9, label = TRUE)), style = "font-size: 50%; height: 15px;")
      , subtitle = NULL
      , color = if(daysInFirstWeek() >= 10){"green"}else{"blue"}
    )
  })
  output$date_11 <- renderValueBox({
    valueBox(
      value = tags$p(paste0(wday(today()+daysDiff()+10, label = T), ' ', mday(today()+daysDiff()+10), ' ', month(today()+daysDiff()+10, label = TRUE)), style = "font-size: 50%; height: 15px;")
      , subtitle = NULL
      , color = if(daysInFirstWeek() >= 11){"green"}else{"blue"}
    )
  })
  output$date_12 <- renderValueBox({
    valueBox(
      value = tags$p(paste0(wday(today()+daysDiff()+11, label = T), ' ', mday(today()+daysDiff()+11), ' ', month(today()+daysDiff()+11, label = TRUE)), style = "font-size: 50%; height: 15px;")
      , subtitle = NULL
      , color = if(daysInFirstWeek() >= 12){"green"}else{"blue"}
    )
  })
  output$date_13 <- renderValueBox({
    valueBox(
      value = tags$p(paste0(wday(today()+daysDiff()+12, label = T), ' ', mday(today()+daysDiff()+12), ' ', month(today()+daysDiff()+12, label = TRUE)), style = "font-size: 50%; height: 15px;")
      , subtitle = NULL
      , color = if(daysInFirstWeek() >= 13){"green"}else{"blue"}
    )
  })
  output$date_14 <- renderValueBox({
    valueBox(
      value = tags$p(paste0(wday(today()+daysDiff()+13, label = T), ' ', mday(today()+daysDiff()+13), ' ', month(today()+daysDiff()+13, label = TRUE)), style = "font-size: 50%; height: 15px;")
      , subtitle = NULL
      , color = if(daysInFirstWeek() >= 14){"green"}else{"blue"}
    )
  })
  
  foodInput <- reactive({
    x <- reactiveValuesToList(input)
    foodMenu <- data.frame(
      varName = names(x)[names(x) != 'sidebarItemExpanded']
      , varValue = unlist(x, use.names = FALSE)
      , stringsAsFactors = FALSE
    ) %>%
      filter(grepl('breakfast', varName) |
               grepl('lunch', varName) |
               grepl('dinner', varName))
    foodMenuSplit <- str_split(foodMenu$varName, '_')
    foodMenu %>%
      mutate(dow = as.integer(unlist(lapply(foodMenuSplit, function(x) x[[2]])))
             , wkNum = case_when(dow <= daysInFirstWeek() ~ 1
                                 , TRUE ~ 2))
  })

  toBuy <- reactive({
    foodInput() %>%
      left_join(y = myMenu, by = c("varValue" = "dish")) %>%
      group_by(wkNum, type, item, metric) %>%
      summarise(quantity = sum(quantity)) %>%
      ungroup() %>%
      arrange(item)
  })
  
  output$vegList1 <- shiny::renderTable({
    toBuy() %>% filter(type == 'v', wkNum == 1) %>% select(item, quantity, metric)
    #foodInput() %>% arrange(varName)
    #foodInput()
  }, sanitize.text.function = function(x) x)
  
  output$vegList2 <- shiny::renderTable({
    toBuy() %>% filter(type == 'v', wkNum == 2) %>% select(item, quantity, metric)
    #foodInput() %>% arrange(varName)
    #foodInput()
  }, sanitize.text.function = function(x) x)
  
  output$supermarketList <- shiny::renderTable({
    toBuy() %>% filter(type == 's') %>% group_by(item, metric) %>% summarise(quantity = sum(quantity)) %>% ungroup()
  }, sanitize.text.function = function(x) x)
  
}

shinyApp(ui, server)
