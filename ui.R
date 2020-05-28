
# Author: Hao Deng

library(shiny)
library(shinythemes)
library(knitr)
library(markdown)
library(rmarkdown)
library(shinyjs)
library(shinyBS)
library(bsplus)
library(tidyverse)
library(rintrojs)
source(file = "guide.R",local = T)
###############################################################

full_width = function(){
tags$head(tags$style(HTML("
                               body {
                          width: 100% !important;
                          max-width: 100% !important;
                          }
                          
                          ")))
}

first_page = function(){
  sidebarLayout(
    # Sidebar
    sidebarPanel(id = "side1",
      titlePanel("Uploading Files"),
      tags$hr(),# Horizontal line ----
      file1_object,
      tags$hr(),# Horizontal line ----
      # Input: Checkbox if file has header ----
      header_object,
      tags$hr(), # Horizontal line ----
      # Input: Select separator ----
      sep_object,
      # Input: Select quotes ----
      quote_object,
      tags$hr(), # Horizontal line ----
      actionButton("submit_button", "Submit",style="color: #fff; background-color: #337ab7; border-color: #2e6da4"),
      downloadButton("downloadData", "Exemplar",style="color: #fff; background-color: #057337; border-color: #2e6da4"),
      useShinyjs(),
      extendShinyjs(text = jscode, functions = c("closeWindow")),
      actionButton("intro", "Tutorial",style="color: #fff; background-color: #00cc99; border-color: #464647"),
      actionButton("close", "Exit",style="color: #fff; background-color: #464647; border-color: #464647")
      ),
    # Main panel
    mainPanel(DT::dataTableOutput("contents"))
  )  
}

second_page = function(){
  source(file = "guide.R",local = T)
  sidebarLayout(
    # Sidebar
    sidebarPanel(
      tagList(
      titlePanel("Summary Statistics"),
      sum_input_object1,
      sum_input_object2,
      tags$hr()# Horizontal line ---- 
      ),
      tagList(
      titlePanel("Graphic Summary"), 
      plot_input_object1,
      plot_input_object2,
      tags$hr()# Horizontal line ----
      ), 
      titlePanel("Model Selection"),
      var_input_object,
      com_mix_input_object,
      tags$hr(),# Horizontal line ----
      actionButton("intro2", "Tutorial",style="color: #fff; background-color: #00cc99; border-color: #464647")
    ),
    # Main panel
    mainPanel(id = "main2",
      conditionalPanel("input.sum_input == true",uiOutput("ui_sum_group")),
      conditionalPanel("input.sum_input2 == true",uiOutput("ui_sum_time")),
      conditionalPanel("input.plot_input == true",uiOutput("ui_plot1")),
      conditionalPanel("input.plot_input2 == true",uiOutput("ui_plot2")),
      conditionalPanel("input.var == true",uiOutput("ui_print_var")),
      conditionalPanel("input.com_mix == true",uiOutput("ui_mod_mixed"))
    )
  )  
}


third_page = function(){
  source(file = "guide.R",local = T)
  sidebarLayout(
    # Sidebar
    sidebarPanel(
      titlePanel("Fixed or Mixed Model"),
      selectInput("mod_type", "choose a model type", 
                  c("choose a model",Fixed = 'Fixed',Mixed = 'Mixed'),selected = "choose a model"),
      tags$hr(),# Horizontal line ----
      conditionalPanel(condition = "input.mod_type == 'Fixed'",
                       selectInput("fix_input_type", 
                                   "choose a fix model", 
                                   choices = 
                                   c("Time only Model" = "mod_fix_1",
                                     "Group plus Time Model" = "mod_fix_2",
                                     "Group by Time Model" = "mod_fix_3")
                                   )),
      
      conditionalPanel(condition = "input.mod_type == 'Mixed'",
                       selectInput("mix_input_type", 
                                   "choose a mix model type",
                                   list(
                                   `Random Intercept Models` = 
                                   c("RI:Log-time Model" = "mod_mix_log",
                                     "RI:Linear-time Model" = "mod_mix_1",
                                     "RI:Quatratic-time Model" = "mod_mix_2",
                                     "RI:Cubic-time Model" = "mod_mix_3"),
                                    `Random Intercept and Slope Models` =  
                                     c("RIS:Log-time Model" = "mod_mix_logr",
                                       "RIS:Linear-time Model" = "mod_mix_1r",
                                       "RIS:Quatratic-time Model" = "mod_mix_2r",
                                       "RIS:Cubic-time Model" = "mod_mix_3r")
                                   ))
                       ),      
    actionButton("intro3", "Tutorial",style="color: #fff; background-color: #00cc99; border-color: #464647")
    ),
    # Main panel
    mainPanel(
      conditionalPanel("input.mod_type == 'Fixed' ",uiOutput("ui_fix_out")),
      conditionalPanel("input.mod_type == 'Mixed' ",uiOutput("ui_mix_out"))
    )
  )  
}

jscode <- "shinyjs.closeWindow = function() { window.close(); }"

##########################################################################################

# Define UI for application that draws a histogram
shinyUI(
fluidPage(theme = shinytheme("yeti"),full_width(),
          rintrojs::introjsUI(),
titlePanel(title=div(img(src="logo.png",
                         height = 127.5,
                         width = 300,
                         style = "margin:1px 1px"), ""), windowTitle = ""),
navbarPage("GC Modelr V1.1",
# Start
tabPanel("START",fluidPage(uiOutput("start")),id = "start"),
# tab 1
tabPanel("Step 1: Load Data",first_page(),id = "one"),
tabPanel("Step 2: Analyze",second_page(), id = "two"),
tabPanel("Step 3: Output",third_page()),id = "three"),
#use bs
use_bs_tooltip(),
use_bs_popover(),
withMathJax()
))

