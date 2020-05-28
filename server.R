#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.

#######################################################################################
library(shiny)
library(knitr)
library(kableExtra)
library(markdown)
library(rmarkdown)
library(rsconnect)
library(DT)
library(shinyjs)
library(tidyverse)
library(lme4)
library(lmerTest)
library(publishr)
library(shinyBS)
library(bsplus)
library(rintrojs)
source(file = "helpers.R",local = T)
#########
# Define server logic required to draw a histogram
shinyServer(function(input, output,session) {
  # use input csv file

# Start
  output$start <- renderUI({
    HTML(markdown::markdownToHTML(knit('start.Rmd', quiet = TRUE)))
  })
  
# data
df <- eventReactive(input$submit_button, {
  req(input$file1)
  read.csv(input$file1$datapath,
             header = input$header,
             sep = input$sep,
             quote = input$quote)
  })

sample_data = read.csv("data/pSNL2.csv")
output$downloadData <- downloadHandler(
  filename = function() {
    paste("sample_data_", Sys.Date(), ".csv", sep="")
  },
  content = function(file) {
    write.csv(sample_data, file,row.names = F)
  }
)


# output1
output$contents = 
    DT::renderDataTable({
    req(df())
    # load data
    df() %>% datatable2()
    })

# summary
output$ui_sum_group = renderUI({
  req(df())
  tagList(
    tags$hr(),
    modal_sum_group_object,
    h2("Summary Statistics by Group") %>% bs_attach_modal(id_modal = "modal_sum_group"),
    renderDataTable({
      df() %>% 
        select(ID,group,time,baseline) %>% 
        group_by(group,ID) %>% 
        filter(row_number()==1) %>% 
        ungroup() %>%
        group_by(group) %>% 
        summarise(n = n(),
                  baseline_median_Q = median_q(baseline),
                  baseline_mean_sd = mean_sd(baseline)) %>% datatable2()
    })
  )
})

output$ui_sum_time = renderUI({
  req(df())
  tagList(
    tags$hr(),
    modal_sum_group_object2,
    h2("Summary Statistics by Group and Time") %>% bs_attach_modal(id_modal = "modal_sum_group_time"),
    renderDataTable({
      df() %>% 
        group_by(group, time) %>% 
        summarise(n = n(),
                  value_median_Q = median_q(value),
                  value_mean_sd = mean_sd(value)) %>% datatable2()
    })
  )
})


# output2 
output$ui_plot1 = renderUI({
  req(df())
  tagList(
  tags$hr(),
  modal_graph_group_object1,
  h2("Graphic Summary") %>% bs_attach_modal(id_modal = "modal_g_group"),
  renderPlot({plot_func1(df())},width = 600,height = 400)
  )
})

output$ui_plot2 = renderUI({
  req(df())
  tagList(
    tags$hr(),
    modal_graph_group_object2,
    h2("Graphic Summary - By Subject") %>% bs_attach_modal(id_modal = "modal_g_group2"),
    renderPlot({plot_func2(df())},width = 600,height = 400)
  )
})

output$ui_print_var = renderUI({
  req(df())
  tagList(
    tags$hr(),
    modal_var_object,
    h2("Variance Components") %>% bs_attach_modal(id_modal = "modal_var"),
    renderPrint({print_var(df())})
  )
})

output$ui_mod_mixed = renderUI({
  req(df())
  tagList(
    tags$hr(),
    modal_com_mix_object,
    h2("Mixed Model Comparisons") %>% bs_attach_modal(id_modal = "modal_com_mix"),
    renderDataTable({
      mixed_cal(df()) %>% datatable2()
      })
  )
})

output$ui_mod_fixed = renderUI({
  req(df())
  tagList(
    tags$hr(),
    modal_fix_print_out_object,
    h2("Fixed Model Results") %>% bs_attach_modal(id_modal = "modal_fix_print_out_object"),
    renderDataTable({
      fixed_cal(df()) %>% datatable2()
      })
  )
})

output$ui_mix_out = renderUI({
  req(df())
  tagList(
    modal_mix_out_object,
    h2("Mixed Model Results") %>% bs_attach_modal(id_modal = "modal_mix_out"),
    tags$hr(),
    modal_mix_anova_object,
    h3("ANOVA Tests")%>% bs_attach_modal(id_modal = "modal_mix_anova"),
    renderPrint({
      mix_out_func_print(mod_name = input$mix_input_type,df())[[1]]
      }),
    tags$hr(),
    modal_mix_sum_object,
    h3("Summary Results") %>% bs_attach_modal(id_modal = "modal_mix_sum"),
    renderPrint({
      mix_out_func_print(mod_name = input$mix_input_type,df())[[2]]
      }),
    renderPrint({
      mix_out_func_print(mod_name = input$mix_input_type,df())[[3]]
    }),   
    tags$hr(),
    modal_mix_sum_tab_object,
    h3("Output Table") %>% bs_attach_modal(id_modal = "modal_mix_sum_tab"),
    renderDataTable({
      mix_out_func_table1(mod_name = input$mix_input_type,df()) %>% datatable2()
      }),
    tags$hr(),
    modal_mix_test_object,
    h3("Likelihood Ratio Test") %>% bs_attach_modal(id_modal = "modal_mix_test"),
    renderDataTable({
      anova_comp(mod_name = input$mix_input_type,df()) %>% datatable2()
    })
  )
})

output$ui_fix_out = renderUI({
  req(df())
  tagList(
    modal_fix_out_object,
    h2("Fixed Model Results") %>% bs_attach_modal(id_modal = "modal_fix_out"),
    tags$hr(),
    renderPrint({
      fix_out_func_print(mod_name = input$fix_input_type,df())
    })
  )
})

# introjs
steps1 <- reactive(data.frame(element = c(NA,"#side1","#submit_data","#close","#intro","#main1",NA),
                             intro = c("Start the Tour!",
                                       "Choices to Set Upload Options",
                                       "Submit Button for Data Upload",
                                       "Close/Restart App",
                                       "Load Sample Data and Start Tour",
                                       "Uploaded Data Output",
                                       "Done for This Step, Go Next Page!")))
steps2 <- reactive(data.frame(element = c(NA,"#sum_input","#sum_input2","#plot_input",
                                          "#plot_input2","#var","#com_mix","#intro2","main",NA),
                              intro = c("Start Tour for Step 2",
                                        "Show Summary by Group?",
                                        "Show Summary by Group and Time?",
                                        "Graphic Summary by Group",
                                        "Graphic Summary by Group and Time",
                                        "Variance Components, Summary",
                                        "Comparisons of Mix Models",
                                        "The Magic Button to Help You",
                                        "Where Results Are",
                                        "Click Section Titles see Detailed Instructions!"
                                        )))

observeEvent(input$intro,{
  introjs(session,options = list(steps=steps1()))
})

observeEvent(input$intro2,{
  introjs(session,options = list(steps=steps2()))
})
  # close window button
  observeEvent(input$close, {
    js$closeWindow()
    stopApp()})
  #close app while closing window
  session$onSessionEnded(stopApp)
})


