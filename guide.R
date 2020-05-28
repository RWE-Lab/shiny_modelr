
# uploding file input
file1_object = 
fileInput(inputId = "file1", label = "Choose CSV File",
          multiple = TRUE,
          accept = c("text/csv","text/comma-separated-values,text/plain",".csv")
          )%>%
  shinyInput_label_embed(
      bs_embed_popover(shiny::icon("info-circle"),
        title = "", 
        content = "Upload a file here (csv preferred). Data will not be uploaded until hit submit button.", 
        placement = "bottom")
  )
# choose whether header
header_object = 
checkboxInput("header", "Header",TRUE) %>%
  shinyInput_label_embed(
    bs_embed_popover(shiny::icon("info-circle"),
                     title = "", 
                     content = "Include header?", placement = "left")
  )
# choose which separtor used
sep_object = 
selectInput("sep", "Separator",choices = c(Comma = ",",Semicolon = ";",Tab = "\t"),selected = ",") %>%
  shinyInput_label_embed(
    bs_embed_popover(shiny::icon("info-circle"),
                     title = "", 
                     content = "Pick a separator: comma, semicolon or tab?", placement = "bottom")
  )
# choose quote
quote_object = 
selectInput("quote", "Quote",choices = c(None = "","Double Quote" = '"',"Single Quote" = "'"),selected = '"') %>%
  shinyInput_label_embed(
    bs_embed_popover(shiny::icon("info-circle"),
                     title = "", 
                     content = "Pick a quote: double or single or none?", placement = "bottom")
  )
# summary options
sum_input_object1 = checkboxInput("sum_input","Group Summary",T)
sum_input_object2 = checkboxInput("sum_input2","Group Summary by Time",F)
plot_input_object1 = checkboxInput("plot_input","Summary Plot by Group",T)
plot_input_object2 = checkboxInput("plot_input2","Spaghetti Plot by Subject ID",F)
var_input_object = checkboxInput("var","Variance Components",T)
com_mix_input_object = checkboxInput("com_mix","Comparison,Mixed Models",T)




