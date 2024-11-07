#################### 1. Load Packages ####################
library(shiny)
library(shinythemes)
library(shinyBS)
library(readxl)
library(readr)
library(dplyr)
library(DT)










#################### 2. Load Questions ####################
# Function to load questions
load_data <- function(folder_path = "Questions") {
  # Create a list of all excel/csv-files containing the questions
  files <- list.files(folder_path, pattern = "\\.(xlsx|csv)$", full.names = TRUE)
  
  # Create a list to save the questions
  data_list <- list()
  
  # Read all files in the file-ist
  for (file in files) {
    # Read excel-files
    if (grepl("\\.xlsx$", file)) {
      df <- read_excel(file)} 
    # Read csv-files
    else if (grepl("\\.csv$", file)) {
      df <- read_csv(file)
    }
    
    # Save the filename as a varialbe
    df$source_file <- basename(file)
    
    # Add the new file to the overall list
    data_list[[length(data_list) + 1]] <- df  # Fügt den Dataframe der Liste hinzu
  }
  
  # Bind all files to one dataframe
  combined_data <- bind_rows(data_list)
  
  
  #Return the overall list with all questions
  return(combined_data)
}


# Run the function to load all questions
Data <- load_data()


# Select selectable Variables
Variable_selection <- select(Data, -ID, -Type, -Version, -Question:-D_cor)
Variable_selection <- colnames(Variable_selection)










#################### 3. Define UI ####################
# Define UI for application that displays loaded data
ui <- navbarPage(
  # Define a theme for the shiny-app
  theme = shinytheme("paper"),

  # Define Title of the application
  "Pool-Tool",

  tabPanel("Datenanzeige",
  # Sidebar with file selection
  sidebarLayout(
    sidebarPanel(
      bsCollapse(id = "collapseVariables",
                 bsCollapsePanel("Select Variables to Display", 
        checkboxGroupInput(
          inputId = "Var_Select",
          label = "",
          choices = Variable_selection,
          selected = Variable_selection
          )
        )
      ),
      bsCollapse(id = "collapseFilters",
                 bsCollapsePanel("Filter Data", 
        #Dropdown zur Auswahl einer Datei aus data_list
        selectInput("Type_Select",
                    "Question-Type:",
                    choices = c("None", unique(Data$Type)),  # Dateinamen aus data_list als Auswahlmöglichkeiten
                    selected = "None"
                    ) # Standardmäßig die erste Datei auswählen
        
        # # Optionaler Slider für andere UI-Komponenten
        # sliderInput("bins",
        #             "Number of bins (Example):",
        #             min = 1,
        #             max = 50,
        #             value = 30)
      )
    )
    ),

    mainPanel(
      DTOutput("data_table")
    )
  )),
  tabPanel("Zusätzliche Analysen",
           fluidRow(
             column(12, 
                    h3("Analyse-Tools"), 
                    p("Hier könnten Sie zusätzliche Analysen hinzufügen."),
                    # Beispiel: Einfache Zusammenfassung
                    verbatimTextOutput("summary_output")
             )
           ))
)











#################### 4. Define Server Logic ####################
server <- function(input, output) {
  
  output$data_table <- renderDT({
    # Auswahl der Variablen basierend auf der Checkbox-Auswahl
    selected_vars <- c("ID", "Type", "Question", input$Var_Select) # IDs und Typen immer einbeziehen
    
    if(input$Type_Select!="None")
    {Data <- filter(Data, Type==input$Type_Select)
    }

    # Erstelle eine neue DataFrame für die farbigen Antworten
    colored_data <- Data %>%
      rowwise() %>%
      mutate(
        A = ifelse((Type == "A" & A_type_cor == "a") | (Type == "K" & A_cor == TRUE),
                   paste0("<span style='color: green;'>", A, "</span>"),
                   paste0("<span style='color: red;'>", A, "</span>")),
        B = ifelse((Type == "A" & A_type_cor == "b") | (Type == "K" & B_cor == TRUE),
                   paste0("<span style='color: green;'>", B, "</span>"),
                   paste0("<span style='color: red;'>", B, "</span>")),
        C = ifelse((Type == "A" & A_type_cor == "c") | (Type == "K" & C_cor == TRUE),
                   paste0("<span style='color: green;'>", C, "</span>"),
                   paste0("<span style='color: red;'>", C, "</span>")),
        D = ifelse((Type == "A" & A_type_cor == "d") | (Type == "K" & D_cor == TRUE),
                   paste0("<span style='color: green;'>", D, "</span>"),
                   paste0("<span style='color: red;'>", D, "</span>")),
        E = ifelse((Type == "A" & A_type_cor == "e") | (Type == "K" & is.na(E)),
                   NA,
                   ifelse((Type == "A" & A_type_cor == "e"),
                          paste0("<span style='color: green;'>", E, "</span>"),
                          paste0("<span style='color: red;'>", E, "</span>")))
      ) %>%
      ungroup() %>%
      select(ID, Type, Question, A, B, C, D, E, all_of(selected_vars))
    
    datatable(as.data.frame(colored_data), filter = "top", #plugins = "select",
              escape = FALSE,  # Erlaube HTML-Rendering
              editable = TRUE,  # Tabelle ist editierbar
              options = list(pageLength = 10))  # Optionen für die Tabelle
  }, sanitize.text.function = function(x) x)  # Erlaube HTML-Rendering
  # Beispiel-Output für die zweite Seite
  output$summary_output <- renderPrint({
    summary(Data)
  })
}










#################### 5. Load Application ####################
shinyApp(ui = ui, server = server)
