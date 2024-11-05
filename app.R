#################### 1. Load Packages ####################
library(shiny)
library(shinythemes)
library(readxl)
library(readr)
library(dplyr)










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
ui <- fluidPage(
  # Define a theme for the shiny-app
  theme = shinytheme("paper"),

  # Define Title of the application
  titlePanel("Pool-Tool"),

  # Sidebar with file selection
  sidebarLayout(
    sidebarPanel(
      checkboxGroupInput(
        inputId = "Var_Select", label = "Select Vriables to display", choices = Variable_selection, selected = Variable_selection), 
      #Dropdown zur Auswahl einer Datei aus data_list
      selectInput("file_select",
                  "Select a file to view:",
                  choices = unique(Data$Type),  # Dateinamen aus data_list als Auswahlmöglichkeiten
                  selected = unique(Data$Type)), # Standardmäßig die erste Datei auswählen
      
      # # Optionaler Slider für andere UI-Komponenten
      # sliderInput("bins",
      #             "Number of bins (Example):",
      #             min = 1,
      #             max = 50,
      #             value = 30)
    ),

    mainPanel(
      tableOutput("data_table"),
    )
  )
)











#################### 4. Define Server Logic ####################
server <- function(input, output) {
  
  output$data_table <- renderTable({
    # Auswahl der Variablen basierend auf der Checkbox-Auswahl
    selected_vars <- c("ID", "Type", "Question", input$Var_Select) # IDs und Typen immer einbeziehen
    
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
    
    # Konvertiere die Tabelle in HTML
    htmlTable::htmlTable(as.data.frame(colored_data), 
                         align = "l",
                         css.cell = "padding: 5px;") # Optionales CSS für bessere Lesbarkeit
  }, sanitize.text.function = function(x) x)  # Erlaube HTML-Rendering
}










#################### 5. Load Questions ####################
shinyApp(ui = ui, server = server)
