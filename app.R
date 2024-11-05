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
      # Dropdown zur Auswahl einer Datei aus data_list
      # selectInput("file_select",
      #             "Select a file to view:",
      #             choices = unique(Data$Questiontype),  # Dateinamen aus data_list als Auswahlmöglichkeiten
      #             selected = unique(Data$Questiontype)[1]),  # Standardmäßig die erste Datei auswählen
      
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











#################### 3. Define Server Logic ####################
server <- function(input, output) {
  
  output$data_table <- renderTable({
    # Filtere nach ausgewähltem Dateinamen in der source_file-Spalte
    
    selected_data <- select(Data, Question_ID, Questiontype, Question, A, B, C, D, E)


    head(selected_data)  # Nur die ersten Zeilen anzeigen
  })
  
  # Beispielplot (wird nicht von data_list beeinflusst)
  output$distPlot <- renderPlot({
    x <- faithful[, 2]
    bins <- seq(min(x), max(x), length.out = input$bins + 1)
    
    hist(x, breaks = bins, col = 'darkgray', border = 'white',
         xlab = 'Waiting time to next eruption (in mins)',
         main = 'Histogram of waiting times')
  })
}

# Run the application
shinyApp(ui = ui, server = server)
