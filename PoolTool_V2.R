#################### 1. Load Packages ####################
library(shiny)
library(shinythemes)
library(shinyBS)
library(shinyjs)
library(readxl)
library(readr)
library(dplyr)
library(writexl)

tags$head(tags$link(rel = "stylesheet", href = "https://cdnjs.cloudflare.com/ajax/libs/font-awesome/5.15.4/css/all.min.css"))




#################### 2. Load Questions ####################
# Funktion zum Laden der Fragen
load_data <- function(folder_path = "Questions") {
  files <- list.files(folder_path, pattern = "\\.(xlsx|csv)$", full.names = TRUE)
  data_list <- list()
  
  for (file in files) {
    if (grepl("\\.xlsx$", file)) {
      df <- read_excel(file)
    } else if (grepl("\\.csv$", file)) {
      df <- read_csv(file)
    }
    df$source_file <- basename(file)
    data_list[[length(data_list) + 1]] <- df
  }
  
  combined_data <- bind_rows(data_list)
  return(combined_data)
}

# Fragen laden
Data <- load_data()


#################### 3. Shiny UI ####################
ui <- fluidPage(
  theme = shinytheme("cerulean"),
  titlePanel("Editor"),
  
  # Nutze shinyjs
  useShinyjs(),
  
  fluidRow(
    style = "background-color: #f0f0f0; padding: 15px; text-align: center;",
    column(1, actionButton("prev_question", 
                           label = HTML('<i class="fas fa-arrow-left"></i>'),
                           style = "font-size: 12px; padding: 10px 20px; width: 100%;")),
    column(1, textOutput("question_counter", inline = TRUE), style = "font-size: 16px; font-weight: bold; text-align: center;"),
    column(1, actionButton("next_question",
                           label = HTML('<i class="fas fa-arrow-right"></i>'),
                           style = "font-size: 12px; padding: 10px 20px; width: 100%;")),
    column(4),  # Leerer Abstand
    column(2, downloadButton("download_data", "Load", 
                             style = "font-size: 12px; padding: 10px 20px; width: 100%;")),
    column(2, actionButton("save_changes",                        
                           label = HTML('<i class="fas fa-save"></i> Save'),
                           style = "font-size: 12px; padding: 10px 20px; width: 100%;"))
  ),

    
  fluidRow(
    style = "padding: 10px; margin: 10px; border-radius: 5px;",
      # Zeile für ID, Version, Type und State
      fluidRow(
        style = "background-color: #f0f0f0; padding: 15px; margin: 10px; border-radius: 5px;",
        column(2, textInput("question_id", "ID", value = "", width = "100%")),
        column(2, textInput("question_version", "Version", value = "", width = "100%")),
        column(4, selectInput("type", "Type", choices = c("A", "K"), selected = "A", width = "100%")),
        column(4, textInput("state", "State", value = "", width = "100%"))
      ),
      
      # Frage über die gesamte Zeile
      fluidRow(
        style = "background-color: #f0f0f0; padding: 10px; margin: 10px; border-radius: 5px;",
        textAreaInput("question_text", "Frage", value = "", width = "100%")
      ),
      
      # Antwortoptionen und Korrektheit
      fluidRow(
        style = "background-color: #f0f0f0; padding: 10px; margin: 10px; border-radius: 5px;",
        
        # Optionen A bis E
        column(9, 
               textInput("option_a", "A", value = "", width = "100%"),
               textInput("option_b", "B", value = "", width = "100%"),
               textInput("option_c", "C", value = "", width = "100%"),
               textInput("option_d", "D", value = "", width = "100%"),
               textInput("option_e", "E", value = "", width = "100%")
        ),
        
        # Korrektheit für A_cor bis D_cor und A_type_cor
        column(3,
               selectInput("a_type_cor", "Correct Answer", choices = c("A", "B", "C", "D", "E"), selected = NULL, width = "100%"),
               selectInput("a_cor", "A correct?", choices = c("TRUE", "FALSE"), selected = NULL, width = "100%"),
               selectInput("b_cor", "B correct?", choices = c("TRUE", "FALSE"), selected = NULL, width = "100%"),
               selectInput("c_cor", "C correct?", choices = c("TRUE", "FALSE"), selected = NULL, width = "100%"),
               selectInput("d_cor", "D correct?", choices = c("TRUE", "FALSE"), selected = NULL, width = "100%")
        )
      ),
      
      # Weitere Felder für Jahr, Woche, Kapitel, Tags und Bemerkungen
      fluidRow(
        style = "background-color: #f0f0f0; padding: 10px; margin: 10px; border-radius: 5px;",
        column(4, textInput("year", "Jahr", value = "", width = "100%")),
        column(4, textInput("week", "Woche", value = "", width = "100%")),
        column(4, textInput("chapter", "Kapitel", value = "", width = "100%"))
      ),
      fluidRow(
        style = "background-color: #f0f0f0; padding: 10px; margin: 10px; border-radius: 5px;",
        column(6, textInput("tags", "Tags", value = "", width = "100%")),
        column(6, textInput("remarks", "Remarks", value = "", width = "100%"))
      )
    )
  )



#################### 4. Shiny Server ####################
server <- function(input, output, session) {
  
  # Speichert die Daten und den aktuellen Index der angezeigten Frage
  questions_data <- reactiveVal(Data)
  current_index <- reactiveVal(1)
  
  
  # Anzahl der Fragen berechnen
  total_questions <- reactive({ nrow(questions_data()) })
  
  # Dynamischer Text für "Frage X von Y"
  output$question_counter <- renderText({
    paste(current_index(), "/", total_questions())
  })
  
  # Funktion zur Anzeige der Frage
  observeEvent(current_index(), {
    idx <- current_index()
    question <- questions_data()[idx, ]
    
    # Initialisieren der Input-Werte mit den Daten der aktuellen Frage
    updateTextInput(session, "question_id", value = question$ID)
    updateTextInput(session, "question_version", value = question$Version)
    updateSelectInput(session, "type", selected = question$Type)
    updateTextAreaInput(session, "question_text", value = question$Question)
    updateTextInput(session, "option_a", value = question$A)
    updateTextInput(session, "option_b", value = question$B)
    updateTextInput(session, "option_c", value = question$C)
    updateTextInput(session, "option_d", value = question$D)
    updateTextInput(session, "option_e", value = question$E)
    updateSelectInput(session, "a_type_cor", selected = question$A_type_cor)
    updateSelectInput(session, "a_cor", selected = question$A_cor)
    updateSelectInput(session, "b_cor", selected = question$B_cor)
    updateSelectInput(session, "c_cor", selected = question$C_cor)
    updateSelectInput(session, "d_cor", selected = question$D_cor)
    updateTextInput(session, "year", value = question$Year)
    updateTextInput(session, "week", value = question$Week)
    updateTextInput(session, "chapter", value = question$Chapter)
    updateTextInput(session, "state", value = question$State)
    updateTextInput(session, "tags", value = question$Tags)
    updateTextInput(session, "remarks", value = question$Remarks)
  })
  
  # Anpassung der Eingabefelder basierend auf dem ausgewählten Typ
  observeEvent(input$type, {
    if (input$type == "A") {
      # Deaktiviere Korrektheitsfelder (a_cor bis e_cor)
      lapply(c("a_cor", "b_cor", "c_cor", "d_cor", "e_cor"), function(id) {
        shinyjs::disable(id)
      })
      
      # Aktiviert das Dropdown für die korrekte Antwort (a_type_cor)
      shinyjs::enable("a_type_cor")
      updateSelectInput(session, "a_type_cor", choices = c("A", "B", "C", "D", "E"), selected = NULL)
      
      # Setze Korrektheitsfelder zurück
      lapply(c("a_cor", "b_cor", "c_cor", "d_cor", "e_cor"), function(id) {
        updateSelectInput(session, id, selected = NULL)
      })
      
    } else if (input$type == "K") {
      # Deaktiviere das Dropdown für die korrekte Antwort (a_type_cor)
      shinyjs::disable("a_type_cor")
      updateSelectInput(session, "a_type_cor", selected = NULL)
      
      # Aktiviere Korrektheitsfelder (a_cor bis e_cor)
      lapply(c("a_cor", "b_cor", "c_cor", "d_cor", "e_cor"), function(id) {
        shinyjs::enable(id)
        updateSelectInput(session, id, choices = c("TRUE", "FALSE"), selected = NULL)
      })
    }
  })
  
  # Nächste Frage anzeigen
  observeEvent(input$next_question, {
    if (current_index() < nrow(questions_data())) {
      current_index(current_index() + 1)
    }
  })
  
  # Vorherige Frage anzeigen
  observeEvent(input$prev_question, {
    if (current_index() > 1) {
      current_index(current_index() - 1)
    }
  })
  
  # Änderungen speichern
  observeEvent(input$save_changes, {
    idx <- current_index()
    updated_data <- questions_data()
    
    # Aktualisieren der Daten entsprechend der eingegebenen Werte
    updated_data[idx, "Version"] <- input$question_version
    updated_data[idx, "Type"] <- input$type
    updated_data[idx, "Question"] <- input$question_text
    updated_data[idx, "A"] <- input$option_a
    updated_data[idx, "B"] <- input$option_b
    updated_data[idx, "C"] <- input$option_c
    updated_data[idx, "D"] <- input$option_d
    updated_data[idx, "E"] <- input$option_e
    updated_data[idx, "A_type_cor"] <- input$a_type_cor
    updated_data[idx, "A_cor"] <- input$a_cor
    updated_data[idx, "B_cor"] <- input$b_cor
    updated_data[idx, "C_cor"] <- input$c_cor
    updated_data[idx, "D_cor"] <- input$d_cor
    updated_data[idx, "Year"] <- input$year
    updated_data[idx, "Week"] <- input$week
    updated_data[idx, "Chapter"] <- input$chapter
    updated_data[idx, "State"] <- input$state
    updated_data[idx, "Tags"] <- input$tags
    updated_data[idx, "Remarks"] <- input$remarks
    
    # Speichern der aktualisierten Daten
    questions_data(updated_data)
    showNotification("Änderungen gespeichert!", type = "message")
  })
  
  # Aktualisierte Daten herunterladen
  output$download_data <- downloadHandler(
    filename = function() {
      paste("Updated_Questions-", Sys.Date(), ".xlsx", sep = "")
    },
    content = function(file) {
      write_xlsx(questions_data(), file)
    }
  )
}


#################### 5. Run App ####################
shinyApp(ui = ui, server = server)
