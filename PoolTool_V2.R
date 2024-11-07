#################### 1. Load Packages ####################
library(shiny)
library(shinythemes)
library(shinyBS)
library(readxl)
library(readr)
library(dplyr)
library(writexl)


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
  titlePanel("Fragen Editor"),
  
  sidebarLayout(
    sidebarPanel(
      actionButton("prev_question", "Vorherige Frage"),
      actionButton("next_question", "Nächste Frage"),
      actionButton("save_changes", "Änderungen speichern"),
      downloadButton("download_data", "Aktuelle Daten herunterladen")
    ),
    
    mainPanel(
      # Zeile für ID und Version (nebeneinander)
      fluidRow(
        style = "background-color: #f0f0f0; padding: 10px; margin: 10px; border-radius: 5px;",  # Stil für grauen Hintergrund, Innenabstand und abgerundete Ecken
        column(2, textInput("question_id", "ID", value = "", width = "100%")),
        column(2, textInput("question_version", "Version", value = "", width = "100%")),
        column(4, textInput("type", "Type", value = "", width = "100%")),
        column(4, textInput("state", "State", value = "", width = "100%"))
      ),
      
      fluidRow(
        style = "background-color: #f0f0f0; padding: 10px; margin: 10px; border-radius: 5px;",  # Stil für grauen Hintergrund, Innenabstand und abgerundete Ecken
        # Frage über die gesamte Zeile
        textAreaInput("question_text", "Frage", value = "", width = "100%"),
        
        # Spalten für Antwortoptionen A bis E und Korrektheit
        fluidRow(
          # Spalte für Optionen A bis E (untereinander)
          column(10,  # 75% Breite
                 textInput("option_a", "A", value = "", width = "100%"),
                 textInput("option_b", "B", value = "", width = "100%"),
                 textInput("option_c", "C", value = "", width = "100%"),
                 textInput("option_d", "D", value = "", width = "100%"),
                 textInput("option_e", "E", value = "", width = "100%")
          ),
          # Spalte für Korrektheit (A_type_cor und A_cor bis D_cor, untereinander)
          column(2,  # 25% Breite
                 textInput("a_type_cor", "Correct Answer", value = "", width = "100%"),
                 textInput("a_cor", "A correct?", value = "", width = "100%"),
                 textInput("b_cor", "B correct?", value = "", width = "100%"),
                 textInput("c_cor", "C correct?", value = "", width = "100%"),
                 textInput("d_cor", "D correct?", value = "", width = "100%")
          )
          )
      ),
      
      
      # Spalten für Antwortoptionen A bis E und Korrektheit
      fluidRow(
        style = "background-color: #f0f0f0; padding: 10px; margin: 10px; border-radius: 5px;",  # Stil für grauen Hintergrund, Innenabstand und abgerundete Ecken
        
        # Spalte für Optionen A bis E (untereinander)
        column(4,  # 75% Breite
               textInput("year", "Jahr", value = "", width = "100%")
        ),
        column(4,  # 75% Breite
               textInput("week", "Woche", value = "", width = "100%")
        ),
        column(4,  # 75% Breite
               textInput("chapter", "Kapitel", value = "", width = "100%")
               )
      ),
      fluidRow(
        style = "background-color: #f0f0f0; padding: 10px; margin: 10px; border-radius: 5px;",  # Stil für grauen Hintergrund, Innenabstand und abgerundete Ecken
        
        column(6,  # 75% Breite
               textInput("tags", "Tags", value = "", width = "100%")
        ),
        column(6,  # 75% Breite
               textInput("remarks", "Remarks", value = "", width = "100%")
        ) 
      )
    )
  )
)


#################### 4. Shiny Server ####################
server <- function(input, output, session) {
  
  # Speichert die Daten und den aktuellen Index der angezeigten Frage
  questions_data <- reactiveVal(Data)
  current_index <- reactiveVal(1)
  
  # Funktion, um die Frage anzuzeigen
  observeEvent(current_index(), {
    idx <- current_index()
    question <- questions_data()[idx, ]
    
    updateTextInput(session, "question_id", value = question$ID)
    updateTextInput(session, "question_version", value = question$Version)
    updateTextInput(session, "type", value = question$Type)
    updateTextAreaInput(session, "question_text", value = question$Question)
    updateTextInput(session, "option_a", value = question$A)
    updateTextInput(session, "option_b", value = question$B)
    updateTextInput(session, "option_c", value = question$C)
    updateTextInput(session, "option_d", value = question$D)
    updateTextInput(session, "option_e", value = question$E)
    updateTextInput(session, "a_type_cor", value = question$A_type_cor)
    updateTextInput(session, "a_cor", value = question$A_cor)
    updateTextInput(session, "b_cor", value = question$B_cor)
    updateTextInput(session, "c_cor", value = question$C_cor)
    updateTextInput(session, "d_cor", value = question$D_cor)
    updateTextInput(session, "year", value = question$Year)
    updateTextInput(session, "week", value = question$Week)
    updateTextInput(session, "chapter", value = question$Chapter)
    updateTextInput(session, "state", value = question$State)
    updateTextInput(session, "tags", value = question$Tags)
    updateTextInput(session, "remarks", value = question$Remarks)
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
