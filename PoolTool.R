# TO DO
# -add new questions
# -csv instead of xlsx
# -delete question
# -order questions
# -filter function


# 1. Load Ressources ####################
## 1.1 Load Packages ##########
library(shiny)
library(shinythemes)
library(shinyBS)
library(shinyjs)
library(readxl)
library(readr)
library(dplyr)
library(writexl)





## 1.2 Load Stylesheet ####################
tags$head(tags$link(rel = "stylesheet", 
                    href = "https://cdnjs.cloudflare.com/ajax/libs/font-awesome/5.15.4/css/all.min.css"))










# 2. Load Questions ####################
## 2.1 Function to Load  <-  ##########
load_data <- function(folder_path = "Questions") {
  # Create a List of all excel- or csv-files
  files <- list.files(folder_path, pattern = "\\.(xlsx)$", full.names = TRUE)
  
  # Create a list for saving the data
  data_list <- list()
  
  # For every excel- or csv-file
  for (file in files) {
    # Call function to read excel-file if file is of type .xlsx
    if (grepl("\\.xlsx$", file)) {df <- read_excel(file)}
    # Call function to read csv-file if file is of type .csv
    else if (grepl("\\.csv$", file)) {df <- read_csv(file)}
    
    # Save the respective filename as a variable
    df$source_file <- basename(file)
    
    # Append data from file in data list
    data_list[[length(data_list) + 1]] <- df
  }
  
  # Bind all data to one list
  combined_data <- bind_rows(data_list)
  
  # Return data
  return(combined_data)
}





## 2.2 Load Data ##########
Data <- load_data()










# 3. Shiny UI ####################
ui <- fluidPage(
  
  ## 3.1 General Settings ##########
  # Define Theme
  theme = shinytheme("cerulean"),
  
  #Set Title of Panel
  titlePanel("Editor"),
  
  # Set to use Shinyjs
  useShinyjs(),
  
  
  
  
  
  ## 3.2 Set Navigation UI ##########
  # Create a new row
  fluidRow(
    # Set stly of row
    style = "background-color: #f0f0f0; padding: 15px; text-align: center;",
    
    # Add a column for the previous-button
    column(1, actionButton("prev_question", 
                           # Load button-icon
                           label = HTML('<i class="fas fa-arrow-left"></i>'),
                           # Set style of button
                           style = "font-size: 12px; padding: 10px 20px; width: 100%;")),
    
    # Add a column for the question counter
    column(1, textOutput("question_counter", inline = TRUE),
           # Set style of counter
           style = "font-size: 16px; font-weight: bold; text-align: center;"),
    
    # Add a column for the next-button
    column(1, actionButton("next_question",
                           # Load button-icon
                           label = HTML('<i class="fas fa-arrow-right"></i>'),
                           # Set style of button
                           style = "font-size: 12px; padding: 10px 20px; width: 100%;")),

    
    # Add a column for the next-button
    column(1, actionButton("new_question",
                           # Load button-icon
                           label =  HTML('<i class="fa-solid fa-plus"></i>'),
                           # Set style of button
                           style = "font-size: 12px; padding: 10px 20px; width: 100%;")),
    
        
    # Add space by means of an empty column
    column(3),
    
    # Add a column for the load-button
    column(2, downloadButton("download_data", "Load",
                             # Set style of button
                             style = "font-size: 12px; padding: 10px 20px; width: 100%;")),
    
    # Add a column for the save-button
    column(2, actionButton("save_changes",                        
                           # Load button-icon
                           label = HTML('<i class="fas fa-save"></i> Save'),
                           # Set style of button
                           style = "font-size: 12px; padding: 10px 20px; width: 100%;"))
  ),
  
  
  
  
  
  
  ## 3.3 Editor UI ##########
  # Create a new row
  fluidRow(
    # Set style of row
    style = "padding: 10px; margin: 10px; border-radius: 5px;",
    
    
    
    
    
    ### 3.3.1 Question ID UI ##########    
    # Create row
    fluidRow(
      # Set style of row
      style = "background-color: #f0f0f0; padding: 15px; margin: 10px; border-radius: 5px;",
      # Add column with textinput for question-id
      column(2, numericInput("question_id", "ID", value = "", width = "100%")),
      # Add column with textinput for question version
      column(2, numericInput("question_version", "Version", value = "", width = "100%")),
      # Add dropdown for question-type
      column(4, selectInput("type", "Type", choices = c("A", "K"), selected = "A", width = "100%")),
      # Add column with textinput for question state
      column(4, textInput("state", "State", value = "", width = "100%"))
    ),
    
    
    
    
    
    ### 3.3.2 Set Question Editor UI ##########
    #### 3.3.2.1 Set Question UI ##########
    # Create row
    fluidRow(
      # Set style of row
      style = "background-color: #f0f0f0; padding: 10px; margin: 10px; border-radius: 5px;",
      # Add textinput for question
      textAreaInput("question_text", "Frage", value = "", width = "100%")
    ),
    
    
    # Create row
    fluidRow(
      # Set style of row
      style = "background-color: #f0f0f0; padding: 10px; margin: 10px; border-radius: 5px;",

            
      #### 3.3.2.2 Set Options UI ##########
      # Create column for answer options
      column(9,
             # Add textinput for option a
             textInput("option_a", "A", value = "", width = "100%"),
             # Add textinput for option b
             textInput("option_b", "B", value = "", width = "100%"),
             # Add textinput for option c
             textInput("option_c", "C", value = "", width = "100%"),
             # Add textinput for option d
             textInput("option_d", "D", value = "", width = "100%"),
             # Add textinput for option e
             textInput("option_e", "E", value = "", width = "100%")
      ),


      #### 3.3.2.3 Set Solutions UI ##########
      # Create column for solutions
      column(3,
             # Add dropdown for solution on option a
             selectInput("a_cor", "A correct?", choices = c("TRUE", "FALSE"), selected = NULL, width = "100%"),
             # Add dropdown for solution on option b
             selectInput("b_cor", "B correct?", choices = c("TRUE", "FALSE"), selected = NULL, width = "100%"),
             # Add dropdown for solution on option c
             selectInput("c_cor", "C correct?", choices = c("TRUE", "FALSE"), selected = NULL, width = "100%"),
             # Add dropdown for solution on option d
             selectInput("d_cor", "D correct?", choices = c("TRUE", "FALSE"), selected = NULL, width = "100%"),
             # Add dropdown for solution on answers of Type A
             selectInput("a_type_cor", "Correct Answer", choices = c("A", "B", "C", "D", "E"), selected = NULL, width = "100%")
             )
      ),
    
    
    
    
    
    ### 3.3.3 Set Meta Data UI ##########
    # Create row
    fluidRow(
      # Set style of row
      style = "background-color: #f0f0f0; padding: 10px; margin: 10px; border-radius: 5px;",
      # Add column with textinput for year of question
      column(4, numericInput("year", "Jahr", value = "", width = "100%")),
      # Add column with textinput for week of question
      column(4, numericInput("week", "Woche", value = "", width = "100%")),
      # Add column with textinput for chapter of question
      column(4, numericInput("chapter", "Kapitel", value = "", width = "100%"))
    ),
    
    # Create second row
    fluidRow(
      # Set style of row
      style = "background-color: #f0f0f0; padding: 10px; margin: 10px; border-radius: 5px;",
      # Add column with textinput for tags for the question
      column(6, textInput("tags", "Tags", value = "", width = "100%")),
      # Add column with textinput for remarks
      column(6, textInput("remarks", "Remarks", value = "", width = "100%"))
      )
    )
  )










# 4. Shiny Server ####################
server <- function(input, output, session) {
  ## 4.1 Disable UI ##########
  shinyjs::disable("question_id")
  shinyjs::disable("question_version")

  
  ## 4.2 Load Data ##########
  # Create an index of available Data
  questions_data <- reactiveVal(Data)
  # Save the index of the current question
  current_index <- reactiveVal(1)

  
  # Count number of available questions
  total_questions <- reactive({ nrow(questions_data()) })
  
  # Update UI text with current question and number of available questions
  output$question_counter <- renderText({
    paste(current_index(), "/", total_questions())
  })
  
  
  
  
  
  ## 4.3 Function to Update Displayed Question ##########
  observeEvent(c(current_index(), input$next_question, input$prev_question), {
    # Set the current index as variable
    idx <- current_index()
    
    # Load question with current index
    question <- questions_data()[idx, ]
    
    # Update input fields for question with current index
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
    
    # Call update_border_colors-function to update question colors
    update_border_colors(question)
    
    # Call toggle_finalized-function to de/activate inputs
    toggle_finalized(question$State == "Finalized")
    
    update_answer_options(session, question)
    
    # Add a Delay
    shinyjs::delay(10, {
      # Call toggle_UI function to deactivate inputs which do not match the question-type
      toggle_UI(session, question)
    })    
  })
  
  
  
  
  
  ## 4.4 On changes on the State-Input ##########
  observeEvent(input$state, {
    # call toggle_finalized-function using the term "Finalized"
    toggle_finalized(input$state == "Finalized")
  })
  
  
  
  
  ## 4.5 On Changing the Question-Type ##########  
  # If input$type is changed
  observeEvent(input$type, {

    # Call update_border_colors-function to update question colors
    update_border_colors(questions_data()[current_index(),])
    
    # Call update_answer_options-function to update the answer-options
    update_answer_options(session, questions_data()[current_index(),])

    # Add a Delay
    shinyjs::delay(10, {
      # Call toggle_UI function to deactivate inputs which do not match the question-type
      toggle_UI(session, question)
    })
  })
  
  
  
  
  

  
  
  
  
  
  ## 4.6 Jump between Questions  ##########
  #### 4.6.1 Show Next Question ##########
  # If next_question is pressed
  observeEvent(input$next_question, {
    # Call save_current_question-function to save the current question
    save_current_question()
    
    # If the current index is smaller than the overall number of questions
    if (current_index() < total_questions()) {
      # Increase the current index by 1
      current_index(current_index() + 1)
    } else {
      # Otherwise reset current index to 1
      current_index(1)
    }
  })
  
  
  
  
  
  #### 4.6.2 Show Previous Question ##########
  # If prev_question is pressed
  observeEvent(input$prev_question, {
    # Call save_current_question-function to save the current question
    save_current_question()
    
    # If the current question is not the first question
    if (current_index() > 1) {
      # Decrease the current index by 1
      current_index(current_index() - 1)
    } else {
      # Otherwise reset current index to the index of the last question
      current_index(total_questions())
    }
  })
  
  
  
  
  
  ## 4.7 On Clicking New Question ##########
  observeEvent(input$new_question, {
    # Hole die aktuellen Daten
    current_data <- questions_data()
    
    # Stelle sicher, dass die ID-Spalte ein Character ist
    current_data$ID <- as.character(current_data$ID)
    
    # Extrahiere das aktuelle Jahr
    current_year <- as.numeric(format(Sys.Date(), "%Y"))
    
    # Finde bestehende IDs, die mit dem aktuellen Jahr beginnen
    existing_ids <- current_data$ID[grepl(paste0("^", current_year), current_data$ID)]
    
    # Generiere die neue ID basierend auf dem höchsten bestehenden Wert
    if (length(existing_ids) > 0) {
      max_id <- max(as.numeric(sub(paste0("^", current_year), "", existing_ids)), na.rm = TRUE)
      new_id <- paste0(current_year, sprintf("%04d", max_id + 1))
    } else {
      new_id <- paste0(current_year, "0001")
    }
    
    # Erstelle einen neuen leeren Eintrag
    new_question <- data.frame(
      ID = as.character(new_id), # ID als Character setzen
      Version = 1,
      Type = "A",
      Question = "",
      A = "",
      B = "",
      C = "",
      D = "",
      E = "",
      A_type_cor = NA,
      A_cor = NA,
      B_cor = NA,
      C_cor = NA,
      D_cor = NA,
      Year = current_year,
      Week = NA,
      Chapter = NA,
      State = "Draft",
      Tags = "",
      Remarks = "",
      stringsAsFactors = FALSE
    )
    
    # Füge die neue Frage zu den Daten hinzu
    updated_data <- bind_rows(current_data, new_question)
    questions_data(updated_data)
    
    # Setze den aktuellen Index auf die neue Frage
    current_index(nrow(updated_data))
    
    # Zeige eine Benachrichtigung
    showNotification("Neue Frage hinzugefügt", type = "message")
  })
  
  
  
  

  
  
  
  
  
  ## 4.7 Updates on inputs ##########
  # On changes on a_type_cor
  observeEvent(input$a_type_cor,
               { update_border_colors(questions_data()[current_index(),]) })
  # On changes on a_cor
  observeEvent(input$a_cor, 
               { update_border_colors(questions_data()[current_index(),]) })
  # On changes on b_cor
  observeEvent(input$b_cor, 
               { update_border_colors(questions_data()[current_index(),]) })
  # On changes on c_cor
  observeEvent(input$c_cor, 
               { update_border_colors(questions_data()[current_index(),]) })
  # On changes on d_cor
  observeEvent(input$d_cor, 
               { update_border_colors(questions_data()[current_index(),]) })
  
  
  
  
  ## 4.18 Deactivate UI based on Question Type on start ##########
  # Add a Delay
  shinyjs::delay(150, {
    # Call toggle_UI function to deactivate inputs which do not match the question-type
    toggle_UI(session, question)
  })  
  
  
  
  
  
  ## 4.19 Save Data ##########  
  # If save_changes is pressed
  observeEvent(input$save_changes, {
    # Set the current index as variable
    idx <- current_index()
    # Save the filtered data as the updated data
    updated_data <- questions_data()
    
    # Update data for question with current index by the current inputs
    updated_data[idx, "Version"] <- input$question_version %>% as.numeric()
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
    updated_data[idx, "Year"] <- input$year %>% as.numeric()
    updated_data[idx, "Week"] <- input$week %>% as.numeric()
    updated_data[idx, "Chapter"] <- input$chapter %>% as.numeric()
    updated_data[idx, "State"] <- input$state
    updated_data[idx, "Tags"] <- input$tags
    updated_data[idx, "Remarks"] <- input$remarks
    
    # Update question-data based on updated data
    questions_data(updated_data)
    
    # Find row-index for current question
    row_in_Data <- which(Data$ID == questions_data()[idx, "ID"] & Data$Version == questions_data()[idx, "Version"])
    
    # Update data at current index
    if (length(row_in_Data) == 1) {
      Data[row_in_Data, ] <- questions_data()[idx, ]
    }
    
    #Convert to dataframe
    data_save <- as.data.frame(Data)
    
    # Write csv
    write_csv(data_save, "Questions/Questions.csv")
    
    # Print Notification
    showNotification("Data Saved", type = "message")
  })  
  
  
  
  
  
  ## 4.20 Function to De/Activate Question-Inputs ##########
  #### 4.20.1 Save Question Changes ##########
  save_current_question <- function() {
    # Extract the current index
    idx <- current_index()
    # Make copy of questions_data
    updated_data <- questions_data()
    
    # Update Question-variables based on inputs
    updated_data[idx, "Version"] <- as.numeric(input$question_version)
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
    updated_data[idx, "Year"] <- as.numeric(input$year)
    updated_data[idx, "Week"] <- as.numeric(input$week)
    updated_data[idx, "Chapter"] <- as.numeric(input$chapter)
    updated_data[idx, "State"] <- input$state
    updated_data[idx, "Tags"] <- input$tags
    updated_data[idx, "Remarks"] <- input$remarks
    
    # Update questions_data with the updated data
    questions_data(updated_data)
  }
  
  
  
  
  

  #### 4.20.2 Function to update question borders ##########
  update_border_colors <- function(question) {
    # If question is A-Type
    if (input$type == "A") {
      # Save correct answer based on a_type_cor input
      correct_answer <- input$a_type_cor
      # Save available options
      options <- c("A", "B", "C", "D", "E")
      
      # Set border of answer options
      lapply(options, function(option) {
        # Save color for every option based on correct answer
        color <- ifelse(correct_answer == option, "green", "red")
        # Apply color for border of answer options
        shinyjs::runjs(sprintf('$("#option_%s").css("border-color", "%s")', tolower(option), color))
      })
    }
    
    # If question is K-Type
    if (input$type == "K") {
      # For every index in number of options (4)
      for (i in 1:4) {
        # Define the current option (a to d) based on the current index
        option <- c("a", "b", "c", "d")[i]
        
        # Define for each option the correct answer based on the respective solution
        correct_value <- switch(option,
                                "a" = input$a_cor,
                                "b" = input$b_cor,
                                "c" = input$c_cor,
                                "d" = input$d_cor)
        # Save color for every option based on correct answer
        color <- ifelse(correct_value == "TRUE", "green", "red")
        # Apply color for border of answer options
        shinyjs::runjs(sprintf('$("#option_%s").css("border-color", "%s")', option, color))
      }
    }
  }

  
  
  
  
  #### 4.20.3 En/Disable UI based on State ##########
  toggle_finalized <- function(is_finalized) {
    # Create a list of all input_ids
    input_ids <- c("type", "question_text", "option_a", "option_b", "option_c", "option_d",
                   "option_e", "a_type_cor", "a_cor", "b_cor", "c_cor", "d_cor",
                   "year", "week", "chapter", "tags", "remarks")
    
    # if state is finalized
    if (is_finalized) {
      # Deactivate questions
      lapply(input_ids, shinyjs::disable)
    } else {
      # Activate questions if state is not finalized
      lapply(input_ids, shinyjs::enable)
    }
  }
  
  
  
  
  
  #### 4.20.4 Update Options ##########
  update_answer_options <- function(session, question) {
    # Update the answer options based on the question dataset
    updateSelectInput(session, "a_cor", selected = question$A_cor)
    updateSelectInput(session, "b_cor", selected = question$B_cor)
    updateSelectInput(session, "c_cor", selected = question$C_cor)
    updateSelectInput(session, "d_cor", selected = question$D_cor)
    updateSelectInput(session, "a_type_cor", selected = question$A_type_cor)
  }


  
  
  
  
  #### 4.20.4 En/Disable UI based on State ##########
  toggle_UI <- function(session, question) {
    # If question is A-Type
    if (input$type == "A") {
      # Disable solutions for a_cor to d_cor and clear their values
      lapply(c("a_cor", "b_cor", "c_cor", "d_cor"), function(id) {
        updateSelectInput(session, id, selected = NA)
        shinyjs::disable(id)
      })
      
      # Enable solutions for a_type_cor and option_e
      shinyjs::enable("a_type_cor")
      shinyjs::enable("option_e")
      
    } 
    # If question is K-Type
    else if (input$type == "K") {
      # Disable solutions for a_type_cor and option_e and clear their values
      updateSelectInput(session, "a_type_cor", selected = NA)
      shinyjs::disable("a_type_cor")
      
      # Disable solutions for option E and option_e and clear their values
      updateTextInput(session, "option_e", value = "")
      shinyjs::disable("option_e")
      
      # Enable solutions for a_cor to d_cor
      lapply(c("a_cor", "b_cor", "c_cor", "d_cor"), function(id) {
        shinyjs::enable(id)
      })
    }
  }
}







# 5. Run Shiny App ####################
shinyApp(ui = ui, server = server)