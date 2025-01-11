library(shiny)
library(shinydashboard)
library(shiny.fluent)
library(ggplot2)
library(dplyr)
library(DT)

set.seed(123)  # Set seed for reproducibility

varNames <- c(
  "",                
  "PatientID",      
  "Name",              
  "Sex",             
  "Age",             
  "BD Type",           
  "Medication",          
  "Mood Score",            
  "Adherence",
  "STABI"
)

patients <- data.frame(
  Photo = NA, 
  PatientID = 1:10,
  Name = paste("Patient", 1:10),
  Sex = sample(c("Male", "Female"), 10, replace = TRUE),
  Age = sample(20:60, 10),
  BDType = sample(c("Type 1", "Type 2"), 10, replace = TRUE),
  Medication = sample(c("MedA", "MedB", "MedC"), 10, replace = TRUE),
  MoodScore = sample(1:10, 10, replace = TRUE),
  MedicationAdherence = sample(80:100, 10, replace = TRUE),
  stringsAsFactors = FALSE,
  STABI = sample(c("Basic", "+"), 10, replace = TRUE)
)

# Update the 'Photo' column based on 'Sex'
patients$Photo <- sapply(patients$Sex, function(sex) {
  if (sex == "Male") {
    return('<img src="man.png" width="40" height="40"></img>')  # Image for male
  } else {
    return('<img src="woman.png" width="40" height="40"></img>')  # Image for female
  }
})

# UI definition
ui <- fluidPage(
  # Custom CSS for a light blue and green theme
  tags$head(tags$style(HTML("
    .btn-primary {
      background-color: #0099cc;
      border-color: #007acc;
    }
    .btn-danger {
      background-color: #ff6666;
      border-color: #cc0000;
    }
    .btn-primary:hover {
      background-color: #007acc;
    }
    .btn-danger:hover {
      background-color: #cc0000;
    }
    table {
      background-color: #ffffff;
    } 
    .navbar-default {
      background-color: #dfffe2; /* Light green background */
      border: none;
    }
    .navbar-default .navbar-brand {
      color: #5a9367; /* Brand text color */
    }
    .navbar-default .navbar-nav > li > a {
      color: #5a9367; /* Navbar link text color */
    }
    .navbar-default .navbar-nav > li > a:hover {
      background-color: #c7f4d8; /* Hover background color */
      color: #407a55; /* Hover text color */
    }
    .navbar-default .navbar-nav > .active > a,
    .navbar-default .navbar-nav > .active > a:focus,
    .navbar-default .navbar-nav > .active > a:hover {
      background-color: #FFFFFF; /* Dark green background for active tab */
    }
  "))),
  navbarPage(
    title = div(
      tags$img(src = "stabi.png", alt = "Stabi logo", style = "height:30px; margin-right:10px;"),
      "STABI"
    ),
    id = "x0",
    tabPanel("Dashboard",
             fluidRow(
               box(
                 title = div(
                   style = "border: 2px solid #ff6666; border-radius: 8px; background-color: #ffe6e6; padding: 10px; display: flex; align-items: center; justify-content: space-between;",
                   tagList(
                     div(
                       style = "display: flex; align-items: center; margin-left: 8px;",
                       icon("bell"),  # Bell icon
                     ),
                     actionButton("newAppointmentBtn", "New appointment!", class = "btn-danger")  # Button next to the bell
                   )
                 ),
                 width = 12,
               )
             ),
             fluidRow(
               column(
                 width = 5,
                 Calendar.shinyInput("calendar", value = Sys.Date())
               ),
               column(
                 width = 7,
                 valueBoxOutput("totalPatients"),
                 valueBoxOutput("avgMoodScore"),
                 valueBoxOutput("avgAdherence")
               )
             ),
             fluidRow(
               box(title = "Patient List", width = 12, DT::dataTableOutput("patientTable"))
             )
    ),
    tabPanel("Patient Details",
             fluidRow(
               box(title = "", width = 12,
                   verbatimTextOutput("profile_details"),
                   tabsetPanel(
                     tabPanel("Stabi wearable",
                              # Basic STABI Section
                              fluidRow(
                                box(
                                  title = "Basic STABI", solidHeader = TRUE, width = 12,
                                  fluidRow(
                                    column(
                                      width = 6,
                                      box(
                                        title = "", solidHeader = TRUE,
                                        plotOutput("heart_rate_trends", height = "400px", width = "400px")
                                      )
                                    ),
                                    column(
                                      width = 6,
                                      box(
                                        title = "", solidHeader = TRUE,
                                        plotOutput("sleep_patterns", height = "400px", width = "400px")
                                      )
                                    ),
                                    column(
                                      width = 6,
                                      box(
                                        title = "", solidHeader = TRUE,
                                        plotOutput("actigraph", height = "400px", width = "400px")
                                      )
                                    )
                                  )
                                )
                              ),
                              # STABI + Section
                              fluidRow(
                                box(
                                  title = "STABI +", solidHeader = TRUE, width = 12,
                                  fluidRow(
                                    column(
                                      width = 6,
                                      box(
                                        title = "", solidHeader = TRUE,
                                        plotOutput("lithium_levels", height = "400px", width = "400px")
                                      )
                                    ),
                                    column(
                                      width = 6,
                                      box(
                                        title = "", solidHeader = TRUE,
                                        plotOutput("glucose_levels", height = "400px", width = "400px")
                                      )
                                    )
                                  )
                                )
                              )
                     ),
                     tabPanel("Mood tracking",
                              plotOutput("mood_graph")
                     ),
                     tabPanel("Medication adherence",
                              h3("Weekly adherence"),
                              plotOutput("adherence_graph"),
                              h3("Daily pill intake"),
                              DT::dataTableOutput("daily_pill_table")
                     ),
                     tabPanel("Summary of alerts",
                              tableOutput("alerts_table")
                     ),
                     tabPanel("Reports",
                              fluidRow(
                                box(title = "", width = 12,
                                    h4("Example Report 1"),
                                    verbatimTextOutput("exampleReport1"),
                                    br(),
                                    h4("Example Report 2"),
                                    verbatimTextOutput("exampleReport2"),
                                    br(),
                                    verbatimTextOutput("savedReport")
                                ),
                                box(title = "", width = 12,
                                    textAreaInput("reportText", "New report:", "", height = "200px"),
                                    fluidRow(
                                      box(title = "", width = 12,
                                      actionButton("saveReport", "Save report"),
                                      downloadButton("downloadReport", "Download report"),
                                      )
                                    )
                                )
                              )
                     )
                   )
               )
             )
    ),
    tabPanel("Chat",
         fluidRow(
           # Patient selection dropdown
           column(4, 
                  box(
                    title = "", 
                    width = 12, 
                    selectInput("selectedPatientChat", "Choose a patient:", 
                                choices = patients$Name, selected = patients$Name[1])
                  )
           ),
           # Chat box and message input
           column(8, 
                  box(
                    title = "", 
                    width = 12, height = "400px",
                    # Display chat messages
                    uiOutput("chatMessages"),
                    # Message input area
                    textAreaInput("messageInput", "Type your message:", "", height = "100px"),
                    actionButton("sendMessage", "Send Message", class = "btn-primary")
                  )
           )
  
        )
    )
  )
)

# Server function
server <- function(input, output, session) {
  
  # Reactive value to store selected patient
  selectedPatient <- reactiveVal(patients[1, ])  # Default to first patient
  
  # Dashboard metrics
  output$totalPatients <- renderValueBox({
    valueBox(
      nrow(patients), "Total Patients", icon = icon("users"), color = "blue"
    )
  })
  
  output$avgMoodScore <- renderValueBox({
    valueBox(
      round(mean(patients$MoodScore), 1), "Avg Mood Score", icon = icon("smile"), color = "green"
    )
  })
  
  output$avgAdherence <- renderValueBox({
    valueBox(
      paste0(round(mean(patients$MedicationAdherence), 1), "%"), 
      "Avg Medication Adherence", 
      icon = icon("pills"), color = "purple"
    )
  })
  
  output$patientTable <- DT::renderDataTable({
    DT::datatable(patients, 
                  options = list(pageLength = 5), 
                  rownames = FALSE, 
                  class = 'cell-border stripe', 
                  style = "bootstrap",
                  colnames = varNames,
                  selection = 'none',
                  escape = FALSE) %>%
      formatStyle(
        'MedicationAdherence',
        color = styleInterval(c(75, 94), c('red', 'orange', 'green'))
      ) %>%
      formatStyle(
        'MoodScore',
        color = styleInterval(c(4, 7), c('red', 'orange', 'green'))
      ) %>%
      formatStyle(
        names(patients),
        border = 'none', 
        backgroundColor = 'transparent',
        textAlign = 'center'
      ) %>% 
      formatStyle('Photo', cursor = 'pointer')
  })
  
  # Patient-specific details
  output$profile_details <- renderText({
    selected <- selectedPatient()
    paste("Name:", selected$Name, "\n",
          "Age:", selected$Age, "\n",
          "BDType:", selected$BDType, "\n",
          "Medication:", selected$Medication, "\n",
          "Mood Score:", selected$MoodScore, "\n",
          "Medication Adherence:", selected$MedicationAdherence, "%")
  })
  
  # Mood Tracking
  output$mood_graph <- renderPlot({
    ggplot(data.frame(Day = 1:10, Mood = sample(1:10, 10, replace = TRUE)), aes(x = Day, y = Mood)) + 
      geom_line() + 
      geom_point() + 
      theme_minimal() + 
      labs(title = "", x = "Day", y = "Mood Score")
  })
  
  # Simulated daily data for 3 months
  adherence_data <- data.frame(
    Day = seq.Date(Sys.Date() - 89, Sys.Date(), by = "days"),
    ReportedPills = round(runif(90, 0, 2)),  # Pills taken (0-2)
    PredictedPills = round(runif(90, 0, 2))  # Predicted pills (0-2)
  )
  
  # Weekly adherence percentages (self-reported and predicted)
  adherence_data$Week <- cut(adherence_data$Day, breaks = "week", start.on.monday = TRUE)
  weekly_adherence <- adherence_data %>%
    group_by(Week) %>%
    summarise(
      ReportedAdherence = round(mean(ReportedPills > 0) * 100, 1),
      PredictedAdherence = round(mean(PredictedPills > 0) * 100, 1)
    )
  
  # Simulated data for heart rate
  heart_rate_data <- data.frame(
    Time = seq.POSIXt(Sys.time() - 86400, Sys.time(), by = "hour"),  # Last 24 hours
    HeartRate = round(runif(25, 60, 100))  # Random heart rate values
  )
  
  # Simulated data for sleep patterns
  sleep_data <- data.frame(
    Date = seq.Date(Sys.Date() - 30, Sys.Date(), by = "days"),
    TotalSleepHours = round(runif(31, 4, 9), 1)  # Random sleep hours
  )
  
  # Simulated data for actigraph (activity over time)
  actigraph_data <- data.frame(
    Time = seq.POSIXt(Sys.time() - 86400, Sys.time(), by = "hour"),  # Last 24 hours
    Activity = round(runif(25, 0, 100))  # Random activity levels (0-100 scale)
  )
  
  # Simulated data for lithium levels
  lithium_data <- data.frame(
    Date = seq.Date(Sys.Date() - 30, Sys.Date(), by = "days"),
    LithiumLevel = round(runif(31, 0.6, 1.2), 2)  # Therapeutic range for lithium
  )
  
  # Simulated daily data for glucose levels (previously hourly)
  glucose_data <- data.frame(
    Date = seq.Date(Sys.Date() - 30, Sys.Date(), by = "days"),
    GlucoseLevel = round(runif(31, 70, 140))  # Random glucose levels for each day
  )
  
  # Heart Rate Trends
  output$heart_rate_trends <- renderPlot({
    ggplot(heart_rate_data, aes(x = Time, y = HeartRate)) +
      geom_line(color = "blue", size = 1) +
      theme_minimal() +
      labs(title = "Heart rate trends", x = "Time", y = "Heart Rate (bpm)") +
      theme(
        plot.title = element_text(size = 15, face = "bold", hjust = 0.5)  # Bigger, bold, centered title
      )
  })
  
  # Sleep Patterns
  output$sleep_patterns <- renderPlot({
    ggplot(sleep_data, aes(x = Date, y = TotalSleepHours)) +
      geom_bar(stat = "identity", fill = "skyblue") +
      theme_minimal() +
      labs(title = "Sleep patterns", x = "Date", y = "Hours") +
      theme(
        plot.title = element_text(size = 15, face = "bold", hjust = 0.5)  # Bigger, bold, centered title
      )
  })
  
  # Actigraph Plot
output$actigraph <- renderPlot({
  ggplot(actigraph_data, aes(x = Time, y = Activity)) +
    geom_line(color = "green", size = 1) +
    theme_minimal() +
    labs(title = "Actigraph trends", x = "Time", y = "Activity levels") +
    theme(
      plot.title = element_text(size = 15, face = "bold", hjust = 0.5)  # Bigger, bold, centered title
    )
})
  
  # Lithium Levels
  output$lithium_levels <- renderPlot({
    ggplot(lithium_data, aes(x = Date, y = LithiumLevel)) +
      geom_line(color = "orange", size = 1) +
      geom_hline(yintercept = c(0.6, 1.2), linetype = "dashed", color = "red") +
      theme_minimal() +
      labs(title = "Lithium levels in blood", x = "Date", y = "Level (mmol/L)") +
      theme(
        plot.title = element_text(size = 15, face = "bold", hjust = 0.5)  # Bigger, bold, centered title
      )
  })
  
  # Glucose Levels
  output$glucose_levels <- renderPlot({
    ggplot(glucose_data, aes(x = Date, y = GlucoseLevel)) +
      geom_line(color = "red", size = 1) +
      theme_minimal() +
      labs(title = "Glucose levels in blood", x = "Date", y = "Glucose (mg/dL)") +
      theme(
        plot.title = element_text(size = 15, face = "bold", hjust = 0.5)  # Bigger, bold, centered title
      )
  })
  
  
  output$adherence_graph <- renderPlot({
    ggplot(weekly_adherence, aes(x = as.Date(Week))) +
      geom_line(aes(y = ReportedAdherence, color = "Self-Reported"), size = 1) +
      geom_line(aes(y = PredictedAdherence, color = "Predicted"), size = 1, linetype = "dashed") +
      scale_color_manual(values = c("Self-Reported" = "blue", "Predicted" = "red")) +
      scale_y_continuous(labels = scales::percent_format(scale = 1)) +
      theme_minimal() +
      labs(
        title = "",
        x = "Week",
        y = "Adherence (%)",
        color = "Source"
      ) +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
  })
  
  output$daily_pill_table <- DT::renderDataTable({
    adherence_data %>%
      select(Day, ReportedPills) %>%
      rename(
        "Date" = Day,
        "Pills Taken" = ReportedPills
      ) %>%
      DT::datatable(
        options = list(pageLength = 10, order = list(list(0, "desc"))),
        rownames = FALSE
      )
  })
  
  # Alerts
  output$alerts_table <- renderTable({
    data.frame(
      Date = Sys.Date(),
      Alert = c("Missed medication dose", "High heart rate detected"),
      Status = c("Unread", "Resolved")
    )
  })
  
  # Simulate the creation of a new appointment and show a notification
  observeEvent(input$newAppointmentBtn, {
    showModal(modalDialog(
      title = "New Appointment Created",
      paste("An appointment has been created for", patients$Name[1], 
            "for the reason:", "Low adherence"),
      easyClose = TRUE,
      footer = NULL
    ))
  })
  
  # Detect row click
  observeEvent(input$patientTable_cell_clicked, {
    rowClicked <- input$patientTable_cell_clicked$row
    if (!is.null(rowClicked)) {
      selectedPatient(patients[rowClicked, , drop = FALSE])
      # Switch to the "Patient Details" tab
      updateTabsetPanel(session, "x0", selected = "Patient Details")
    }
  })
  
  # Predefined example reports
  output$exampleReport1 <- renderText({
    paste(
      "Patient Report\n",
      "Name: Patient 1\n",
      "Age: 33\n",
      "BD Type: Type 2\n",
      "Medication: MedC\n",
      "Mood Score: 7\n",
      "Medication Adherence: 92%\n\n",
      "Based on the data provided, the following observations have been made:\n",
      "1. Mood score is within normal range.\n",
      "2. Medication adherence is at 92%, which is considered acceptable.\n",
      "3. No significant changes in medication are recommended at this time.\n\n",
      "Recommendations:\n",
      "Continue monitoring medication adherence and mood over the next few months.\n",
      "Consider follow-up in 3 months to assess mood stability and any changes in adherence.\n"
    )
  })
  
  output$exampleReport2 <- renderText({
    paste(
      "Patient Report\n",
      "Name: Patient 1\n",
      "Age: 33\n",
      "BD Type: Type 2\n",
      "Medication: MedC\n",
      "Mood Score: 4\n",
      "Medication Adherence: 85%\n\n",
      "Based on the data provided, the following observations have been made:\n",
      "1. Mood score indicates possible mood instability.\n",
      "2. Medication adherence is at 85%, a slight concern.\n",
      "3. A review of the medication regimen may be necessary.\n\n",
      "Recommendations:\n",
      "Monitor mood and medication adherence closely. Consider medication adjustments if necessary.\n",
      "Follow-up in 1 month to reassess mood stability and adherence.\n"
    )
  })
  
  # Reactive value to store the written report
  savedReport <- reactiveVal("")
  
  # Save the report when the "Save Report" button is clicked
  observeEvent(input$saveReport, {
    savedReport(input$reportText)  # Store the text entered in the text area
  })
  
  # Display the saved report
  output$savedReport <- renderText({
    savedReport()  # Display the saved report text
  })
  
  # Download the saved report as a .txt file
  output$downloadReport <- downloadHandler(
    filename = function() {
      paste("patient_report_", Sys.Date(), ".txt", sep = "")
    },
    content = function(file) {
      write(savedReport(), file)  # Save the report to a text file
    }
  )
  
  # Reactive value to store the chat messages for the selected patient
  chatMessages <- reactiveVal(list())
  
  # Reactive value to store the selected patient
  selectedPatientChat <- reactiveVal(patients$Name[1])
  
  # Observe patient selection
  observeEvent(input$selectedPatientChat, {
    selectedPatientChat(input$selectedPatientChat)  # Update selected patient
    # Reset chat messages when switching patients
    if (input$selectedPatientChat == "Patient 1") {
      # Default conversation for Patient 1
      chatMessages(list(
        "Hello, how have you been feeling today?",
        "I've been feeling a bit better, but still some mood swings.",
        "I see. Have you been taking your medication regularly?",
        "Yes, I’ve been sticking to the schedule.",
        "That’s good to hear. Are there any side effects you've noticed?",
        "No, no side effects so far.",
        "Great! Let's continue monitoring your progress."
      ))
    } else {
      # Default empty chat if it's not Patient 1
      chatMessages(list())
    }
  })
  
  # Generate chat messages for the selected patient
  output$chatMessages <- renderUI({
    patient <- selectedPatientChat()
    
    # Get current chat messages
    messages <- chatMessages()
    
    # Render messages as a list of divs with alternating message styles
    messageOutput <- lapply(seq_along(messages), function(i) {
      if (i %% 2 != 0) {
        # Doctor's message
        div(style = "text-align: right; padding: 5px; background-color: lightblue; border-radius: 10px; margin-bottom: 5px;",
            paste0("Doctor: ", messages[[i]]))
      } else {
        # Patient's message
        div(style = "text-align: left; padding: 5px; background-color: lightgray; border-radius: 10px; margin-bottom: 5px;",
            paste0(patient, ": ", messages[[i]]))
      }
    })
    
    # Render the message output
    do.call(tagList, messageOutput)
  })
  
  # Send a new message
  observeEvent(input$sendMessage, {
    message <- input$messageInput
    if (message != "") {
      # Add new message to the chat history
      currentMessages <- chatMessages()
      currentMessages <- append(currentMessages, list(message))
      chatMessages(currentMessages)  # Update the message history
      
      # Clear the message input field
      updateTextAreaInput(session, "messageInput", value = "")
    }
  })
  
  
}

# Run the application
shinyApp(ui, server)



