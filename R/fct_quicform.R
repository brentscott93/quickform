#' Create a Google Form look-a-like shiny app
#'
#' @description Create a shiny app with minimal code that mimicks that looks of a Google Form. Currently only stores data in Google Drive (or locally) and has the option to allow survey participants to return and edit their survey with a unique ID. Optionally, this ID can be emailed to the user. Requires one-time setup of {googledrive}, {googlesheets4}, and {gmailr}
#' @param title a character string.
#' @param description a character string.
#' @param questions a nested list of questions.
#' @param gmail FALSE or your gmail account to store data in drive and optionally to email IDs to users.
#' @param folder a character string specicifying the folder on desktop/drive to store results in.
#' @param color a character string of a hex color to theme the app. Default is blue.
#' @param returningUser logical. Do you want users to return to edit/update their survey. Default is FALSE
#' @param emailId logical. Do you want to email ID to users. Default is FALSE
#' @param subject a character string. For the subject of your emaill. Default is 'Your survey ID'
#' @param public logical. Do you want the results to be saved in a public google drive folder?
#'
#' @return a shiny app
#' @export
#' @import shiny shinydashboard shinyjs magrittr
#' @examples
#' if(interactive()){
#' library(shiny)
#' library(shinydashboard)
#' library(shinyforms)
#'googleform(
#' title = "My Survey",
#' description = 'Describe your survey here',
#' questions = list(
#'   list(id = "age",
#'        type = "numeric",
#'        title = "Age (yrs)",
#'        required = T),
#'   list(id = "height",
#'        type = "height",
#'        title = "Height (ft-in)",
#'        required = T),
#'   list(id = "weight_lbs",
#'        type = "numeric",
#'        title = "Weight (lbs)",
#'        required = T),
#'   list(id = 'ethnicity',
#'        type = "multiplechoice",
#'        title = "Are you of Hispanic, Latino, or of Spanish origin?" ,
#'        choices = list('No', 'Yes'),
#'        required = T),
#'   list(id = "race",
#'        type = "multiplechoice",
#'        title = "Race",
#'        choices = list('American Indian or Alaska Native',
#'                       'Asian',
#'                       'Black or African American',
#'                       'Native Hawaiian or Other Pacific Islander',
#'                       'White',
#'                       'Other'),
#'         required = T),
#'   list(id = "bp_systolic",
#'        type = "numeric",
#'        title = "Blood Pressure (Systolic)"),
#'   list(id = "bp_diastolic",
#'        type = "numeric",
#'        title = "Blood Pressure (Diastolic)"),
#'   list(id = "resting_HR",
#'        type = "numeric",
#'        title = "Resting Heart Rate")
#' ),
#'
#' gmail = F,
#' folder = 'shinyforms'
#'
#' )
#' }
quickform <- function(title = NULL,
                       description = NULL,
                       questions = NULL,
                       gmail = F,
                       folder,
                       color = '#2e77ff',
                       returningUser = F,
                       emailId = F,
                       subject = 'Your survey ID',
                       public = F){


  options(scipen=999)
  quickformCSS <- "shinyforms-ui .mandatory_star { color: #db4437; font-size: 20px; line-height: 0; }"

  #setup storage locations and authentications to google services
  if(gmail != FALSE){

    googledrive::drive_auth(email = gmail, cache = '.secrets')
    googlesheets4::gs4_auth(token = googledrive::drive_token())
    #googledrive::drive_auth(path = service_account)
    #googlesheets4::gs4_auth(path = service_account)
  }
  # Create new sheets folder for responses


  #if folder does not exist create it
  if(nrow(googledrive::drive_find(folder, n_max = 1)) == 0){
    googledrive::drive_mkdir(folder)
  }
  #if user wants folder to be public change permission
  if(public){
    googledrive::drive_share(folder, role = 'reader', type = 'anyone')
  }

  #if the user wants the participants to be able to allow people to edit/return to update survey
  #option to email unique id with gmailr
  if(returningUser){
    if(emailId){
      # Configure your app
      gmailr::gm_auth_configure(path = "credentials.json")

      # Authenticate with the tokens in the copied cache
      gmailr::gm_auth(email = gmail, cache = '.gm-secrets')
    }}
  #   }
  # } else {
  #   #if in desktop mode make folder locally
  #   dir.create(folder)
  # }

  # getRequiredValues <- function(questions){
  #   requiredValues <<- data.frame(id = NA, required = F, type = NA)
  #   for(i in seq_along(questions)){
  #
  #     if(!is.null(questions[[i]]$required)){
  #       if (questions[[i]]$required) {
  #         requiredValues[i,] <<- c(questions[[i]]$id, T, questions[[i]]$type)
  #
  #       } else {
  #         requiredValues[i,] <<- c(questions[[i]]$id, F, questions[[i]]$type)
  #
  #       }
  #     } else {
  #       requiredValues[i,] <<- c(questions[[i]]$id, F, questions[[i]]$type)
  #
  #     }
  #   }
  # }

  #token <- readRDS(list.files(here::here('.secrets')))
  #user interface
  #this is a shinydashboard with no header or sidebar
  #mimics looks of a google form
  ui <- dashboardPage(
    dashboardHeader(disable = T),
    dashboardSidebar(disable = T),
    dashboardBody(fresh::use_theme(theme_quickform(color)),
                  style = paste0("background-color:", scales::alpha(color, 0.5), ";"),
                  shinyjs::useShinyjs(),
                  shinyjs::inlineCSS(quickformCSS),
                  fluidRow(
                    column(3),
                    column(6,
                           box(width = NULL,
                               status = 'primary',
                               title = title,
                               description),
                           uiOutput('returningUser'),
                           lapply(questions, formQ),
                           actionButton('submit',
                                        'Submit',
                                        style = paste0("background-color: ", color, "; color: #fff;"))
                           #verbatimTextOutput('d')
                    )
                  )
    )
  )

  server <- function(input, output, session) {

    rv <- reactiveValues(saved = 0)
    observeEvent(input$submit, {
      #check if returning user has entered a valid ID (or at least entered something really)
      if(returningUser){
        if(input$user == 'return'){
          if(input$userId == '') showNotification('Please enter ID if you are a returning user.')
          req(input$userId)
        }
      }

      #would be nice to have tests to check required inputs and throw informative notifications
      # need <- requiredValues[which(requiredValues$id == T),]
      # need <- split(need, seq(nrow(need)))
      # lapply(need, validateRequired)

      withProgress(message = 'Saving', {
        setProgress(0.2)
        #probably a better way to do this
        #removing uneeded rows from the results that prevent from converting list to data.frame
        l <- reactiveValuesToList(input)
        l$submit <- NULL
        l$sidebarCollapsed <- NULL
        l$sidebarItemExpanded <- NULL
        l$userId <- NULL

        if(returningUser) l$loadReturning <- NULL

        data <- as.data.frame(l)
        setProgress(0.5)
        if(returningUser){
          if(input$user == 'new'){
            filename <- paste0(gsub( "[^[:alnum:]]", '', Sys.time()), round(runif(1, 1000000, 2000000)))
            rv$filename <- filename
            data$id <- filename
            if(gmail != FALSE){
              #create new sheet and then move to desired location
              #googlesheets doesn't seem to support making sheets in desired locations
              googlesheets4::gs4_create(name = filename, sheets = data)
              setProgress(0.8, detail = 'Uploading results to google drive')
              googledrive::drive_mv(filename, path = file.path(folder, filename))
            } else {
              #if local write to disk
              write.csv(data, file.path(folder, paste0(filename, '.csv.')))
            }

            setProgress(1)

          } else if(input$user == 'return'){
            #if returning user check if an ID is entered else show a notification to enter one
            if(input$userId == '')showNotification('Please enter an ID', type = 'error')
            #if valid ID and googledrive storage overwrite sheets data
            if(input$userId != ''){
              if(gmail != FALSE){

                setProgress(0.8, detail = 'Uploading results to google drive')
                userDataOverwrite <- googledrive::drive_find(input$userId, n_max = 1)
                #check to make sure user supplied ID matches a file on drive
                if(nrow(userDataOverwrite) == 0){
                  showNotification('Not a valid ID', type = 'error')
                } else {
                  #if ID matches a file overwrite with new survey data
                  data$id <- input$userId
                  rv$filename <- input$userId
                  googlesheets4::write_sheet(data, ss = userDataOverwrite, sheet = 'data')
                }
              } else {
                #if local overwrite on disk
                write.csv(data, file = file.path(folder, paste0(input$userId, '.csv')))
              }
              setProgress(1)
            }
          }
        }
      })
      #once complete saving reactitiy - update a reactive value to launch a modal
      rv$saved <- rv$saved + 1
    })

    output$showId <- renderPrint({
      cat(isolate(rv$filename))
    })

    observeEvent(rv$saved, ignoreInit = T, {
      showModal(modalDialog(
        title = "Response Saved!",
        "Your response has been saved. You can return to this survery and update your answers by entering in the following ID in the returning user section at the top of the form: ",
        br(),
        verbatimTextOutput('showId'),
        br(),
        #if user wants to email unique IDs to client show an email textInput
        if(emailId){
          textInput('address',
                    label = 'Would you like this emailed?',
                    placeholder = 'yourEmail@here.com')
        },
        footer = tagList(
          modalButton("Cancel"),
          actionButton("ok", "OK")
        )
      ))
    })

    observeEvent(input$ok, ignoreInit = T, {
      #if ok is clicked in modal dialogue and a valid email is entered - email id to address with gmailr if emailId is T
      if(emailId){
        if(grepl('@', input$address)){

          email <- gmailr::gm_mime() %>%
            gmailr::gm_to(input$address) %>%
            gmailr::gm_from(gmail) %>%
            gmailr::gm_subject(subject) %>%
            gmailr::gm_text_body(paste('Thanks for taking the survey. Your returning ID is: ' , rv$filename))

          gmailr::gm_send_message(email)
          showNotification('Email sent!', type = 'message')
          removeModal()

        } else {

          showNotification('Not a valid email address', type = 'error')

        }

      } else {
        #if emailId is False both Ok and cancel remove the modal
        removeModal()

      }
    })

    #### Returning User ####
    #makes the returning user UI box
    output$returningUser <- renderUI({
      if(returningUser){
        box(width = NULL,
            solidHeader = T,
            title = 'Returning User?',
            radioButtons('user',
                         label = NULL,
                         choices = list('New User' = 'new',
                                        'Returning' = 'return'),
                         inline = T),
            textInput('userId', label = 'Enter ID', placeholder = 'Enter ID Here'),
            actionButton('loadReturning', 'Load')
        )
      }
    })


    #reactive events that occur if a returning user ID is entered
    #the app will search for a matching filename to the unique ID
    #if one is found the survey is updated with those results
    observeEvent(input$loadReturning, {
      if(returningUser){
        if(input$user == 'return'){
          if(input$userId == '') showNotification('Please enter ID if you are a returning user.')
          req(input$userId)
          withProgress(message = 'Loading',{
            if(gmail != FALSE){
              #googledrive storage
              userFile <-  googledrive::drive_find(input$userId, n_max = 1)
              if(nrow(userFile) == 0) showNotification('No file matches that ID')
              req(nrow(userFile) == 1)
              setProgress(0.5, detail = 'Updating')
              data <- googlesheets4::read_sheet(userFile)
            } else if(gmail == FALSE){
              #local file storage
              userFile <- list.files(folder, pattern = input$userId, full.names = T)
              data <- read.csv(userFile)
            }
            #loop through every questions list element
            #update the shiny widget with the saved value in the google sheet
            for(i in seq_along(questions)){
              if (questions[[i]]$type == "numeric") {
                updateNumericInput(session = session,
                                   inputId = questions[[i]]$id,
                                   value =  data[[questions[[i]]$id]])

              } else if (questions[[i]]$type == "checkbox") {
                updateCheckboxInput(session = session,
                                    inputId = questions[[i]]$id,
                                    value =  data[[questions[[i]]$id]])


              } else if (questions[[i]]$type == 'multiplechoice'){
                updateRadioButtons(session = session,
                                   inputId =  questions[[i]]$id,
                                   selected =  data[[questions[[i]]$id]])

              } else if (questions[[i]]$type == 'dropdown'){
                updateSelectInput(session = session,
                                  inputId =  questions[[i]]$id,
                                  selected =  data[[questions[[i]]$id]])


              } else if (questions[[i]]$type == 'shortanswer'){
                updateTextInput(session = session,
                                inputId =  questions[[i]]$id,
                                value =  data[[questions[[i]]$id]])

              } else if (questions[[i]]$type == 'paragraph'){

                updateTextAreaInput(session = session,
                                    inputId = questions[[i]]$id,
                                    value =  data[[questions[[i]]$id]])

              } else if (questions[[i]]$type == 'height'){
                updateSelectInput(session = session,
                                  inputId =  questions[[i]]$id,
                                  selected =  data[[questions[[i]]$id]])

              }

            }
          })
          showNotification('Survey Loaded', type = 'message')
        }
      }
    })

    #user-experience with shinyjs
    observeEvent(input$user, {
      if(input$user == 'return'){
        shinyjs::show('userId')
        shinyjs::show('loadReturning')

      } else {
        shinyjs::hide('userId')
        shinyjs::hide('loadReturning')
      }

    })

    #     output$d <- renderPrint({
    #       requiredValues
    # })
  }

  shinyApp(ui, server)

}
