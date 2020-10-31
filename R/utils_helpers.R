
#' Generates UI
#' @description This is the 'muscle' behind the UI building and taken from the UI of shinyforms. It takes a list(id, type, etc.) and decides what to make. Used with lapply in main googleform() function over questions arguments.
#' @param question A list containing id, type, required, and (optionally) choices
formQ <- function(question){

  #decide what widget to make
  #all the shiny widgets have been renamed with wrappers to mimic the google form options
  #built off of shinyforms
  if (question$type == "numeric") {
    input <- shiny::numericInput(question$id, NULL, 0)
  } else if (question$type == "checkbox") {
    input <- shiny::checkbox(question$id, question$choices)
  } else if (question$type == 'multiplechoice'){
    input <- multipleChoice(question$id, question$choices)
  } else if (question$type == 'dropdown'){
    input < dropdown(question$id, choices)
  } else if (question$type == 'shortanswer'){
    input <- shortAnswer(question$id)
  } else if (question$type == 'paragraph'){
    input <- paragraph(question$id)
  } else if (question$type == 'height'){
    input <- selectHeight(question$id)
  }

  #if questions is marked as required add a 'Required *' tag before widget
  if(!is.null(question$required)){
    if (question$required) {
      ui <- shiny::tagList(h5('Required *', style = 'color:#fd0800;'), input)
    } else {
      ui <- input
    }
  } else {
    ui <- input
  }

  #put everything ina dashboard box to make it look like a googleform
  #one widget to a box
  shinydashboard::box(width = NULL,
                      solidHeader = T,
                      title = question$title,
                      ui )


}

#' Convenient wrappers for shiny widgets using the googleForm lingo
#' @param id an inputId
#' @param choices a list
#' @export
multipleChoice <- function(id, choices){
  shiny::radioButtons(inputId = id,
                      label = NULL,
                      choices = choices,
                      inline = F)
}

#' Limited theme option to change color of form
#' @param color a character string of a hex color code
theme_quickform <- function(color){
  fresh::create_theme(
    fresh::adminlte_color(
      light_blue = color)
  )
}



#' Convenient wrappers for shiny widgets using the googleForm lingo
#' @param id an inputId
#' @param choices a list
#' @export
checkbox <- function(id, choices){
  shiny::checkboxInput(inputId =  id,
                       label = NULL,
                       value= F)
}

#' Convenient wrappers for shiny widgets using the googleForm lingo
#' @param id an inputId
#' @param choices a list
#' @export
dropdown <- function(id, choices){
  shiny::selectInput(inputId = id,
                     label = NULL,
                     choices = choices)
}


#' Convenient wrappers for shiny widgets using the googleForm lingo
#' @param id an inputId
#' @export
shortAnswer <- function(id){
  shiny::textInput(inputId = id,
                   label = NULL)

}

#' Convenient wrappers for shiny widgets using the googleForm lingo
#' @param id an inputId
#' @export
paragraph <- function(id){
  shiny::textAreaInput(inputId = id,
                       label = NULL,
                       width = '100%')
}

#' Helper for height wrapper
#' @description Makes a list of height in feet-height notation
heightChoices <- function(){
  ft <- 0:7
  inch <-  1:11
  ht <- expand.grid(ft, inch)
  ht <- ht[order(ht$Var1),]

  ht_string <- vector()
  for(i in 1:nrow(ht)){
    ht_string[[i]] <- paste0(ht$Var1[[i]], "-", ht$Var2[[i]])
  }
  return(ht_string)
}

#' Convenient wrapper for making a selectInput about choosing a person's height in feet and inches
#' @param id an inputId
#' @export
selectHeight <- function(id){
  shiny::selectInput(inputId = id,
                     label = NULL,
                     choices = heightChoices())
}
