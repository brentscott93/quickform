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
