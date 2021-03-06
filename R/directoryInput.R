#' Choose a Folder Interactively (Mac OS X)
#'
#' Display a folder selection dialog under Mac OS X
#'
#' @param default which folder to show initially
#' @param caption the caption on the selection dialog
#'
#' @return
#' A length one character vector, character NA if 'Cancel' was selected.
#'
#'@name choose.dir
#'@export
#'
if (Sys.info()['sysname'] == 'Darwin') {
  choose.dir <- function(default = NA, caption = NA) {
    command <- 'osascript'
    args = '-e "POSIX path of (choose folder{{prompt}}{{default}})"'
    
    if (!is.null(caption) && !is.na(caption) && nzchar(caption)) {
      prompt = sprintf(' with prompt \\"%s\\"', caption)
    } else {
      prompt = ''
    }
    args = sub('{{prompt}}', prompt, args, fixed = T)
    
    if (!is.null(default) && !is.na(default) && nzchar(default)) {
      default = sprintf(' default location \\"%s\\"', path.expand(default))
    } else {
      default = ''
    }
    args = sub('{{default}}', default, args, fixed = T)
    
    suppressWarnings({
      path = system2(command, args = args, stderr = TRUE)
    })
    if (!is.null(attr(path, 'status')) && attr(path, 'status')) {
      # user canceled
      path = NA
    }
    
    return(path)
  }
} else if (Sys.info()['sysname'] == 'Linux') {
  choose.dir <- function(default = NA, caption = NA) {
    command <- 'zenity'
    args <- '--file-selection --directory --title="Choose a folder"'
    
    suppressWarnings({
      path <- system2(command, args = args, stderr = TRUE)
    })
    
    #Return NA if user hits cancel
    if (!is.null(attr(path, 'status')) && attr(path, 'status')) {
      # user canceled
      return(default)
    }
    
    #Error: Gtk-Message: GtkDialog mapped without a transient parent
    if(length(path) == 2){
      path = path[2]
    }
    
    return(path)
  }
}

#' Directory Selection Control
#'
#' Create a directory selection control to select a directory on the server
#'
#' @param inputId The \code{input} slot that will be used to access the value
#' @param label Display label for the control, or NULL for no label
#' @param value A directory path to set
#'
#' @usage directoryInput(inputId, label, value)
#'
#' @return
#' A directory input control that can be added to a UI definition.
#' 
#' 
#'@import htmltools
#'@export
directoryInput <- function(inputId, label, value = NULL) {
  if (!is.null(value) && !is.na(value)) {
    value = path.expand(value)
  }
  
  tagList(
    singleton(
      tags$head(
        tags$script(src = 'directory_input_binding.js')
      )
    ),
    
    div(
      class = 'form-group directory-input-container',
      shiny:::`%AND%`(label, tags$label(label)),
      div(
        span(
          class = 'col-xs-8 col-md-10',
          style = 'padding-left: 0; padding-right: 0px;',
          div(
            class = 'input-group shiny-input-container',
            style = 'width:100%;',
              # tags$button(
              #   id = inputId,
              #   class = 'btn btn-default directory-input',
              #   icon('folder-o')
              # ),
            div(class = 'input-group-addon', icon('folder-o')),
            tags$input(
              id = sprintf('%s__chosen_dir', inputId),
              value = value,
              type = 'text',
              class = 'form-control directory-input-chosen-dir'
              #readonly = 'readonly'
            )
          )
        ),
        span(
          class = 'shiny-input-container',
          tags$button(
            id = inputId,
            class = 'btn btn-default directory-input',
            '...'
          )
        )
       )
    )
    
  )
  
}

#' Change the value of a directoryInput on the client
#'
#' @param session The \code{session} object passed to function given to \code{shinyServer}.
#' @param inputId The id of the input object.
#' @param value A directory path to set
#'
#' @usage updateDirectoryInput(session, inputId, value)
#'
#' @details
#' Sends a message to the client, telling it to change the value of the input
#' object.  For \code{directoryInput} objects, this changes the value displayed
#' in the text-field and triggers a client-side change event.  A directory
#' selection dialog is not displayed.
#'
#'@export
updateDirectoryInput <- function(session, inputId, value = NULL) {
  if (is.null(value)) {
    value = choose.dir('~')
  }
  session$sendInputMessage(inputId, list(chosen_dir = value))
}

#' Read the value of a directoryInput
#'
#' @param session The \code{session} object passed to function given to \code{shinyServer}.
#' @param inputId The id of the input object
#'
#' @details
#' Reads the value of the text field associated with a \code{directoryInput}
#' object that stores the user selected directory path.
#' 
#'@return path of selected directory
#'
#'@export
readDirectoryInput <- function(session, inputId) {
  session$input[[sprintf('%s__chosen_dir', inputId)]]
}
