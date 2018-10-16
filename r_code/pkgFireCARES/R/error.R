#' Open an error log
#'
#' This function opens the error log.
#'
#' @param dest character vector. destinations to send the error output to.
#' Destinations are usually file names.
#'
#' @details
#' This sets up the error logging system. The parameter \code{dest} lists
#' all the output locations to which messages are to be sent. If more
#' than one location is specified, then the messages are sent to all
#' the specified locations.
#'
#' With three exceptions, the values in \code{dest} are interpreted
#' as file names. The exceptions are 'console' (case is ignored here),
#' the empty string (''), and NA. Either of the first two cases are
#' interpreted as requesting that output be sent to the console.
#'
#' For every NA included in the \code{dest} vector, a file name
#' is generated. The file name has the form 'messages.xx.txt' where
#' the 'xx' is a two (or more) digit number, starting with '01'.
#' The function finds the largest such file name in the working directory
#' and uses the next larger number for the new file name. So, I can
#' auto-generate 3 (or any other arbitrary number) of log files simply
#' by listing \code{NA} three times in the \code{dest} parameter vector.
#'
#' Duplicates are automatically removed. Note that (aside from the
#' 'console'), names are case sensitive. So two entries with the same
#' name, aside from case differences, will both be retained. For operating
#' systems that are case insensitive, that will result in duplicate
#' messages being sent to the file.
#'
#' If the session is not interactive, then the request to send messages
#' to the console is automatically ignored.
#'
#' If an error environment has been set up for a calling function, then
#' some of the information for the calling function is imported for this
#' function. In particular, \code{context} is imported, and the \code{dest}
#' values are imported. If there is an error environment for a calling
#' function, then no new file name is generated, even if \code{NA} is
#' included in this call. As usual, duplicates are removed. So, any
#' messages are sent to all log files used for calling functions, and
#' to any new files specifically specified in this call. But no new
#' file is automatically generated, even if \code{NA} is specified in
#' this call.
#'
#' If an error environment has already been set up for this
#' function, this deletes it and starts over.
#'
#' @importFrom magrittr %>%
#'
#' @return
#' This function invisibly returns the stored error information.
#'
openLog <- function(dest=c(NA, "console")){
  if(exists("err.Log", parent.frame(), inherits=FALSE)){
    rm(list="err.Log", pos=parent.frame())
  }
  console <- grep("^console$", dest, ignore.case=TRUE)
  if(length(console > 0)){
    dest <- dest[-console]
    dest <- c(dest, "")
  }
  log.tmp <- new.env()
  if("err.Log" %in% do.call("c", lapply(sys.frames(), ls))){
    pf <- rev(sys.parents())
    for(i in pf){
      if(exists("err.Log", frame=i)){
        d2 <- get0("err.Log", i)
        break
      }
    }
  } else {
    ff <- length(dest[is.na(dest)])
    if(ff > 0){
      n0 <- list.files(pattern="^messages[.][0-9][0-9]+[.]txt$") %>%
            strsplit("[.]") %>%
            sapply(function(x) x[2]) %>%
            as.integer %>%
            c(0) %>%
            max
      dest[is.na(dest)] <- paste0("messages.",
                                 formatC(n0 + 1:ff, width=2, flag="0"),
                                 ".txt")
    }
  }
  dest <- unique(dest)
  if(! interactive()){
    dest <- setdiff(dest, "")
  }
  if(exists("d2")){
    log.tmp$destination <- union(d2$destination, dest) %>% setdiff(NA)
    if(exists("context", d2)){
      log.tmp$context <- d2$context
    }
  } else {
    log.tmp$destination <- dest
  }

  for(i in setdiff(log.tmp$destination, "")){
    if(! file.exists(i)) file.create(i)
  }

  assign("err.Log", log.tmp, parent.frame())
  invisible(log.tmp)
}

#' Deletes an error log
#'
#' This function deletes the error log.
#'
#' @details
#' This checks to see if an error logging environment has been set up,
#' and if it has, it deletes it.
#'
#' @return
#' This function invisibly returns the deleted error information.
#'
closeLog <- function(){
  if(exists("err.Log", parent.frame(), inherits=FALSE)){
    tmp.Log <- err.Log
    rm(list="err.Log", pos=parent.frame())
  } else {
    tmp.Log <- NULL
  }
  invisible(tmp.Log)
}

#' Send messages to the error log(s).
#'
#' This function sends messages to all error logs that have been set up.
#'
#' @param message character. The text of the message to be sent. [optional]
#'
#' @param type character. The type of message. One of 'error', 'warning',
#' 'message', 'status', 'start', or 'stop'. This parameter is not case
#' sensitive. Only the first option is used. Any unknown entry is treated
#' as equivalent to 'status'. See \code{details} for what these parameters
#' mean.
#'
#' @details
#' The message that is posted depends on the type.
#'
#' If \code{type} is one of 'error', 'warning', or 'message', then
#' the message (with \code{context} prepended) is posted to all logs.
#'
#' If \code{type} is 'status' (or any equivalent--any unidentified type is
#' interpreted as 'status'), then the message (with \code{context} prepended)
#' is posted to the console IF console is one of the logs set up.
#'
#' If \code{type} is 'start', then a message (with \code{context} prepended)
#' is posted to the console (IF console is one of the logs set up) stating
#' that a process has started. The 'start' code also records the starting
#' time.
#'
#' If \code{type} is 'stop', then a message (with \code{context} prepended)
#' is posted to the console (IF console is one of the logs set up) stating
#' that a process has completed, with the elapsed time (if a starting time
#' has been recorded).
#'
#' If no error logs have been set up, this routine automatically calls
#' \code{openLog} with a basic option to generate one log file.
#'
#' @return
#' This function invisibly returns the deleted error information.
#'
msgOut <- function(message=NULL, type="error"){
  if(! exists("err.Log", parent.frame(), inherits=FALSE)){
    openLog(as.character(NA))
  }
  err.Log <- as.list(parent.frame()$err.Log)
  type <- tolower(type[1])
  if(type == "error"){
    msg <- paste0("\nERROR:   ", paste(c(err.Log$context, message), collapse="; "), "\n")
  } else if(type == "warning"){
    msg <- paste0("\nWARNING: ", paste(c(err.Log$context, message), collapse="; "), "\n")
  } else if(type == "message"){
    msg <- paste0("\nMESSAGE: ", paste(c(err.Log$context, message), collapse="; "), "\n")
  } else if(type == "start"  ){
    msg <- paste0(paste(c(err.Log$context, message), collapse="; "), ": ")
    assign("start.time", Sys.time(), get("err.Log", parent.frame()))
  } else if(type == "stop"   ){
    if("start.time" %in% names(err.Log)){
      t <- Sys.time() - get("err.Log", parent.frame())$start.time
      t <- format(t, digits=max(0, ceiling(log(as.numeric(t), 10))) + 1)
      rm(list="start.time", pos=get("err.Log", parent.frame()))
      msg <- paste0(message, t, "\n")
    } else {
      msg <- paste0(message, "\n")
    }
  } else {# type == "status"
    msg <- paste0(paste(c(err.Log$context, message), collapse="; "), "\n")
  }
  for(i in err.Log$destination){
    if(i == "" || type %in% c("error", "warning", "message")){
      cat(msg, file=i, append=TRUE)
    }
  }
}

#' Retrieve message context.
#'
#' This function returns the message context.
#'
#' @details
#' Message context is information that is printed with the message. The
#' intent is that message context will provide information that will help
#' contextualize the message.
#'
#' @return
#' This function returns the message context in a character vector. If
#' logging has not been set up, or if no context has been set, NULL is returned.
#'
getContext <- function(){
  tmp <- get0("err.Log", parent.frame())
  if(is.null(tmp)) NULL
  else             tmp$context
}

#' Sets message context.
#'
#' This function sets the message context.
#'
#' @param context Character Vector. A character vector representing the context
#' information to be printed with the messages.
#'
#' @details
#' Message context is information that is printed with the message. The
#' intent is that message context will provide information that will help
#' contextualize the message.
#'
#' If no logging has been set up, this automatically calls the setup routine
#' with the type set to generate one message file.
#'
#' @return
#' This function invisibly returns the old message context.
#'
setContext <- function(context){
  if(! exists("err.Log", parent.frame(), inherits=FALSE)){
    openLog(as.character(NA))
  }
  old.context <- get("err.Log", parent.frame())$context
  assign("context", context, get("err.Log", parent.frame()))
  invisible(old.context)
}

#' Returns the files to which messages are sent.
#'
#' Returns the files to which messages are sent.
#'
#' @details
#' This function returns the list of files to which messages are set.
#' It does not return 'console' in the vector, even if messages are
#' sent to it.
#'
#' @return
#' This function returns a character vector containing the files to
#' which messages are sent. If an error environment has not been set
#' up, then it returns NULL.
#'
getDests <- function(){
  tmp <- get0("err.Log", parent.frame())
  if(is.null(tmp)) NULL
  else             setdiff(tmp$destination, "")
}
