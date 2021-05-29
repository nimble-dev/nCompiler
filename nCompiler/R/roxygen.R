getRoxygenFlag <- function(objs, roxygen) {
  # Handle roxygen input
  if (!is.list(roxygen)) {
    if (is.character(roxygen)) roxygen <- list(roxygen)
    else stop("in nWritePackage: unknown roxygen type")
  }
  
  if (length(roxygen) == 0) {
    roxygenFlag <- "none"
  } else if (sum(nchar(names(roxygen)) > 0) == length(roxygen)) { # Are all rox entries named?
    roxygenFlag <- "names"
  } else if (length(roxygen) == length(objs)) { # Are there as many rox entries as objs?
    roxygenFlag <- "indices"
  } else { # If neither, we don't know what to do about it
    stop("If fewer roxygen entries are provided than objects, they must be named",
         " in the input list to indicate the objects to which they correspond.")
  }

  roxygenFlag
}


roxify <- function(objname, RcppPacket, roxygenFlag, roxygen) {
  thisRox <- switch(roxygenFlag,
                    none = NULL,
                    indices = if(length(roxygen) < i) roxygen[[i]] else NULL,
                    names = if (objName %in% names(roxygen)) roxygen[[objName]] else NULL
                    )
  if (!is.null(thisRox)) {
    exportIndex <- which(RcppPackets$cppContent == "// [[Rcpp::export]]")
    return(c(RcppPackets$cppContent[1:(exportIndex - 1)],
             thisRox$header, 
             RcppPackets$cppContent[exportIndex:length(RcppPacket$cppContent)]))
  }
  else {
    return(RcppPacket$cppContent)
  }
  
}
