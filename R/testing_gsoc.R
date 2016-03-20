#' @export
markovchainSequenceDeepak <- function (n, markovchain, t0 = sample(markovchain@states, 1),
                               include.t0 = FALSE) {
  if (!(t0 %in% markovchain@states))
    stop("Error! Initial state not defined")
  
  return(markovchainSequenceRcpp(n, markovchain, t0, include.t0))
}

#' @export
rmarkovchainDeepak <- function(n, object, what = "data.frame", ...) {
  if (class(object) == "markovchain")
    out <- markovchainSequenceDeepak(n = n, markovchain = object, ...)
  
  if (class(object) == "markovchainList") {
    dataList <- markovchainListRcpp(n, object)
    
    if (what == "data.frame")
      out <- data.frame(iteration = dataList[[1]], values = dataList[[2]])
    else 
      out <- matrix(data = dataList[[2]], nrow = n, byrow = TRUE)
  }
  
  return(out)
}