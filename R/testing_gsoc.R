#' Function to generate a sequence of states from homogeneous or non-homogeneous Markov chains.
#' 
#' @description Provided any markovchain or markovchainList objects, 
#' it returns a sequence of states coming from the underlying stationary distribution.
#' 
#' @usage 
#' rmarkovchainDeepak(n, object, what="data.frame",...)
#' markovchainSequenceDeepak(n, markovchain, t0 = sample(markovchain@states, 1), 
#'                           include.t0 = FALSE)
#'                    
#' @param n Sample size                      
#' @param object Either a markovchain or a markovchainList object.
#' @param what It specifies whether either a data.frame or 
#'        a matrix (each rows represent a simulation) or a list is returned.
#' @param ... additional parameters passed to the internal sampler
#' @param t0 The initial state.          
#' @param include.t0 Specify if the initial state shall be used.
#' 
#' @details 
#' When an homogeneous process is assumed (markovchain object) 
#' a sequence is sampled of size n. When an non - homogeneous 
#' process is assumed, n samples are taken but the process is 
#' assumed to last from the begin to the end of the non-homogeneous 
#' markov process.
#' 
#' @examples 
#' # define the Markov Chain
#' myMc <- as(matrix(c(.2,.8,.5,.5), byrow = TRUE, nrow=2), "markovchain")  
#' 
#' # show the sequence
#' outs1 <- markovchainSequenceDeepak(n=100,markovchain=myMc)
#' outs2 <- rmarkovchainDeepak(n=20, object=myMc)
#' 
#' @export

markovchainSequenceDeepak <- function (n, markovchain, t0 = sample(markovchain@states, 1),
                                       include.t0 = FALSE) {
  if (!(t0 %in% markovchain@states))
    stop("Error! Initial state not defined")
  
  return(.markovchainSequenceRcpp(n, markovchain, t0, include.t0))
}

#' Function to generate a sequence of states from homogeneous or non-homogeneous Markov chains.
#' 
#' @description Provided any markovchain or markovchainList objects, 
#' it returns a sequence of states coming from the underlying stationary distribution.
#' 
#' @usage 
#' rmarkovchainDeepak(n, object, what="data.frame",...)
#' markovchainSequenceDeepak(n, markovchain, t0 = sample(markovchain@states, 1), 
#'                           include.t0 = FALSE)
#'                    
#' @param n Sample size                      
#' @param object Either a markovchain or a markovchainList object.
#' @param what It specifies whether either a data.frame or 
#'        a matrix (each rows represent a simulation) or a list is returned.
#' @param ... additional parameters passed to the internal sampler
#' @param t0 The initial state.          
#' @param include.t0 Specify if the initial state shall be used.
#' 
#' @details 
#' When an homogeneous process is assumed (markovchain object) 
#' a sequence is sampled of size n. When an non - homogeneous 
#' process is assumed, n samples are taken but the process is 
#' assumed to last from the begin to the end of the non-homogeneous 
#' markov process.
#' 
#' @examples 
#' # define the Markov Chain
#' myMc <- as(matrix(c(.2,.8,.5,.5), byrow = TRUE, nrow=2), "markovchain")  
#' 
#' # show the sequence
#' outs1 <- markovchainSequenceDeepak(n=100,markovchain=myMc)
#' outs2 <- rmarkovchainDeepak(n=20, object=myMc)
#' 
#' @export

rmarkovchainDeepak <- function(n, object, what = "data.frame", ...) {
  if (class(object) == "markovchain")
    out <- markovchainSequenceDeepak(n = n, markovchain = object, ...)
  
  if (class(object) == "markovchainList") {
    include.t0 <- list(...)$include.t0
    include.t0 <- ifelse(is.null(include.t0), FALSE, include.t0)
    
    dataList <- .markovchainListRcpp(n, object@markovchains, include.t0)
    
    if (what == "data.frame")
      out <- data.frame(iteration = dataList[[1]], values = dataList[[2]])
    else {
      out <- matrix(data = dataList[[2]], nrow = n, byrow = TRUE)
      if (what == "list") {
        outlist <- list()
        for (i in 1:nrow(out))
          outlist[[i]] <- out[i, ]
        out <- outlist
      }
    }
    
  }
  
  return(out)
}
