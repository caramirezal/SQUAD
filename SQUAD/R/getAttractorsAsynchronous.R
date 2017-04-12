#' getAttractors calculates all asynchronous attractors of a Boolean Regulatory Network model
#' @export

getAttractorsAsynchronous <- function() {
        searcher <- .jnew("SSSearcher")
        res <- .jcall(searcher,"[[B","getAttractors")
        attractors <- list()
        #as.numeric(.jevalArray(res[[1]]))
        for (i in 1:length(res)) {
                # cat(as.numeric(.jevalArray(res[[i]])),"\n")
                attractors[[i]] <- as.numeric(.jevalArray(res[[i]]))
        }
        attractors
}