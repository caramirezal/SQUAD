#' helloJavaWorld prints a hello Java World msj for testing purposes
#' @export
#' 

helloJavaWorld <- function(){
   hjw <- .jnew("HelloJavaWorld") # create instance of HelloJavaWorld class
   out <- .jcall(hjw, "S", "sayHello") # invoke sayHello method
   res <- .jevalArray(out)
}


