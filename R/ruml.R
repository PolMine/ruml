#' Auxiliary functions.
#' 
#' @details \code{class_get_subclasses} will retur a two-column
#'   \code{data.frame} (columns "super" and "sub") with the relations between
#'   super- and subclasses.
#' @export
#' @param x A class (length-one \code{character} vector).
#' @rdname helpers
#' @examples
#' library(polmineR)
#' class_get_subclasses("count")
#' @importFrom methods getClass new slotNames slot findMethods getGenerics
class_get_subclasses <- function(x){
  cl <- getClass(x)
  li <- lapply(cl@subclasses, function(sub){
    if (sub@distance > 1){
      return(NULL)
    } else {
      inherit <- data.frame(super = x, sub = sub@subClass, stringsAsFactors = FALSE)
      if (length(getClass(sub@subClass)@subclasses) > 0){
        return( rbind(inherit, class_get_subclasses(sub@subClass)) )
      } else {
        return( inherit )
      }
    }
  })
  y <- do.call(rbind, li)
  rownames(y) <- NULL
  y
}


#' @details \code{make_plantuml_relations} will generate a \code{character}
#'   vector with all relations between super- and subclasses.
#' @rdname ruml
#' @examples
#' make_plantuml_relations("count")
#' @export
make_plantuml_relations <- function(x){
  class_relations <- class_get_subclasses(x)
  paste(
    class_relations[["super"]],
    class_relations[["sub"]],
    sep = " <|-- "
  )
}



#' @details \code{class_get_subclasses_vec} will return a \code{character} vector with 
#' all subclasses that inherit from \code{x}.
#' @examples class_get_subclasses_vec
#' class_get_subclasses_vec("textstat")
#' @rdname helpers
#' @export
class_get_subclasses_vec <- function(x) unique(unlist(class_get_subclasses(x)))


#' @rdname ruml
#' @details \code{make_plantuml_class_description} will 
#' @export
make_plantuml_class_description <- function(x, generics){
  if (length(x) == 1L){
    cl_instance <- new(x)
    slot_names <- slotNames(cl_instance)
    inherited_slots <- unique(unlist(lapply(names(getClass(x)@contains), slotNames)))
    if (length(inherited_slots) > 0) slot_names <- slot_names[!slot_names %in% inherited_slots]
    slot_class <- sapply(slot_names, function(sn) class(slot(cl_instance, name = sn)))
    generic_is_defined <- sapply(
      generics,
      function(m) if (length(findMethods(m, classes = x)) > 0) TRUE else FALSE
    )
    defined_generics <- generics[generic_is_defined]
    y <- sprintf(
      "class %s {\n%s\n%s\n}",
      x,
      paste("  ", paste(slot_names, unname(slot_class), sep = ": "), collapse = "\n"),
      paste(
        paste("{method}  ", defined_generics, unname(ifelse(sapply(defined_generics, function(x) x %in% c("!=", "%in%", "[", "[[", "$")), "", "()")), sep = ""),
        collapse = "\n", sep = ""
      )
    )
  } else {
    y <- paste(
      sapply(
        x,
        function(cl) make_plantuml_class_description(cl, generics = generics)
      ),
      collapse = "\n\n"
    )
  }
  y
}


#' @rdname helpers
#' @param pkg Unquoted package name.
#' @examples 
#' pkg_get_all_methods(polmineR)
#' @export 
pkg_get_all_methods <- function(pkg) getGenerics(where = environment(pkg))@.Data

#' Generate plantuml data.
#' 
#' @param x The name of a class stated as a (length-one) \code{character} vector.
#' @param pkg An (unquoted) package name.
#' @param generics The generic methods to consider.
#' @rdname ruml
#' @examples
#' library(polmineR)
#' plantuml_code <- make_plantuml_code("corpus", pkg = polmineR)
#' @export make_plantuml_code
make_plantuml_code <- function(x, pkg, generics = pkg_get_all_methods(pkg)){
  sprintf(
    "%s\n\n%s",
    paste(make_plantuml_relations(x), collapse = "\n"),
    make_plantuml_class_description(
      class_get_subclasses_vec(x),
      generics
    )
  )
}

