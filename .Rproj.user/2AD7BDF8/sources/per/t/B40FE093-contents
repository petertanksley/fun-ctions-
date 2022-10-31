
#====Functions that I might want to keep ahold of=====#

#=quick_codebook====
quick_codebook <- function(x, out_df=FALSE) {
  require(glue)
  require(sjlabelled)
  require(tibble)
  if(out_df=="FALSE") {
    view(enframe(get_label(x)))
  } else {
    name <- deparse(substitute(x))
    assign(glue("{name}_codebook"),
           enframe(get_label(x)),
           inherits=TRUE)
  } 
}

#====END====

