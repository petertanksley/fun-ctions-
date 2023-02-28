
#====Functions that I might want to keep ahold of=====#

#=Internal_codebook====
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

#=External Codebook 
quick_codebook_ext <- function(path, exist_skip=TRUE) {
  require(glue)
  require(sjlabelled)
  require(tibble)
  if(exist_skip=="TRUE") {
    out_path <- sub("^(.*)[.].*", "\\1", path)
    out_path <- sub("^(.*)[.].*", "\\1", out_path) #extra layer allows for .zip files 
    if(!file.exists(glue("{out_path}_codebook.rds"))){
      codebook <- enframe(get_label(import(glue("{path}"))))
      export(codebook, glue("{out_path}_codebook.rds"))
      print("NEW CODEBOOK CREATED!")
    } else {
      print("CODEBOOK ALREADY EXISTS!")
    }
  } else { 
    out_path <- sub("^(.*)[.].*", "\\1", path)
    out_path <- sub("^(.*)[.].*", "\\1", out_path) #extra layer allows for .zip files 
    codebook <- enframe(get_label(import(glue("{path}"))))
    export(codebook, glue("{out_path}_codebook.rds"))
    print("NEW CODEBOOK CREATED!")
  }
} 

#====END====

