
#====Functions that I might want to keep ahold of=====#

#=Internal codebook============================================================
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

#=External codebook============================================================
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


#====Delta_r2==================================================================
#Add support for: 
#more engines
#exporting dataframes
#different R2 specifications

delta_r2 <- function(base, full, data, engine="lm"){
  require(broom)
  if(engine=="lm") {
    r2_base <- lm(base, data=data) %>% glance() %>% pull(r.squared)
    r2_full <- lm(full, data=data) %>% glance() %>% pull(r.squared)
    delta_r2 <- r2_full - r2_base
    delta_r2
  } else {
    print("ENGINE NOT SUPPORTED!")
  }
}

#====END====

