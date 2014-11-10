# Some customization to yaml


# custom handler for booleans: only convert "TRUE" and "FALSE" to boolean
yaml.bool.handler.yes <- function(val) {
  #message(paste("bool: ", val))
  if (val=="TRUE" | val=="true")
    return(TRUE)
  return(val)
}

# custom handler for booleans: only convert "TRUE" and "FALSE" to boolean
yaml.bool.handler.no <- function(val) {
  #message(paste("bool: ", val))
  if (val=="FALSE" | val=="false")
    return(FALSE)
  return(val)
}    

#' Reads a yaml file and returns as a list
#' @export
read.yaml = function(file=NULL, verbose=FALSE, quote.char = "__QUOTE__", text=NULL, catch.error = TRUE) {
  restore.point("read.yaml")
  if (is.null(text)) {
    str = suppressWarnings(paste(readLines(file), collapse = "\n"))
    file.str = paste0(" in ", file)
  } else {
    str = text
    file.str = ""
  }
  #message(paste("read.yam:", file))
  # Convert tabs to spaces
  str = gsub("\t","   ",str)  
  # Convert ":text" into ": text"
  str = gsub(":",": ",str)
  str = gsub(":  ",": ",str)
  str = gsub('"',quote.char,str,fixed=TRUE)  
  
  if (verbose)
    cat(str)

  
  yaml.string.handler = function(val) {
    gsub(quote.char,'"',val,fixed=TRUE)
  }
  
  tryCatch(
    yaml.load(str, handlers=list("bool#yes"=yaml.bool.handler.yes,"bool#no"=yaml.bool.handler.no, "str"=yaml.string.handler)),
    error = function(e) {
      str = paste0(as.character(e),file.str)
      stop(str, call.=FALSE)
    }
  )
  
  #suppressWarnings(yaml.load(str, handlers=list("bool#yes"=yaml.bool.handler.yes,"bool#no"=yaml.bool.handler.no)))
  
}

examples.read.yaml = function() {
  setwd("C:/libraries/ExpEconDB")
  init.ee("C:/libraries/ExpEconDB")
  
  fn = paste0(ee$struc.path,"/Games/NewLureOfAuthority.yaml")
  read.yaml(fn)
}
  

#' Prints list read from a yaml file
#' @export
print.yaml = function(obj) {
  if (class(obj)=="character") {
    cat(obj)
  } else {
    cat(as.yaml(obj))
  }
}

read.yaml.blocks = function(txt, add.txt =TRUE, omit.header=FALSE, tab.width=3) {
  restore.point("read.yaml.blocks")
  
  first.char = substring(txt,1,1)
  start = nchar(txt)>0 & first.char!=" " & first.char!="#" & first.char !="\t"
  start = which(start)
  name = str.left.of(txt[start],":")
  end = c(start[-1]-1,length(txt))
  
  if (!add.txt) {
   ret = quick.df(name=name, start.row=start, end.row=end)
  } else {
    if (!omit.header) {
      block.txt = sapply(seq_along(start), function(i) {
        paste0(txt[int.seq(start[i]+omit.header,end[i])], collapse="\n")
      })
    } else {
      i = 4
      block.txt = sapply(seq_along(start), function(i) {
        space.str = paste0(rep(" ", tab.width), collapse="")
        str = txt[int.seq(start[i]+omit.header,end[i])]
        left = ifelse(str.starts.with(str,space.str), tab.width+1,1)
        str = substring(str, left)
        paste0(str, collapse="\n")
      }) 
    }
    ret = quick.df(name=name, start.row=start, end.row=end, txt=block.txt)    
  }
  ret
}

