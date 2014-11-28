
#' Compute the depth of a nested list
list.depth <- function(this,thisdepth=0, add.vector=FALSE){
  #restore.point("list.depth")
  if(!is.list(this)){
    return(thisdepth + add.vector*(length(this)>1))
  }else{
    depths = unlist(lapply(this,list.depth,thisdepth=thisdepth+1, add.vector=add.vector))
    #restore.point("list.depth1")
    return(max(depths))    
  }
}

int.seq = function(from, to) {
  if (from > to)
    return(NULL)
  from:to
}

examples.splice = function() {
  v = "searchA"
  splice(summarise(dt, mean.v=mean(v, na.rm=TRUE)), v=v, eval=FALSE)
  
}

display= function (..., collapse = "\n", sep = "") 
{
    str = paste(paste(..., collapse = collapse, sep = sep), 
        "\n", sep = "")
    invisible(cat(str))
}

intersect.list <- function(li) {
  Reduce(intersect, li)
} 

robust.rbindlist = function(li) {
  restore.point("robust.rbindlist")
  
  cols = intersect.list(lapply(li, function(li) names(li)))
  ili = lapply(li, function(li) li[cols])
  rbindlist(ili)
}

na.as.zero = function(x) {
  x[is.na(x)] = 0
  x
}

rowProds = function(mat, cols = 1:NCOL(mat), default=NA) {
  if (length(cols) == 0)
    return(rep(default,NROW(mat)))
  
  if (is.numeric(cols)) {
    com = paste0("mat[,",cols,"]", collapse="*")
  } else {
    com = paste0("mat[,'",cols,"']", collapse="*")    
  }
  parse.eval(com)
  
}

rows_along = function(x) {
  if (NROW(x)==0)
    return(integer(0))
  return(1:NROW(x))
}


#' Like paste0 but returns an empty vector if some string is empty
sc = function(..., sep="", collapse=NULL) {
  str = list(...)
  restore.point("str.combine")
  len = sapply(str,length)
  if (any(len==0))
    return(vector("character",0))
  paste0(...,sep=sep,collapse=collapse)
}


any.field = function(li, field, val) {
  any(sapply(li, function(el) isTRUE(el[[field]] == val)))
}
all.fields = function(li, field, val) {
  all(sapply(li, function(el) isTRUE(el[[field]] == val)))
}


nlist = function (...) 
{
  li = list(...)
  li.names = names(li)
  names = unlist(as.list(match.call())[-1])
  if (!is.null(li.names)) {
    no.names = li.names == ""
    names(li)[no.names] = names[no.names]
  }
  else {
    names(li) = names
  }
  li
}

#' Like paste0 but returns an empty vector if some string is empty
str.combine = function(..., sep="", collapse=NULL) {
  str = list(...)
  restore.point("str.combine")
  len = sapply(str,length)
  if (any(len==0))
    return(vector("character",0))
  paste0(...,sep=sep,collapse=collapse)
}


remove.list.elements = function(li, remove=NULL) {
  #restore.point("remove.list.elements")
  if (length(remove)==0)
    return(li)
  if (is.character(remove)) {
    remove = which(names(li)==remove)
  }
  if (length(remove)==0)
    return(li)
  return(li[-remove])
}

#' Does an environment / list contain the objects named as names
contains = function(env,names, inherits=FALSE,...) {
  if (is.environment(env))
    return(sapply(names, exists, where=env, inherits=inherits, ...))
  
  return(names %in% names(env))
}

str.ends.with = function(txt,pattern) {
  substring(txt,nchar(txt)-nchar(pattern)+1,)==pattern
} 

#' Returns a string constisting of times spaces, vectorized over times
str.space = function(times, space=" ") {
  space.str = paste0(rep(space,max(times)),collapse="")
  substring(space.str,1,last=times)
}

example.str.space = function() {
  str.space(0:4)  
}

#' An operator that is true if the string str starts with the substring key
str.starts.with = function(str,key) {
  substring(str,1,nchar(key))==key
} 

is.true = function(val) {
  if (length(val)==0)
    return(FALSE)
  val[is.na(val)] = FALSE
  return(val)
}



is.false = function(val) {
  if (length(val)==0)
    return(FALSE)
  val[is.na(val)] = TRUE  
  return(!val)
}

path.parts = function(path,sep=".") {
  str.split(path,sep)
}


common.and.distinct.path.parts = function(opath, npath,sep=".") {
  restore.point("common.and.distinct.path.parts")
  op = str.split(opath,sep)[[1]]
  np = str.split(npath,sep)[[1]]
  len = length(np)
  if (len == 0)
    return(list(common=NULL,distinct=NULL))
  op = fill.vec(op,len,"")[1:len]
  
  common = op == np
  if (all(common)) 
    return(list(common=np,distinct=NULL))
    
  ind = which(!common)[1]-1
  if (ind==0)
    return(list(common=NULL,distinct=np))
  
  return(list(common=np[1:ind], distinct=np[(ind+1):len]))
}

examples.common.and.distinct.path.parts = function() {
  opath = "a.b.cd.e"
  npath = "a.b.e.f"
  common.and.distinct.path.parts(opath,npath)
  
  opath = ""
  npath = ".stages.intensityChoice.actions.intensityA"
  common.and.distinct.path.parts(opath,npath)
  
}

#' Cuts away early stuff from a tree path
cut.to.sub.tree.path = function(tree.path, after) {
  pos = str.locate.first(tree.path, after)
  substring(tree.path,pos[,2]+1)
}

#' Index a list tree with a tree path
at.tree.path = function(li, tree.path) {
  restore.point("get.from.tree.path")
  tree.path = str.replace(tree.path,".","$")
  code = paste0("list(",paste0("li",tree.path,collapse=","),")")
  return(eval(parse(text=code,srcfile=NULL)))
}


intersect.vector.list = function(li, init) {
  if (missing(init))
    return(Reduce(intersect,li))
  else
    return(Reduce(intersect,li,init))
  
}

#' Gets game variants that correspond to a tree path
variants.from.tree.path = function(tree.path) {
  restore.point("variants.from.tree.path")
  variants = str.extract.all(tree.path,"_if_variant_.*`")
  
  variants = lapply(variants, function(str) str.replace(str,"_if_variant_",""))
  variants = lapply(variants, function(str) str.replace(str,"`",""))
  variants = lapply(variants, function(str) str.split(str,"_"))
  variants = lapply(variants, intersect.vector.list)
  variants
}

#' Adapts whisker render for different whisker formats
custom.whisker.render = function(template,data,...,whiskers=c("<<",">>")) {
  library(whisker)
  if (!is.null(whiskers)) {
    template = str.replace(template,whiskers[1],"{{")
    template = str.replace(template,whiskers[2],"}}")
  }
  whisker.render(template,data,...)
}

#' Comverts a list of vectors into a matrix, shorter vectors will be filled up
vec.list.to.matrix = function(li,fill=NA, transpose=TRUE) {
  restore.point("vec.list.to.matrix")
  cols = max(sapply(li,length))
  ret = sapply(li, fill.vec, len=cols, fill=fill)
  if (transpose)
    return(t(ret))
  return(ret)
}

#' fill a vector up to a specified length with fill
fill.vec = function(vec,len=length(vec),fill=NA) {
  if (len == length(vec))
    return(vec)
  if (len > length(vec))
    return(c(vec,rep(fill,len-length(vec))))
  return(vec)
}

#' Returns all variable names in an R expression
var.in.expr.str = function(expr.str, envir=baseenv(), union = TRUE) {
  if (length(expr.str)>1) {
    vars = lapply(expr.str,var.in.expr.str,envir=envir)
    if (union) {
      return(unique(unlist(vars)))
    } else {
      return(vars)
    }
  } else {
    return(var.in.expr(expr.str=expr.str, envir = envir))    
  }
}

union.of.list = function(li) {
  unique(unlist(li))
}

#' Returns all variable names in an R expression
var.in.expr = function(expr,expr.str, envir=baseenv()) {
  library(codetools)
  if (!missing(expr.str)) {
    if (length(expr.str)==0)
      return(NULL)
    expr = parse(text=expr.str,srcfile=NULL)
  }
  f <- function() {} # a dummy function
  body(f) <- expr       # containing the expression as its body
  codetools::findGlobals(f,merge=FALSE)$variables
}

examples.var.in.expr = function() {
  var.in.expr(quote(x*y+2+sin(z)))
  var.in.expr(parse(text="x*y+2+sin(z)"))
  var.in.expr(expr.str = "x*y+2+sin(z)")
  
}


#' Names lists are used to recursively store order of columns
as.names.list = function(names) {
  li = vector("list",length(names))
  names(li) = names
  li
}


#' Names lists are used to recursively store order of columns
flatten.names.list = function(li,name="") {
  if (length(li)==0)
    return(name)
  sub.names = sapply(seq_along(li), function(i) flatten.names.list(li[[i]],names(li)[i]))
  if (nchar(name)>0) {
    ret.names = paste0(name,"_",sub.names)
    ret.names[nchar(sub.names)==0] = name
  } else {
    ret.names = sub.names
  }
  return(unlist(ret.names))
}

flatten.names.list.examples = function() {
  li = list(A=list(B1=list(),B2=list()),C=list(),list(D=list()))
  flatten.names.list(li)
  as.names.list(c("A","B","C"))
}
