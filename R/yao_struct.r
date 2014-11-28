#' Parsing and assessing experiment and game structures 



is.condition = function(obj,typeName=get.typeName(obj), types=get.types()) {
  return(typeName  == "if" | typeName=="else")
}


#' Retrieves for an object a structure consisting of an obj.li containing all descendants and a data frame df that contains some summary information
obj.to.struct = function(obj,name) {
  restore.point("obj.to.struct")
  st = table.tree(obj,name=name)
  
  obj.li = tt.obj.li(st)
  
  names = colnames(st)
  typeNames = sapply(obj.li, function(obj) {
    attr(obj,"typeName")
  })

  st$typeName = typeNames  
  setcolorder(st, c(names[1:2],"typeName",names[-(1:2)]))
  txt = lapply(obj.li, get.object.string)
  txt = substring(txt,1,40)
  st$content = txt
  

  #names(obj.li) = st$name
  #attr(st,"obj.li") <- obj.li  
  
  st 
}

get.object.string = function(obj) {
  if (is.list(obj) & length(obj)>0) {
    str = paste0("list(", length(obj),"): ",
                 paste0(names(obj),collapse=","))
  } else {
    if (length(obj)>1) {
      str = paste0(substring(class(obj)[1],1,3),"(", length(obj),"): ",
                   paste0(as.character(obj), collapse=","))
    } else {
      str = paste0(as.character(obj), collapse=",")
    }
  }
  str
}


#' Load a game, experiment or dataLink structure
load.struct = function(name, file=paste0(default.struct.path(),"/",name,".yaml"), typeName="game", just.obj = FALSE, types=get.types(),...) {
  restore.point("load.struct")
 
  struct.tree = read.yaml(file,...)
  
  #print.yaml(struct.tree)
  obj = tree.obj.to.struct.obj(tree.obj=struct.tree,name=name, typeName=typeName,types=types)
  
  if (just.obj)
    return(obj)
    
  struc = obj.to.struct(obj,name=name)
  return(struc)
}

#' Get all rows whose type is a subtype of supertype
get.subtype.rows = function(st, supertype) {
  which(is.subtype(st$typeName,supertype))
}

#' Gets the closest parent field that is a subtype of supertype 
get.parent.field.row = function(row, st, supertype) {
  #restore.point("get.parent.field.row")
  row = st$parent[row]
  while (row !=0) {
    if (is.subtype(st$typeName[row],supertype)) {
      return(row)
    }
    row = st$parent[row]    
  }
  return(NULL)
}

#' Gets all rows of objects that are structure parents of the current objects
get.parents.rows = function(row, st) {
  restore.point("get.parents.rows")
  pr = st$parent[row]    
  if (pr >0) {
    return(c(pr, get.parents.rows(pr,st)))
  } else {
    return(NULL)
  }
}

examples.get.conditions = function() {
  examples.yaml.objects.settings()
  file = "D:/libraries/XEconDB/Structures/Games/CostCoord.yaml"
  name="LureOfAuthority"
  gs = load.struct(name=name)
 
  get.conditions(130,gs)
  
  get.conditions(65,gs)
  
  get.conditions(96,gs)
  get.conditions(234,gs)
  get.conditions(228,gs)
  
}



