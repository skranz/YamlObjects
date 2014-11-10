#' Parsing and assessing experiment and game structures 



is.condition = function(obj,typeName=get.typeName(obj), types=get.types()) {
  return(typeName  == "if" | typeName=="else")
}


#' Assign default values for non-specified fields of a game object (action etc...) 
set.defaults = function(obj,type,fields=type$fields) {
  restore.point("setDefaults")
  for (na in names(fields)) {
    field.typeName = deduce.types.field.type(type,na)
    field.type = get.type(typeName=field.typeName)
    
    if (length(fields[[na]])==0)
      next
    if (length(fields[[na]]$default)==0)
      next
    
    # An atomic type (unknown will be treated like atomic)
    if (field.typeName == "unknown" |
        is.subtype(field.typeName,"atomic") |
        is.true(field.type$atomic))
    {
      if ("default" %in% names(fields[[na]]) & is.null(obj[[na]])) {
        obj[[na]]=fields[[na]]$default  
      }
    # Default is an non-atomic type
    } else {

      # obj has not specified the default field
      # generate a new field object
      if (length(obj[[na]])==0) {
        restore.point("setDefaults.newobj")
        
        field.obj = new.obj(type=field.type,name=na,values = type$fields[[na]]$default)
        obj[[na]] = field.obj
      
      # obj already has the default field, just copy the field values
      } else {
        if (is.list(type$fields[[na]]$default)) {          
          for (subna in type$fields[[na]]$default) {
            if (is.null(obj[[na]][[subna]])) {
              obj[[na]][[subna]] = type$fields[[na]]$default[[subna]]
            }
          }
        } else {
          field.type = get.type(field.typeName)
          subna = field.type$defaultField
          if (is.null(obj[[na]][[subna]])) {
            obj[[na]][[subna]] = type$fields[[na]]$default[[subna]]
          }          
        }
      }
    }
  }
  return(obj)
}

#' Retrieves for an object a structure consisting of an obj.li containing all descendants and a data frame df that contains some summary information
obj.to.struct = function(obj,name) {
  #restore.point("obj.to.struct")
  st = table.tree(obj,name=name)
  
  # TO DO: set typeName as attribute for each object in obj.li
  st
}

#' Load a game, experiment or dataLink structure
load.struct = function(name, file=paste0(default.struct.path(),"/",name,".yaml"), typeName="game", just.obj = FALSE, types=get.types()) {
  restore.point("load.struct")
  #file = "C:/research/eedb/Structures/DataLinks/UGPersuAp_DataLink.yaml"
  
  struct.tree = read.yaml(file)
  
  #print.yaml(struct.tree)
  obj = tree.obj.to.struct.obj(tree.obj=struct.tree,name=name, typeName=typeName,types=types)
  
  if (just.obj)
    return(obj)
    
  struc = obj.to.struct(obj,name=name)
  return(struc)
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

#' Gets all _if conditions of children nodes
get.all.children.if.cond = function(row,st) {
  restore.point("get.all.children.if.cond")
  pobj = tt.object(st,row)
  cond = NULL
  for (obj in pobj) {
    type = get.type(obj)
    if (type$name == "if") {
      name = get.name(obj)
      new.cond = substring(name,4)  
      cond = c(cond,new.cond)  
    } 
  }
  return(cond)
}

#' Gets all conditions from _if, _if_variant and _switch of an object as a character string 
get.conditions = function(row,st) {
  restore.point("get.conditions")
  parents = get.parents.rows(row,st)

  cond = NULL
  pind = 1
  for (pind in seq_along(parents)) {
    pr = parents[pind]
    obj = tt.object(st,pr)
    type = get.type(obj)
    name = get.name(obj)
    if (type$name == "if") {
      new.cond = substring(name,4)  
      cond = c(cond,new.cond)
    } else if (type$name == "else") {
      gpr = st$parent[pr]
      ifconds = get.all.children.if.cond(gpr,st)
      cond = c(cond,paste0("!(", paste0("(", ifconds,")", collapse = "|"),")")) 
    }
  }
  if (length(cond) == 0)
    return(NULL)
  
  return(paste0("(",cond,")", collapse="&"))
}

combine.conditions = function(...) {
  conds = list(...)
  
  is.cond = sapply(conds, function(cond) length(cond)>0)
  conds = conds[is.cond]
  if (length(conds)==0)
    return(NULL)
  
  if (length(conds)==1)
    return(conds[[1]])
  
  return(paste0("(",conds,")", collapse="&"))  
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

