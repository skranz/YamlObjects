
#' Gets name of an object
get.name = function(obj) {
  attr(obj,"name")
}

set.obj.name = function(obj,name) {
  attr(obj,"name") <- name
  obj
} 

#' Generate a new object of the specified type
new.obj = function(type=get.types()[[typeName]],name,values = NULL,typeName=NULL) {
  restore.point("new.obj")
  if (!is.null(values)) {
    obj = values
  } else {
    obj = list()
  }
  obj = set.type(obj,type=type)
  obj = set.obj.name(obj,name=name)
  
  obj = set.defaults(obj,type=type)
  obj
}



has._if.children = function(obj) {
  if (length(obj)==0)
    return(FALSE)
  if (!is.subtype(get.type(obj[[1]])$name,"_vectorElement"))
    return(FALSE)
  if (!is.subtype(get.type(obj[[1]][[1]])$name,"_if"))
    return(FALSE)
  return(TRUE)
}

# An object that has a vector with if conditions as  children
# will be converted in a vector of if conditions with a corrresponding whole object as children
move._if.upwards = function(obj) {
  restore.point("move._if.upwards")
  
  if (length(obj)==0)
    return(obj)
  if (!is.subtype(get.type(obj[[1]])$name,"_vectorElement"))
    return(obj)
  
  i = 1
  for (i in 1:length(obj)) {
    nobj = new.obj(type=get.type(obj),name=get.name(obj),values = obj[[i]][-1])
    names = c(names(obj[[i]])[1], get.name(obj))
    obj[[i]][] = list(obj[[i]][[1]],nobj)
    names(obj[[i]]) = names
  }
  return(obj)
}

obj.to.yaml = function(obj,...) {
  as.yaml(obj)  
}

yaml.to.obj = function(yaml, name=NULL,typeName=NULL, parent=NULL, parent.for.type=parent) {
  tree.obj =  read.yaml(text=yaml)
  if (is.null(name)) {
    if (length(tree.obj)>1 | is.null(names(tree.obj))) {
      stop("if you call yaml.to.obj without name, your yaml code must have exactly one named top level element.")
    }
    name = names(tree.obj)
    tree.obj = tree.obj[[1]]
  }
  obj = tree.obj.to.struct.obj(tree.obj, name=name, typeName=typeName, parent=parent, parent.for.type=parent)
  obj
} 


#' Recursively parse a structure object of an experiment or game
tree.obj.to.struct.obj = function(tree.obj, name, typeName=NULL,parent=NULL,parent.for.type =parent,types=get.types(), tree.path="") {
  restore.point("tree.obj.to.struct.obj")
  if (str.starts.with(name,"if")) {
    restore.point("parse.struct.tree.special")
  }

  if (substring(name,1,1)=="_") {
    tree.path = paste0(tree.path,".`",name,"`")    
  } else {
    tree.path = paste0(tree.path,".",name)
  }
  
  obj = tree.obj
  if (is.null(obj))
    obj =list()
  
  if (is.null(typeName)) {
    typeName = deduce.typeName(tree.obj=tree.obj, name=name, parent=parent.for.type)
  }  
      
  if (!typeName %in% names(types)) {
    stop(paste0(as.yaml(parent),"\nType ", typeName, " is not specified in types! Cannot parse the tree object. Check for spelling errors or augment types.yaml for that new object type.\n"))
  }
  
  type = types[[typeName]]
  expectValue = type$expectValue
  if (is.null(expectValue))
    expectValue=FALSE
  
  # Object is not a list.
  # Convert it to a list and assign the value to the defaultField 
  if (!is.list(tree.obj) & !is.true(type$atomic)) {    
    if (! "defaultField" %in% names(type)) {
      message(print.yaml(tree.obj))
      #restore.point("error 724z7h")
      stop(paste0("\nNo default field specified for non-atomic type ", typeName, ", but needed for the object.\n",as.yaml(parent),"\nNo default field specified for non-atomic type ", typeName, ", but needed for the object"))
    }
    new.obj = list()
    new.obj[[type$defaultField]]=obj
    obj = new.obj
  }    
  obj = set.type(obj,type)
  attr(obj,"name") = name
    
  obj = init.special.object(obj,typeName=typeName)
  if (is.false(attr(obj,"parse.fields")))
    return(obj)
  if (is.list(obj) & length(obj)>0) {
    #restore.point("parse.struct.tree2a", deep.copy=FALSE)    
    obj = set.defaults(obj,type)
    
    # Parse children objects
    field.names = names(obj)

    new.parent = obj
    new.parent.for.type = obj
    new.typeName = NULL
    
    # A yaml vector that is not an atomic type used in _if constructions
    if (is.null(field.names) & !is.true(type$atomic)) {
      vector.struct = TRUE
      field.names = paste0(name,"_",seq_along(obj))
      names(obj) = field.names
      new.typeName = "_vectorElement"
    # A condition
    } else if (str.starts.with(type$name,"_") | is.condition(obj)) {
      new.parent.for.type = parent.for.type      
    }
    
    for (i in seq_along(field.names)) {
      ret  <- tree.obj.to.struct.obj(tree.obj=obj[[i]], name=field.names[i], parent=new.parent,parent.for.type=new.parent.for.type, types=types,tree.path=tree.path,typeName=new.typeName)
      obj[[i]] = ret
    }
  }
  #names(obj[[1]])
  return(obj)
}

init.special.object = function(obj,typeName,...) {
  fun = ya.glob$init.special.obj.fun
  if (!is.null(fun))
    return(fun(obj,typeName,...))
  return(obj)
}

