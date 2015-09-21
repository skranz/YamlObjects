# functions to define

# default.types.path
# default.types.file

#' Reads information about action types
load.one.type.file = function(file = default.types.file()) {
  restore.point("load.one.type.file")
  types = read.yaml(file)
  typeNames = names(types)
  for (i in seq_along(types)) {
    types[[i]]$name = typeNames[i]
    attr(types[[i]],"file") <- file
  }
  types  
}


#' Reads information about action types
load.yaml.types = load.types = function(type.files = paste0(types.path,"/",list.files(types.path)),  store.as.default=TRUE, high.prio.types=list(), low.prio.types=list(), types.path = default.types.path()) {
  restore.point("load.types")
  types = do.call("c",
                  c(high.prio.types,
                    lapply(type.files,load.one.type.file),
                    low.prio.types)
                  )
  # Remove duplicated, first type files have priority
  if (anyDuplicated(names(types))) {
    dup = duplicated(names(types))
    dup.names = names(types)[dup]
    message(paste("Removed duplicated types:", paste(dup.names,collapse=",")))
    types = types[!dup]
  }
  
  for (i in seq_along(types)) {
    types[[i]]$definedFields = names(types[[i]]$fields)
  }
  
  types = make.type.hierachy(types)
  if (store.as.default)
    yaml.objects.settings(types=types)
  
  for (i in seq_along(types)) {
    class(types[[i]]) = c("type","list")
  }
  types  
}

set.type = function(obj,type, typeName=type$name) {
  attr(obj,"typeName")<-typeName
  obj
}

get.typeName = function(obj) {
  attr(obj,"typeName")  
}


get.data.format = function(format.name) {
  get.types()[[format.name]]
}

#' Generates an empty list that has all fields of a type
make.object.from.type = function(typeName, types=get.types()) {
  type = types[[typeName]]
  obj = vector("list", length(type$fields))
  names(obj) = names(type$fields)
  attr(obj,"typeName") = typeName
  obj
} 

#' Converts a list to a type, by copying just the fields that the type knows of
convert.list.to.type = function(li, typeName, types=get.types()) {
  obj = make.object.from.type(typeName,types=types)
  copy = intersect(names(obj),names(li))
  obj[copy] = li[copy]
  obj
}


make.type.hierachy = function(types = get.types()) {
  restore.point("make.type.hierachy")
  names = names(types)
  
  # Types that have no parent types
  super = sapply(types, function(type) {
    is.null(type$parentType)
  })
  for (i in which(super)) {
    types[[i]]$superTypes = NULL
  }
  while (TRUE) {
    act = which(!super)
    did.change = FALSE
    for (i in act) {
      type = types[[i]]
      
      
      
      # A type can possibly have multiple parent types
      # we first inherit from earlier types
      parentTypes = types[[i]]$parentType
      
      if (any(!(parentTypes %in% names))) {
        missing = setdiff(parentTypes, names)
        stop("The type '", type$name, "'' has unknown parentTypes ", paste0("'",missing,"'", collapse=", "),".")
      }
      
      # All parentTypes must already have inherited 
      if (any(!super[parentTypes]))
        next
      
      did.change = TRUE
      for (parentType in parentTypes) {      
        parent = types[[parentType]]      
        type = inherit.from.parent.type(type,parent)
        
        parent$child_types = c(parent$child_types, type$name)
        parent$subTypes = c(parent$subTypes, type$name)
        for (st in parent$superTypes) {
          types[[st]]$subTypes = unique(c(types[[st]]$subTypes, type$name))    
        }
        types[[parent$name]] = parent
      }
      types[[i]] = type
      super[i]=TRUE
    }
    if (!did.change)
      break
  }
  return(types)
}

# A type inherits values from its parent type
inherit.from.parent.type = function(type, parent) {
  type$superTypes = unique(c(type$superTypes,parent$name,parent$superTypes))

  # Copy some type information from parent. Should probably copy more
  if (is.null(type$inheritFromParentObject)) {
    type$inheritFromParentObject = parent$inheritFromParentObject
  }

  if (is.null(type$expectValue)) {
    type$expectValue = parent$expectValue
  }
  if (is.null(type$defaultField)) {
    type$defaultField = parent$defaultField
  }
  if (is.null(type$otherFields)) {
    type$otherFields = parent$otherFields
  }
  if (is.null(type$atomic)) {
    type$atomic = parent$atomic
  }
  
  if (is.null(type$fields))
    type$fields = list()

  
  # Inherit all fields
  if (!is.false(type$inheritFields)) {
    for (f in names(parent$fields)) {    
      if (is.null(type$fields[[f]]))
        type$fields[[f]] = parent$fields[[f]]
        type$fields[[f]]$fieldInherited=TRUE
    }
  }
  type$inheritedFields = setdiff(names(type$fields), type$definedFields)
  
  type
}


#' Deduce the type of a field of a type
deduce.types.field.type = function(type,field) {
  
  
  # There is a specific type defined for the current field
  if (!is.null(type$fields[[field]]$type))
    return(type$fields[[field]]$type)
  
  # There is a general field type for all fields  
  if (!is.null(type$fieldType))
    return(type$fieldType)
  
  # The name of the field is its types
  if (!is.null(type$typeByFieldName))
    if (type$typeByFieldName)
      return(field)
  
  return("unknown")
}

#' Returns the type of a field. Uses the definition in parent.type to derive the field type
deduce.typeName = function(tree.obj,name,parent, parent.type = get.type(parent)) {
  
  #restore.point("get.field.typeName", deep.copy=FALSE)
   
  # The object has a field "_type"
  if ("_type" %in% names(tree.obj)) {
    return(tree.obj[["_type"]])
  }
  
  # There is a specific type defined for the current field
  if (!is.null(parent.type$fields[[name]]$type))
    return(parent.type$fields[[name]]$type)
  
  # There is a general field type for all fields  
  if (!is.null(parent.type$fieldType))
    return(parent.type$fieldType)
  
  # The name of the field is its types
  if (!is.null(parent.type$typeByFieldName))
    if (parent.type$typeByFieldName)
      return(name)
  
  # The field is not a list, therefore atomic
  if (!is.list(tree.obj))
    return("atomic")
  
  # An unknown list type
  return("unknown")
  stop("Undefined non-atomic type found!")
  
}

#' Vectorized on sub.names
is.subtype = function(sub.type, type, same.type.true = TRUE, types=get.types()) {
  if (is(sub.type[[1]],"type"))
    sub.type = sapply(sub.type, function(st) st$name)
  if (is(type,"type"))
    type = type$name
  
  if (length(type)==1) {
    if (same.type.true) {
      sub.types = c(type,types[[type]]$subTypes)
    } else {
      sub.types = types[[type]]$subTypes    
    }
  } else {
    sub.types = unique(unlist(lapply(type, function(t) types[[t]]$subTypes)))    
    if (same.type.true) 
      sub.types = c(type,sub.types)
  }
  sub.type %in% sub.types
}


get.type = function(obj,types=get.types(), typeName=attr(obj,"typeName")) {
  if (is.null(typeName))
    typeName = "unknown"
  
  types[[typeName]]
}
