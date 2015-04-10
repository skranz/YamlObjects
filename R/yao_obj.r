
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
  
  obj = set.object.defaults(obj,type=type)
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

yaml.to.obj = function(yaml, name=NULL,typeName=NULL, parent=NULL) {
  tree.obj =  read.yaml(text=yaml)
  if (is.null(name)) {
    if (length(tree.obj)>1 | is.null(names(tree.obj))) {
      stop("if you call yaml.to.obj without name, your yaml code must have exactly one named top level element.")
    }
    name = names(tree.obj)
    tree.obj = tree.obj[[1]]
  }
  obj = tree.obj.to.struct.obj(tree.obj, name=name, typeName=typeName, parent=parent)
  obj
} 


#' Recursively parse a structure object of an experiment or game
tree.obj.to.struct.obj = function(tree.obj, name, typeName=NULL,parent=NULL,types=get.types()) {
  restore.point("tree.obj.to.struct.obj")
 
  obj = tree.obj
  if (is.null(obj))
    obj =list()
  
  if (is.null(typeName)) {
    typeName = deduce.typeName(tree.obj=tree.obj, name=name, parent=parent)
  }  
  typeName = get.special.type(tree.obj = tree.obj,name = name,typeName = typeName, parent=parent)
      
  if (!typeName %in% names(types)) {
    stop(paste0("The type '", typeName, "' is not specified in types! Cannot parse the tree object. Check for spelling errors or augment types.yaml for that new object type.\n:\n\n", as.yaml(parent)))
  }
  
  type = types[[typeName]]
  expectValue = type$expectValue
  if (is.null(expectValue))
    expectValue=FALSE
  
  # Object is not a list.
  # Convert it to a list and assign the value to the defaultField 
  if (!is.list(tree.obj) & !is.true(type$atomic)) {    
    new.obj = list()
    if (! "defaultField" %in% names(type)) {
      #message(print.yaml(tree.obj))
      #restore.point("error 724z7h")
      #stop(paste0("\nNo default field specified for non-atomic type ", typeName, ", but needed for the object.\n",as.yaml(parent),"\nNo default field specified for non-atomic type ", typeName, ", but needed for the object"))
    } else {
      new.obj[[type$defaultField]]=obj
      obj = new.obj
    }
  }    
  obj = set.type(obj,type)
  attr(obj,"name") = name
    
  
  if (is.false(attr(obj,"parse.fields")))
    return(obj)
  if (is.list(obj) & length(obj)>0) {
    #restore.point("parse.struct.tree2a", deep.copy=FALSE)  
    #if (identical(get.name(obj),"card"))
    #  stop()

    
    obj = inherit.from.parent.object(obj, parent, type=type)
    obj = set.object.defaults(obj,type)
    obj = transform.special.object(obj,typeName=typeName, parent=parent)

    
    # Parse children objects
    field.names = names(obj)
    
    # Remove # from object name
    field.names = str.left.of(field.names,"#")
    names(obj) = field.names
    
    
    new.parent = obj
    new.typeName = NULL
    
    # A yaml vector that is not an atomic type
    if (is.null(field.names) & !is.true(type$atomic)) {
      vector.struct = TRUE
      undef = paste0("UNDEFINED_VECTOR_ELEMENT_",seq_along(obj))
      if (!is.null(type$fieldType)) {
        field.names = paste0(type$fieldType,"_",seq_along(obj))
      } else if (isTRUE(type$allowAsVector)) {
        field.names = c(names(type$fields),undef)[seq_along(obj)]
      } else {
        field.names = undef
      }
      names(obj) = field.names
    }
    
    for (i in seq_along(field.names)) {
      ret  <- tree.obj.to.struct.obj(tree.obj=obj[[i]], name=field.names[i], parent=new.parent, types=types,typeName=new.typeName)
      obj[[i]] = ret
    }
  }
  
  obj = post.transform.special.object(obj,typeName=typeName, parent=parent)

  #names(obj[[1]])
  return(obj)
}

transform.special.object = function(obj,typeName=get.typeName(obj),parent=NULL,...) {
  fun = ya.glob$transform.special.object
  if (!is.null(fun))
    return(fun(obj,typeName,parent,...))
  return(obj)
}

#' transform obj after children have been generated
post.transform.special.object = function(obj,typeName=get.typeName(obj),parent=NULL,...) {
  fun = ya.glob$post.transform.special.object
  if (!is.null(fun))
    return(fun(obj,typeName,parent,...))
  return(obj)
}


inherit.from.parent.object = function(obj, parent, type=get.type(obj)) {
  fields = type$inheritFromObject
  if (is.null(fields)) {
    return(obj)
  }
  # don't overwrite existing fields in obj
  fields = setdiff(fields, names(obj))
  # only copy fields that exist in parent
  fields = intersect(fields, names(parent))
  obj[fields] = parent[fields]
  obj
}


#' Assign default values for non-specified fields of a game object (action etc...) 
set.object.defaults = function(obj,type,fields=type$fields) {
  restore.point("set.object.defaults")
  
  # deal with a vector for default field
  if (!is.null(type$defaultField) & is.null(names(obj)) & length(obj)>0) {
    new.obj = list()
    field.obj = obj
    attributes(field.obj) = NULL
    attributes(new.obj) = attributes(obj)
    new.obj[[type$defaultField]] = field.obj
    obj = new.obj
  }

  
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

