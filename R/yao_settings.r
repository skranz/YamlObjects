
ya.glob <- new.env()


examples.yaml.objects.settings = function(){
  yaml.objects.settings(
    default.types.path = "D:/libraries/XEconDB/Types",
    default.types.file = "D:/libraries/XEconDB/Types/types.yaml",
    default.struct.path = "D:/libraries/XEconDB/Structures/Games",
    transform.special.obj = function(obj, typeName,parent,...) {
      return(obj)
    },
    get.special.type = function(obj, name, typeName, parent,...) {
      return(typeName)
    }
  )
  types=load.types()
  yaml.objects.settings(types=types) 
}


yaml.objects.examples = function() {
  yaml.objects.settings(
    default.types.path = "D:/libraries/XEconDB/Types",
    default.types.file = "D:/libraries/XEconDB/Types/types.yaml",
    default.struct.path = "D:/libraries/XEconDB/Structures/Games",
    init.special.obj.fun = function(obj, typeName,...) {
      return(obj)
    }
  )
  types=load.types()
  tt = table.tree(types)
  yaml.objects.settings(types=types)

  file = "D:/libraries/XEconDB/Structures/Games/CostCoord.yaml"
  name="CostCoord"
  st = load.struct(name=name)
  df = st$df
  
  yo = read.yaml(file)
  tt = table.tree(yo)
  oo = tree.obj.to.struct.obj(yo,name,typeName="game")
  tt = table.tree(oo)
  fun = obj.table.tree.extras
  
  to = tt.add.extra.cols(tt,fun)
  tt.object(to,1)
}

obj.table.tree.extras = function(obj) {
  type = get.type(obj)
  
  
  expectValue = type$expectValue
  if (is.null(expectValue))
    expectValue=FALSE
  

  typeName = type$name
  data.table(typeName=typeName, expectValue=expectValue)
  
}


# default.types.path, default.types.file, get.types.fun, types, init.special.obj.fun
# default.struct.path
yaml.objects.settings = function(...) {
  li = list(...)
  copy.into.env(dest=ya.glob, source=li)
}


init.special.obj = function(obj,typeName,parent,...) {
  if (!is.null(ya.glob$init.special.obj.fun)) {
    return(ya.glob$init.special.obj(obj=obj, typeName,parent=parent,...))
  }
  return(obj)
}
  
get.special.type = function(tree.obj,name, typeName, parent,...) {
  if (!is.null(ya.glob$get.special.type)) {
    return(ya.glob$get.special.type(tree.obj=tree.obj, name=name, typeName=typeName, parent=parent,...))
  }
  return(typeName)
}


default.struct.path = function(...) {
  ya.glob$default.struct.path
}


default.types.path = function(...) {
  ya.glob$default.types.path
}

default.types.file = function(...) {
  ya.glob$default.types.file
}

get.types = function(...) {
  ya.glob$get.types.fun(...)
}

get.types = function() {
  types = ya.glob$types
  if (is.null(types)) {
    if (!is.null(ya.glob$get.types.fun))
      return(ya.glob$get.types.fun(...))
    types = load.types()
  }
  return(types)
}

