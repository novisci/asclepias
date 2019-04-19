interval <- setClass("interval", slots = c(a = "integer", b = "integer"))

setMethod("as.character", "interval", function(x) {
  sprintf("(%s, %s)", slot(x, "a"), slot(x, "b"))
})

setMethod("show", "interval", function(object) {
  cat(as.character(object))
})
interval_pair <- setClass("interval_pair", slots = c(a = "interval", b = "interval"))

interval_pair_o <- setClass(
  "interval_pair_o", 
  contains = "interval_pair",
  validity = function(object){
    `<`(slot(object, "a"), slot(object, "b"))
  })

setMethod("show", "interval_pair", function(object) {
  cat(sprintf("(%s, %s)", as.character(slot(object, "a")), as.character(slot(object, "b"))))
})

setMethod("show", "interval_pair_o", function(object) {
  cat(sprintf("<%s, %s>", as.character(slot(object, "a")), as.character(slot(object, "b"))))
})
setGeneric("s", function(object) standardGeneric("s") )
setMethod("s", "interval", function(object) slot(object, "a"))
setGeneric("e", function(object) standardGeneric("e") )
setMethod("e", "interval", function(object) slot(object, "b"))
setGeneric("ennummerate", function(object) standardGeneric("ennummerate") )
setMethod("ennummerate", "interval", function(object) s(object):e(object))
setMethod("length", "interval", function(x) e(x) - s(x))
setGeneric("flipl", function(object, ...) standardGeneric("flipl") )
setMethod("flipl", "interval", 
          function(object) interval(a = s(object) - length(object), b = s(object)))
setGeneric("flipr", function(object, ...) standardGeneric("flipr") )
setMethod("flipr", "interval", 
          function(object) interval(a = e(object), b = e(object) + length(object)))
setGeneric("shift", function(object, ...) standardGeneric("shift") )
setMethod("shift", "interval", function(object, c) interval(a = s(object) + c, b = e(object) + c))
setGeneric("expandl", function(object, ...) standardGeneric("expandl") )
setMethod("expandl", "interval", function(object, c) interval(a = s(object) - c, b = e(object)))
setGeneric("expandr", function(object, ...) standardGeneric("expandr") )
setMethod("expandr", "interval", function(object, c) interval(a = s(object), b = e(object) + c))
setGeneric("expand", function(object, ...) standardGeneric("expand") )
setMethod("expand", "interval", function(object, c)  expandr(expandl(object, c), c))
# setGeneric("expand", function(object, ...) standardGeneric("expand") )
setMethod(
  `==`, 
  c("interval", "interval"), 
  function(e1, e2) { 
    if(s(e1) == s(e2) && e(e1) == e(e2)) TRUE
    else FALSE
  })
setMethod(
  `<`, 
  c("interval", "interval"), 
  function(e1, e2) { 
    if(s(e1) < s(e2) | ((s(e1) == s(e2)) & e(e1) < e(e2))) TRUE
    else FALSE
  })

setGeneric("order", function(x) standardGeneric("order") )
setMethod(
  "order", 
  "interval_pair",
  function(x) {
    if(`<`(slot(x, "a"), slot(x, "b"))) {
      interval_pair_o(a = slot(x, "a"), b = slot(x, "b"))  
    } else {
      interval_pair_o(a = slot(x, "b"), b = slot(x, "a"))  
    }
  })
setGeneric("overlaps", function(x) standardGeneric("overlaps") )
setMethod(
  "overlaps", 
  "interval_pair_o", 
  function(x) { 
    s(slot(x, "b")) <= e(slot(x, "a")) 
  })


suit  <- setClass(
  "suit", contains = "character", 
  validity = function(object) { object %in% c("spade", "heart", "club", "diamond")})

setMethod(
  "print", "suit",
  function(x){
    switch(x,
           "spade"   = "\U2660",
           "club"    = "\U2663",
           "heart"   = "\U2665",
           "diamond" = "\U2666"
    )
  }
)

setMethod("show", "suit", function(object) cat(print(object)))
setGeneric("suit", function(x){standardGeneric("suit")})
setMethod("suit", "character", function(x) new("suit", x))
setGeneric("value", function(x){standardGeneric("value")})
context <- setClass("context", slots = c(suit = "suit", value = "character"))
setMethod("as.character", "context", function(x) {
  sprintf("{%s, %s}", print(slot(x, "suit")), slot(x, "value"))
})
setMethod("show", "context", function(object) {
  cat(as.character(object))
})

setMethod("suit", "context", function(x) x@suit)
setMethod("value", "context", function(x) x@value)

event <- setClass("event", slots = c("interval" = "interval", "context" = "context"))
setMethod("as.character", "event", function(x){
  sprintf("{%s, %s}",  as.character(slot(x, "interval")), as.character(slot(x, "context")))
})
setMethod("show", "event", function(object) { cat(as.character(object)) })
setGeneric("as.event", function(x) standardGeneric("as.event"))
setMethod("as.event", "list", function(x) new("event", interval = x[[1]], context = x[[2]]))