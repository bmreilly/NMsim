##' Set variability parameters to zero
##' @param file.mod path to control stream to edit
##' @param lines control stream as lines. Use either file.sim or
##'     lines.sim.
##' @param section The sections (parameter types) to edit. Default is
##'     `c("OMEGA", "OMEGAP", "OMEGAPD")`.
##' @param newfile path and filename to new run. If missing or NULL,
##'     output is returned as a character vector rather than written.
##' @keywords internal

typicalize <- function(file.mod,lines,section,newfile){
    
    file.sim <- NULL
    FIX <- NULL
    init <- NULL
    par.type <- NULL
    

    if(missing(file.mod)) file.mod <- NULL
    if(missing(lines)) lines <- NULL
    if(missing(section)) section <- NULL
    if(missing(newfile)) newfile <- NULL
    
    lines <- NMdata:::getLines(file=file.mod,lines=lines,simplify=TRUE)
    
    if(is.null(section)){
        section <- c("OMEGA","OMEGAP","OMEGAPD")
    }

    inits <- NMreadInits(lines=lines,as.fun="data.table",section=section)

    valc.0 <- 1e-30
    
    inits[par.type%in%section,init:=valc.0]
    inits[par.type%in%section,FIX:=1]

    mod.new <- NMwriteInits(lines=lines,inits.tab=inits,update=FALSE)

    ## write to file if requested
    if(is.null(newfile)){
        return(mod.new)            
    }

    writeTextFile(lines=mod.new,file=newfile)

    return(file.sim)    

}
