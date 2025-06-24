##' Create file names for multiple list elements
##' @param fn File name to provide stem for all file names
##' @param list.obj List of objects to provide names for
##' @keywords internal

nameMultipleFiles <- function(fn,list.obj,simplify=TRUE){
    submodel <- NULL
    
    length.num.char <- length(list.obj)
    if(!simplify || length.num.char>1){
        submodels <- sprintf(fmt=paste0("%0",length.num.char,"d"),1:length.num.char)
        ## pars[,submodel:=sprintf(fmt=paste0("%0",length.num.char,"d"),.GRP),by=bycols]
        fn <- fnAppend(fn,submodels)
        ## pars[,path.sim:=fnAppend(path.sim.0,submodel),by=.(ROW)]
        ##pars[,fn.sim:=basename(path.sim)]
        ##pars[,run.sim:=fnExtension(fn.sim,"")]
    }

    fn
}
