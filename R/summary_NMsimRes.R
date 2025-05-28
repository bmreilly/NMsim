summary.NMsimRes <- function(object,...){
    
    dots <- list(...)
    defaults <- list(
        evid=TRUE
    )
    ## prioritize dots
    dots <- modifyList(defaults,dots)
    
    if(!inherits(object,"NMsimRes")){
        stop("object is not an NMsimRes object.")
    }


    cl.obj <- class(object)
    cl.obj <- setdiff(cl.obj,"NMsimRes")
    if(length(cl.obj)>1) cl.obj <- setdiff(cl.obj,"data.frame")

    message(sprintf("NMsim results in a %s",cl.obj))
    
    ## drop class without editing by ref
    res <- copy(object)
    unNMsimRes(res)
    ## the rest will be done on data.table
    if(!is.data.table(res)) res <- as.data.table(res)

    ## add NMREP,
    if("NMREP"%in%colnames(res)) res[,NMREP:=1]
    fun.print.NMREP <- function(SD){
        if("NMREP"%in%colnames(SD)) return(SD[,uniqueN(NMREP)])
        return(0)
    }

    res.main <- res[,.(N.rows=nrow(.SD),N.cols=ncol(.SD),"uniqueN(model)"=uniqueN(model),"uniqueN(model.sim)"=uniqueN(model.sim),"uniqueN(NMREP)"=fun.print.NMREP(.SD))] 

    
    ## IDs - number
    res.id.evid <- NULL 
    res.evid.miss <- NULL

    res.id <- res[,.(RepID=.N),by=ID][,.(N.ID=uniqueN(ID),median.Rep=median(RepID),min.Rep=min(RepID),max.Rep=max(RepID))]
    if( dots$evid && "EVID"%in%colnames(res)){
        res.id2 <- res[,.(RepID=.N),by=c("ID","EVID")]

        res.id.evid <- res.id2[
           ,.(N.ID=uniqueN(ID),median.Rep=median(RepID),min.Rep=min(RepID),max.Rep=max(RepID)),
            keyby=c("EVID")]

        res.id2[,evid:=reorder(paste0("EVID=",EVID),EVID)]
        id.nEVID <- dcast(res.id2,ID~evid,value.var="RepID",fill=0)
        id.nEVID.miss <- id.nEVID[rowSums(id.nEVID[,!"ID",with=FALSE]==0,na.rm=TRUE)>0]
        ##
        
        if(nrow(id.nEVID.miss)){
            cols.not.id <- setdiff(colnames(id.nEVID.miss),"ID")
            id.nEVID.miss[,grp0:=apply(.SD==0,1,function(z)paste(which(z)+1,  collapse="_")),.SDcols=cols.not.id  ]
            res.evid.miss <- id.nEVID.miss[,lapply(.SD,function(x){
                fifelse(all(unique(x))==0,"0",sprintf("%-s - %s",min(x),max(x)))
            }),
            .SDcols=cols.not.id,
            by=grp0]
        }
    }

    

    res <- list(main=res.main,
                id=res.id,
                id.evid=res.id.evid,
                evid.miss=res.evid.miss)
    
    setattr(res,"class",c("summary_NMsimRes",class(res)))

    res

}


##' print method for NMsimRes summaries
##' @param x The summary object to be printed. See ?summary.NMsimRes
##' @param ... Arguments passed to other print methods.
##' @return NULL (invisibly)
##' @import data.table
##' @export
print.summary_NMsimRes <- function(x,...){
    
    dots <- list(...)
    message("Number of rows, columns, source models (file.mod), NMsim model runs, and subproblems")
    message_dt(x$main)
    message("Number of distinct ID's, with median, min and max of rows with each ID.")
    message_dt(x$id)

    if( !is.null(x$id.evid )){
        message_dt(x$id.evid)
    }
    if( !is.null(x$evid.miss )){
        message_dt(x$evid.miss)
    }
    
    return(invisible(NULL))
}
