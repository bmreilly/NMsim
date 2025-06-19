
deleteTmpDirs <- function(dir,methods=c("nmsim","psn"),recursive=FALSE){
    library(data.table)
    
    deleteTmpType <- function(pattern){
        
        dirsToDelete <- list.files(path=dir,pattern=pattern,full.names=TRUE,recursive=recursive,include.dirs=T)
        unlink(dirsToDelete,recursive=TRUE)
    }


    dt.methods <- data.table(method=c("nmsim","psn"),
                             pattern=c(".*\\_dir[0-9]+$",
                                       "_*\\.dir[0-9]+$")
                             )
    dt.methods[
       ,apply:=method%in%methods][
       ,row:=.I]

    dt.methods[apply==TRUE,{
        deleteTmpType(pattern=pattern)
        NULL
        }
    ,by=row]


    
}
