##' Conveniently write text lines to file
##'
##' @param lines the character lines to write
##' @param file The file name path to write to
##' @param simplify Passed to `nameMultipleFiles()`
##' @return File paths as character strings
##' @keywords internal

writeTextFile <- function(lines,file,simplify=TRUE){
    
    
    if(is.list(lines)){
        
        all.files <- nameMultipleFiles(file,lines,simplify=simplify)
        res <- lapply(1:length(all.files),function(x){
            writeTextFile(lines=lines[[x]],file=all.files[x])
        })
        return(unlist(res))
    }

    con <- file(file, "wb")
    writeLines(lines, con = con)
    close(con)
    return(file)
}
