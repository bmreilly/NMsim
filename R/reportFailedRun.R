

### todo: look for lines.lst and recursively for nmtran_error.txt
reportFailedRun <- function(lst){
    ## path.err.nmtran <- list.files()
    lines.lst <- readLines(lst)
    nlines <- length(lines.lst)
    message(sprintf("%s:\n----------------------------------------------\n%s\n----------------------------------------------",simplePath(lst),paste(lines.lst[(nlines-25):nlines],collapse="\n")))

    ## if(any(grepl("THERE ARE ERROR MESSAGES IN FILE PRDERR",lines.lst))){
    ##     lines.prderr <- readLines()
    ## }

}
