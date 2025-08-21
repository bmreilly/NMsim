context("typicalize")

test_that("basic",{

    fileRef <- "testReference/typicalize_01.rds"

    file.mod="testData/nonmem/xgxr011.mod"
    lines.in <- readLines(file.mod,warn=FALSE)
    ## res.all <- NMsim:::typicalize(lines=lines.in,file.mod=file.mod,return.text = TRUE)
    res.all <- NMsim:::typicalize(lines=lines.in)
    res <- NMreadSection(lines=res.all,section="OMEGA")

    expect_equal_to_reference(res,fileRef)
    if(F){
        res
        readRDS(fileRef)
    }


})



if(FALSE){
### NMriteInits for typicalize
    library(devtools)

    load_all("~/wdirs/NMdata",export_all = F)
    load_all("~/wdirs/NMsim",export_all = F)


    file.mod="testData/nonmem/xgxr011.mod"
    file.mod="testData/nonmem/xgxr032_sd1_NWPRI.mod"

    newfile <- "testOutput/typicalize1.mod"

    res <- NMsim:::typicalize(file.mod)
    
    inits <- NMreadInits(file.mod,as.fun="data.table",section="all")

    valc.0 <- 1e-30
    sections.typ <- c("OMEGA","OMEGAP","OMEGAPD")
    inits[par.type%in%sections.typ,init:=valc.0]
    inits[par.type%in%sections.typ,FIX:=1]


    inits[par.type=="OMEGAP",init:=0]
    inits[par.type=="OMEGAPD",init:=0]

    inits



    load_all("~/wdirs/NMdata",export_all = F)
    load_all("~/wdirs/NMsim",export_all = F)

    inits



#### not working with OMEGAP. Seems to work with OMEGA and OMEGAPD.
    ## NMreadSection(file.mod)
    NMwriteInits(file.mod,inits.tab=inits,update=FALSE,newfile=newfile)
    
    NMwriteInits(file.mod,inits.tab=inits[par.type%in%c("THETA","OMEGA")],update=FALSE,newfile=newfile)


    readLines(newfile)
}



