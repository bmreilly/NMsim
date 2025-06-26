if(F){
    NMdataConf(as.fun="data.table")

    file.mod <- "testData/nonmem/xgxr032.mod"

    ext <- NMreadExt(file.mod)

    ext
    ext[par.type=="OMEGA",.(i,j,iblock,blocksize,FIX,value)]

    newfile <- "testOutput/xgxr032_replaceInits1.mod"

    NMreplaceInits(files=file.mod,inits=ext,newfile=newfile)

    readLines(newfile)
}
