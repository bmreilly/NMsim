dir.NMsim <- "/data/home/philipde/wdirs/NMsim/R"
dir.NMData <- "/data/home/philipde/wdirs/NMsim/R"

dt.pkgs <- data.table(pkg=cc(NMsim,NMdata))[
   ,dir:=sprintf("/data/home/philipde/wdirs/%s/R",pkg)][]

dt.files <- dt.pkgs[,.(file=list.files(dir)),by=pkg]
dt.files <- dt.files[file!="zzz.R"]
dt.files[,N:=.N,by=.(file)]

dt.files <- mergeCheck(dt.files,dt.pkgs,by="pkg")
dt.files[,fpath:=file.path(dir,file)]

dtf.w <- dt.files[N>1] |>
    dcast(file~pkg,value.var="fpath")

dtf.w

res.list <- lapply(1:nrow(dtf.w),function(row){
    diffr(dtf.w[row,NMsim],dtf.w[row,NMdata])
})

## Maybe NMwriteSection belongs better in NMsim?

## NMreadInits. Belongs in NMdata. NMdata 0.2.2 required.
## NMwriteFilters. Belongs in NMsim (internal). Dropped in NMdata 0.2.1.
## NMwriteInits. Belongs in NMsim (internal). Dropped in NMdata 0.2.1.
## NMwriteInitsOne. Belongs in NMsim (internal). Dropped in NMdata 0.2.1.
## addBlocks. Belongs in NMdata (internal). Requires NMdata 0.2.1.
## addParType+addParameter. Belongs in NMdata (internal). Requires NMdata 0.2.1.
## dcastSe. Belongs in NMdata (internal). Requires NMdata 0.2.1.
## message_dt. Belongs in NMdata (internal). Requires NMdata 0.2.1.
## fnAppend. Belongs in NMdata (internal). Requires NMdata 0.2.1. (collapse arg)
## NMreadSection. Belongs in NMdata. NMdata 0.2.1 required.
## NMextractText. Belongs in NMdata. NMdata 0.2.1 required.
## NMreadSizes. Belongs in NMdata. NMdata 0.2.1 required. NMreadSizes uses NMreadSection.
## NMwriteSectionOne. Belongs in NMdata. NMdata 0.2.1 required. 
## dtapply?

res.list[[1]]
res.list[[2]]
res.list[[3]]
res.list[[4]]
res.list[[5]]
res.list[[6]]
res.list[[7]]

for(I
    res <- dtf.w[,list(diffr(NMdata,NMsim)),by="file"]



    dt.overlap <- dt.files[,.N,by=.(file)][N>1]

    library(tools)
    dt.overlap[,md5sum()]



    |>
    dcast(V1~pkg)

    dir=
        list.
