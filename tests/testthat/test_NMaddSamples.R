context("NMaddSamples")
library(data.table)
data.table::setDTthreads(1) 
library(NMdata)

NMdataConf(reset=T)
test_that("Basic",{
    fileRef <- "testReference/NMaddSamples_01.rds"

    df.doses <- NMcreateDoses(TIME=c(0,12),AMT=10,CMT=2)
    seq.time <- c(0,4,12,24)

    res <- NMaddSamples(df.doses,TIME=seq.time,CMT=2)

    df.doses
    
    expect_equal_to_reference(res,fileRef)
    if(F){
        res
        readRDS(fileRef)
    }
    
})


test_that("Multiple compartments",{
    fileRef <- "testReference/NMaddSamples_02.rds"

    dt.doses <- NMcreateDoses(TIME=c(0,12),AMT=10,CMT=1)
    seq.time <- c(0,4,12,24)

    res <- NMaddSamples(dt.doses,time.sim=seq.time,CMT=c(2,3))

    ## dt.doses
    ## res
    ## readRDS(fileRef)
    expect_equal_to_reference(res,fileRef)
    
    if(F){
        res
        readRDS(fileRef)
    }
    
})



test_that("compartments with covariates",{
    fileRef <- "testReference/NMaddSamples_03.rds"

    dt.doses <- NMcreateDoses(TIME=c(0,12),AMT=10,CMT=1)
    seq.time <- c(0,4,12,24)
    dt.cmt <- data.table(CMT=c(2,3),analyte=c("parent","metabolite"))
    
    res <- NMaddSamples(dt.doses,time.sim=seq.time,CMT=dt.cmt)

    ## res
    ## readRDS(fileRef)

    expect_equal_to_reference(res,fileRef)

    if(F){
        res
        readRDS(fileRef)
    }

})



test_that("data.frame CMT",{
    fileRef <- "testReference/NMaddSamples_04.rds"

    df.doses <- NMcreateDoses(TIME=c(0,12),AMT=10,CMT=1,as.fun=as.data.frame)
    seq.time <- c(0,4,12,24)

    res <- NMaddSamples(df.doses,time.sim=seq.time,CMT=c(2,3),as.fun=as.data.frame)

    df.doses
    
    expect_equal_to_reference(res,fileRef)
    if(F){
        res
        readRDS(fileRef)
    }

})


test_that("time with covariates",{
    fileRef <- "testReference/NMaddSamples_05.rds"

    dt.doses <- NMcreateDoses(TIME=data.table(regimen=c("SD","MD","MD"),TIME=c(0,0,12)),AMT=10,CMT=1)

    seq.time.sd <- data.table(regimen="SD",TIME=seq(0,6))
    seq.time.md <- data.table(regimen="MD",TIME=c(0,4,12,24))
    seq.time <- rbind(seq.time.sd,seq.time.md)
    
    res <- NMaddSamples(dt.doses,time.sim=seq.time,CMT=2,by="regimen")

    ## dt.doses
    ## res
    
    expect_equal_to_reference(res,fileRef)

    if(F){
        res
        readRDS(fileRef)
    }


})

test_that("EVID=0",{
    fileRef <- "testReference/NMaddSamples_06.rds"

    df.doses <- NMcreateDoses(TIME=c(0,12),AMT=10,CMT=2)
    seq.time <- c(0,4,12,24)

    res <- NMaddSamples(df.doses,time.sim=seq.time,CMT=2,EVID=0)

    expect_equal_to_reference(res,fileRef)

    if(F){
        res
        readRDS(fileRef)
    }
    
})


test_that("time after dose",{
    fileRef <- "testReference/NMaddSamples_07.rds"
    df.doses <- NMcreateDoses(TIME=c(0,12),AMT=10,CMT=1)
    df.doses
    seq.time <- c(0,4,12,24)

    res <- NMaddSamples(df.doses,TAPD=seq.time,CMT=2)
    res
    expect_equal_to_reference(res,fileRef)

    if(F){
        res
        readRDS(fileRef)
    }


})


test_that("TIME and TAPD",{
    fileRef <- "testReference/NMaddSamples_08.rds"
    df.doses <- NMcreateDoses(TIME=c(0,12),AMT=10,CMT=1)
    df.doses
    seq.time <- c(0,4,12,24)

    res <- NMaddSamples(df.doses,TIME=seq.time,TAPD=3,CMT=2)
    res
    expect_equal_to_reference(res,fileRef)

    if(F){
        res
        readRDS(fileRef)
    }


})

test_that("TAPD with ADDL/II",{
    fileRef <- "testReference/NMaddSamples_09.rds"
    df.doses <- NMcreateDoses(TIME=c(0),ADDL=2,II=12,AMT=10,CMT=1)

    seq.time <- c(0,2,4,18,24)

    ## res0 is when doses are expanded in advance
    df.doses.0 <- NMexpandDoses(df.doses)
    
    res0 <- NMaddSamples(df.doses.0,TAPD=seq.time,CMT=2)
    
    res1 <- NMaddSamples(df.doses,TAPD=seq.time,CMT=2)

    expect_equal_to_reference(res1,fileRef)

    expect_equal(as.data.table(res0)[EVID==2],
                 as.data.table(res1)[EVID==2])
    
    
    if(F){
        res
        readRDS(fileRef)
    }


})

test_that("simple, more than one id",{
    NMdataConf(as.fun="data.table")
    fileRef <- "testReference/NMaddSamples_10.rds"
    dt.dos <- NMcreateDoses(TIME=c(0), AMT=data.table(AMT=1:2,grp=letters[1:2]),CMT=1) 
    dt.dos[,grp:=NULL]

    res <- NMaddSamples(dt.dos,TAPD=c(1),CMT=2)

    expect_equal_to_reference(res,fileRef)

    if(F){
        res
        readRDS(fileRef)
    }

})




test_that("TAPD -  covariates on dosing data, not on TAPD",{
    fileRef <- "testReference/NMaddSamples_11.rds"
    dt.dos <- NMcreateDoses(TIME=c(0,12), AMT=data.table(AMT=1:2,grp=letters[1:2]),CMT=1) 
    dt.dos
    res <- NMaddSamples(dt.dos,TAPD=c(1),CMT=2)
    res
    expect_equal_to_reference(res,fileRef)
})


test_that("TAPD -  covariates on dosing data, and on TAPD",{
### 
    ## this is replicating too much
    fileRef <- "testReference/NMaddSamples_12.rds"
    dt.dos <- NMcreateDoses(TIME=c(0,12), AMT=data.table(AMT=1:2,grp=letters[1:2]),CMT=1) 
    dt.dos
    ## not all subjects covered
    res <- NMaddSamples(dt.dos,TAPD=data.frame(grp="a",TAPD=1),CMT=2)
    expect_equal_to_reference(res,fileRef)
})

test_that("all subjects covered",{
### 
    fileRef <- "testReference/NMaddSamples_13.rds"
    dt.dos <- NMcreateDoses(TIME=c(0,12), AMT=data.table(AMT=1:2,grp=letters[1:2]),CMT=1) 
    res <- NMaddSamples(dt.dos,TAPD=data.frame(grp=c("a","b"),TAPD=c(1,2)),CMT=2)
    res
    expect_equal_to_reference(res,fileRef)
})

test_that("sampling schemes overlapping into next doses",{
    fileRef <- "testReference/NMaddSamples_14.rds"
    dt.dos <- NMcreateDoses(TIME=c(0,12), AMT=data.table(AMT=1:2,grp=letters[1:2]),CMT=1) 
    dt.dos
    res <- NMaddSamples(dt.dos,TAPD=data.frame(TAPD=c(1,13)),CMT=2)
    res
    expect_equal_to_reference(res,fileRef)
})


test_that("sampling at time of doses",{
    fileRef <- "testReference/NMaddSamples_15.rds"
    dt.dos <- NMcreateDoses(TIME=c(0,12), AMT=data.table(AMT=1:2,grp=letters[1:2]),CMT=1) 
    dt.dos
    res <- NMaddSamples(dt.dos,TAPD=data.frame(TAPD=c(1,12)),CMT=2)
    res
    expect_equal_to_reference(res,fileRef)
})


test_that("sampling times not unique",{
    fileRef <- "testReference/NMaddSamples_16.rds"
    dt.dos <- NMcreateDoses(TIME=c(0,12), AMT=data.table(AMT=1:2,grp=letters[1:2]),CMT=1) 
    dt.dos
    res <- NMaddSamples(dt.dos,TAPD=data.frame(TAPD=c(1,1)),CMT=2)
    res
    expect_equal_to_reference(res,fileRef)
})

test_that("No CMT column",{

    dt.dos <- NMcreateDoses(TIME=c(0,12), AMT=data.table(AMT=1:2,grp=letters[1:2]),CMT=NA) 
    res <- NMaddSamples(dt.dos,TAPD=c(1,1))

    expect_false("CMT"%in%colnames(res))

    dt.dos <- NMcreateDoses(TIME=c(0,12), AMT=data.table(AMT=1:2,grp=letters[1:2]))
    expect_error(NMaddSamples(dt.dos,TAPD=c(1,1)))
    
})

test_that("Providing DV",{
    NMdataConf(reset=TRUE)
    dt.dos <- NMcreateDoses(TIME=c(0,12), AMT=data.table(AMT=1:2,grp=letters[1:2]),CMT=1) 
## dt.dos contains MDV
    dt.dos
    res0 <- NMaddSamples(dt.dos,TAPD=data.frame(TAPD=c(1,1)),CMT=2,DV=0,as.fun="data.table")
    res2 <- NMaddSamples(dt.dos,TAPD=data.frame(TAPD=c(1,1)),CMT=2,as.fun="data.table")
### res0 should have MDV=0 because DV is provided
    res0
    res2
    expect_equal(
        transform(res0,EVID=NULL,MDV=1,DV=NULL)
       ,
        transform(res2,EVID=NULL)
    )

    expect_equal(
        res0[,sort(unique(MDV))]
       ,c(0,1))

    expect_equal(
        res0[,sort(unique(DV))]
       ,c(0))

})

test_that("default - common columns are not merged by",{
    fileRef <- "testReference/NMaddSamples_17.rds"
    samp.times <- c(1, 4, 8)
    dt.dos <- NMsim::NMcreateDoses(TIME=data.frame(TIME=0, TSTRAT=0),AMT=70,CMT=NA)

    TIME <- data.table(TIME=c(1,4,8),
                       TSTRAT=seq(1:length(samp.times)))

    if(FALSE){ ## Commented for non-interactive execution
        dt.dos
        TIME
    }
    res <- NMaddSamples(dt.dos,TIME)
###  TSTRAT does not match, but samples added with own TSTRAT values
    ## res

    expect_equal_to_reference(res,fileRef)

    if(F){
        readRDS(fileRef)
    }
    
})

test_that("by unmatched (covariates)",{

    samp.times <- c(1, 4, 8)
    dt.dos <- NMsim::NMcreateDoses(TIME=data.frame(TIME=0, TSTRAT=0),AMT=70,CMT=NA,as.fun="data.table")

    TIME <- data.table(TIME=c(1,4,8), TSTRAT=seq(1:length(samp.times)), 
                       TMIN=rep(0.01,length(samp.times)),
                       TMAX=rep(200,length(samp.times)))

### TMIN and and TSTRAT does not match, so no samples added.
    ## Result is dt.dos unmodified
    if(FALSE){ ## Commented for non-interactive execution
        dt.dos
        TIME
    }

    res <- expect_message(
### There is no simple way to match by all. This could be obtained with a `byAll` function that returns intersect of colnames of x and y.
        ##    NMaddSamples(dt.dos,TIME,by=setdiff(intersect(colnames(dt.dos),colnames(TIME)),"TIME"))
        NMaddSamples(dt.dos,TIME,by=cc(TSTRAT),as.fun=data.table)
        
    )

    expect_equal(
        dt.dos
       ,
        res[,colnames(dt.dos),with=FALSE]
    )
    ## expect_equal_to_reference(res,fileRef)

    if(F){
        readRDS(fileRef)
    }
    
})


test_that("Unmatched covariates not in by",{
    fileRef <- "testReference/NMaddSamples_18.rds"

    samp.times <- c(1, 4, 8)
    dt.dos <- NMsim::NMcreateDoses(TIME=data.frame(TIME=0, TSTRAT=0, TMIN=0, TMAX=200),AMT=70,CMT=NA)

    dt.time <- data.frame(TIME=samp.times, TSTRAT=c(1,2,3))

    res <- NMaddSamples(dt.dos,TIME = dt.time )

    expect_equal_to_reference(res,fileRef)

    if(F){
        readRDS(fileRef)
    }
    
})


test_that("Unmatched are not covs",{
    fileRef <- "testReference/NMaddSamples_19.rds"

    samp.times <- c(1, 4, 8)
    dt.dos <- NMsim::NMcreateDoses(TIME=data.frame(TIME=0, TSTRAT=0, TMIN=0, TMAX=200),AMT=70,CMT=NA)
    dt.time <- data.frame(TIME=c(1,4,8), TSTRAT=c(1,2,3))
    
    res <- NMsim::NMaddSamples(dt.dos,
                               TIME =dt.time
                              ,
                               by=FALSE)

    ## res
    expect_equal_to_reference(res,fileRef)

    if(F){
        res
        readRDS(fileRef)
    }
})

### maybe this should not be needed if by is sufficiently tested above. 
## context("NMaddSamples - Observed TIME")

### EVID and CMT are overwritten by respective arguments.

### IDs in time - not in data - are still used

NMdataConf(reset=T)
test_that("MDV missing on doses",{
    fileRef <- "testReference/NMaddSamples_20.rds"

    dt.doses <- NMcreateDoses(TIME=c(0),AMT=10,CMT=2,as.fun="data.table")
    seq.time <- c(4,12,24)
    res1 <- NMaddSamples(dt.doses,TIME=seq.time,CMT=2)
    
    dt.doses[,MDV:=NULL]
    res2 <- NMaddSamples(dt.doses,TIME=seq.time,CMT=2)
    
    res1
    res2

    res <- list(res1=res1,res2=res2)
    expect_equal_to_reference(res,fileRef)
    if(F){
        res
        readRDS(fileRef)
    }
    
})
