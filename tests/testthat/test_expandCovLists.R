context("expandCovLists")


test_that("basic",{

    fileRef <- "testReference/forestDefineCovs_01.rds"
    ## file.mod <- system.file("examples/nonmem/xgxr134.mod",package="NMdata")
    file.mod <- "testData/nonmem/xgxr134.mod"
    modres <- NMdata::NMscanData(file.mod,quiet=TRUE)
    
    res <- forestDefineCovs(
        WEIGHTB=list(ref=70,values=c(40,60,80,100),label="Bodyweight (kg)"),
        ## notice, values OR quantiles can be provided
        AGE=list(ref=median, quantiles=c(10,25,75,90)/100, label="Age (years)"),
        data=modres,
        as.fun="data.table"
    )

    expect_equal_to_reference(res,fileRef)
    
})


test_that("with categorical",{

    fileRef <- "testReference/forestDefineCovs_02.rds"
    ## file.mod <- system.file("examples/nonmem/xgxr134.mod",package="NMdata")
    file.mod <- "testData/nonmem/xgxr134.mod"
    modres <- NMdata::NMscanData(file.mod,quiet=TRUE)
    
    res <- forestDefineCovs(
        WEIGHTB=list(ref=70,values=c(40,60,80,100),label="Bodyweight (kg)"),
        ## notice, values OR quantiles can be provided
        AGE=list(ref=median, quantiles=c(10,25,75,90)/100, label="Age (years)"),
        MALEN=list(ref=0,values=c(Male = 1, Female = 0),label = "Sex"),
        data=modres,
        as.fun="data.table"
    )
    res

    expect_equal_to_reference(res,fileRef)
    
})


test_that("labeling",{

    fileRef <- "testReference/forestDefineCovs_03.rds"
    file.mod <- "testData/nonmem/xgxr134.mod"
    modres <- NMdata::NMscanData(file.mod,quiet=TRUE)

    res <- forestDefineCovs(
        WEIGHTB=list(ref=70,values=c(40,60,80,100),label="Bodyweight (kg)"),
        ## notice, values OR quantiles can be provided
        AGE=list(ref=median, quantiles=c("10%"=10,"25%"=25,"75%"=75,"90%"=90)/100, label="Age (years)"),
        MALEN=list(ref=c(Female=0),values=c(Male = 1, Female = 0),label = "Sex"),
        data=modres,
        as.fun="data.table"
    )


    expect_equal_to_reference(res,fileRef)

})



test_that("keep all refs",{

    fileRef <- "testReference/forestDefineCovs_04.rds"
    ## file.mod <- system.file("examples/nonmem/xgxr134.mod",package="NMdata")
    file.mod <- "testData/nonmem/xgxr134.mod"
    modres <- NMdata::NMscanData(file.mod,quiet=TRUE)
    
    res <- forestDefineCovs(
        WEIGHTB=list(ref=70,values=c(40,60,80,100),label="Bodyweight (kg)"),
        ## notice, values OR quantiles can be provided
        AGE=list(ref=median, quantiles=c(10,25,75,90)/100, label="Age (years)"),
        data=modres,
        as.fun="data.table",
        reduce.ref=F        
    )

    expect_equal_to_reference(res,fileRef)
    
})

