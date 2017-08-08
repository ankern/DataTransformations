# zscoreNorm is a function that normalized variables based on their mean
# and standard deviation or mean absolute deviation if the data has outliers

# input:  trData:   training data
#         teData:   testing data (if available)
#         madFlag:  indicator for mean absolute deviation
#
# output: trDataNew:  normalized training data
#         teDataNew:  normalized testing data (if available)
#
#


zscoreNorm<-function(trData,teData=NULL,madFlag=FALSE){
  require(lsr)
  
  # find dimension of input training data
  # check if input is a vector or matrix
  if (is.null(dim(trData))){
    m<-1
    n<-length(trData)[1]
  } else if (!is.null(dim(trData))){
    n<-dim(trData)[1]; m<-dim(trData)[2]
  }
  #return(n)
  
  # allocate space for transformed training data
  trDataNew<-matrix(NA,nrow=n,ncol=m)
  # place holder for SD or MAD
  sA<-rep(NA,m)
  
  # check if training data is vector or matrix and then transform each column
  if (m==1){
    # test for standard deviation or mean absolute deviation
    if (madFlag==TRUE){
      library(lsr)
      sA<-aad(trData)
    } else { 
      
    sA<-sd(trData)}
    trDataNew<-(trData-mean(trData))/sA
  } else if (m > 1){
    
    # test for standard deviation or mean absolute deviation
    if(madFlag==TRUE) {
      for(i in 1:m){
        library(lsr)
        sA[i]<-aad(trData[,i])
      }
    } else {
      for (i in 1:m){
      sA[i]<-sd(trData[,i])
      }}
    
    for(i in 1:m){
      
      trDataNew[,i]<-(trData[,i]-mean(trData[,i]))/sA[i]
    }  
  }
  
 
  # perform transformation on test data 
  # allocate space for transformed test data
  if (!is.null(teData)){
  teDataNew<-matrix(NA,nrow=n,ncol=m)
  
  # check if training data is vector or matrix and then transform each column
  if (m==1){
    teDataNew<-(teData-mean(trData))/sA
  } else if (m > 1){
    
    for(i in 1:m){
      teDataNew[,i]<-(teData[,i]-mean(trData[,i]))/sA[i]
    }  
    
  }
  }
  
  if (is.null(teData)){
    teDataNew=NULL
  }
  
  # create list of output info
  my_list<-list("trData"=trData, "teData"=teData,
                "trDataNew"=trDataNew,
                "teDataNew"=teDataNew)
  return(my_list)
  
}