# minmaxNorm is a generalized function to perform a linear transformation
# on the original data. Min-max normalizaton maps a value vi of A to vi' 
# in the range [new_minA,new_maxA]

# input:  trData: training data
#         teData: testing data (if available)
#         minV:   minimum value of new range
#         maxV:   maximum value of new range
#
# output: trDataNew:  normalized training data
#         teDataNew:  normalized testing data (if available)
#
#


minmaxNorm<-function(trData,teData=NULL,minV,maxV){
  
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
  
  # check if training data is vector or matrix and then transform each column
  if (m==1){
    trDataNew<-(trData-min(trData))/(max(trData)-min(trData))*(maxV-minV)+minV
  } else if (m > 1){
  
  for(i in 1:m){
   trDataNew[,i]<-(trData[,i]-min(trData[,i]))/(max(trData[,i])-min(trData[,i]))*(maxV-minV)+minV
  }  
    
  }
  
  # check if test data exists
  if (!is.null(teData)){
  
  # Transform the test data on the same scale as the training data
  # first check that the test data is within the same range as training data
  rangeCheck<-rep(NA,m)
  
  # for vectors:
  if (m==1){
    if (min(trData) <= min(teData) & max(trData) >= max(teData)) {
      rangeCheck<-1
    } else {rangeCheck<-0}
  } else if (m > 1){  # for matrices
  
  for (i in 1:m){
    if (min(trData[,i]) <= min(teData[,i]) & max(trData[,i]) >= max(teData[,i])){
      rangeCheck[i]<-1
    } else {rangeCheck[i]<-0}
  }
    
  }
  
  if(length(rangeCheck[rangeCheck==0])>0){ 
    print('Test data is out of training data bounds')
    return(rangeCheck)
  }
  
  
  # perform transformation on traingin data if ranges are sufficient
 
    
    # allocate space for transformed test data
    teDataNew<-matrix(NA,nrow=n,ncol=m)
    
    # check if training data is vector or matrix and then transform each column
    if (m==1){
      teDataNew<-(teData-min(trData))/(max(trData)-min(trData))*(maxV-minV)+minV
    } else if (m > 1){
      
      for(i in 1:m){
        teDataNew[,i]<-(teData[,i]-min(trData[,i]))/(max(trData[,i])-min(trData[,i]))*(maxV-minV)+minV
      }  
      
    }
  }
  
  # if there is no test data, set transformed test data and the range check var equal to null
  if (is.null(teData)){
    teDataNew=NULL
    rangeCheck=NULL
  }

  # create list of output info
  my_list<-list("trData"=trData, "teData"=teData,
               "trDataNew"=trDataNew,
               "teDataNew"=teDataNew,
               "rangeCheck"=rangeCheck)
  return(my_list)
  
}