# Last Masked and Learning

MLe=function(iter){
  max_svm=max_nb=max_knn=max_lda=max_cart=max_RF=max_ML=matrix(0,ncol=6,nrow=iter,byrow=T)
  
t1=t2=t3=r=t=k1=k2=k3=p11=p12=p13=p21=p22=p23=p31=p32=p33=percent=c()
pk1=pk2=pk3=percentR1=percentR2=percentR3=percentR2k1=percentR2k2=percentR2k3=c()
percentR3k1=percentR3k2=percentR3k3=c()

accuracy=matrix(0,ncol= 12, nrow=iter, byrow = T)
colnames(accuracy)=c("RF~Accuracy","RF~Kappa","SVM~Accuracy","SVM~Kappa","knn~Accuracy","knn~Kappa","LDA~Accuracy","LDA~Kappa","CART~Accuracy","CART~Kappa","NB~Accuracy","NB~Kappa" )

 for (g in 1:iter) {
   
MM=matrix()

generate=function(alpha1,alpha2,alpha3,b,b01,b02){
  t1=rweibull(n,b1,mu1)
  t2=rweibull(n,b2,mu2)
  t3=rweibull(n,b3,mu3)
  
  for(i in 1:n){
    t[i]=min(t1[i],t2[i],t3[i])
    if(t1[i]<t2[i]&t1[i]<t3[i])  k1[i]=1
    if(t2[i]<t1[i]&t2[i]<t3[i])  k1[i]=2
    if(t3[i]<t1[i]&t3[i]<t2[i])  k1[i]=3
  }
  
  for(i in 1:n){
    t[i]=min(t1[i],t2[i],t3[i])
    if((t1[i]<t2[i]&t2[i]<t3[i])|(t3[i]<t2[i]&t2[i]<t1[i]))  k2[i]=2
    if((t2[i]<t1[i]&t1[i]<t3[i])|(t3[i]<t1[i]&t1[i]<t2[i]))  k2[i]=1
    if((t1[i]<t3[i]&t3[i]<t2[i])|(t2[i]<t3[i]&t3[i]<t1[i]))  k2[i]=3
  }
  
  for(i in 1:n){
    if((k1[i]==1&k2[i]==2) | (k1[i]==2&k2[i]==1)){k3[i]=3}
    if((k1[i]==1&k2[i]==3) | (k1[i]==3&k2[i]==1)){k3[i]=2}
    if((k1[i]==3&k2[i]==2) | (k1[i]==2&k2[i]==3)){k3[i]=1}
  }
  
  #k=1
  for(i in 1:n){
    p11[i]=exp(b11+b*t[i])/(1+exp(b11+b*t[i])+2*exp(b12+b*t[i]))
    p21[i]=exp(b12+b*t[i])/(1+exp(b11+b*t[i])+2*exp(b12+b*t[i]))
    p31[i]=              1/(1+exp(b11+b*t[i])+2*exp(b12+b*t[i]))
  }
  #k=2
  for(i in 1:n){
    p12[i]=exp(b21+b*t[i])/(1+exp(b21+b*t[i])+2*exp(b22+b*t[i]))
    p22[i]=exp(b22+b*t[i])/(1+exp(b21+b*t[i])+2*exp(b22+b*t[i]))
    p32[i]=              1/(1+exp(b21+b*t[i])+2*exp(b22+b*t[i]))
  }
  #k=3
  for(i in 1:n){
    p13[i]=exp(b31+b*t[i])/(1+exp(b31+b*t[i])+2*exp(b32+b*t[i]))
    p23[i]=exp(b32+b*t[i])/(1+exp(b31+b*t[i])+2*exp(b32+b*t[i]))
    p33[i]=              1/(1+exp(b31+b*t[i])+2*exp(b32+b*t[i]))
  }
  
  
  R=matrix(0,3,n)
  for(i in 1:n){
    if(k1[i]==1){R[,i]=rmultinom(1,1,c(p11[i],p21[i],p31[i]))}
    if(k1[i]==2){R[,i]=rmultinom(1,1,c(p12[i],p22[i],p32[i]))}
    if(k1[i]==3){R[,i]=rmultinom(1,1,c(p13[i],p23[i],p33[i]))}
  }
  
  for(i in 1:n){
    for(j in 1:J){
      if(R[j,i]==1) {r[i]=j} 
      else return
    }
  }
  MM=matrix(c(t1,t2,t3,k1,k2,k3,r),byrow = F,nrow=n)
  colnames(MM)=c("t1","t2","t3","k1","k2","k3","r")
  return(MM)
  
}

MM.gen=generate(alpha1,alpha2,alpha3,b,b01,b02)

pk1[g]=sum(MM.gen[,4]==1)/n
pk2[g]=sum(MM.gen[,4]==2)/n
pk3[g]=sum(MM.gen[,4]==3)/n

percentR1[g]=sum(MM.gen[,7]==1)/n
percentR2[g]=sum(MM.gen[,7]==2)/n
percentR3[g]=sum(MM.gen[,7]==3)/n

percentR2k1[g]=sum(MM.gen[,7]==2&MM.gen[,4]==1)/sum(MM.gen[,4]==1)
percentR2k2[g]=sum(MM.gen[,7]==2&MM.gen[,4]==2)/sum(MM.gen[,4]==2)
percentR2k3[g]=sum(MM.gen[,7]==2&MM.gen[,4]==3)/sum(MM.gen[,4]==3)

percentR3k1[g]=sum(MM.gen[,7]==3&MM.gen[,4]==1)/sum(MM.gen[,4]==1)
percentR3k2[g]=sum(MM.gen[,7]==3&MM.gen[,4]==2)/sum(MM.gen[,4]==2)
percentR3k3[g]=sum(MM.gen[,7]==3&MM.gen[,4]==3)/sum(MM.gen[,4]==3)


  ###Visualization
 # scatterplot matrix

  t=apply(MM.gen[,1:3],1 , min)
  k=MM.gen[,4]
  k=as.factor(k)
  #caret::featurePlot(x=t, y=k, plot="box",labels = c("Cause","Time"),font=50,lwd=2)

  #boxplot(t~k)
  # density plots for each attribute by class value
  scales <- list(x=list(relation="free"), y=list(relation="free"))
  #caret::featurePlot(x=t, y=k, plot="density", scales=scales,font=4,lwd=2)


#r=3
 MM=MM.gen
 MM1=matrix(MM[MM[,7]==1],ncol=7,byrow = F)
 
 t=apply(MM1[,1:3],1,min)
 k=MM1[,4]
 M=matrix(c(t,k),ncol=2,byrow = F)
 colnames(M)=c("t","k")
 train=data.frame(M)
 train$k=as.factor(train$k)
  
  fitRF <- caret::train(k~t, # Survived is a function of the variables we decided to include
                        data = train, # Use the train data frame as the training data
                        method = 'rf',# Use the 'random forest' algorithm
                        trControl = caret::trainControl(method = 'cv', # Use cross-validation
                                                        number = 10) # Use 5 folds for cross-validation
  ) #End 
  
  #summary(fitRF)
  #fitRF
  
  MM2=matrix(MM[MM[,7]==3],ncol=7,byrow = F)
  
  t.test=apply(MM2[,1:3],1,min)
  k.test=MM2[,4]
  M.test=matrix(c(t.test,k.test),ncol=2,byrow = F)
  colnames(M.test)=c("t","k")
  test=data.frame(M.test)
  test$k=as.factor(test$k)
  
  
  
  
  prRF=predict(fitRF,test,type = "raw")
  #confusionMatrix(prRF,test$k)
  
  
  MM[,4][MM[,7]==3]=prRF
  
  #r=2
  #r=2 and k1 and k2= 1 or 2
  #train
  MM12= matrix(MM[MM[,7]==1&
                    ((MM[,4]==1)|
                       (MM[,4]==2))],
               ncol=7,byrow = F)
  t1=apply(MM12[,1:3], 1, min)
  k1=MM12[,4]
  MM12.1=matrix(c(t1,k1),ncol=2,byrow = F)
  colnames(MM12.1)=c("t1","k1")
  train12=data.frame(MM12.1)
  train12$k1=as.factor(train12$k1)
  
  fitRF <- caret::train(k1~t1, # Survived is a function of the variables we decided to include
                        data = train12, # Use the train data frame as the training data
                        method = 'rf',# Use the 'random forest' algorithm
                        trControl = caret::trainControl(method = 'cv', # Use cross-validation
                                                        number = 10) # Use 5 folds for cross-validation
  ) #End 
  
  
  #test
  MM12= matrix(MM[MM[,7]==2&
                    ((MM[,4]==1&MM[,5]==2)|
                       (MM[,4]==2&MM[,5]==1))],
               ncol=7,byrow = F)
  t1=apply(MM12[,1:3], 1, min)
  k1=MM12[,4]
  MM12.1=matrix(c(t1,k1),ncol=2,byrow = F)
  colnames(MM12.1)=c("t1","k1")
  test12=data.frame(MM12.1)
  test12$k1=as.factor(test12$k1)
  
  prRF=predict(fitRF,test12,type = "raw")
  
  MM[,4][MM[,7]==2 & ((MM[,4]==1&MM[,5]==2)|
           (MM[,4]==2&MM[,5]==1)) ]=prRF
  
  
  #r=2 and k1 and k2= 3 or 1
  
  
  MM13= matrix(MM[MM[,7]==1&
                    ((MM[,4]==1)|
                       (MM[,4]==3))],
               ncol=7,byrow = F)
  t1=apply(MM13[,1:3], 1, min)
  k1=MM13[,4]
  MM13.1=matrix(c(t1,k1),ncol=2,byrow = F)
  colnames(MM13.1)=c("t1","k1")
  train13=data.frame(MM13.1)
  train13$k1=as.factor(train13$k1)
  
  fitRF <- caret::train(k1~t1, # Survived is a function of the variables we decided to include
                        data = train13, # Use the train data frame as the training data
                        method = 'rf',# Use the 'random forest' algorithm
                        trControl = caret::trainControl(method = 'cv', # Use cross-validation
                                                        number = 10) # Use 5 folds for cross-validation
  )
  
  MM13= matrix(MM[MM[,7]==2&
                    ((MM[,4]==1&MM[,5]==3)|
                       (MM[,4]==3&MM[,5]==1))],
               ncol=7,byrow = F)
  t1=apply(MM13[,1:3], 1, min)
  k1=MM13[,4]
  MM13.1=matrix(c(t1,k1),ncol=2,byrow = F)
  colnames(MM13.1)=c("t1","k1")
  test13=data.frame(MM13.1)
  test13$k1=as.factor(test13$k1)
  
  prRF=predict(fitRF,test13,type = "raw")
  
  MM[,4][MM[,7]==2 & ((MM[,4]==1&MM[,5]==3)|
                        (MM[,4]==3&MM[,5]==1)) ]=prRF
  
  
  #r=2 and k1 and k2= 3 or 2
  MM23= matrix(MM[MM[,7]==1&
                    ((MM[,4]==2)|
                       (MM[,4]==3))],
               ncol=7,byrow = F)
  t1=apply(MM23[,1:3], 1, min)
  k1=MM23[,4]
  MM23.1=matrix(c(t1,k1),ncol=2,byrow = F)
  colnames(MM23.1)=c("t1","k1")
  train23=data.frame(MM23.1)
  train23$k1=as.factor(train23$k1)
  
  fitRF <- caret::train(k1~t1, # Survived is a function of the variables we decided to include
                        data = train23, # Use the train data frame as the training data
                        method = 'rf',# Use the 'random forest' algorithm
                        trControl = caret::trainControl(method = 'cv', # Use cross-validation
                                                        number = 10) # Use 5 folds for cross-validation
  )
  
  
  MM23= matrix(MM[MM[,7]==2&
                    ((MM[,4]==2&MM[,5]==3)|
                       (MM[,4]==3&MM[,5]==2))],
               ncol=7,byrow = F)
  t1=apply(MM23[,1:3], 1, min)
  k1=MM23[,4]
  MM23.1=matrix(c(t1,k1),ncol=2,byrow = F)
  colnames(MM23.1)=c("t1","k1")
  test23=data.frame(MM23.1)
  test23$k1=as.factor(test23$k1)
  
  prRF=predict(fitRF,test23,type = "raw")
  
  MM[,4][MM[,7]==2 & ((MM[,4]==2&MM[,5]==3)|
                        (MM[,4]==3&MM[,5]==2)) ]=prRF
  
  t=apply(MM[,1:3],1,min)
  k=MM[,4]
  
  ###MLERF
  z1=c()
  like=function(rr){
    b1=rr[1];b2=rr[2];b3=rr[3]
    mu1=rr[4];mu2=rr[5];mu3=rr[6]
    
    
    for(i in 1:n) {
      
      
      if(k[i]==1){
        
        z1[i]=dweibull(t[i],b1,mu1)*(1-pweibull(t[i],b2,mu2))*(1-pweibull(t[i],b3,mu3))}
      
      if(k[i]==2){ 
        
        z1[i]=dweibull(t[i],b2,mu2)*(1-pweibull(t[i],b1,mu1))*(1-pweibull(t[i],b3,mu3))}
     
       if(k[i]==3){ 
  
        z1[i]=dweibull(t[i],b3,mu3)*(1-pweibull(t[i],b1,mu1))*(1-pweibull(t[i],b2,mu2))}
    }
    return(-sum(log(z1)))
  }
  init=c(rep(0.05,6))
  tic()
  maxRF=nlminb(init,like,lower=c(0,0,0,0,0,0),upper=c(rep(Inf,6)))
  toc()
  #####################################################SVM
  #r=3
  MM=MM.gen
  MM1=matrix(MM[MM[,7]==1],ncol=7,byrow = F)
  
  t=apply(MM1[,1:3],1,min)
  k=MM1[,4]
  M=matrix(c(t,k),ncol=2,byrow = F)
  colnames(M)=c("t","k")
  train=data.frame(M)
  train$k=as.factor(train$k)
  
  fitsvm <- caret::train(k~t, # Survived is a function of the variables we decided to include
                        data = train, # Use the train data frame as the training data
                        method = 'svmRadial',# Use the 'random forest' algorithm
                        trControl = caret::trainControl(method = 'cv', # Use cross-validation
                                                        number = 10) # Use 5 folds for cross-validation
  ) #End 
  
  #summary(fit0)
  #fit0
  
  MM2=matrix(MM[MM[,7]==3],ncol=7,byrow = F)
  
  t.test=apply(MM2[,1:3],1,min)
  k.test=MM2[,4]
  M.test=matrix(c(t.test,k.test),ncol=2,byrow = F)
  colnames(M.test)=c("t","k")
  test=data.frame(M.test)
  test$k=as.factor(test$k)
  
  
  
  
  prsvm=predict(fitsvm,test,type = "raw")
  #confusionMatrix(prsvm,test$k)
  
  
  MM[,4][MM[,7]==3]=prsvm
  
  #r=2
  #r=2 and k1 and k2= 1 or 2
  #train
  MM12= matrix(MM[MM[,7]==1&
                    ((MM[,4]==1)|
                       (MM[,4]==2))],
               ncol=7,byrow = F)
  t1=apply(MM12[,1:3], 1, min)
  k1=MM12[,4]
  MM12.1=matrix(c(t1,k1),ncol=2,byrow = F)
  colnames(MM12.1)=c("t1","k1")
  train12=data.frame(MM12.1)
  train12$k1=as.factor(train12$k1)
  
  fitsvm <- caret::train(k1~t1, # Survived is a function of the variables we decided to include
                        data = train12, # Use the train data frame as the training data
                        method = 'svmRadial',# Use the 'random forest' algorithm
                        trControl = caret::trainControl(method = 'cv', # Use cross-validation
                                                        number = 10) # Use 5 folds for cross-validation
  ) #End 
  
  
  #test
  MM12= matrix(MM[MM[,7]==2&
                    ((MM[,4]==1&MM[,5]==2)|
                       (MM[,4]==2&MM[,5]==1))],
               ncol=7,byrow = F)
  t1=apply(MM12[,1:3], 1, min)
  k1=MM12[,4]
  MM12.1=matrix(c(t1,k1),ncol=2,byrow = F)
  colnames(MM12.1)=c("t1","k1")
  test12=data.frame(MM12.1)
  test12$k1=as.factor(test12$k1)
  
  prsvm=predict(fitsvm,test12,type = "raw")
  
  MM[,4][MM[,7]==2 & ((MM[,4]==1&MM[,5]==2)|
                        (MM[,4]==2&MM[,5]==1)) ]=prsvm
  
  
  #r=2 and k1 and k2= 3 or 1
  
  
  MM13= matrix(MM[MM[,7]==1&
                    ((MM[,4]==1)|
                       (MM[,4]==3))],
               ncol=7,byrow = F)
  t1=apply(MM13[,1:3], 1, min)
  k1=MM13[,4]
  MM13.1=matrix(c(t1,k1),ncol=2,byrow = F)
  colnames(MM13.1)=c("t1","k1")
  train13=data.frame(MM13.1)
  train13$k1=as.factor(train13$k1)
  
  fitsvm <- caret::train(k1~t1, # Survived is a function of the variables we decided to include
                        data = train13, # Use the train data frame as the training data
                        method = 'svmRadial',# Use the 'random forest' algorithm
                        trControl = caret::trainControl(method = 'cv', # Use cross-validation
                                                        number = 10) # Use 5 folds for cross-validation
  )
  
  MM13= matrix(MM[MM[,7]==2&
                    ((MM[,4]==1&MM[,5]==3)|
                       (MM[,4]==3&MM[,5]==1))],
               ncol=7,byrow = F)
  t1=apply(MM13[,1:3], 1, min)
  k1=MM13[,4]
  MM13.1=matrix(c(t1,k1),ncol=2,byrow = F)
  colnames(MM13.1)=c("t1","k1")
  test13=data.frame(MM13.1)
  test13$k1=as.factor(test13$k1)
  
  prsvm=predict(fitsvm,test13,type = "raw")
  
  MM[,4][MM[,7]==2 & ((MM[,4]==1&MM[,5]==3)|
                        (MM[,4]==3&MM[,5]==1)) ]=prsvm
  
  
  #r=2 and k1 and k2= 3 or 2
  MM23= matrix(MM[MM[,7]==1&
                    ((MM[,4]==2)|
                       (MM[,4]==3))],
               ncol=7,byrow = F)
  t1=apply(MM23[,1:3], 1, min)
  k1=MM23[,4]
  MM23.1=matrix(c(t1,k1),ncol=2,byrow = F)
  colnames(MM23.1)=c("t1","k1")
  train23=data.frame(MM23.1)
  train23$k1=as.factor(train23$k1)
  
  fitsvm <- caret::train(k1~t1, # Survived is a function of the variables we decided to include
                        data = train23, # Use the train data frame as the training data
                        method = 'svmRadial',# Use the 'random forest' algorithm
                        trControl = caret::trainControl(method = 'cv', # Use cross-validation
                                                        number = 10) # Use 5 folds for cross-validation
  )
  
  
  MM23= matrix(MM[MM[,7]==2&
                    ((MM[,4]==2&MM[,5]==3)|
                       (MM[,4]==3&MM[,5]==2))],
               ncol=7,byrow = F)
  t1=apply(MM23[,1:3], 1, min)
  k1=MM23[,4]
  MM23.1=matrix(c(t1,k1),ncol=2,byrow = F)
  colnames(MM23.1)=c("t1","k1")
  test23=data.frame(MM23.1)
  test23$k1=as.factor(test23$k1)
  
  prsvm=predict(fitsvm,test23,type = "raw")
  
  MM[,4][MM[,7]==2 & ((MM[,4]==2&MM[,5]==3)|
                        (MM[,4]==3&MM[,5]==2)) ]=prsvm
  
  t=apply(MM[,1:3],1,min)
  k=MM[,4]
  ###MLEsvm
  z1=c()
  like=function(rr){
    b1=rr[1];b2=rr[2];b3=rr[3]
    mu1=rr[4];mu2=rr[5];mu3=rr[6]
    
    
    for(i in 1:n) {
      
      
      if(k[i]==1){
        
        z1[i]=dweibull(t[i],b1,mu1)*(1-pweibull(t[i],b2,mu2))*(1-pweibull(t[i],b3,mu3))}
      
      if(k[i]==2){ 
        
        z1[i]=dweibull(t[i],b2,mu2)*(1-pweibull(t[i],b1,mu1))*(1-pweibull(t[i],b3,mu3))}
      
      if(k[i]==3){ 
        
        z1[i]=dweibull(t[i],b3,mu3)*(1-pweibull(t[i],b1,mu1))*(1-pweibull(t[i],b2,mu2))}
    }
    return(-sum(log(z1)))
  }
  init=c(rep(0.5,6))
  tic()
  maxsvm=nlminb(init,like,lower=c(0,0,0,0,0,0),upper=c(rep(Inf,6)))
  toc()
  ################################KNN
  
  #r=3
  MM=MM.gen
  MM1=matrix(MM[MM[,7]==1],ncol=7,byrow = F)
  
  t=apply(MM1[,1:3],1,min)
  k=MM1[,4]
  M=matrix(c(t,k),ncol=2,byrow = F)
  colnames(M)=c("t","k")
  train=data.frame(M)
  train$k=as.factor(train$k)
  
  fitknn <- caret::train(k~t, # Survived is a function of the variables we decided to include
                        data = train, # Use the train data frame as the training data
                        method = 'knn',# Use the 'random forest' algorithm
                        trControl = caret::trainControl(method = 'cv', # Use cross-validation
                                                        number = 10) # Use 5 folds for cross-validation
  ) #End 
  
  #summary(fit0)
  #fit0
  
  MM2=matrix(MM[MM[,7]==3],ncol=7,byrow = F)
  
  t.test=apply(MM2[,1:3],1,min)
  k.test=MM2[,4]
  M.test=matrix(c(t.test,k.test),ncol=2,byrow = F)
  colnames(M.test)=c("t","k")
  test=data.frame(M.test)
  test$k=as.factor(test$k)
  
  
  
  
  prknn=predict(fitknn,test,type = "raw")
  #confusionMatrix(prknn,test$k)
  
  
  MM[,4][MM[,7]==3]=prknn
  
  #r=2
  #r=2 and k1 and k2= 1 or 2
  #train
  MM12= matrix(MM[MM[,7]==1&
                    ((MM[,4]==1)|
                       (MM[,4]==2))],
               ncol=7,byrow = F)
  t1=apply(MM12[,1:3], 1, min)
  k1=MM12[,4]
  MM12.1=matrix(c(t1,k1),ncol=2,byrow = F)
  colnames(MM12.1)=c("t1","k1")
  train12=data.frame(MM12.1)
  train12$k1=as.factor(train12$k1)
  
  fitknn <- caret::train(k1~t1, # Survived is a function of the variables we decided to include
                        data = train12, # Use the train data frame as the training data
                        method = 'knn',# Use the 'random forest' algorithm
                        trControl = caret::trainControl(method = 'cv', # Use cross-validation
                                                        number = 10) # Use 5 folds for cross-validation
  ) #End 
  
  
  #test
  MM12= matrix(MM[MM[,7]==2&
                    ((MM[,4]==1&MM[,5]==2)|
                       (MM[,4]==2&MM[,5]==1))],
               ncol=7,byrow = F)
  t1=apply(MM12[,1:3], 1, min)
  k1=MM12[,4]
  MM12.1=matrix(c(t1,k1),ncol=2,byrow = F)
  colnames(MM12.1)=c("t1","k1")
  test12=data.frame(MM12.1)
  test12$k1=as.factor(test12$k1)
  
  prknn=predict(fitknn,test12,type = "raw")
  
  MM[,4][MM[,7]==2 & ((MM[,4]==1&MM[,5]==2)|
                        (MM[,4]==2&MM[,5]==1)) ]=prknn
  
  
  #r=2 and k1 and k2= 3 or 1
  
  
  MM13= matrix(MM[MM[,7]==1&
                    ((MM[,4]==1)|
                       (MM[,4]==3))],
               ncol=7,byrow = F)
  t1=apply(MM13[,1:3], 1, min)
  k1=MM13[,4]
  MM13.1=matrix(c(t1,k1),ncol=2,byrow = F)
  colnames(MM13.1)=c("t1","k1")
  train13=data.frame(MM13.1)
  train13$k1=as.factor(train13$k1)
  
  fitknn <- caret::train(k1~t1, # Survived is a function of the variables we decided to include
                        data = train13, # Use the train data frame as the training data
                        method = 'knn',# Use the 'random forest' algorithm
                        trControl = caret::trainControl(method = 'cv', # Use cross-validation
                                                        number = 10) # Use 5 folds for cross-validation
  )
  
  MM13= matrix(MM[MM[,7]==2&
                    ((MM[,4]==1&MM[,5]==3)|
                       (MM[,4]==3&MM[,5]==1))],
               ncol=7,byrow = F)
  t1=apply(MM13[,1:3], 1, min)
  k1=MM13[,4]
  MM13.1=matrix(c(t1,k1),ncol=2,byrow = F)
  colnames(MM13.1)=c("t1","k1")
  test13=data.frame(MM13.1)
  test13$k1=as.factor(test13$k1)
  
  prknn=predict(fitknn,test13,type = "raw")
  
  MM[,4][MM[,7]==2 & ((MM[,4]==1&MM[,5]==3)|
                        (MM[,4]==3&MM[,5]==1)) ]=prknn
  
  
  #r=2 and k1 and k2= 3 or 2
  MM23= matrix(MM[MM[,7]==1&
                    ((MM[,4]==2)|
                       (MM[,4]==3))],
               ncol=7,byrow = F)
  t1=apply(MM23[,1:3], 1, min)
  k1=MM23[,4]
  MM23.1=matrix(c(t1,k1),ncol=2,byrow = F)
  colnames(MM23.1)=c("t1","k1")
  train23=data.frame(MM23.1)
  train23$k1=as.factor(train23$k1)
  
  fitknn <- caret::train(k1~t1, # Survived is a function of the variables we decided to include
                        data = train23, # Use the train data frame as the training data
                        method = 'knn',# Use the 'random forest' algorithm
                        trControl = caret::trainControl(method = 'cv', # Use cross-validation
                                                        number = 10) # Use 5 folds for cross-validation
  )
  
  
  MM23= matrix(MM[MM[,7]==2&
                    ((MM[,4]==2&MM[,5]==3)|
                       (MM[,4]==3&MM[,5]==2))],
               ncol=7,byrow = F)
  t1=apply(MM23[,1:3], 1, min)
  k1=MM23[,4]
  MM23.1=matrix(c(t1,k1),ncol=2,byrow = F)
  colnames(MM23.1)=c("t1","k1")
  test23=data.frame(MM23.1)
  test23$k1=as.factor(test23$k1)
  
  prknn=predict(fitknn,test23,type = "raw")
  
  MM[,4][MM[,7]==2 & ((MM[,4]==2&MM[,5]==3)|
                        (MM[,4]==3&MM[,5]==2)) ]=prknn
  
  t=apply(MM[,1:3],1,min)
  k=MM[,4]
  
  ###MLEknn
  z1=c()
  like=function(rr){
    b1=rr[1];b2=rr[2];b3=rr[3]
    mu1=rr[4];mu2=rr[5];mu3=rr[6]
    
    
    for(i in 1:n) {
      
      
      if(k[i]==1){
        
        z1[i]=dweibull(t[i],b1,mu1)*(1-pweibull(t[i],b2,mu2))*(1-pweibull(t[i],b3,mu3))}
      
      if(k[i]==2){ 
        
        z1[i]=dweibull(t[i],b2,mu2)*(1-pweibull(t[i],b1,mu1))*(1-pweibull(t[i],b3,mu3))}
      
      if(k[i]==3){ 
        
        z1[i]=dweibull(t[i],b3,mu3)*(1-pweibull(t[i],b1,mu1))*(1-pweibull(t[i],b2,mu2))}
    }
    return(-sum(log(z1)))
  }
  init=c(rep(0.5,6))
  tic()
  maxknn=nlminb(init,like,lower=c(0,0,0,0,0,0),upper=c(rep(Inf,6)))
  toc()
  ######################### LDA
  
  #r=3
  MM=MM.gen
  MM1=matrix(MM[MM[,7]==1],ncol=7,byrow = F)
  
  t=apply(MM1[,1:3],1,min)
  k=MM1[,4]
  M=matrix(c(t,k),ncol=2,byrow = F)
  colnames(M)=c("t","k")
  train=data.frame(M)
  train$k=as.factor(train$k)
  
  fitlda <- caret::train(k~t, # Survived is a function of the variables we decided to include
                        data = train, # Use the train data frame as the training data
                        method = 'lda',# Use the 'random forest' algorithm
                        trControl = caret::trainControl(method = 'cv', # Use cross-validation
                                                        number = 10) # Use 5 folds for cross-validation
  ) #End 
  
  #summary(fit0)
  #fit0
  
  MM2=matrix(MM[MM[,7]==3],ncol=7,byrow = F)
  
  t.test=apply(MM2[,1:3],1,min)
  k.test=MM2[,4]
  M.test=matrix(c(t.test,k.test),ncol=2,byrow = F)
  colnames(M.test)=c("t","k")
  test=data.frame(M.test)
  test$k=as.factor(test$k)
  
  
  
  
  prlda=predict(fitlda,test,type = "raw")
  #confusionMatrix(prlda,test$k)
  
  
  MM[,4][MM[,7]==3]=prlda
  
  #r=2
  #r=2 and k1 and k2= 1 or 2
  #train
  MM12= matrix(MM[MM[,7]==1&
                    ((MM[,4]==1)|
                       (MM[,4]==2))],
               ncol=7,byrow = F)
  t1=apply(MM12[,1:3], 1, min)
  k1=MM12[,4]
  MM12.1=matrix(c(t1,k1),ncol=2,byrow = F)
  colnames(MM12.1)=c("t1","k1")
  train12=data.frame(MM12.1)
  train12$k1=as.factor(train12$k1)
  
  fitlda <- caret::train(k1~t1, # Survived is a function of the variables we decided to include
                        data = train12, # Use the train data frame as the training data
                        method = 'lda',# Use the 'random forest' algorithm
                        trControl = caret::trainControl(method = 'cv', # Use cross-validation
                                                        number = 10) # Use 5 folds for cross-validation
  ) #End 
  
  
  #test
  MM12= matrix(MM[MM[,7]==2&
                    ((MM[,4]==1&MM[,5]==2)|
                       (MM[,4]==2&MM[,5]==1))],
               ncol=7,byrow = F)
  t1=apply(MM12[,1:3], 1, min)
  k1=MM12[,4]
  MM12.1=matrix(c(t1,k1),ncol=2,byrow = F)
  colnames(MM12.1)=c("t1","k1")
  test12=data.frame(MM12.1)
  test12$k1=as.factor(test12$k1)
  
  prlda=predict(fitlda,test12,type = "raw")
  
  MM[,4][MM[,7]==2 & ((MM[,4]==1&MM[,5]==2)|
                        (MM[,4]==2&MM[,5]==1)) ]=prlda
  
  
  #r=2 and k1 and k2= 3 or 1
  
  
  MM13= matrix(MM[MM[,7]==1&
                    ((MM[,4]==1)|
                       (MM[,4]==3))],
               ncol=7,byrow = F)
  t1=apply(MM13[,1:3], 1, min)
  k1=MM13[,4]
  MM13.1=matrix(c(t1,k1),ncol=2,byrow = F)
  colnames(MM13.1)=c("t1","k1")
  train13=data.frame(MM13.1)
  train13$k1=as.factor(train13$k1)
  
  fitlda <- caret::train(k1~t1, # Survived is a function of the variables we decided to include
                        data = train13, # Use the train data frame as the training data
                        method = 'lda',# Use the 'random forest' algorithm
                        trControl = caret::trainControl(method = 'cv', # Use cross-validation
                                                        number = 10) # Use 5 folds for cross-validation
  )
  
  MM13= matrix(MM[MM[,7]==2&
                    ((MM[,4]==1&MM[,5]==3)|
                       (MM[,4]==3&MM[,5]==1))],
               ncol=7,byrow = F)
  t1=apply(MM13[,1:3], 1, min)
  k1=MM13[,4]
  MM13.1=matrix(c(t1,k1),ncol=2,byrow = F)
  colnames(MM13.1)=c("t1","k1")
  test13=data.frame(MM13.1)
  test13$k1=as.factor(test13$k1)
  
  prlda=predict(fitlda,test13,type = "raw")
  
  MM[,4][MM[,7]==2 & ((MM[,4]==1&MM[,5]==3)|
                        (MM[,4]==3&MM[,5]==1)) ]=prlda
  
  
  #r=2 and k1 and k2= 3 or 2
  MM23= matrix(MM[MM[,7]==1&
                    ((MM[,4]==2)|
                       (MM[,4]==3))],
               ncol=7,byrow = F)
  t1=apply(MM23[,1:3], 1, min)
  k1=MM23[,4]
  MM23.1=matrix(c(t1,k1),ncol=2,byrow = F)
  colnames(MM23.1)=c("t1","k1")
  train23=data.frame(MM23.1)
  train23$k1=as.factor(train23$k1)
  
  fitlda <- caret::train(k1~t1, # Survived is a function of the variables we decided to include
                        data = train23, # Use the train data frame as the training data
                        method = 'lda',# Use the 'random forest' algorithm
                        trControl = caret::trainControl(method = 'cv', # Use cross-validation
                                                        number = 10) # Use 5 folds for cross-validation
  )
  
  
  MM23= matrix(MM[MM[,7]==2&
                    ((MM[,4]==2&MM[,5]==3)|
                       (MM[,4]==3&MM[,5]==2))],
               ncol=7,byrow = F)
  t1=apply(MM23[,1:3], 1, min)
  k1=MM23[,4]
  MM23.1=matrix(c(t1,k1),ncol=2,byrow = F)
  colnames(MM23.1)=c("t1","k1")
  test23=data.frame(MM23.1)
  test23$k1=as.factor(test23$k1)
  
  prlda=predict(fitlda,test23,type = "raw")
  
  MM[,4][MM[,7]==2 & ((MM[,4]==2&MM[,5]==3)|
                        (MM[,4]==3&MM[,5]==2)) ]=prlda
  
  t=apply(MM[,1:3],1,min)
  k=MM[,4]
  
  
  ###MLElda
  z1=c()
  like=function(rr){
    b1=rr[1];b2=rr[2];b3=rr[3]
    mu1=rr[4];mu2=rr[5];mu3=rr[6]
    
    
    for(i in 1:n) {
      
      
      if(k[i]==1){
        
        z1[i]=dweibull(t[i],b1,mu1)*(1-pweibull(t[i],b2,mu2))*(1-pweibull(t[i],b3,mu3))}
      
      if(k[i]==2){ 
        
        z1[i]=dweibull(t[i],b2,mu2)*(1-pweibull(t[i],b1,mu1))*(1-pweibull(t[i],b3,mu3))}
      
      if(k[i]==3){ 
        
        z1[i]=dweibull(t[i],b3,mu3)*(1-pweibull(t[i],b1,mu1))*(1-pweibull(t[i],b2,mu2))}
    }
    return(-sum(log(z1)))
  }
  init=c(rep(0.5,6))
  tic()
  maxlda=nlminb(init,like,lower=c(0,0,0,0,0,0),upper=c(rep(Inf,6)))
  toc()
  ################## CART
  
  #r=3
  MM=MM.gen
  MM1=matrix(MM[MM[,7]==1],ncol=7,byrow = F)
  
  t=apply(MM1[,1:3],1,min)
  k=MM1[,4]
  M=matrix(c(t,k),ncol=2,byrow = F)
  colnames(M)=c("t","k")
  train=data.frame(M)
  train$k=as.factor(train$k)
  
  fitrpart <- caret::train(k~t, # Survived is a function of the variables we decided to include
                        data = train, # Use the train data frame as the training data
                        method = 'rpart',# Use the 'random forest' algorithm
                        trControl = caret::trainControl(method = 'cv', # Use cross-validation
                                                        number = 10) # Use 5 folds for cross-validation
  ) #End 
  
  #summary(fit0)
  #fit0
  
  MM2=matrix(MM[MM[,7]==3],ncol=7,byrow = F)
  
  t.test=apply(MM2[,1:3],1,min)
  k.test=MM2[,4]
  M.test=matrix(c(t.test,k.test),ncol=2,byrow = F)
  colnames(M.test)=c("t","k")
  test=data.frame(M.test)
  test$k=as.factor(test$k)
  
  
  
  
  prrpart=predict(fitrpart,test,type = "raw")
  #confusionMatrix(prrpart,test$k)
  
  
  MM[,4][MM[,7]==3]=prrpart
  
  #r=2
  #r=2 and k1 and k2= 1 or 2
  #train
  MM12= matrix(MM[MM[,7]==1&
                    ((MM[,4]==1)|
                       (MM[,4]==2))],
               ncol=7,byrow = F)
  t1=apply(MM12[,1:3], 1, min)
  k1=MM12[,4]
  MM12.1=matrix(c(t1,k1),ncol=2,byrow = F)
  colnames(MM12.1)=c("t1","k1")
  train12=data.frame(MM12.1)
  train12$k1=as.factor(train12$k1)
  
  fitrpart <- caret::train(k1~t1, # Survived is a function of the variables we decided to include
                        data = train12, # Use the train data frame as the training data
                        method = 'rpart',# Use the 'random forest' algorithm
                        trControl = caret::trainControl(method = 'cv', # Use cross-validation
                                                        number = 10) # Use 5 folds for cross-validation
  ) #End 
  
  
  #test
  MM12= matrix(MM[MM[,7]==2&
                    ((MM[,4]==1&MM[,5]==2)|
                       (MM[,4]==2&MM[,5]==1))],
               ncol=7,byrow = F)
  t1=apply(MM12[,1:3], 1, min)
  k1=MM12[,4]
  MM12.1=matrix(c(t1,k1),ncol=2,byrow = F)
  colnames(MM12.1)=c("t1","k1")
  test12=data.frame(MM12.1)
  test12$k1=as.factor(test12$k1)
  
  prrpart=predict(fitrpart,test12,type = "raw")
  
  MM[,4][MM[,7]==2 & ((MM[,4]==1&MM[,5]==2)|
                        (MM[,4]==2&MM[,5]==1)) ]=prrpart
  
  
  #r=2 and k1 and k2= 3 or 1
  
  
  MM13= matrix(MM[MM[,7]==1&
                    ((MM[,4]==1)|
                       (MM[,4]==3))],
               ncol=7,byrow = F)
  t1=apply(MM13[,1:3], 1, min)
  k1=MM13[,4]
  MM13.1=matrix(c(t1,k1),ncol=2,byrow = F)
  colnames(MM13.1)=c("t1","k1")
  train13=data.frame(MM13.1)
  train13$k1=as.factor(train13$k1)
  
  fitrpart <- caret::train(k1~t1, # Survived is a function of the variables we decided to include
                        data = train13, # Use the train data frame as the training data
                        method = 'rpart',# Use the 'random forest' algorithm
                        trControl = caret::trainControl(method = 'cv', # Use cross-validation
                                                        number = 10) # Use 5 folds for cross-validation
  )
  
  MM13= matrix(MM[MM[,7]==2&
                    ((MM[,4]==1&MM[,5]==3)|
                       (MM[,4]==3&MM[,5]==1))],
               ncol=7,byrow = F)
  t1=apply(MM13[,1:3], 1, min)
  k1=MM13[,4]
  MM13.1=matrix(c(t1,k1),ncol=2,byrow = F)
  colnames(MM13.1)=c("t1","k1")
  test13=data.frame(MM13.1)
  test13$k1=as.factor(test13$k1)
  
  prrpart=predict(fitrpart,test13,type = "raw")
  
  MM[,4][MM[,7]==2 & ((MM[,4]==1&MM[,5]==3)|
                        (MM[,4]==3&MM[,5]==1)) ]=prrpart
  
  
  #r=2 and k1 and k2= 3 or 2
  MM23= matrix(MM[MM[,7]==1&
                    ((MM[,4]==2)|
                       (MM[,4]==3))],
               ncol=7,byrow = F)
  t1=apply(MM23[,1:3], 1, min)
  k1=MM23[,4]
  MM23.1=matrix(c(t1,k1),ncol=2,byrow = F)
  colnames(MM23.1)=c("t1","k1")
  train23=data.frame(MM23.1)
  train23$k1=as.factor(train23$k1)
  
  fitrpart <- caret::train(k1~t1, # Survived is a function of the variables we decided to include
                        data = train23, # Use the train data frame as the training data
                        method = 'rpart',# Use the 'random forest' algorithm
                        trControl = caret::trainControl(method = 'cv', # Use cross-validation
                                                        number = 10) # Use 5 folds for cross-validation
  )
  
  
  MM23= matrix(MM[MM[,7]==2&
                    ((MM[,4]==2&MM[,5]==3)|
                       (MM[,4]==3&MM[,5]==2))],
               ncol=7,byrow = F)
  t1=apply(MM23[,1:3], 1, min)
  k1=MM23[,4]
  MM23.1=matrix(c(t1,k1),ncol=2,byrow = F)
  colnames(MM23.1)=c("t1","k1")
  test23=data.frame(MM23.1)
  test23$k1=as.factor(test23$k1)
  
  prrpart=predict(fitrpart,test23,type = "raw")
  
  MM[,4][MM[,7]==2 & ((MM[,4]==2&MM[,5]==3)|
                        (MM[,4]==3&MM[,5]==2)) ]=prrpart
  
  t=apply(MM[,1:3],1,min)
  k=MM[,4]
  
  
  ###MLEcart
  z1=c()
  like=function(rr){
    b1=rr[1];b2=rr[2];b3=rr[3]
    mu1=rr[4];mu2=rr[5];mu3=rr[6]
    
    
    for(i in 1:n) {
      
      
      if(k[i]==1){
        
        z1[i]=dweibull(t[i],b1,mu1)*(1-pweibull(t[i],b2,mu2))*(1-pweibull(t[i],b3,mu3))}
      
      if(k[i]==2){ 
        
        z1[i]=dweibull(t[i],b2,mu2)*(1-pweibull(t[i],b1,mu1))*(1-pweibull(t[i],b3,mu3))}
      
      if(k[i]==3){ 
        
        z1[i]=dweibull(t[i],b3,mu3)*(1-pweibull(t[i],b1,mu1))*(1-pweibull(t[i],b2,mu2))}
    }
    return(-sum(log(z1)))
  }
  init=c(rep(0.5,6))
  tic()
  maxcart=nlminb(init,like,lower=c(0,0,0,0,0,0),upper=c(rep(Inf,6)))
  toc()
  ################################### Naive Bayes
  
  #r=3
  MM=MM.gen
  MM1=matrix(MM[MM[,7]==1],ncol=7,byrow = F)
  
  t=apply(MM1[,1:3],1,min)
  k=MM1[,4]
  M=matrix(c(t,k),ncol=2,byrow = F)
  colnames(M)=c("t","k")
  train=data.frame(M)
  train$k=as.factor(train$k)
  
  fitnb <- caret::train(k~t, # Survived is a function of the variables we decided to include
                        data = train, # Use the train data frame as the training data
                        method = 'nb',# Use the 'random forest' algorithm
                        trControl = caret::trainControl(method = 'cv', # Use cross-validation
                                                        number = 10) # Use 5 folds for cross-validation
  ) #End 
  
  #summary(fit0)
  #fit0
  
  MM2=matrix(MM[MM[,7]==3],ncol=7,byrow = F)
  
  t.test=apply(MM2[,1:3],1,min)
  k.test=MM2[,4]
  M.test=matrix(c(t.test,k.test),ncol=2,byrow = F)
  colnames(M.test)=c("t","k")
  test=data.frame(M.test)
  test$k=as.factor(test$k)
  
  
  
  
  prnb=predict(fitnb,test,type = "raw")
  #confusionMatrix(prnb,test$k)
  
  
  MM[,4][MM[,7]==3]=prnb
  
  #r=2
  #r=2 and k1 and k2= 1 or 2
  #train
  MM12= matrix(MM[MM[,7]==1&
                    ((MM[,4]==1)|
                       (MM[,4]==2))],
               ncol=7,byrow = F)
  t1=apply(MM12[,1:3], 1, min)
  k1=MM12[,4]
  MM12.1=matrix(c(t1,k1),ncol=2,byrow = F)
  colnames(MM12.1)=c("t1","k1")
  train12=data.frame(MM12.1)
  train12$k1=as.factor(train12$k1)
  
  fitnb <- caret::train(k1~t1, # Survived is a function of the variables we decided to include
                        data = train12, # Use the train data frame as the training data
                        method = 'nb',# Use the 'random forest' algorithm
                        trControl = caret::trainControl(method = 'cv', # Use cross-validation
                                                        number = 10) # Use 5 folds for cross-validation
  ) #End 
  
  
  #test
  MM12= matrix(MM[MM[,7]==2&
                    ((MM[,4]==1&MM[,5]==2)|
                       (MM[,4]==2&MM[,5]==1))],
               ncol=7,byrow = F)
  t1=apply(MM12[,1:3], 1, min)
  k1=MM12[,4]
  MM12.1=matrix(c(t1,k1),ncol=2,byrow = F)
  colnames(MM12.1)=c("t1","k1")
  test12=data.frame(MM12.1)
  test12$k1=as.factor(test12$k1)
  
  prnb=predict(fitnb,test12,type = "raw")
  
  MM[,4][MM[,7]==2 & ((MM[,4]==1&MM[,5]==2)|
                        (MM[,4]==2&MM[,5]==1)) ]=prnb
  
  
  #r=2 and k1 and k2= 3 or 1
  
  
  MM13= matrix(MM[MM[,7]==1&
                    ((MM[,4]==1)|
                       (MM[,4]==3))],
               ncol=7,byrow = F)
  t1=apply(MM13[,1:3], 1, min)
  k1=MM13[,4]
  MM13.1=matrix(c(t1,k1),ncol=2,byrow = F)
  colnames(MM13.1)=c("t1","k1")
  train13=data.frame(MM13.1)
  train13$k1=as.factor(train13$k1)
  
  fitnb <- caret::train(k1~t1, # Survived is a function of the variables we decided to include
                        data = train13, # Use the train data frame as the training data
                        method = 'nb',# Use the 'random forest' algorithm
                        trControl = caret::trainControl(method = 'cv', # Use cross-validation
                                                        number = 10) # Use 5 folds for cross-validation
  )
  
  MM13= matrix(MM[MM[,7]==2&
                    ((MM[,4]==1&MM[,5]==3)|
                       (MM[,4]==3&MM[,5]==1))],
               ncol=7,byrow = F)
  t1=apply(MM13[,1:3], 1, min)
  k1=MM13[,4]
  MM13.1=matrix(c(t1,k1),ncol=2,byrow = F)
  colnames(MM13.1)=c("t1","k1")
  test13=data.frame(MM13.1)
  test13$k1=as.factor(test13$k1)
  
  prnb=predict(fitnb,test13,type = "raw")
  
  MM[,4][MM[,7]==2 & ((MM[,4]==1&MM[,5]==3)|
                        (MM[,4]==3&MM[,5]==1)) ]=prnb
  
  
  #r=2 and k1 and k2= 3 or 2
  MM23= matrix(MM[MM[,7]==1&
                    ((MM[,4]==2)|
                       (MM[,4]==3))],
               ncol=7,byrow = F)
  t1=apply(MM23[,1:3], 1, min)
  k1=MM23[,4]
  MM23.1=matrix(c(t1,k1),ncol=2,byrow = F)
  colnames(MM23.1)=c("t1","k1")
  train23=data.frame(MM23.1)
  train23$k1=as.factor(train23$k1)
  
  fitnb <- caret::train(k1~t1, # Survived is a function of the variables we decided to include
                        data = train23, # Use the train data frame as the training data
                        method = 'nb',# Use the 'random forest' algorithm
                        trControl = caret::trainControl(method = 'cv', # Use cross-validation
                                                        number = 10) # Use 5 folds for cross-validation
  )
  
  
  MM23= matrix(MM[MM[,7]==2&
                    ((MM[,4]==2&MM[,5]==3)|
                       (MM[,4]==3&MM[,5]==2))],
               ncol=7,byrow = F)
  t1=apply(MM23[,1:3], 1, min)
  k1=MM23[,4]
  MM23.1=matrix(c(t1,k1),ncol=2,byrow = F)
  colnames(MM23.1)=c("t1","k1")
  test23=data.frame(MM23.1)
  test23$k1=as.factor(test23$k1)
  
  prnb=predict(fitnb,test23,type = "raw")
  
  MM[,4][MM[,7]==2 & ((MM[,4]==2&MM[,5]==3)|
                        (MM[,4]==3&MM[,5]==2)) ]=prnb
  
  t=apply(MM[,1:3],1,min)
  k=MM[,4]
  
  ###MLEnb
  z1=c()
  like=function(rr){
    b1=rr[1];b2=rr[2];b3=rr[3]
    mu1=rr[4];mu2=rr[5];mu3=rr[6]
    
    
    for(i in 1:n) {
      
      
      if(k[i]==1){
        
        z1[i]=dweibull(t[i],b1,mu1)*(1-pweibull(t[i],b2,mu2))*(1-pweibull(t[i],b3,mu3))}
      
      if(k[i]==2){ 
        
        z1[i]=dweibull(t[i],b2,mu2)*(1-pweibull(t[i],b1,mu1))*(1-pweibull(t[i],b3,mu3))}
      
      if(k[i]==3){ 
        
        z1[i]=dweibull(t[i],b3,mu3)*(1-pweibull(t[i],b1,mu1))*(1-pweibull(t[i],b2,mu2))}
    }
    return(-sum(log(z1)))
  }
  init=c(rep(0.5,6))
  tic()
  maxnb=nlminb(init,like,lower=c(0,0,0,0,0,0),upper=c(rep(Inf,6)))
  toc()
  ############################ glm
 
  ##################Accuracy 
  # summarize accuracy of models
  results <- resamples(list(RF=fitRF, SVM=fitsvm, knn=fitknn, LDA=fitlda, CART=fitrpart, NB=fitnb))
  #summary(results)
  # compare accuracy of models
  #dotplot(results)
  
  accuracy[g,] = apply(summary(results)[1]$values, 2  , mean)
  
  
  
  
  
  ############################### time and cause Dependent
  t=apply(MM.gen[,1:3],1,min);k1=MM.gen[,4];k2=MM.gen[,5]
  k3=MM.gen[,6];r=MM.gen[,7]
  z1=c()
  like=function(rr){
    b1=rr[1];b2=rr[2];b3=rr[3]
    mu1=rr[4];mu2=rr[5];mu3=rr[6]
    
    b11=rr[7];b12=rr[8]
    b21=rr[9];b22=rr[10]
    b31=rr[11];b32=rr[12]
    
    for(i in 1:n) {
      if(r[i]==1){
        
        if(k1[i]==1){
          p11[i]=exp(b11+b*t[i])/(1+exp(b11+b*t[i])+2*exp(b12+b*t[i]))
          z1[i]=dweibull(t[i],b1,mu1)*p11[i]*(1-pweibull(t[i],b2,mu2))*(1-pweibull(t[i],b3,mu3))}
        
        if(k1[i]==2){ 
          p12[i]=exp(b21+b*t[i])/(1+exp(b21+b*t[i])+2*exp(b22+b*t[i]))
          z1[i]=dweibull(t[i],b2,mu2)*p12[i]*(1-pweibull(t[i],b1,mu1))*(1-pweibull(t[i],b3,mu3))}
        
        if(k1[i]==3){ 
          p13[i]=exp(b31+b*t[i])/(1+exp(b31+b*t[i])+2*exp(b32+b*t[i]))
          z1[i]=dweibull(t[i],b3,mu3)*p13[i]*(1-pweibull(t[i],b1,mu1))*(1-pweibull(t[i],b2,mu2))}
      }
      
      if(r[i]==2){
        
        if(k1[i]==1){
          p21[i]=exp(b12+b*t[i])/(1+exp(b11+b*t[i])+2*exp(b12+b*t[i]))
          
          if(k2[i]==2){z1[i]=dweibull(t[i],b1,mu1)*(p21[i])*(1-pweibull(t[i],b2,mu2))*(1-pweibull(t[i],b3,mu3))+
                             dweibull(t[i],b2,mu2)*(p21[i])*(1-pweibull(t[i],b1,mu1))*(1-pweibull(t[i],b3,mu3))}
          
          if(k2[i]==3){z1[i]=dweibull(t[i],b1,mu1)*(p21[i])*(1-pweibull(t[i],b2,mu2))*(1-pweibull(t[i],b3,mu3))+
                             dweibull(t[i],b3,mu3)*(p21[i])*(1-pweibull(t[i],b1,mu1))*(1-pweibull(t[i],b3,mu3))}
        }
        if(k1[i]==2){
          p22[i]=exp(b22+b*t[i])/(1+exp(b21+b*t[i])+2*exp(b22+b*t[i]))
          
          if(k2[i]==1){z1[i]=dweibull(t[i],b1,mu1)*(p22[i])*(1-pweibull(t[i],b2,mu2))*(1-pweibull(t[i],b3,mu3))+
                             dweibull(t[i],b2,mu2)*(p22[i])*(1-pweibull(t[i],b1,mu1))*(1-pweibull(t[i],b3,mu3))}
          
          if(k2[i]==3){z1[i]=dweibull(t[i],b2,mu2)*(p22[i])*(1-pweibull(t[i],b1,mu1))*(1-pweibull(t[i],b3,mu3))+
                             dweibull(t[i],b3,mu3)*(p22[i])*(1-pweibull(t[i],b1,mu1))*(1-pweibull(t[i],b2,mu2))}
        }
        if(k1[i]==3){
          p23[i]=exp(b32+b*t[i])/(1+exp(b31+b*t[i])+2*exp(b32+b*t[i]))
          
          if(k2[i]==1){z1[i]=dweibull(t[i],b1,mu1)*(p23[i])*(1-pweibull(t[i],b2,mu2))*(1-pweibull(t[i],b3,mu3))+
                             dweibull(t[i],b3,mu3)*(p23[i])*(1-pweibull(t[i],b1,mu1))*(1-pweibull(t[i],b3,mu3))}
          if(k2[i]==2){z1[i]=dweibull(t[i],b2,mu2)*(p23[i])*(1-pweibull(t[i],b1,mu1))*(1-pweibull(t[i],b3,mu3))+
                             dweibull(t[i],b3,mu3)*(p23[i])*(1-pweibull(t[i],b1,mu1))*(1-pweibull(t[i],b2,mu2))}
        }
      }
      
      if(r[i]==3){
        
        
          p31[i]=              1/(1+exp(b11+b*t[i])+2*exp(b12+b*t[i]))
          p32[i]=              1/(1+exp(b21+b*t[i])+2*exp(b22+b*t[i]))
          p33[i]=              1/(1+exp(b31+b*t[i])+2*exp(b32+b*t[i]))
          
          z1[i]=dweibull(t[i],b1,mu1)*(p31[i])*(1-pweibull(t[i],b2,mu2))*(1-pweibull(t[i],b3,mu3))+
                dweibull(t[i],b2,mu2)*(p32[i])*(1-pweibull(t[i],b1,mu1))*(1-pweibull(t[i],b3,mu3))+
                dweibull(t[i],b3,mu3)*(p33[i])*(1-pweibull(t[i],b1,mu1))*(1-pweibull(t[i],b2,mu2))
        
        
       
        
          
      }
      
    }
    return(-sum(log(z1)))
  }
  init=c(rep(0.5,12))
  tic()
  maxML=nlminb(init,like,lower=c(0,0,0,0,0,0,rep(-Inf,6)),upper=c(rep(Inf,12)))
  toc()
  
   max_RF[g,]=maxRF$par
 max_svm[g,]=maxsvm$par
 max_lda[g,]=maxlda$par
 max_nb[g,]=maxnb$par
 max_knn[g,]=maxknn$par
 max_cart[g,]=maxcart$par
 max_ML[g,]=maxML$par[1:6]
 
 
}
OUT=list(max_RF,max_svm,max_knn,max_lda,max_cart,max_nb,max_ML,
         pk1,pk2,pk3,percentR1,percentR2,percentR3,percentR2k1,percentR2k2,percentR2k3,
         percentR3k1,percentR3k2,percentR3k3,
         accuracy)

return(OUT)
}

n=500;J=3

mu1=2;mu2=5;mu3=2.5
b1= 1.5;b2=2;b3=1.2

b11=0.2;b12=0.5
b21=0.5;b22=0.8
b31=0.8;b32=0.1

b=0.5


iter=1
system.time(
  results <- parallel::mclapply(iter, MLe, mc.cores = 1)
)

ML=matrix(0,nrow = 7,ncol=6)
ML=rbind(ML,c(b1,b2,b3,mu1,mu2,mu3))
row.names(ML)=c("max_RF","max_svm","max_knn","max_lda","max_cart","max_nb","max_ML","True.Val")
colnames(ML) = c("b1","b2","b3","mu1","mu2","mu3")

percent=matrix(0,nrow = 1, ncol = 12)
rownames(percent)="percent"
colnames(percent)=c("pk1","pk2","pk3","percentR1","percentR2","percentR3","percentR2k1","percentR2k2","percentR2k3",
                    "percentR3k1","percentR3k2","percentR3k3")


for(i in 1:7){
  ML[i,]=apply(results[[1]][[i]],2,mean)
}
for (i in 1:12) {
  percent[1,i]=mean(results[[1]][[7+i]])
}

print(ML)
print(percent)

accurm=apply(results[[1]][[20]], 2, mean, na.rm =T)
accurs=apply(results[[1]][[20]], 2, sd, na.rm =T)

dotplot(c(accurm+c(-1)*1.96*accurs,accurm, accurm+c(+1)*1.96*accurs),xlab = "Confidence Level: 0.95")


