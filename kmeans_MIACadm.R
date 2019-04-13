##Kmeans algorithm Source Code by MIACadm
#--------------------------
#sample data:iris 
#--------------------------
kmeans_SC = function(data_input,Num_of_groups)
{
  data =data_input
  N=Num_of_groups
  centers_unif = data[sample(N),]+replicate(N,rnorm(N,mean = 0,0.003))
  
  #euclidean
  for (i in 1:nrow(data)) {
    for(j in 1:N){
      if(i==1 & j==1){
        euc_int=(sqrt(sum((data[i,]-centers_unif[j,])^2)))
      }
      else{
        euc_int = c(euc_int,sqrt(sum((data[i,]-centers_unif[j,])^2)))  
      }  
    }
  }
  euc_int
  
  fir = which(euc_int[1:N]==min(euc_int[1:N]))
  for (x in seq(1+N,length(euc_int)-N+1,N) ){
    fir = c(fir,which(euc_int[x:(x+2)]==min(euc_int[x:(x+2)])))
  }
  data_newGF = data
  data_newGF$GroupFlag = fir 
  
  centers=colMeans(data_newGF[data_newGF$GroupFlag==1,])
  for(i in 2:N){
    centers = rbind(centers,colMeans(data_newGF[data_newGF$GroupFlag==i,]))
    centers[is.nan(centers)]=0}
  centers = data.frame(centers)
  print(centers)
  centers=centers[,-5]
  
  #1.euclidean 2.flag 3.mean
  cnt=0
  while(cnt<20)
  { #1.
    for (i in 1:nrow(data)) {
      for(j in 1:N){
        if(i==1 & j==1){
          euc_int=(sqrt(sum((data[i,]-centers[j,])^2)))
        }
        else{
          euc_int = c(euc_int,sqrt(sum((data[i,]-centers[j,])^2)))  
        }  
      }
    }
    euc_int
    
    #2.
    fir = which(euc_int[1:N]==min(euc_int[1:N]))
    for (x in seq(1+N,length(euc_int)-N+1,N) ){
      fir = c(fir,which(euc_int[x:(x+2)]==min(euc_int[x:(x+2)])))
    }
    data_newGF = data
    data_newGF$GroupFlag = fir
    
    #3.
    centers_new=colMeans(data_newGF[data_newGF$GroupFlag==1,])
    par(bg='#FCFCFC',cex=1)
    plot(data_newGF$Petal.Length,data_newGF$Petal.Width,col="#81D8D0",
         pch=20, xlim = c(0,10), ylim =c(0,3),
         xlab = "X:Petal Length",ylab="Y:Petal Width") #feel free to change axis name ^^
    title("Kmeans Algorithm")
    col.vector=c("#FFA5D3","#EEC591","#CDAA7D","#EEE685","#9A32CD")
    for(i in 2:N){
      points(data_newGF[data_newGF$GroupFlag==i,]$Petal.Length,
             data_newGF[data_newGF$GroupFlag==i,]$Petal.Width, 
             col=col.vector[i-1] , pch=20)
      centers_new = rbind(centers_new,colMeans(data_newGF[data_newGF$GroupFlag==i,]))
      centers_new[is.nan(centers_new)]=0}
    centers_new = data.frame(centers_new)
    print(centers_new) 
    centers_new=centers_new[,-5]
    
    centers=centers_new
    cnt=cnt+1
  }
  centers.final <<- centers
  data_newGF.final <<- data_newGF
  GroupStat.final<<- data.frame(table(data_newGF.final$GroupFlag))
  colnames(GroupStat.final)[1:2]=c('Group','Numbers')
  print(GroupStat.final)
  
}

data <- iris[, -5]
kmeans_SC(data,3)

