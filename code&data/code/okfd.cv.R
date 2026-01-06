#####################################################################
# This function allows to carry out cross-validation analysis
# for ordinary kriging for function-value data by considering a 
# Fourier basis for smoothing the observed data set
####################################################################

okfd.cv <- function(coord, data, argvals, array.nbasis=array.nbasis,
                 max.dist.variogram=NULL,nugget.fix=NULL)
{
   n <- dim(data)[1]
   diff.argvals <- diff(argvals)
   s <- dim(coord)[1]
   MSE.CV <- MSE.CV <- matrix(0,nrow=length(array.nbasis))
   krig.CV <- array(0,dim=c(length(array.nbasis),n,s))
                      
   k<-0
   k.opt<-1
   for(nbasis.k in array.nbasis)
       {
         k<-k+1
         for (i in 1:s)
              {
               print(paste("k= ",nbasis.k,"; i= ",i))
               okfd.res<-okfd(coord[-i,],data[,-i],argvals,nbasis=nbasis.k,
                              new.coord=matrix(coord[i,],nrow=1,ncol=2))
               
               krig.CV[k,,i]<-okfd.res$krig.new.data
               aux <- (data[,i]-okfd.res$krig.new.data)^2
               aux <- diff.argvals * (aux[1:(n-1)]+aux[2:n])/2
               MSE.CV[k]<-MSE.CV[k]+sum(aux)
               }
        if (MSE.CV[k]<=MSE.CV[k.opt]) k.opt<-k
        }
      MSE.CV.opt<-MSE.CV[k.opt]
return(list(k.opt=k.opt,krig.CV=krig.CV,MSE.CV=MSE.CV,MSE.CV.opt=MSE.CV.opt))
}