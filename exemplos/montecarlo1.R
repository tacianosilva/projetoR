require(MonteCarlo)

test_func<-function(n,loc,scale){
  sample<-rnorm(n, loc, scale)
  stat<-sqrt(n)*mean(sample)/sd(sample)
  decision<-abs(stat)>1.96
  return(list("decision"=decision))
}

n_grid<-c(50,100,250,500)

loc_grid<-seq(0,1,0.2)
scale_grid<-c(1,2)

param_list=list("n"=n_grid, "loc"=loc_grid, "scale"=scale_grid)
erg<-MonteCarlo(func=test_func, nrep=250, param_list=param_list, ncpus=1)
str(erg)

rows<-c("n")
cols<-c("loc","scale")
MakeTable(output=erg, rows=rows, cols=cols, digits=2)