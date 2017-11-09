require(MonteCarlo)

# Função que gera dados para o método estatístico t-test
test_func<-function(n,loc,scale) {
  
  # Gerando amostra normal (n: número de observações, loc: média, scale: descio padrão)
  sample<-rnorm(n, loc, scale)
  
  # Calculo do teste estatístico (t-test)
  stat<-sqrt(n)*mean(sample)/sd(sample)
  
  # get test decision
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
