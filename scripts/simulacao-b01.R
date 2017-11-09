library(MonteCarlo)
library(rstudioapi)

diretorio_atual <- dirname(rstudioapi::getActiveDocumentContext()$path)
workdir <- dirname(diretorio_atual)
sample_b01 = read.csv(paste(workdir, '/entradas/B01.csv', sep = ""))

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
  
# Amostra de Bairro por Turno
b01_t1 = sample_b01$Turno.1
n = length(sample_b01$Turno.1)

hist(b01_t1)
  
# Calculo da média e do desvio padrão do turno 1
media <- mean(b01_t1)
desvio_padrao <- sd(b01_t1)
  
n_grid<-c(n/4, n/2, n)

loc_grid<-seq(0,1,0.25)

scale_grid<-c(1,2)

param_list=list("n"=n_grid, "loc"=loc_grid, "scale"=scale_grid)
erg<-MonteCarlo(func=test_func, nrep=1000, param_list=param_list, ncpus=1)
str(erg)

rows<-c("n")
cols<-c("loc","scale")
MakeTable(output=erg, rows=rows, cols=cols, digits=2)
