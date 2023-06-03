##################################################################
#                                                                #
# Desenvolvedor: THIAGO ARA?JO DOS SANTOS                        #
#                                                                #
# contact: thiagosantosac96@outlook.com                          #
#                                                                #
##################################################################

#----------------------------------------------------------------#
# Reiniciando sessão R                                                                       
# ---------------------------------------------------------------#
  rm(list=ls())
  graphics.off()
  options(warn=0)
  .rs.restartR()
  
#----------------------------------------------------------------#
# Checando pacotes necessários                                             
# ---------------------------------------------------------------#
  .packages = c("EBImage")
  .inst <- .packages %in% installed.packages()
  lapply(.packages, require, character.only=TRUE)

#----------------------------------------------------------------#
# Informando o diretório de trabalho                                                                       
# ---------------------------------------------------------------#
  setwd("C:/Users/Usuário/OneDrive/UFAC/RScripts/Análise foliar/Área foliar/Área foliar afetada")
  getwd()
  dir()

#----------------------------------------------------------------#
# Carregando imagens                                                                            
# ---------------------------------------------------------------#
  f0 <- F0 <- readImage("folha.jpg")
  ref <- readImage("referencia.jpg")
  rd <- 2000              ## Redimensionamento, largura da imagem
  f0 <- resize(F0, h=rd)
  display(f0,method=c("raster"))
  
#----------------------------------------------------------------#
# Adicionando os atributos a serem calculados                                                         
# ---------------------------------------------------------------#
  af <- c()              ## Área foliar em pixels
  al <- c()              ## Área foliar afetada em pixels
  p.aln <- c()           ## % de área foliar afetada 
  tempo <- c()
  j <- 0

#----------------------------------------------------------------#
# Informando variáveis padronizadas                                                 
# ---------------------------------------------------------------#
  ch   <- 'green' ## Canal de cor da imagem (verde)
  gf   <- 2.0     ## Fator de correção gama para a folha/fundo
  gm   <- 0.3     ## Fator de correção gama para a mancha/fundo
  aref <- 1       ## Área conhecida da imagem "ref" em cm2
  
#----------------------------------------------------------------#
# Determinando área foliar                                                        
# ---------------------------------------------------------------#
  jpeg(filename = "Figura1.jpeg",res = 300) ## Salvando foto 
  display(f0, method=c("raster"))       
  f.f <- channel(f0, ch) ^ gf       
  display(f.f, method=c("raster")) 

#----------------------------------------------------------------#
# Separando a folha a ser analisada do fundo                                                        
# ---------------------------------------------------------------#
  bf <- otsu (f.f)       ## Limiarização
  f.f.sep <- f.f
  f.f.sep[f.f> bf] <- 0  ## Preto
  f.f.sep[f.f<=bf] <- 1  ## Branco
  display(f.f.sep, method=c("raster")) 

#----------------------------------------------------------------#
# Limpando possíveis manchas no interior da folha                                                        
# ---------------------------------------------------------------#
  f.f.sep <- fillHull(f.f.sep)
  display(f.f.sep, method=c("raster"))
  folha <- computeFeatures.shape(f.f.sep);folha #calcula a área da folha
       
  dev.off() 

#----------------------------------------------------------------#
# Criando uma imagem sem fundo                                                       
# ---------------------------------------------------------------#       
  f.sf <- f0
  f.sf[f.f.sep == 0] <- NA  ## Preenche o fundo com NA (sem valor)
  display(f.sf, method=c("raster"))

#----------------------------------------------------------------#
# Determinando área foliar afetada                                                       
# ---------------------------------------------------------------#       
  jpeg(filename = "Figura2.jpeg",res = 300)
  par(mfrow=c(2,2))
  display(f0, method=c("raster"))
  f.m <- channel(f0, ch) ^ gm
  display(f.m, method=c("raster"))
  
#----------------------------------------------------------------#
# Separando área afetada da folha                                                     
# ---------------------------------------------------------------# 
  bm <- otsu (channel(f.sf, ch)^gm)
  f.m.sep <- f.m
  f.m.sep[f.m>bm] <- 0
  f.m.sep[f.m<=bm] <- 1
  display(f.m.sep,method=c("raster"))
  mancha <- computeFeatures.shape(f.m.sep);mancha

#----------------------------------------------------------------#
# Calculando área de cada mancha                                                     
# ---------------------------------------------------------------#        
  mancha.sep <- computeFeatures.shape(bwlabel(f.m.sep));mancha.sep
  display(f.f.sep-f.m.sep, method=c("raster"))
  dev.off()

#----------------------------------------------------------------#
# Calculando área afetada em %                                                      
# ---------------------------------------------------------------#
  af <- c(folha[1])      ## Área foliar em pixels        
  al <- c(mancha[1])     ## Área afetada em pixels          
  p.aln <- c(mancha[1]/folha[1]*100)  ## Área afetada em %
       
  dados <- data.frame(af,al,
                      p.aln);
                dados  ## Adicionando informações em um data frame   

#----------------------------------------------------------------#
# Transformando valores para cm2                                                      
# ---------------------------------------------------------------#  
  display(ref, method=c("raster"))
  display(ref@.Data[,,1]) 
       
  ref2=ref@.Data[,,1]>0.35
  display(ref2, method=c("raster"))
  ref3=fillHull(ref2)
  display(ref3, method=c("raster"))
       
  ref<-computeFeatures.shape(ref3)
       
  refe<-c(ref[1])
       
  dados <- data.frame(af,
                      al,p.aln,
                     refe)  ## Adicionando valores em um data frame
       
  afcm <- c(af*aref/refe)   ## Convertendo área foliar           
  alcm <- c(al*aref/refe)   ## Convertendo área afetada
  
  dados2 <- data.frame(af,al,afcm,alcm, p.aln)
  colnames(dados2)=c("Área foliar (Px)",  ## Renomeando colunas
                     "Área afetada (Px)",
                        "Área foliar (cm2)",
                     "Área afetada (cm2)",
                     "Área afetada (%)")
  
   dados2 = round(dados2, 1) ## Padronizando casas decimais do data fram  
   