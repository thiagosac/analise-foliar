##################################################################
#                                                                #
# Desenvolvedor: THIAGO ARA?JO DOS SANTOS                        #
#                                                                #
# Contato: thiagosantosac96@outlook.com                          #
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
  .packages = c("EBImage", "DT")
  .inst <- .packages %in% installed.packages()
  lapply(.packages, require, character.only=TRUE)

#----------------------------------------------------------------#
# Informando o diretório de trabalho                                                                       
# ---------------------------------------------------------------#
  setwd("C:/Users/Usuário/OneDrive/UFAC/RScripts/Análise foliar/Área foliar/Área foliar")
  getwd()
  dir()

#----------------------------------------------------------------#
# Carregando imagens & Informando paleta de cores                                                                            
# ---------------------------------------------------------------#
  imagem <- readImage("foto1.jpg")
  display(imagem,method=c("raster"))
  
  fundo <- readImage("fundo.jpg")
  display(fundo,method=c("raster"))
  
  folha <- readImage("folha.jpg")
  display(folha,method=c("raster"))
  
  ref <- readImage("ref.jpg")
  display(ref,method=c("raster"))
  
#----------------------------------------------------------------#
# Isolando os folíolos do restante da imagem                                                        
# ---------------------------------------------------------------#
  tiff("Figura1.tiff", width = 4, height = 4, units = 'in', res = 300) ## Salvando foto
  folhas.seg=segmentation_logit(imagem,foreground=folha,
                              background=list(fundo,ref),
                              sample=2000,fillHull=TRUE,plot=TRUE)
  dev.off()

#----------------------------------------------------------------#
# Isolando a imagem referência do restante da imagem                                                 
# ---------------------------------------------------------------#
  tiff("Figura2.tiff", width = 4, height = 4, units = 'in', res = 300) ## Salvando foto
  ref.seg=segmentation_logit(imagem,foreground=ref,
                             background=list(fundo,folha),
                             sample=2000,fillHull=TRUE,plot=TRUE)
  dev.off()
  
#----------------------------------------------------------------#
# Estimando medidas em pixels                                                       
# ---------------------------------------------------------------#
  medidas=measure_image(folhas.seg,noise = 1000)
  medidas
  
#----------------------------------------------------------------#
# Convertendo medidas para cm² a partir da imagem referência (ref)                                                       
# ---------------------------------------------------------------#
  medidasref=measure_image(img = folhas.seg,noise =1000,
                           id=ref.seg,length =1,width =1 )
  medidasref

#----------------------------------------------------------------#
# Adicionando valores em uma tabela para melhor visualização                                                       
# ---------------------------------------------------------------#    
  tiff("Figura4.tiff", width = 4, height = 4, units = 'in', res = 300) ## Salvando foto
  datatable(medidasref[["measures"]])
  dev.off()
    
#----------------------------------------------------------------#
# Adicionando área de cada foliolo em cm²                                                    
# ---------------------------------------------------------------#
  tiff("Figura3.tiff", width = 4, height = 4, units = 'in', res = 300) ## Salvando foto
  plot(imagem) 
  text(medidasref$measures[,1],medidasref$measures[,2],
       round(medidasref$measures[,3],1), col="red",cex=1, font=2)
  dev.off()

    
  
  
  
