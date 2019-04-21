# Clustering jerarquico Votacion Nominal
#
# Alejandro Baranek @kenarab 130908
#
#install.packages("ape")
#library(ape)

#install.packages("geiger")
#library(geiger)

futile.logger::flog.info("Not migrated to open data source yet")

if (FALSE){

  home<-"path/to/data"
  homeData<-paste(home,"data/",sep="")
  output.dir <- "inst/extdata/"


  tamanioTortaPhylo<-400
  colorDefault<-"black"

  factorImagen<-2.5
  heightPNG<-720*factorImagen
  widthPNG<-1380*factorImagen

  cantidadClustersDiputados<-4

  #leo datos
  asuntos<-read.csv2(paste(homeData,'asuntos.csv',sep=''),sep=',')
  diputados<-read.csv2(paste(homeData,'diputados.csv',sep=''),sep=',')
  bloques<-read.csv2(paste(homeData,'bloques.csv',sep=''),sep=',')
  votaciones<-read.csv2(paste(homeData,'votaciones.csv',sep=''),sep=',')

  votaciones$ano<-
    asuntos[match(votaciones$asuntoId,asuntos$asuntoId),]$ano
  votaciones$fecha<-
    as.Date(asuntos[match(votaciones$asuntoId,asuntos$asuntoId),]$fecha,format="%m/%d/%Y")

  #preprocesamiento
  #normalizo el nombre de los campos
  colnames(diputados)[1]<-"diputadoId"

  #abrevio algunos bloques
  bloques$bloqueAbreviado<-""

  bloques[nchar(as.character(bloques$color))>0,]
  #1           1                              A.R.I #2ca02c
  bloques[1,]$bloqueAbreviado<-"ARI"
  #2           2                 A.R.I Autónomo 8 + #2ca02c
  bloques[2,]$bloqueAbreviado<-"ARI"
  #  15         15                   Coalición Cívica #2ca02c
  bloques[15,]$bloqueAbreviado<-"CC"
  #16         16             Coalición Cívica - ARI #2ca02c
  bloques[16,]$bloqueAbreviado<-"CC-ARI"
  #17         17 Coalición Cívica - ARI - GEN - UPT #2ca02c
  bloques[17,]$bloqueAbreviado<-"CC-ARI-GEN"
  #56         56       Frente para la Victoria - PJ #1f77b4
  bloques[56,]$bloqueAbreviado<-"FPV"
  #58         58                   Frente Peronista #0b615e
  bloques[58,]$bloqueAbreviado<-"FP"
  #74         74                      Justicialista #1f77b4
  bloques[74,]$bloqueAbreviado<-"Justicialista"
  #104       104                 Partido Socialista #fa58f4
  bloques[104,]$bloqueAbreviado<-"PS"
  #116       116                                PRO #e7ba52
  bloques[116,]$bloqueAbreviado<-"PRO"
  #118       118              Propuesta Republicana #e7ba52
  bloques[118,]$bloqueAbreviado<-"PRO"
  #147       147               Unión Cívica Radical #d62728
  bloques[147,]$bloqueAbreviado<-"UCR"
  levels(bloques$color) <- c(levels(bloques$color), "#ffffff")

  bloques[nchar(as.character(bloques$color))==0,]$color<-"#ffffff"


  bloques[nchar(bloques$bloqueAbreviado)==0,]$bloqueAbreviado<-as.character(bloques$bloque)[nchar(bloques$bloqueAbreviado)==0]



  diputadosAgrupados<-NULL
  periodos<-sort(unique(votaciones$ano))
  periodoActual<-periodos[1]
  for (periodoActual in periodos){
    # 0 = Afirmativo, 1 = Negativo, 2 = Abstención, 3 = Ausente
    votacionesPeriodoActual<-votaciones[votaciones$ano==periodoActual,]
    print(paste(date(), "periodoactual",periodoActual))
    #Construyo bloques candidatos para cada diputado
    diputadosPeriodoActual<-diputados[match(sort(unique(votacionesPeriodoActual$diputadoId)),diputados$diputadoId),]
    bloquesCandidatosCant<-aggregate(rep(1,nrow(votacionesPeriodoActual)),by=list(bloqueId=votacionesPeriodoActual$bloqueId,diputadoId=votacionesPeriodoActual$diputadoId),FUN=sum)
    names(bloquesCandidatosCant)[3]<-"sesiones"
    bloquesCandidatosFechaMax<-aggregate(votacionesPeriodoActual$fecha,by=list(bloqueId=votacionesPeriodoActual$bloqueId,diputadoId=votacionesPeriodoActual$diputadoId),FUN=max)
    names(bloquesCandidatosFechaMax)[3]<-"fechaMax"
    bloquesCandidatosFechaMin<-aggregate(votacionesPeriodoActual$fecha,by=list(bloqueId=votacionesPeriodoActual$bloqueId,diputadoId=votacionesPeriodoActual$diputadoId),FUN=min)
    names(bloquesCandidatosFechaMin)[3]<-"fechaMin"
    bloquesCandidatos<-cbind(bloquesCandidatosCant,fechaMax=bloquesCandidatosFechaMax$fechaMax,fechaMin=bloquesCandidatosFechaMin$fechaMin)
    bloquesCandidatos$color<-bloques[match(bloquesCandidatos$bloqueId,bloques$id_bloque),]$color
    bloquesCandidatos$bloque<-bloques[match(bloquesCandidatos$bloqueId,bloques$id_bloque),]$bloque
    bloquesCandidatos$bloqueAbreviado<-bloques[match(bloquesCandidatos$bloqueId,bloques$id_bloque),]$bloqueAbreviado

  #  bloquesCandidatos[c(1:10),]
    #calculo los bloques en los que el diputado participo en mas sesiones
    diputadoBloquePeriodoActual<-aggregate(bloquesCandidatos$sesiones,by=list(diputadoId=bloquesCandidatos$diputadoId),FUN=max)
    names(diputadoBloquePeriodoActual)[2]<-"maxSesiones"
  #  diputadoBloquePeriodoActual[c(1:10),]

    #Busco la maxima cantidad de sesiones para cada bloque candidato
    bloquesCandidatos$maxSesiones<-diputadoBloquePeriodoActual[match(bloquesCandidatos$diputadoId,diputadoBloquePeriodoActual$diputadoId),]$maxSesiones
    bloquesCandidatosFiltrados<-bloquesCandidatos[bloquesCandidatos$sesiones==bloquesCandidatos$maxSesiones,]
  #  bloquesCandidatosFiltrados[c(1:10),]

    #FINAL: defino el bloque de cada diputado, junto con color y nombre
    diputadosPeriodoActual$bloqueId<-bloquesCandidatosFiltrados[match(diputadosPeriodoActual$diputadoId, bloquesCandidatosFiltrados$diputadoId),]$bloqueId
    diputadosPeriodoActual$color<-bloquesCandidatosFiltrados[match(diputadosPeriodoActual$diputadoId, bloquesCandidatosFiltrados$diputadoId),]$color
    diputadosPeriodoActual$bloque<-bloquesCandidatosFiltrados[match(diputadosPeriodoActual$diputadoId, bloquesCandidatosFiltrados$diputadoId),]$bloque
    diputadosPeriodoActual$bloqueAbreviado<-bloquesCandidatosFiltrados[match(diputadosPeriodoActual$diputadoId, bloquesCandidatosFiltrados$diputadoId),]$bloqueAbreviado
  #  diputadosPeriodoActual[nchar(as.character(diputadosPeriodoActual$color))==0]$color
  #Hago flattened de votaciones y clustering jerarquico
    votacionesFlattened<-xtabs(voto ~  diputadoId+ asuntoId,data = votacionesPeriodoActual)
    votacionesDist<-dist(votacionesFlattened,"euclidean",p=2)
    votacionesCluster<-hclust(votacionesDist,"ward")

  #grupos de votacion
    votacionesGroups <- cutree(votacionesCluster, k=cantidadClustersDiputados) # cut tree into 5 clusters
  #armo tabla con datos
    diputadosClustersPeriodoActual<-as.data.frame(votacionesGroups)
    diputadosClustersPeriodoActual$diputadoId<-as.numeric(rownames(diputadosClustersPeriodoActual))
    names(diputadosClustersPeriodoActual)
    diputadosPeriodoActual$periodo<-periodoActual
    diputadosPeriodoActual$cluster<-diputadosClustersPeriodoActual[match(diputadosPeriodoActual$diputadoId, diputadosClustersPeriodoActual$diputadoId),]$votacionesGroups
  #Consolido los datos
    if (is.null(diputadosAgrupados))
      diputadosAgrupados<-diputadosPeriodoActual
    else
      diputadosAgrupados<-rbind(diputadosAgrupados, diputadosPeriodoActual)

    #exporto dendograpma
    png(file.path(output.dir, paste("cargoGrafias-hclust-",periodoActual, ".png",sep="")), height=heightPNG, width=widthPNG, bg="white")
    plot(votacionesCluster,  hang = -1, labels=paste( diputadosPeriodoActual$bloqueAbreviado, "-", diputadosPeriodoActual$nombre), ylab="altura",xlab="diputados", col.lab=c("blue"),main = paste("Clusters Diputados",periodoActual) )
    rect.hclust(votacionesCluster, k=cantidadClustersDiputados, border="red")
    dev.off()
    # vector of colors
    # cutting dendrogram in 5 clusters
    clus5 = cutree(votacionesCluster, cantidadClustersDiputados)
    # plo
    op = par(bg="#E8DDCB")
    # Size reflects miles per gallon
    votacionesCluster$labels<-paste( diputadosPeriodoActual[match(as.numeric(votacionesCluster$labels), diputadosPeriodoActual$diputadoId),]$bloqueAbreviado,"-", diputadosPeriodoActual[match(as.numeric(votacionesCluster$labels),diputadosPeriodoActual$diputadoId),]$nombre)
    png(file.path(output.dir, paste("cargoGrafias-circ-hclust",periodoActual, ".png",sep="")),
        height=heightPNG, width=widthPNG, bg="white")

    #votacionesCluster2 <- votacionesCluster$edge.length <- rep.int(cantidadClustersDiputados,544)
    tamanioTortaPhylo<-500
    #relacionXY<-1.23
    relacionXY<-0.89
    mypal = c("#556270", "#4ECDC4", "#1B676B", "#FF6B6B", "#C44D58")

    votacionesCluster$labels
    plot(as.phylo(votacionesCluster), type="fan", tip.color=mypal[clus5],
         label.offset=2,use.edge.length=T,
    #     x.lim=c(tamanioTortaPhylo*relacionXY),
         y.lim=c(-20,tamanioTortaPhylo),
         #       labels=paste( diputadosPeriodoActual$bloqueAbreviado, "-", diputadosPeriodoActual$nombre),
         col="red")
    dev.off()
    par(op)
  }

  unique(diputadosAgrupados$periodo)

  write.csv2(diputadosAgrupados,"diputadosAgrupados2003-2013.csv")

  #Clusters de bloques

  #Todavia no funcionan bien los bloques
  cantidadClustersBloques<-4

  votacionesAgrupados<-NULL
  for (periodoActual in periodos){
    # 0 = Afirmativo, 1 = Negativo, 2 = Abstención, 3 = Ausente
    votacionesPeriodoActual<-votaciones[votaciones$ano==periodoActual,]
    print(paste(date(),"agrupacion de bloques", "periodoactual",periodoActual))
    #Construyo bloques candidatos para cada diputado
    diputadosPeriodoActual<-diputados[match(sort(unique(votacionesPeriodoActual$diputadoId)),diputados$diputadoId),]
    bloquesCandidatosCant<-aggregate(rep(1,nrow(votacionesPeriodoActual)),by=list(bloqueId=votacionesPeriodoActual$bloqueId,diputadoId=votacionesPeriodoActual$diputadoId),FUN=sum)
    names(bloquesCandidatosCant)[3]<-"sesiones"
    bloquesCandidatosFechaMax<-aggregate(votacionesPeriodoActual$fecha,by=list(bloqueId=votacionesPeriodoActual$bloqueId,diputadoId=votacionesPeriodoActual$diputadoId),FUN=max)
    names(bloquesCandidatosFechaMax)[3]<-"fechaMax"
    bloquesCandidatosFechaMin<-aggregate(votacionesPeriodoActual$fecha,by=list(bloqueId=votacionesPeriodoActual$bloqueId,diputadoId=votacionesPeriodoActual$diputadoId),FUN=min)
    names(bloquesCandidatosFechaMin)[3]<-"fechaMin"
    bloquesCandidatos<-cbind(bloquesCandidatosCant,fechaMax=bloquesCandidatosFechaMax$fechaMax,fechaMin=bloquesCandidatosFechaMin$fechaMin)
    bloquesCandidatos$color<-bloques[match(bloquesCandidatos$bloqueId,bloques$id_bloque),]$color
    bloquesCandidatos$bloque<-bloques[match(bloquesCandidatos$bloqueId,bloques$id_bloque),]$bloque
    bloquesCandidatos$bloqueAbreviado<-bloques[match(bloquesCandidatos$bloqueId,bloques$id_bloque),]$bloqueAbreviado

    #  bloquesCandidatos[c(1:10),]
    #calculo los bloques en los que el diputado participo en mas sesiones
    diputadoBloquePeriodoActual<-aggregate(bloquesCandidatos$sesiones,by=list(diputadoId=bloquesCandidatos$diputadoId),FUN=max)
    names(diputadoBloquePeriodoActual)[2]<-"maxSesiones"
    #  diputadoBloquePeriodoActual[c(1:10),]

    #Busco la maxima cantidad de sesiones para cada bloque candidato
    bloquesCandidatos$maxSesiones<-diputadoBloquePeriodoActual[match(bloquesCandidatos$diputadoId,diputadoBloquePeriodoActual$diputadoId),]$maxSesiones
    bloquesCandidatosFiltrados<-bloquesCandidatos[bloquesCandidatos$sesiones==bloquesCandidatos$maxSesiones,]
    #  bloquesCandidatosFiltrados[c(1:10),]

    #FINAL: defino el bloque de cada diputado, junto con color y nombre
    diputadosPeriodoActual$bloqueId<-bloquesCandidatosFiltrados[match(diputadosPeriodoActual$diputadoId, bloquesCandidatosFiltrados$diputadoId),]$bloqueId
    diputadosPeriodoActual$color<-bloquesCandidatosFiltrados[match(diputadosPeriodoActual$diputadoId, bloquesCandidatosFiltrados$diputadoId),]$color
    diputadosPeriodoActual$bloque<-bloquesCandidatosFiltrados[match(diputadosPeriodoActual$diputadoId, bloquesCandidatosFiltrados$diputadoId),]$bloque
    diputadosPeriodoActual$bloqueAbreviado<-bloquesCandidatosFiltrados[match(diputadosPeriodoActual$diputadoId, bloquesCandidatosFiltrados$diputadoId),]$bloqueAbreviado


    votacionesPeriodoActual$bloqueId<-bloquesCandidatosFiltrados[match(votacionesPeriodoActual$diputadoId, bloquesCandidatosFiltrados$diputadoId),]$bloqueId
    votacionesPeriodoActual$color<-bloquesCandidatosFiltrados[match(votacionesPeriodoActual$diputadoId, bloquesCandidatosFiltrados$diputadoId),]$color
    votacionesPeriodoActual$bloque<-bloquesCandidatosFiltrados[match(votacionesPeriodoActual$diputadoId, bloquesCandidatosFiltrados$diputadoId),]$bloque
    votacionesPeriodoActual$bloqueAbreviado<-bloquesCandidatosFiltrados[match(votacionesPeriodoActual$diputadoId, bloquesCandidatosFiltrados$diputadoId),]$bloqueAbreviado

    #  diputadosPeriodoActual[nchar(as.character(diputadosPeriodoActual$color))==0]$color
    #Hago flattened de votaciones y clustering jerarquico
    votacionesFlattened<-xtabs(voto ~  bloqueId+ asuntoId,data = votacionesPeriodoActual)
    votacionesDist<-dist(votacionesFlattened,"euclidean",p=2)
    votacionesCluster<-hclust(votacionesDist,"ward")

    #grupos de votacion
    votacionesGroups <- cutree(votacionesCluster, k=cantidadClustersBloques) # cut tree into 5 clusters
    #armo tabla con datos
    bloquesClustersPeriodoActual<-as.data.frame(votacionesGroups)
    bloquesClustersPeriodoActual$bloqueId<-as.numeric(rownames(bloquesClustersPeriodoActual))
    names(bloquesClustersPeriodoActual)
    bloquesClustersPeriodoActual$periodo<-periodoActual
    votacionesPeriodoActual$cluster<-bloquesClustersPeriodoActual[match(votacionesPeriodoActual$bloqueId, bloquesClustersPeriodoActual$bloqueId),]$votacionesGroups

    votacionesPeriodoActual$periodo<-periodoActual
    votacionesPeriodoActual$cluster<-bloquesClustersPeriodoActual[match(votacionesPeriodoActual$diputadoId, votacionesPeriodoActual$diputadoId),]$votacionesGroups

    #Consolido los datos
    if (is.na(votacionesAgrupados))
      votacionesAgrupados<-votacionesPeriodoActual
    else
      votacionesAgrupados<-rbind(diputadosPeriodoActual, votacionesPeriodoActual)

    #exporto dendograma
    png(file.path(output.dir, paste("cargoGrafias-bloques-hclust-",periodoActual, ".png",sep="")), height=heightPNG, width=widthPNG, bg="white")

    plot(votacionesCluster,  hang = -1, labels=votacionesPeriodoActual$bloque, ylab="altura",xlab="bloques", col.lab=c("blue"),main = paste("Clusters Bloques",periodoActual) )
    rect.hclust(votacionesCluster, k=cantidadClustersBloques, border="red")
    dev.off()
    # vector of colors
    # cutting dendrogram in 5 clusters
    clus5 = cutree(votacionesCluster, cantidadClustersBloques)
    # plo
    op = par(bg="#E8DDCB")
    # Size reflects miles per gallon
    votacionesCluster$labels<-votacionesPeriodoActual[match(as.numeric(votacionesCluster$labels), votacionesPeriodoActual$bloqueId),]$bloque
    png(file.path(output.dir, paste("cargoGrafias-bloques-circ-hclust-",periodoActual, ".png",sep="")), height=heightPNG, width=widthPNG, bg="white")

    #votacionesCluster2 <- votacionesCluster$edge.length <- rep.int(cantidadClustersDiputados,544)
    tamanioTortaPhylo<-500
    #relacionXY<-1.23
    relacionXY<-0.89

    votacionesCluster$labels
    plot(as.phylo(votacionesCluster), type="fan", tip.color=votacionesPeriodoActual$color,
         label.offset=2,use.edge.length=T,
         #     x.lim=c(tamanioTortaPhylo*relacionXY),
         y.lim=c(-20,tamanioTortaPhylo),
         #       labels=paste( diputadosPeriodoActual$bloqueAbreviado, "-", diputadosPeriodoActual$nombre),
         col="red")
    dev.off()
    par(op)
  }
}
