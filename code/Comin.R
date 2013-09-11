# complexity and information
#first part
Comin=function(it,b,ld,ext,r,f){
  t<-34
  dv<-34
  rm(list=ls())
  pmatriz<-function(matriz,b){
    fias<-nrow(matriz)
    colum<-ncol(matriz)
    AUTOP<-0
    max1<-0
    min1<-0
    rango<-0
    basetrans<-matriz
    info<-0
    logar<-0
    cont<-0
    prob<-0
    E<-0
    S<-0
    C<-0
    CE<-0
    CS<-0
    CC<-0
    CA<-0
    color<-0
    
    for(f in 1:colum){
      info[f]<-0
    }
    b<-b-0.0001
    for (i in 1:colum){
      r<-matriz[,i]
      omit<-na.omit(r)
      max1[i]<-max(omit)
      min1[i]<-min(omit)
      rango[i]<-max1[i]-min1[i]
      
      for(j in 1:fias){
        if(is.na(matriz[j,i])==TRUE){
          basetrans[j,i]<-NA
        }
        if(is.na(matriz[j,i])==FALSE){
        if(rango[i]==0){
          basetrans[j,i]<-0
        }
        else{
          basetrans[j,i]<-floor(b*((matriz[j,i]-min1[i])/rango[i]))
        }
        
      }
      }
      lim=floor(b)
      for(k in 1:lim+1){
        cont[k]=0
      }
      for(h in 0:lim){
        for(j in 1:fias){
          if(is.na(matriz[j,i])==FALSE){
          if(basetrans[j,i]==h){
            cont[h+1]<-cont[h+1]+1
          }
        }
      }
      }
      prob<-cont/fias
      cont<-0
      for(u in 0:lim){
        if(prob[u+1]==0){
          logar[i]<-0
        }
        else
        {
          logar[i]<-log(prob[u+1],lim+1)
        }
        info[i]<-info[i]+(prob[u+1]*logar[i])
      }
      info[i]=info[i]*(-1)
      E[i]=info[i]
      S[i]=1-E[i]
      C[i]=4*E[i]*S[i]
      if(E[i]<=1 & E[i]>=0.8){
        CE[i]<-"darkblue"
      }
      if(E[i]<0.8& E[i]>=0.6){
        CE[i]<-"green"
      }
      if(E[i]<0.6 & E[i]>=0.4){
        CE[i]<-"yellow"
      }
      if(E[i]<0.4 & E[i]>=0.2){
        CE[i]<-"orange"
      }
      if(E[i]<0.2 & E[i]>=0){
        CE[i]<-"red"
      }
      if(S[i]<=1 & S[i]>=0.8){
        CS[i]<-"darkblue"
      }
      if(S[i]<0.8& S[i]>=0.6){
        CS[i]<-"green"
      }
      if(S[i]<0.6 & S[i]>=0.4){
        CS[i]<-"yellow"
      }
      if(S[i]<0.4 & S[i]>=0.2){
        CS[i]<-"orange"
      }
      if(S[i]<0.2 & S[i]>=0){
        CS[i]<-"red"
      }
      if(C[i]<=1 & C[i]>=0.8){
        CC[i]<-"darkblue"
      }
      if(C[i]<0.8& C[i]>=0.6){
        CC[i]<-"green"
      }
      if(C[i]<0.6 & C[i]>=0.4){
        CC[i]<-"yellow"
      }
      if(C[i]<0.4 & C[i]>=0.2){
        CC[i]<-"orange"
      }
      if(C[i]<0.2 & C[i]>=0){
        CC[i]<-"red"
      }
      
    }
    aux<-1
    conta<-1
    for(fg in 1:colum){
      if(aux==1){
        color[conta]<-CE[fg]
        aux<-2
        conta<-conta+1
      }
      if(aux==2){
        color[conta]<-CS[fg]
        aux<-3
        conta<-conta+1
      }
      if(aux==3){
        color[conta]<-CC[fg]
        aux<-1
        conta<-conta+1
      }
      
    }
    factor<-colum*(1/105)
    factor<-1-factor
    haming<-c(1:fias)
    for(g in 1:fias){
      haming[g]<-0
    }
    #hasta aca
    for (i in 1:colum){
      aux<-1
      c<-1
      for(j in 1: fias){
        if(is.na(basetrans[j,i])==FALSE){
          if(is.na(basetrans[aux,i])==FALSE){
        if((basetrans[j,i])!=(basetrans[aux,i])){
          haming[c]<-haming[c]+1
        }
          }
        }
        aux<-j
        c<-c+1
      
      }
    }
    haming<-haming/(colum)
    homeostasis<-1-haming
    sumcom<-0
    sumcom<-sum(C)
    res<-colum-1
    if(colum==1){
      AUTOP<-1
      CA<-"black"
    }
    else
    {
      for(i in 1:colum){
        gh<-(sumcom-C[i])/res
        AUTOP[i]<-(C[i]/gh)
        if(AUTOP[i]==1){
          CA[i]<-"black"
        }
        if(AUTOP[i]>1){
          CA[i]<-"blue"
        }
        if(AUTOP[i]<1){
          CA[i]<-"brown1"
        }
      }
    }
    resul<-matrix(c(E,S,C,AUTOP),ncol=4,nrow=colum)
    lim<-max(AUTOP)
    rownames(resul)<-names(matriz)
    colnames(resul)<-c("Emergence", "Self-organization", "Complexity","Autopoiesis")
    K<-rownames(resul)
    names(E)<-rownames(resul)
    names(S)<-rownames(resul)
    names (C)<-rownames(resul)
    names(AUTOP)<-rownames(resul)
    attach(mtcars) 
    layout(matrix(c(1,2,3,4,5,5), 3,2, byrow = TRUE)) 
    barplot(E,beside=TRUE, ylim=c(0,1),main="Emergence",col=c(CE),cex.names=factor)
    barplot(S,beside=TRUE, ylim=c(0,1),main="Self-organization",col=c(CS),cex.names=factor)
    barplot(C,beside=TRUE, ylim=c(0,1),main="Complexity",col=c(CC),cex.names=factor)
    plot(homeostasis,type="l",main="Homeostasis",xlab="",ylab="",ylim=c(0,1))
    barplot(AUTOP,beside=TRUE, ,main="Autopoiesis",col=c(CA),ylim=c(0,lim),cex.names=factor)
    M <-resul
    return(M)
  }
  pvectores<-function(data,b){
    max1<-max(data)
    min1<-min(data)
    rango<-(max1-min1)
    tama<-length(data)
    i=0
    basetrans<-3
    cont<-0
    info<-0
    b<-b-0.001
    for(i in 1:tama){
      if(rango==0){
        basetrans[i]<-0
      }
      else{
        basetrans[i]<-floor(b*((data[i]-min1)/rango))
      }
    }
    lim=floor(b)
    for(k in 1:lim+1){
      cont[k]=0
    }
    for(i in 0:lim){
      for(j in 1:tama){
        if(basetrans[j]==i){
          cont[i+1]<-cont[i+1]+1
        }
      }
    }
    prob<-cont/tama
    for(u in 0:lim){
      if(prob[u+1]==0){
        logar<-0
      }
      else
      {
        logar<-log(prob[u+1],lim+1)
      }
      info<-info+(prob[u+1]*logar)
    }
    info=info*-1
    E=info
    S=1-E
    C=4*E*S
    if(E<=1 & E>=0.8){
      CE<-"darkblue"
    }
    if(E<0.8& E>=0.6){
      CE<-"green"
    }
    if(E<0.6 & E>=0.4){
      CE<-"yellow"
    }
    if(E<0.4 & E>=0.2){
      CE<-"orange"
    }
    if(E<0.2 & E>=0){
      CE<-"red"
    }
    if(S<=1 & S>=0.8){
      CS<-"darkblue"
    }
    if(S<0.8& S>=0.6){
      CS<-"green"
    }
    if(S<0.6 & S>=0.4){
      CS<-"yellow"
    }
    if(S<0.4 & S>=0.2){
      CS<-"orange"
    }
    if(S<0.2 & S>=0){
      CS<-"red"
    }
    if(C<=1 & C>=0.8){
      CC<-"darkblue"
    }
    if(C<0.8& C>=0.6){
      CC<-"green"
    }
    if(C<0.6 & C>=0.4){
      CC<-"yellow"
    }
    if(C<0.4 & C>=0.2){
      CC<-"orange"
    }
    if(C<0.2 & C>=0){
      CC<-"red"
    }
    resul<-c(E,S,C)
    
    names(resul)<-c("Emergence","Self-organization","Complexity")
    par(mfrow=c(1,1)) 
    barplot(resul,ylim=c(0,1), main="Emergence,Self-organization and Complexity", col=c(CE,CS,CC))
    V <- matrix(c(E,S,C),nrow =1, ncol = 3, byrow=TRUE,  dimnames=list( c("value"),c("Emergence", "Selforganization", "Complexity")))
    V
    return(V)
  }
  
  
  
  #third part
  if(it==1){
    V<-pvectores(ld,b)
    return(V)
  }
  
  if(it==3) {
    #1
    #2
    setwd(r)
    #3
    if(ext==1){
      #4
      data<-read.csv(f) # lee toda la tabla Nelson
      
      attach(data)
      M<-pmatriz(data,b)
      M
      return(M)
    }
    if(ext==2){
      #4
      data<-read.delim(f, header = TRUE, sep = "\t") # lee toda la tabla Nelson
      
      attach(data)
      M<-pmatriz(data,b)
      M
      return(M)
    }
    if(ext==3){
      #4
      library(foreign)
      data <- read.spss(f, to.data.frame = TRUE)
      
      attach(data)
      M<-pmatriz(data,b)
      M
      return(M)
    }
  }
  if(it==2){
    M<-pmatriz(ld,b)
    return(M)
  }
  if(it==4){
    #2
    setwd(r)
    #3
    if(ext==1){
      data<-read.csv(f) # lee toda la tabla Nelson
      
      V<-pvectores(data,b)
      V
      return(V)
    }
    if(ext==2){
      #4
      data<-read.delim(f, header = TRUE, sep = "\t") # lee toda la tabla Nelson
      
      V<-pvectores(data,b)
      V
      return(V)
    }
    
    if(ext==3){
      #4
      library(foreign)
      V <- read.spss(f, to.data.frame = TRUE)
      V<-pvectores(data,b)
      V
      return(V)
    }
  }
}
