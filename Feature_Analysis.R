##############################################################################
# PACKAGES: To facilitate the analysis, load the following packages.         #
# (Please ensure you have installed the latest version of each package.)     #
##############################################################################

library("seqinr")

##############################################################################
# DATA IMPORT: Load and prepare the dataset for analysis.                    #
##############################################################################

irseq<-as.matrix(read.csv("retention.csv"))


##############################################################################
# SEQUENCE LENGTH COMPUTATION                                                #
##############################################################################

getlength<-function(x)
{
  y<- s2c(tolower(x));
  Loseq<-length(y);
  return(Loseq)
}
ir_len<-apply(irseq,1,getlength)

mean(ir_len)
median(ir_len)
max(ir_len)
min(ir_len)
quantile(ir_len)

##############################################################################
# PRESENCE OF K-MER SEQUENCE MOTIFS                                          #
##############################################################################

Frefac<-function(x)# calculate the occurrence number of motifs
{
  Tc<-s2c(tolower(x))
  L<-length(Tc)
  T1<-count(Tc,1)
  for(i in 1:4)
  {T1[[i]]=T1[[i]]/L}
  T2<-count(Tc,2)
  for(i in 1:16)
  {T2[[i]]=T2[[i]]/L}
  T3<-count(Tc,3)
  for(i in 1:64)
  {T3[[i]]=T3[[i]]/L}
    Result<-c(T1,T2,T3)
  
  return(Result);}

Rmultifre<-apply(conseq,1,Frefac)
Rmultifre<-t(Rmultifre)
rownames(Rmultifre)<-NULL

##############################################################################
# SEQUENCE GC CONTENT                                                        #
##############################################################################

g<-Rmultifre[,"g"]
c<-Rmultifre[,"c"]
GC_con<-g+c
mean(GC_con)

##############################################################################
# STRENGTH OF 5' AND 3' SPLICE SITES                                         #
##############################################################################

# 5' splice site strength #

me_5<-read.fasta("maxent_donor.fasta", as.string=TRUE,seqonly=TRUE)

Extractnum<-function(x)
{
  y=substr(x, 18, nchar(x)-1)# substr (x, start, stop)
  y=as.numeric(y)
  return(y)
}
MaxEnt<-lapply(me_5,Extractnum)
MaxEnt<-as.numeric(MaxEnt)

# 3' splice site strength #

me_3<-read.fasta("MaxEnt_acceptor.fasta", as.string=TRUE,seqonly=TRUE)

Extractnum2<-function(x)
{
  y=substr(x, 32, nchar(x)-1)# substr (x, start, stop)
  y=as.numeric(y)
  return(y)
}
MaxEnt2<-lapply(me_3,Extractnum2)
MaxEnt2<-as.numeric(MaxEnt2)
