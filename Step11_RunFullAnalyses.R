setwd("/workspaces/r-ver-4/gendercitation-master/Journals") # Change to your project folder path
source("HelperFunctions.R")
#install.packages("ggplot2")
#install.packages("mgcv")
#install.packages("patchwork")
#install.packages("pbmcapply")
#install.packages("quantreg")
#install.packages("data.table")
#install.packages("readxl")
#install.packages("stringr")
#install.packages("plyr")
library(stringr)
library(ggplot2);library(mgcv);library(patchwork)
library(boot);library(pbmcapply)
library(data.table);library(quantreg)
library(readxl); library(plyr)
library(tm) # for text mining 
library(rcrossref)
library(RColorBrewer)
library(wordcloud)
library(wordcloud2)
# Load in data from step 10
load("df10_articledata.RData")
# should contain article.data, ref_proportions (that's what's most important)

#############################################
## Plotting Functions ##
#############################################

get.plotdf.violin=function(boot){
    plotdf=data.frame(Group = factor(c(rep("Man &\nMan",500),rep("Woman &\nMan",500),rep("Man &\nWoman",500),rep("Woman &\nWoman",500)),levels=c("Man &\nMan","Woman &\nMan",
                                "Man &\nWoman","Woman &\nWoman")))
    # reshape the matrix
    plotdf$Prop[1:500]=boot$t[,1]
    plotdf$Prop[501:1000]=boot$t[,2]
    plotdf$Prop[1001:1500]=boot$t[,3]
    plotdf$Prop[1501:2000]=boot$t[,4]
    return(plotdf)}



violinplot=function(data,title,ymin,ymax,yl=T,xl=T,shortlab=F){
  p=ggplot(data,aes(x=Group, y=Prop, fill=Group))+
    geom_violin()+
    geom_boxplot(width = 0.1)+
    #geom_errorbar(aes(ymin=LB,ymax=UB),width=.2)+
    theme_bw()+theme(legend.position="n")+
    geom_hline(yintercept=0,color='black',lty=1)+
    ggtitle(title)
  if(xl==T){
    p=p+xlab("Cited authors' gender (first & last)")
  }else{
    p=p+xlab(NULL)
  }
  if(shortlab==T){
    p=p+xlab("Cited authors")+
      scale_fill_manual(values=c("Overall"="gray24",
                                 "MM"="darkslateblue",
                                 "WM"="darkslategray4",
                                 "MW"="lightcyan3",
                                 "WW"="lightsalmon3"))
  }else{
    p=p+scale_fill_manual(values=c("Overall"="gray24",
                                   "Man &\nMan"="darkslateblue",
                                   "Woman &\nMan"="darkslategray4",
                                   "Man &\nWoman"="lightcyan3",
                                   "Woman &\nWoman"="lightsalmon3"))
  }
  if(yl==T){
    p=p+ylab("Over- and undercitation (%)")
  }else{
    p=p+ylab(NULL)
  }
  min.break=ceiling(ymin*10)/10
  max.break=floor(ymax*10)/10
  breaks=seq(min.break,max.break,(max.break-min.break)/4)
  p=p+scale_y_continuous(breaks=breaks,
                         limits=c(ymin,ymax))
  return(p)
}
timeplot=function(data,title,yl=T,xl=T){
  p=ggplot(data,aes(x=Year, y=Prop, fill=Color)) + 
    geom_area(alpha=0.9,size=.5,color="black")+theme_bw()+
    scale_fill_identity()+
    scale_x_continuous(limits=c(min(data$Year),max(data$Year)),
                       expand=c(0,0))+
    scale_y_continuous(limits=c(0,1.0000000001),expand=c(0,0))+ # Note: adding the tiny offset to y avoids strange gaps in coverage
    ggtitle(title)
  if(yl==T & xl==T){
    p+xlab("Year")+ylab("Proportion")
  }else{
    p
  }
}
##################
## Read in Data ##
##################
# Save number of cores on machine
cores=4

# Add in the eigenfactor data
journalStats = read_excel("JournalStatistics.xlsx") # in this case, journal statistics downloaded from Web of Science as of September 2023
# sorted in terms of eigenfactor
# keep only the top 50 rows
journalStats = journalStats[1:50,]

# add top 5 classification

for(j in 1:50){
    journ = journalStats$'Journal name'[j]
    if (j<6){
        article.data$eigengroup[article.data$SO==journ]=1
    }
    else{article.data$eigengroup[article.data$SO==journ]=0}
}

## Find subset of articles for analysis
## I.e., articles in a specific window that contain at least one relevant reference
time_window=article.data$PY%in%c(2009:2023)
has_citations=ref_proportions[,13]>0
subset_articles=time_window & has_citations

## Create gender category vectors
gend_group_4=unlist(lapply(article.data$AG,transform.cat.4))
gend_group_4=factor(gend_group_4,lev=c("MM","WM","MW","WW","NA"))
gend_group_2=unlist(lapply(article.data$AG,transform.cat.2))
gend_group_2=factor(gend_group_2,lev=c("MM","W|W","NA"))


#########################################################
## Calculate citation gaps across cited author genders ##
#########################################################
# Get subset of ref_proportions data
ref_prop_sub=ref_proportions[subset_articles,]
ref_tot_sub=ref_prop_sub[,1:12]*ref_prop_sub[,13]
# Gap relative to overall literature
citeprops(ref_tot_sub,type='randomdraw')
citegap(ref_tot_sub,type='randomdraw')

# Gap conditional on papers' characteristics
citeprops(ref_tot_sub,type='conditional')
citegap(ref_tot_sub,type='conditional')


###################################
## Recreate graphs from Figure 1 ##
###################################

# Get bootstrap standard errors for gap values
boot.rd=boot(ref_tot_sub,citegap,R=500,type='randomdraw')
boot.cn=boot(ref_tot_sub,citegap,R=500,type='conditional')

# Create ggplot compatible data frames

plot.df.randomdraw=get.plotdf.violin(boot.rd)
plot.df.conditional=get.plotdf.violin(boot.cn)
p.all.rd=violinplot(plot.df.randomdraw,"Gap relative to literature",
                ymin=-0.20,ymax=0.20)
p.all.cn=violinplot(plot.df.conditional,"Gap conditional on characteristics",
                ymin=-0.20,ymax=0.20)

p.all.rd
p.all.cn

p.all.rd+p.all.cn
#############################################
## Cluster based on gap scores, by journal ##
#############################################

gapstats=data.frame("Journal"=character(),"MM"=double(),"WM"=double(),"MW"=double(), "WW"=double())

for (j in 1:50){
    journ = journalStats$'Journal name'[j]
    # get the subset of articles you're working with - in this case, all articles within the time window with an identified gender
   subsetj=(article.data$SO==journ)& has_citations & time_window & (gend_group_2!="NA")
    
    ref_prop_subj=ref_proportions[subsetj,]
    ref_tot_subj=ref_prop_subj[,1:12]*ref_prop_subj[,13]
    gapstats[j,"Journal"]=journ
    gapstats[j,2:5]=citegap(ref_tot_subj,type='conditional') # get conditional gap 
    
}
# Plot this 
gapplots = data.frame(Group = factor(c(rep("MM",50),rep("WM",50),rep("MW",50),rep("WW",50)),levels=c('MM','WM','MW','WW')))
gapplots$Prop[1:50] = gapstats[,'MM']
gapplots$Prop[51:100]=gapstats[,'WM']
gapplots$Prop[101:150]=gapstats[,'MW']
gapplots$Prop[151:200]=gapstats[,'WW']

p.gap.journals=ggplot(gapplots, aes(x=Group, y=Prop, fill=Group))+geom_violin()+
    geom_boxplot(width = 0.1)+geom_jitter(shape=16, position=position_jitter(0.2))+
    xlab('Cited Author Gender (first and last)')+
    ylab('Over- and Undercitation (%)')+theme_bw()+theme(legend.position="n")+
    scale_fill_manual(values=c("Overall"="gray24",
                                 "MM"="darkslateblue",
                                 "WM"="darkslategray4",
                                 "MW"="lightcyan3",
                                 "WW"="lightsalmon3"))
  
p.gap.journals
gapstats$Eigenfactor = journalStats$Eigenfactor
# Cluster based on gap statistics 

# note: Journal of Alzheimers Disease is a known outlier, so it's removed and added back after clustering
# doing so in this manner creates a clustering that remains consistent over 1000 simulations
journs_kmeans = kmeans(x=gapstats[c(1:10,12:50),2:5],centers=3)
for(i in 1:50){
    if(!(i==11)){
        gapstats$Cluster[i]=journs_kmeans$cluster[sprintf("%d",i)]
    }
}

# Align to the cluster definitions in the paper
# Neural Networks, Row 28, is in Group 1. Emblematic because overcitation of MM, undercitation of everyone else 
# Psychoeuroendocrinology, Row 43, is Group 2. Emblematic because undercitation of MM, overcitation of everyone else
old = c(1:3)
old[1]=gapstats$Cluster[28]# for networks
old[2]=gapstats$Cluster[43] #for neuroendocrinology
old[3] = 6-(old[1]+old[2])
gapstats$Cluster=mapvalues(gapstats$Cluster, from = c(old[1], old[2], old[3]), to = c(1,2,3))
# The outlier, Journal of Alzheimer's Disease, is closest to Group 2 by distance to centroid
gapstats$Cluster[11]=2

# Factor for easier plotting
gapstats$Cluster=factor(gapstats$Cluster,lev=c("1","2","3"),labels=c("1","2","3"))

# Plot


p.gap.clusters.MM=ggplot(gapstats, aes(x=Cluster, y=MM, fill=Cluster))+geom_violin()+
    geom_boxplot(width = 0.1)+geom_jitter(shape=16, position=position_jitter(0.2))+
    xlab('Cluster')+ theme_bw()+theme(legend.position="n")+
    geom_hline(yintercept=0,color='black',lty=1)+
    ggtitle('MM')+
    ylab('Over- and Undercitation (%)')
p.gap.clusters.WM=ggplot(gapstats, aes(x=Cluster, y=WM, fill=Cluster))+geom_violin()+
    geom_boxplot(width = 0.1)+geom_jitter(shape=16, position=position_jitter(0.2))+
    xlab('Cluster')+ theme_bw()+theme(legend.position="n")+
    geom_hline(yintercept=0,color='black',lty=1)+
    ggtitle('WM')+
    ylab('Over- and Undercitation (%)')
p.gap.clusters.MW=ggplot(gapstats, aes(x=Cluster, y=MW, fill=Cluster))+geom_violin()+
    geom_boxplot(width = 0.1)+geom_jitter(shape=16, position=position_jitter(0.2))+
    xlab('Cluster')+ theme_bw()+theme(legend.position="n")+
    geom_hline(yintercept=0,color='black',lty=1)+
    ggtitle('MW')+
    ylab('Over- and Undercitation (%)')
p.gap.clusters.WW=ggplot(gapstats, aes(x=Cluster, y=WW, fill=Cluster))+geom_violin()+
    geom_boxplot(width = 0.1)+geom_jitter(shape=16, position=position_jitter(0.2))+
    xlab('Cluster')+ theme_bw()+theme(legend.position="n")+
    geom_hline(yintercept=0,color='black',lty=1)+
    ggtitle('WW')+
    ylab('Over- and Undercitation (%)')

(p.gap.clusters.MM + p.gap.clusters.WM) / (p.gap.clusters.MW +p.gap.clusters.WW)

# Create all 4 in one plot 
gapplotswithclust = gapplots
gapplotswithclust$Cluster = NA
for(i in 1:50){
    gapplotswithclust$Cluster[c(i,50+i,100+i,150+i)]=gapstats$Cluster[i]
}
gapplotswithclust$Cluster = factor(gapplotswithclust$Cluster)
p.gap.clusters=ggplot(gapplotswithclust[c(1:10,12:50,52:110,112:160,162:200),], aes(x=Group, y=Prop, fill=Cluster))+
    geom_violin(position=position_dodge(0.5))+geom_point(position=position_jitterdodge(dodge.width=0.5))+
    geom_boxplot(width = 0.1,position=position_dodge(0.5))+
    xlab('Cluster')+ theme_bw()+theme(legend.position="n")+
    geom_hline(yintercept=0,color='black',lty=1)+
    ggtitle('Clustered Groups')+
    ylab('Over- and Undercitation (%)')
# Add the clusters back onto the original data
for(j in 1:50){
    journ = gapstats$Journal[j]
    article.data$Cluster[article.data$SO==journ]=gapstats$Cluster[j]
}
###############################################
## Create Conditional Gaps for All Subgroups ##
###############################################

# From earlier, subset, ref_prop_sub, and boot.rd and boot.cn are for the full data
# Repeat for subgroups of interest (using only conditional gap)

print("Original CiteProps/CiteGaps")
citeprops(ref_tot_sub,type='conditional')
citegap(ref_tot_sub,type='conditional')
# Top 5 journals:
subset_t5 = article.data$eigengroup==1 
ref_prop_sub_t5=ref_proportions[subset_t5& has_citations & time_window,]
ref_tot_sub_t5=ref_prop_sub_t5[,1:12]*ref_prop_sub_t5[,13]
print("Top 5 CiteProps/CiteGaps")
citeprops(ref_tot_sub_t5,type='conditional')
citegap(ref_tot_sub_t5,type='conditional')
# Remaining Journals:
subset_nt5 = article.data$eigengroup==0 
ref_prop_sub_nt5=ref_proportions[subset_nt5& has_citations & time_window,]
ref_tot_sub_nt5=ref_prop_sub_nt5[,1:12]*ref_prop_sub_nt5[,13]
print("Remaining CiteProps/CiteGaps")
citeprops(ref_tot_sub_nt5,type='conditional')
citegap(ref_tot_sub_nt5,type='conditional')
# Group 1 Journals:
subset_g1 = article.data$Cluster==1 
ref_prop_sub_g1=ref_proportions[subset_g1& has_citations & time_window,]
ref_tot_sub_g1=ref_prop_sub_g1[,1:12]*ref_prop_sub_g1[,13]
print("Group 1 CiteProps/CiteGaps")
citeprops(ref_tot_sub_g1,type='conditional')
citegap(ref_tot_sub_g1,type='conditional')
# Group 2 Journals:
subset_g2 = article.data$Cluster==2 
ref_prop_sub_g2=ref_proportions[subset_g2& has_citations & time_window,]
ref_tot_sub_g2=ref_prop_sub_g2[,1:12]*ref_prop_sub_g2[,13]
print("Group 2 CiteProps/CiteGaps")
citeprops(ref_tot_sub_g2,type='conditional')
citegap(ref_tot_sub_g2,type='conditional')
# Group 3 Journals:
subset_g3 = article.data$Cluster==3 
ref_prop_sub_g3=ref_proportions[subset_g3& has_citations & time_window,]
ref_tot_sub_g3=ref_prop_sub_g3[,1:12]*ref_prop_sub_g3[,13]
print("Group 3 CiteProps/CiteGaps")
citeprops(ref_tot_sub_g3,type='conditional')
citegap(ref_tot_sub_g3,type='conditional')

# Bootstrap estimates
boot.cn.t5=boot(ref_tot_sub_t5,citegap,R=500,type='conditional')
boot.cn.nt5=boot(ref_tot_sub_nt5,citegap,R=500,type='conditional')
boot.cn.g1=boot(ref_tot_sub_g1,citegap,R=500,type='conditional')
boot.cn.g2=boot(ref_tot_sub_g2,citegap,R=500,type='conditional')
boot.cn.g3=boot(ref_tot_sub_g3,citegap,R=500,type='conditional')

###############################################
## Plot Figure 2 ##
###############################################


# Create ggplot compatible data frames
plot.df.conditional.all=get.plotdf.violin(boot.cn)
p.all.cn=violinplot(plot.df.conditional,"Citing: All 50 Journals",
                ymin=-0.20,ymax=0.20)
plot.df.conditional.t5=get.plotdf.violin(boot.cn.t5)
p.all.cn.t5=violinplot(plot.df.conditional.t5,"Citing: Top 5 Journals",
                ymin=-0.20,ymax=0.20)
plot.df.conditional.nt5=get.plotdf.violin(boot.cn.nt5)
p.all.cn.nt5=violinplot(plot.df.conditional.nt5,"Citing: Remaining Journals",
                ymin=-0.20,ymax=0.20)
plot.df.conditional.g1=get.plotdf.violin(boot.cn.g1)
p.all.cn.g1=violinplot(plot.df.conditional.g1,"Citing: Group 1 Journals",
                ymin=-0.20,ymax=0.20)
plot.df.conditional.g2=get.plotdf.violin(boot.cn.g2)
p.all.cn.g2=violinplot(plot.df.conditional.g2,"Citing: Group 2 Journals",
                ymin=-0.20,ymax=0.20)
plot.df.conditional.g3=get.plotdf.violin(boot.cn.g3)
p.all.cn.g3=violinplot(plot.df.conditional.g3,"Citing: Group 3 Journals",
                ymin=-0.20,ymax=0.20)

(p.all.cn+p.all.cn.t5+p.all.cn.nt5)/(p.all.cn.g1+p.all.cn.g2+p.all.cn.g3)


###################################
## Recreate graphs from Figure 3 ##
###################################

# Get overall authorship breakdown by year
timedata=get.timedf(article.data)
p.ov=timeplot(timedata,"Overall Authorship Breakdown by Year")

# Get group-specific graphs
td.t5=get.timedf(article.data[subset_t5,])
td.nt5=get.timedf(article.data[subset_nt5,])
td.1=get.timedf(article.data[subset_g1,])
td.2=get.timedf(article.data[subset_g2,])
td.3=get.timedf(article.data[subset_g3,])

# You could also get the timeline for specific journals
# Change functions to the journal names in your data
#td.2=get.timedf(article.data,'PAIN')
# ...

# Make plots
p.t5=timeplot(td.t5,'Top 5 Journals',yl=F,xl=F)
p.nt5=timeplot(td.nt5,'Remaining Journals',yl=F,xl=F)
p.1=timeplot(td.1,'Group 1',yl=F,xl=F)
p.2=timeplot(td.2,'Group 2',yl=F,xl=F)
p.3=timeplot(td.3,'Group 3',yl=F,xl=F)

# ...

# View plots
(p.ov)/(p.t5+p.nt5)+(p.1+p.2+p.3)




##################################################
## Get breakdowns based on citing author gender ##
##################################################

# Get subset of group labels
gend2_sub=gend_group_2[subset_articles]
gend4_sub=gend_group_4[subset_articles]

# Gap within reference lists of MM papers
citegap(ref_tot_sub[gend2_sub=="MM",],type='conditional')

# Gap within reference lists of W|W papers
citegap(ref_tot_sub[gend2_sub=="W|W",],type='conditional')

# Gap within subgroups of W|W papers
citegap(ref_tot_sub[gend4_sub=="WM",],type='conditional')
citegap(ref_tot_sub[gend4_sub=="MW",],type='conditional')
citegap(ref_tot_sub[gend4_sub=="WW",],type='conditional')

###################################
## Recreate graphs from Figure 3 ##
###################################
# Get bootstrap standard errors for gap values
boot.mm=boot(ref_tot_sub[gend2_sub=="MM",],citegap,R=500)
boot.worw=boot(ref_tot_sub[gend2_sub=="W|W",],citegap,R=500)
boot.wm=boot(ref_tot_sub[gend4_sub=="WM",],citegap,R=500)
boot.mw=boot(ref_tot_sub[gend4_sub=="MW",],citegap,R=500)
boot.ww=boot(ref_tot_sub[gend4_sub=="WW",],citegap,R=500)

# Create ggplot compatible data frames
plot.df.mm=get.plotdf.violin(boot.mm)
plot.df.worw=get.plotdf.violin(boot.worw)
plot.df.wm=get.plotdf.violin(boot.wm)
plot.df.mw=get.plotdf.violin(boot.mw)
plot.df.ww=get.plotdf.violin(boot.ww)
p.mm=violinplot(plot.df.mm,"Citing: MM",ymin=-0.30,ymax=0.30)
p.worw=violinplot(plot.df.worw,"Citing: W or W",ymin=-0.30,ymax=0.30)
p.wm=violinplot(plot.df.wm,"Citing: WM",ymin=-0.30,ymax=0.30)
p.mw=violinplot(plot.df.mw,"Citing: MW",ymin=-0.30,ymax=0.30)
p.ww=violinplot(plot.df.ww,"Citing: WW",ymin=-0.30,ymax=0.30)

# View plots
p.mm
p.worw
p.wm
p.mw
p.ww

(p.mm+p.worw)/(p.wm+p.mw+p.ww)

##################################################
## This can also be done for individual groups  ##
##################################################
# Get subset of group labels
gend2_sub_g1=gend_group_2[subset_g1&time_window & has_citations]
gend4_sub_g1=gend_group_4[subset_g1&time_window & has_citations]

# Gap within reference lists of MM papers
citegap(ref_tot_sub_g1[gend2_sub_g1=="MM",],type='conditional')

# Gap within reference lists of W|W papers
citegap(ref_tot_sub_g1[gend2_sub_g1=="W|W",],type='conditional')

# Gap within subgroups of W|W papers
citegap(ref_tot_sub_g1[gend4_sub_g1=="WM",],type='conditional')
citegap(ref_tot_sub_g1[gend4_sub_g1=="MW",],type='conditional')
citegap(ref_tot_sub_g1[gend4_sub_g1=="WW",],type='conditional')

###################################
## Recreate graphs (Supplemental) ##
###################################
# Get bootstrap standard errors for gap values
boot.mm.g1=boot(ref_tot_sub_g1[gend2_sub_g1=="MM",],citegap,R=500)
boot.worw.g1=boot(ref_tot_sub_g1[gend2_sub_g1=="W|W",],citegap,R=500)
boot.wm.g1=boot(ref_tot_sub_g1[gend4_sub_g1=="WM",],citegap,R=500)
boot.mw.g1=boot(ref_tot_sub_g1[gend4_sub_g1=="MW",],citegap,R=500)
boot.ww.g1=boot(ref_tot_sub_g1[gend4_sub_g1=="WW",],citegap,R=500)

# Create ggplot compatible data frames
plot.df.mm.g1=get.plotdf.violin(boot.mm.g1)
plot.df.worw.g1=get.plotdf.violin(boot.worw.g1)
plot.df.wm.g1=get.plotdf.violin(boot.wm.g1)
plot.df.mw.g1=get.plotdf.violin(boot.mw.g1)
plot.df.ww.g1=get.plotdf.violin(boot.ww.g1)
p.mm.g1=violinplot(plot.df.mm.g1,"Citing: MM",ymin=-0.30,ymax=0.30)
p.worw.g1=violinplot(plot.df.worw.g1,"Citing: W or W",ymin=-0.30,ymax=0.30)
p.wm.g1=violinplot(plot.df.wm.g1,"Citing: WM",ymin=-0.30,ymax=0.30)
p.mw.g1=violinplot(plot.df.mw.g1,"Citing: MW",ymin=-0.30,ymax=0.30)
p.ww.g1=violinplot(plot.df.ww.g1,"Citing: WW",ymin=-0.30,ymax=0.30)

# View plots
p.mm.g1
p.worw.g1
p.wm.g1
p.mw.g1
p.ww.g1

(p.mm.g1+p.worw.g1)/(p.wm.g1+p.mw.g1+p.ww.g1)


##################################################
## This can also be done for individual groups  ##
##################################################
# Get subset of group labels
gend2_sub_g2=gend_group_2[subset_g2&time_window & has_citations]
gend4_sub_g2=gend_group_4[subset_g2&time_window & has_citations]

# Gap within reference lists of MM papers
citegap(ref_tot_sub_g2[gend2_sub_g2=="MM",],type='conditional')

# Gap within reference lists of W|W papers
citegap(ref_tot_sub_g2[gend2_sub_g2=="W|W",],type='conditional')

# Gap within subgroups of W|W papers
citegap(ref_tot_sub_g2[gend4_sub_g2=="WM",],type='conditional')
citegap(ref_tot_sub_g2[gend4_sub_g2=="MW",],type='conditional')
citegap(ref_tot_sub_g2[gend4_sub_g2=="WW",],type='conditional')

###################################
## Recreate graphs (Supplemental) ##
###################################
# Get bootstrap standard errors for gap values
boot.mm.g2=boot(ref_tot_sub_g2[gend2_sub_g2=="MM",],citegap,R=500)
boot.worw.g2=boot(ref_tot_sub_g2[gend2_sub_g2=="W|W",],citegap,R=500)
boot.wm.g2=boot(ref_tot_sub_g2[gend4_sub_g2=="WM",],citegap,R=500)
boot.mw.g2=boot(ref_tot_sub_g2[gend4_sub_g2=="MW",],citegap,R=500)
boot.ww.g2=boot(ref_tot_sub_g2[gend4_sub_g2=="WW",],citegap,R=500)

# Create ggplot compatible data frames
plot.df.mm.g2=get.plotdf.violin(boot.mm.g2)
plot.df.worw.g2=get.plotdf.violin(boot.worw.g2)
plot.df.wm.g2=get.plotdf.violin(boot.wm.g2)
plot.df.mw.g2=get.plotdf.violin(boot.mw.g2)
plot.df.ww.g2=get.plotdf.violin(boot.ww.g2)
p.mm.g2=violinplot(plot.df.mm.g2,"Citing: MM",ymin=-0.30,ymax=0.30)
p.worw.g2=violinplot(plot.df.worw.g2,"Citing: W or W",ymin=-0.30,ymax=0.30)
p.wm.g2=violinplot(plot.df.wm.g2,"Citing: WM",ymin=-0.30,ymax=0.30)
p.mw.g2=violinplot(plot.df.mw.g2,"Citing: MW",ymin=-0.30,ymax=0.30)
p.ww.g2=violinplot(plot.df.ww.g2,"Citing: WW",ymin=-0.30,ymax=0.30)

# View plots
p.mm.g2
p.worw.g2
p.wm.g2
p.mw.g2
p.ww.g2

(p.mm.g2+p.worw.g2)/(p.wm.g2+p.mw.g2+p.ww.g2)
##################################################
## This can also be done for individual groups  ##
##################################################
# Get subset of group labels
gend2_sub_g3=gend_group_2[subset_g3&time_window & has_citations]
gend4_sub_g3=gend_group_4[subset_g3&time_window & has_citations]

# Gap within reference lists of MM papers
citegap(ref_tot_sub_g3[gend2_sub_g3=="MM",],type='conditional')

# Gap within reference lists of W|W papers
citegap(ref_tot_sub_g3[gend2_sub_g3=="W|W",],type='conditional')

# Gap within subgroups of W|W papers
citegap(ref_tot_sub_g3[gend4_sub_g3=="WM",],type='conditional')
citegap(ref_tot_sub_g3[gend4_sub_g3=="MW",],type='conditional')
citegap(ref_tot_sub_g3[gend4_sub_g3=="WW",],type='conditional')

###################################
## Recreate graphs (Supplemental) ##
###################################
# Get bootstrap standard errors for gap values
boot.mm.g3=boot(ref_tot_sub_g3[gend2_sub_g3=="MM",],citegap,R=500)
boot.worw.g3=boot(ref_tot_sub_g3[gend2_sub_g3=="W|W",],citegap,R=500)
boot.wm.g3=boot(ref_tot_sub_g3[gend4_sub_g3=="WM",],citegap,R=500)
boot.mw.g3=boot(ref_tot_sub_g3[gend4_sub_g3=="MW",],citegap,R=500)
boot.ww.g3=boot(ref_tot_sub_g3[gend4_sub_g3=="WW",],citegap,R=500)

# Create ggplot compatible data frames
plot.df.mm.g3=get.plotdf.violin(boot.mm.g3)
plot.df.worw.g3=get.plotdf.violin(boot.worw.g3)
plot.df.wm.g3=get.plotdf.violin(boot.wm.g3)
plot.df.mw.g3=get.plotdf.violin(boot.mw.g3)
plot.df.ww.g3=get.plotdf.violin(boot.ww.g3)
p.mm.g3=violinplot(plot.df.mm.g3,"Citing: MM",ymin=-0.30,ymax=0.30)
p.worw.g3=violinplot(plot.df.worw.g3,"Citing: W or W",ymin=-0.30,ymax=0.30)
p.wm.g3=violinplot(plot.df.wm.g3,"Citing: WM",ymin=-0.30,ymax=0.30)
p.mw.g3=violinplot(plot.df.mw.g3,"Citing: MW",ymin=-0.30,ymax=0.30)
p.ww.g3=violinplot(plot.df.ww.g3,"Citing: WW",ymin=-0.30,ymax=0.30)

# View plots
p.mm.g3
p.worw.g3
p.wm.g3
p.mw.g3
p.ww.g3

(p.mm.g3+p.worw.g3)/(p.wm.g3+p.mw.g3+p.ww.g3)

#################################################################
## Combining all of this information in one plot for Figure 5  ##
#################################################################
groupplotdf=data.frame(Citer = factor(rep(c(rep("MM",2000),rep("WM",2000),rep("MW",2000),rep("WW",2000)),3),
                levels=c("MM","WM","MW","WW")), Cluster = factor(c(rep(1,8000),rep(2,8000),rep(3,8000)),levels=c(1,2,3)))
groupplotdf[1:2000,c('Group','Prop')]=plot.df.mm.g1
groupplotdf[2001:4000,c('Group','Prop')]=plot.df.wm.g1
groupplotdf[4001:6000,c('Group','Prop')]=plot.df.mw.g1
groupplotdf[6001:8000,c('Group','Prop')]=plot.df.ww.g1
groupplotdf[8001:10000,c('Group','Prop')]=plot.df.mm.g2
groupplotdf[10001:12000,c('Group','Prop')]=plot.df.wm.g2
groupplotdf[12001:14000,c('Group','Prop')]=plot.df.mw.g2
groupplotdf[14001:16000,c('Group','Prop')]=plot.df.ww.g2
groupplotdf[16001:18000,c('Group','Prop')]=plot.df.mm.g3
groupplotdf[18001:20000,c('Group','Prop')]=plot.df.wm.g3
groupplotdf[20001:22000,c('Group','Prop')]=plot.df.mw.g3
groupplotdf[22001:24000,c('Group','Prop')]=plot.df.ww.g3
p.mm.all=ggplot(groupplotdf[groupplotdf$Citer=='MM',],aes(x=Group, y=Prop, fill=Cluster))+
    geom_violin(position=position_dodge(0.3))+
    geom_boxplot(width = 0.1,position=position_dodge(0.3))+
    theme_bw()+theme(legend.position="n")+
    geom_hline(yintercept=0,color='black',lty=1)+
    ggtitle('Citing: MM')+
    ylab("Over- and undercitation (%)")+xlab("Cited authors")+
    theme(legend.position="right")+scale_y_continuous(limits=c(-0.3,0.3))
p.wm.all=ggplot(groupplotdf[groupplotdf$Citer=='WM',],aes(x=Group, y=Prop, fill=Cluster))+
    geom_violin(position=position_dodge(0.3))+
    geom_boxplot(width = 0.1,position=position_dodge(0.3))+
    theme_bw()+theme(legend.position="n")+
    geom_hline(yintercept=0,color='black',lty=1)+
    ggtitle('Citing: WM')+
    ylab("Over- and undercitation (%)")+xlab("Cited authors")+
    theme(legend.position="right")+scale_y_continuous(limits=c(-0.3,0.3))
p.mw.all=ggplot(groupplotdf[groupplotdf$Citer=='MW',],aes(x=Group, y=Prop, fill=Cluster))+
    geom_violin(position=position_dodge(0.3))+
    geom_boxplot(width = 0.1,position=position_dodge(0.3))+
    theme_bw()+theme(legend.position="n")+
    geom_hline(yintercept=0,color='black',lty=1)+
    ggtitle('Citing: MW')+
    ylab("Over- and undercitation (%)")+xlab("Cited authors")+
    theme(legend.position="right")+scale_y_continuous(limits=c(-0.3,0.3))
p.ww.all=ggplot(groupplotdf[groupplotdf$Citer=='WW',],aes(x=Group, y=Prop, fill=Cluster))+
    geom_violin(position=position_dodge(0.31))+
    geom_boxplot(width = 0.1,position=position_dodge(0.3))+
    theme_bw()+theme(legend.position="n")+
    geom_hline(yintercept=0,color='black',lty=1)+
    ggtitle('Citing: WW')+
    ylab("Over- and undercitation (%)")+xlab("Cited authors")+
    theme(legend.position="right")+scale_y_continuous(limits=c(-0.3,0.3))

p.mm.all
p.wm.all
p.mw.all
p.ww.all

(p.mm.all+p.wm.all)/(p.mw.all+p.ww.all)

#############################################
## Making Word Clouds ##
#############################################

wordlists = list()

for(i in 1:3 ){
    arts =which(article.data$Cluster==i) # get articles in the group
    words=list()
    for(j in 1:floor(length(arts)/1000)){
        Sys.sleep(1) # prevent timeouts 
        if(!is.na(article.data$DI[arts[j*1000]])){
            a <- try({qdapRegex::ex_between(cr_cn(article.data$DI[arts[j*1000]]), "title={","}")[[1]]  }, silent = TRUE) # get the words 
     
    # a = qdapRegex::ex_between(cr_cn(article.data$DI[arts[j*100]]), "title={","}")[[1]]
        words = append(words, c(strsplit(a," ")))
        } # add the words to the list 
   
   # print(qdapRegex::ex_between(cr_cn(article.data$DI[arts[j*1000]]), "title={","}")[[1]])
    }
    words = unlist(words)
    docs <- Corpus(VectorSource(words))
    docs <- docs %>%
    tm_map(removeNumbers) %>%
    tm_map(removePunctuation) %>%
    tm_map(stripWhitespace)
    docs <- tm_map(docs, content_transformer(tolower))
    docs <- tm_map(docs, removeWords, stopwords("english"))
    dtm <- TermDocumentMatrix(docs) 
    matrix <- as.matrix(dtm) 
    words <- sort(rowSums(matrix),decreasing=TRUE) 
    df <- data.frame(word = names(words),freq=words) # create the dataframe of words sorted by frequency 
    print(wordcloud(words = df$word, freq = df$freq, min.freq = 1,max.words=100, random.order=FALSE, scale=c(2, 0.5),
        colors=brewer.pal(8, "Dark2")))
    wordlists=append(wordlists, df)
    print("Finished group:")
    print(i) 
  }
