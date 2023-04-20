rm(list=ls())  

install.packages("vegan")
install.packages("gclus")
install.packages("FD")
install.packages("factoextra")

library(ade4)
library(vegan)
library(cluster)
library(gclus)
library(FD)
library(ggplot2)
library(caret)
library(tidyverse)

#1 Read in the doubs into your R and delete the site 8 which has no fishes.
data(doubs, package = "ade4")
doubs

spe <- doubs$fish
summary(spe)
spe <- spe[-8, ] #remove site with no data
env <- doubs$env
env <- env[-8, ]
fish <- doubs$fish
fish <- doubs$fish %>% 
  mutate(fish,tfs=rowSums(doubs$fish))
total_fish<-subset(fish,tfs!=0)

#Which site has the most species (and how many species)? 
seq(along=total_fish[,28])[total_fish[,28]==max(total_fish[,28])]
seq(along=total_fish[,28])[total_fish[,28]==min(total_fish[,28])]
max<-max(total_fish$tfs)
min<-min(total_fish$tfs)

#Which species is the most widespread (found in the most sites)?
mwide_species=data.frame()
for (i in 1:27){
  null_0 <-sum(total_fish[,i]==0)
  mwide_species=rbind(mwide_species,cbind(colnames(total_fish[i]),null_0))
}
head(mwide_species)
mwide_species
#seq(along=mw_species[,2])[mw_species[,2]==min(mw_species[,2])]
#min_species<-min(mw_species$null_0)

#2. Select a suitable association measure of species.
#2.1 选择R模式聚类鱼类群落
spe.t<-t(spe)  #用t函数转置矩阵（物种丰度）
spe.t.chi<-decostand(spe.t,"chi.square")   #卡方转化
spe.t.dhel<-dist(spe.t.chi)
#计算ward & single最小方差聚类
spe.t.chi.single<-hclust(spe.t.dhel,method = "single")
plot(spe.t.chi.single)
spe.t.chi.ward <- hclust(spe.t.dhel, method="ward.D2")
plot(spe.t.chi.ward)
#2.2 选择Q模式聚合地点
spe.norm<-decostand(spe,"normalize")
spe.ch<-vegdist(spe.norm,"euc")  #弦距离矩阵
spe.dc<- vegdist(spe.norm)   #计算Chord距离聚类
spe.hel<-decostand(spe,"hel")
spe.dh<-vegdist(spe.hel)     # Hellinger距离矩阵
#计算ward & single最小方差聚类
spe.ch.ward <- hclust(spe.ch, method="ward.D2")   
spe.ch.single<-hclust(spe.ch,method = "single")
plot(spe.ch.ward)
plot(spe.ch.single)
#2.3 选择Q模式聚类env
env.norm <- decostand(env, "normalize")
env.ch <- vegdist(env.norm)  #弦距离矩阵
env.ch.ward <- hclust(env.ch, method="ward.D2")   #计算ward & single最小方差聚类
env.ch.single<-hclust(env.ch,method = "single")
plot(env.ch.ward)
plot(env.ch.single)
#鱼类群落与地点均可对应

#3. Do RDA analysis.
#选择正确的分析方法
decorana(spe)
#Axis Lengths值在3.0~4.0之间,选择RDA
RDA<-rda(spe,env,scale=T)
#提取数据
spe_rda<-data.frame(RDA$CCA$u[,1:2],rownames(env))
colnames(spe_rda)=c("RDA1","RDA2","samples")
#提取物种得分
spe_rda_score<-data.frame(RDA$CCA$v[,1:2])
#计算轴标签数据（=轴特征值/sum(所有轴的特征值)
RDA1=round(RDA$CCA$eig[1]/sum(RDA$CCA$eig)*100,2)
RDA2=round(RDA$CCA$eig[2]/sum(RDA$CCA$eig)*100,2)
plot(RDA)
#物种数据 Hellinger 预转化
spe_hel <- decostand(spe, method = 'hellinger')
spe_rda <- rda(spe_hel~., env, scale = FALSE)   #使用全部的环境数据
plot(spe_rda)    #绘图
summary(spe_rda)  
#除pH外的其他因素均对鱼类分布有显著影响