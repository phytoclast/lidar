library(cluster)


crownlist <- unique(crowndist$site)
crownformulae <- data.frame(site = 'zzz',intercept=0.1, width=0.1)

for (i in 1:length(crownlist)){
trees <- subset(crowndist, site %in% crownlist[i])
m =  lm(htmax ~ width, trees)
m$coefficients[1]
crownformulae1 <- data.frame(site = crownlist[i],intercept=m$coefficients[1], width=m$coefficients[2])
rownames(crownformulae1) <- crownformulae1$site
crownformulae <- rbind(crownformulae, crownformulae1)
}; crownformulae <- crownformulae[-1,]
crownformulae$w2 <- crownformulae$width*20
d <- dist(crownformulae[,c(2,4)])
dn <- agnes(d, method = 'ward')
plot(as.hclust(dn))
crowndist$gentype <- ifelse(grepl('successional cove hardwoods', crowndist$site), '2.successional cove hardwoods',
                            ifelse(grepl('cove',crowndist$site),'1.cove forest',
                                   ifelse(grepl('successional beech-maple', crowndist$site), '4.successional beech-maple',
                                          ifelse(grepl('beech-maple', crowndist$site), '3.beech-maple','other'))))
trees <- subset(crowndist, !gentype %in% 'other')
m <-  lm(htmax ~ width, trees)
summary(m)

ggplot(trees, aes(x=gentype))+
  geom_boxplot(aes(y=htmax))+
  geom_boxplot(aes(y=ht50))

ggplot(trees, aes(x=width, y=htmax, color=gentype))+
  geom_smooth()+
  scale_x_continuous(name= "Crown Width (m)", 
                     breaks=c(0,10,20,30,40))+
  scale_y_continuous(name= "Height (m)", 
                     breaks=c(0,5,10,15,20,25,30,35,40,45,50,55,60))+
  labs(caption = 'Crown width relative to tree height')


ggplot(trees, aes(x=htmax,fill=gentype, color=gentype))+
  geom_density(alpha = 0.5)+
  scale_x_continuous(name= "Tree Height (m)", 
                     breaks=c(0,5,10,15,20,25,30,35,40,45,50,55,60))+
   labs(caption = 'Tree height distribution')



