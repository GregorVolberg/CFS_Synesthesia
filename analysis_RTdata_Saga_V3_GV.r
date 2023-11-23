# see get_raw_data.r for description

library(tidyverse)
library(ez)

dframe <-
  read_csv('./cfs_synesthesia.csv') %>%
  mutate(across(c(vp:stimulus, targetVF,
                  response, fontType), as_factor),
         correct   = (targetVF == 'LVF' & response  == 'left') |
                     (targetVF == 'RVF' & response == 'right'))
#      mutate(., na = as.numeric(is.na(correct)))

# criterion1 for case removal:
# proportion NA > .04.
# only apply to grapheme trials
crit1 <- dframe %>% 
  filter(!stimulus == "gabor") %>%
  group_by(vp) %>%
  summarize(avg = sum(is.na(response))/length(response)) %>%
  filter(avg > .4) %>%
  pull(vp) %>%
  as.character()

# criterion 2 for case removal:
# number of trials != 288 
crit2 <- dframe %>% 
    group_by(vp) %>%
    summarize(nrow = n()) %>%
    filter(nrow != 288) %>%
    pull(vp) %>%
    as.character()

# remove cases that meet criterion 1 or 2
cat(c('remove cases: ', union(crit1, crit2)), '\n')
dframe = dframe %>%
  filter(! vp %in% union(crit1, crit2))

# overall valid responses
ezdat = dat %>%
  group_by(vp) %>%
  summarize(avg = sum(na)/length(na))
cat('valid responses (M +- SD): ', 1-mean(ezdat$avg), '+-', sd(ezdat$avg), '\n')

# overall accuracy
ezdat = dat %>%
  group_by(vp) %>%
  filter(na==F) %>%
  summarize(avg = sum(correct)/length(correct))
cat('accuracy (M +- SD): ', mean(ezdat$avg), '+-', sd(ezdat$avg), '\n')

# nochmal pro stimulusbedingung
ezdat = dat %>%
  group_by(vp, graphtype) %>%
  summarize(avg = sum(na)/length(na))
aggregate(ezdat$avg, FUN=mean, by = list(ezdat$graphtype))

# overall accuracy
ezdat = dat %>%
  group_by(vp, graphtype) %>%
  filter(na==F) %>%
  summarize(avg = sum(correct)/length(correct))
aggregate(ezdat$avg, FUN=mean, by = list(ezdat$graphtype))


# RT distributions, syn vs. control
przdat =  dat %>%
  filter(correct == 1) %>%
  group_by(vp, group)

desiredquantiles = seq(0.05,0.95,0.05)
nparticipants1 = length(unique(przdat$vp[przdat$group == 'syn']))
nparticipants2 = length(unique(przdat$vp[przdat$group == 'ctr']))
nconditions   = 2

quantdata1 = data.frame(avg = numeric(nparticipants1)/0) # 8 conditions, pre-allocate
quantdata2 = data.frame(avg = numeric(nparticipants2)/0) # 8 conditions, pre-allocate

for (i in 1:length(desiredquantiles)){
  quantdata1[,i] =  przdat %>%
    filter(group == 'syn') %>%
    summarize(avg = quantile(RT, desiredquantiles[i]))%>%
    ungroup() %>%
    select(avg)

  quantdata2[,i] =  przdat %>%
    filter(group == 'ctr') %>%
    summarize(avg=quantile(RT, desiredquantiles[i]))%>%
    ungroup() %>%
    select(avg)}

xx=apply(quantdata1, 2, mean)
yy=apply(quantdata2, 2, mean)
plot(xx, desiredquantiles, ylim=c(0,1), xlim=c(0,8), type='l', col='red', xlab = 'ReactionTime (s)', ylab = 'Quantile', lwd=2)
points(yy, desiredquantiles, type='l', col='black')
sexx  = apply(quantdata1, 2, FUN= sd)/sqrt(nparticipants1)
seqq2 = apply(quantdata2, 2, FUN= sd)/sqrt(nparticipants2)
yyy = c(desiredquantiles, rev(desiredquantiles))
xxx  = c(xx-sexx, rev(xx+sexx))
xxx2 = c(yy-seqq2, rev(yy+seqq2))
polygon(xxx,yyy,col='indianred', border = 'indianred')
polygon(xxx2,yyy, col='lightskyblue', border = 'lightskyblue')
points(xx, desiredquantiles, ylim=c(0,1), xlim=c(0,8), type='l', col='red', lwd=2)
points(yy, desiredquantiles, type='l', col='blue')
text(6, 0.6, 'ctr', col='blue')
text(6, 0.5, 'syn', col='red')

# distribution for inducer and noninducer ==========
przdat =  dat %>%
  filter(correct == 1) %>%
  group_by(vp, group)

desiredquantiles = seq(0.05,0.95,0.05)
nparticipants1 = length(unique(przdat$vp[przdat$group == 'syn']))
nparticipants2 = length(unique(przdat$vp[przdat$group == 'ctr']))
nconditions   = 2

quantdata1 = data.frame(avg = numeric(nparticipants1)/0) # 8 conditions, pre-allocate
quantdata2 = data.frame(avg = numeric(nparticipants1)/0) # 8 conditions, pre-allocate

cquantdata1 = data.frame(avg = numeric(nparticipants2)/0) # 8 conditions, pre-allocate
cquantdata2 = data.frame(avg = numeric(nparticipants2)/0) # 8 conditions, pre-allocate

gquantdata1 = data.frame(avg = numeric(nparticipants1)/0) # 8 conditions, pre-allocate
gquantdata2 = data.frame(avg = numeric(nparticipants2)/0) # 8 conditions, pre-allocate


for (i in 1:length(desiredquantiles)){
  quantdata1[,i] =  przdat %>%
    filter(group == 'syn' & graphtype == 'inducer') %>%
    summarize(avg = quantile(RT, desiredquantiles[i]))%>%
    ungroup() %>%
    select(avg)
  quantdata2[,i] =  przdat %>%
    filter(group == 'syn' & graphtype == 'noninducer') %>%
    summarize(avg=quantile(RT, desiredquantiles[i]))%>%
    ungroup() %>%
    select(avg)

  cquantdata1[,i] =  przdat %>%
    filter(group == 'ctr' & graphtype == 'inducer') %>%
    summarize(avg = quantile(RT, desiredquantiles[i]))%>%
    ungroup() %>%
    select(avg)
  cquantdata2[,i] =  przdat %>%
    filter(group == 'ctr' & graphtype == 'noninducer') %>%
    summarize(avg=quantile(RT, desiredquantiles[i]))%>%
    ungroup() %>%
    select(avg)}

xx=apply(quantdata1, 2, mean)
yy=apply(quantdata2, 2, mean)
sexx  = apply(quantdata1, 2, FUN= sd)/sqrt(nparticipants1)
seqq2 = apply(quantdata2, 2, FUN= sd)/sqrt(nparticipants2)
yyy = c(desiredquantiles, rev(desiredquantiles))
xxx  = c(xx-sexx, rev(xx+sexx))
xxx2 = c(yy-seqq2, rev(yy+seqq2))
polygon(xxx,yyy,col='indianred', border = 'indianred')
polygon(xxx2,yyy, col='lightskyblue', border = 'lightskyblue')

plot(xx, desiredquantiles, ylim=c(0,1), xlim=c(0,8), type='l', col='red', xlab = 'ReactionTime (s)', ylab = 'Quantile', lwd=2)
points(yy, desiredquantiles, type='l', col='blue')
points(xx, desiredquantiles, type='l', col='red')
text(6, 0.5, 'syn-ind', col='red')
text(6, 0.6, 'syn-nonind', col='blue')

xx=apply(cquantdata1, 2, mean)
yy=apply(cquantdata2, 2, mean)
plot(xx, desiredquantiles, ylim=c(0,1), xlim=c(0,8), type='l', col='red', xlab = 'ReactionTime (s)', ylab = 'Quantile', lwd=2)
points(yy, desiredquantiles, type='l', col='black')
text(6, 0.5, 'ctr-ind', col='red')
text(6, 0.6, 'ctr-nonind', col='blue')


# ANOVA on mean RT differences, select quantile > 0.4 ==========
ezdat =  dat %>%
  filter(correct==1 & (graphtype == 'inducer' | graphtype == 'noninducer')) %>%
  group_by(vp, group, graphtype) %>%
#  filter((RT > quantile(RT, 0.4)) & (RT < quantile(RT, 0.8))) %>%
  filter((RT > quantile(RT, 0.4))) %>%
  summarize(avg = mean(RT))

erg1 = ezdat %>% ezANOVA(dv = avg, wid = vp, within = list(graphtype), between = list(group))
erg2 = ezdat %>% ezStats(dv = avg, wid = vp, within = list(graphtype), between = list(group))
ezdat %>% ezPlot(dv  = avg, wid = vp, within = list(graphtype), between = list(group),
                 x=graphtype, col=group, do_lines=F)
print(erg1)
print(erg2)


# ANOVA on mean RT differences for font type, select quantile > 0.4 ==========
ezdat =  dat %>%
  filter(correct==1 & (graphtype == 'inducer' | graphtype == 'noninducer')) %>%
  group_by(vp, group, graphtype) %>%
  #  filter((RT > quantile(RT, 0.4)) & (RT < quantile(RT, 0.8))) %>%
  filter((RT > quantile(RT, 0.4))) %>%
  ungroup() %>%
  group_by(vp, group, fonttype) %>%
  summarize(avg = mean(RT))

erg1 = ezdat %>% ezANOVA(dv = avg, wid = vp, within = list(fonttype), between = list(group))
erg2 = ezdat %>% ezStats(dv = avg, wid = vp, within = list(fonttype), between = list(group))
ezdat %>% ezPlot(dv  = avg, wid = vp, within = list(graphtype), between = list(group),
                 x=graphtype, col=group, do_lines=F)
print(erg1)
print(erg2)



# correlation with Projector-Associator-Score =========
xls = import("Part.Data.xlsx") %>%
      as_tibble() %>%
      select(., c(1:7)) %>%
      set_names(c('vp', 'age', 'gender', 'DomEye', 'handScore', 'PAScore',
              'PAType')) %>%
      filter(., grepl('S', .$vp)) %>%
      mutate(., vpnum = c(19:36)) %>% # Achtung: in tibble 'dat' ist syn 19:36
      filter(., !(vpnum %in% rem1)) %>%
      mutate(., pa = as.numeric(gsub(',','.',.$PAScore)))

indu = filter(ezdat, group == 'syn' & graphtype == 'inducer') %>%
       pull(., avg)
noni = filter(ezdat, group == 'syn' & graphtype == 'noninducer') %>%
  pull(., avg)

plot((noni - indu), xls$pa, xlab = 'RT (noninducer - inducer)', ylab = 'PA Score')
abline(lm(xls$pa ~ (noni - indu)))
cor.test(xls$pa, (noni - indu))

plot(indu, xls$pa, xlab = 'RT inducer', ylab = 'PA Score')
abline(lm(xls$pa ~ indu))
cor.test(indu, xls$pa)

# plot Gabor RT
przdat =  dat %>%
  filter(correct == 1 & vp != 19) %>%
  group_by(vp, group)

# vp19 hat keinen einzigen gaborpatch gesehen
#dat[dat$vp == 19 & dat$correct == 1 & dat$graphtype == 'gaborpatch',]
nparticipants1 = length(unique(przdat$vp[przdat$group == 'syn']))
nparticipants2 = length(unique(przdat$vp[przdat$group == 'ctr']))

gquantdata1 = data.frame(avg = numeric(nparticipants1)/0) # 8 conditions, pre-allocate
gquantdata2 = data.frame(avg = numeric(nparticipants2)/0) # 8 conditions, pre-allocate


for (i in 1:length(desiredquantiles)){

  gquantdata1[,i] =  przdat %>%
    filter(group == 'syn' & graphtype == 'gaborpatch') %>%
    summarize(avg=quantile(RT, desiredquantiles[i]))%>%
    ungroup() %>%
    select(avg)
  gquantdata2[,i] =  przdat %>%
    filter(group == 'ctr' & graphtype == 'gaborpatch') %>%
    summarize(avg=quantile(RT, desiredquantiles[i]))%>%
    ungroup() %>%
    select(avg)}

xx=apply(gquantdata1, 2, mean)
yy=apply(gquantdata2, 2, mean)
plot(xx, desiredquantiles, ylim=c(0,1), xlim=c(0,8), type='l', col='red', xlab = 'ReactionTime (s)', ylab = 'Quantile', lwd=2)
points(yy, desiredquantiles, type='l', col='black')
text(6, 0.5, 'syn-gabor', col='red')
text(6, 0.6, 'ctr-gabor', col='blue')

# plot(apply(gquantdata1, 2, mean), desiredquantiles, ylim=c(0,1), xlim=c(0,10), type='l', col='black', xlab = 'ReactionTime (s)', ylab = 'Quantile', lwd=2)
# points(apply(quantdata1, 2, mean), desiredquantiles, type='l', col='red')
# points(apply(quantdata2, 2, mean), desiredquantiles, type='l', col='blue')
# text(6, 0.5, 'syn-gabor', col='black')
# text(6, 0.6, 'syn-indr', col='red')
# text(6, 0.7, 'syn-nonind', col='blue')

# correlatin with PA and Gabor (remove vp 19) ====
ezdat =  dat %>%
  filter(correct==1 & group == 'syn' & !vp==19 & graphtype == 'gaborpatch') %>%
  group_by(vp) %>%
  #  filter((RT > quantile(RT, 0.4)) & (RT < quantile(RT, 0.8))) %>%
  filter((RT > quantile(RT, 0.4))) %>%
  summarize(avg = mean(RT))

gab =  pull(ezdat, avg)
pa_select = xls$pa[xls$vpnum != 19]

plot(gab, pa_select, xlab = 'RT Gabor', ylab = 'PA Score')
abline(lm(pa_select ~ gab))
cor.test(pa_select, gab)

# ANOVA on fraction NAN ==========
ezdat = dat %>%
  group_by(vp, group, graphtype) %>%
  summarize(avg = sum(na)/length(na))
aggregate(ezdat$avg, FUN=mean, by = list(ezdat$graphtype, ezdat$group))

erg1 = ezdat %>% ezANOVA(dv = avg, wid = vp, within = list(graphtype), between = list(group))
erg2 = ezdat %>% ezStats(dv = avg, wid = vp, within = list(graphtype), between = list(group))
ezdat %>% ezPlot(dv  = avg, wid = vp, within = list(graphtype), between = list(group),
                 x=graphtype, col=group, do_lines=F)
print(erg1)
print(erg2)

# Corr on fraction NAN ==========


# post hoc
phoc = dat %>%
  filter(graphtype != 'gaborpatch') %>%
  group_by(vp, group, graphtype) %>%
  summarize(avg = sum(na)/length(na))
erg1 = phoc %>% ezANOVA(dv = avg, wid = vp, within = list(graphtype), between = list(group))
erg2 = phoc %>% ezStats(dv = avg, wid = vp, within = list(graphtype), between = list(group))
phoc %>% ezPlot(dv  = avg, wid = vp, within = list(graphtype), between = list(group),
                 x=graphtype, col=group, do_lines=F)
print(erg1)
print(erg2)


indu = filter(ezdat, group == 'syn' & graphtype == 'inducer') %>%
  pull(., avg)
noni = filter(ezdat, group == 'syn' & graphtype == 'noninducer') %>%
  pull(., avg)
gab = filter(ezdat, group == 'syn' & graphtype == 'gaborpatch') %>%
  pull(., avg)

plot((noni - indu), xls$pa, xlab = 'Fraction NA (noninducer - inducer)', ylab = 'PA Score')
abline(lm(xls$pa ~ (noni - indu)))
cor.test((noni - indu), xls$pa)

plot(indu, xls$pa, xlab = 'Fraction NA, inducer', ylab = 'PA Score')
abline(lm(xls$pa ~ indu))
cor.test(indu, xls$pa)





# ANOVA on accuracy
ezdat = dat %>%
  filter(graphtype != 'gaborpatch') %>%
  group_by(vp, group, graphtype) %>%
  filter(na==F) %>%
  summarize(avg = sum(correct)/length(correct))
aggregate(ezdat$avg, FUN=mean, by = list(ezdat$graphtype, ezdat$group))
erg1 = ezdat %>% ezANOVA(dv = avg, wid = vp, within = list(graphtype), between = list(group))
erg2 = ezdat %>% ezStats(dv = avg, wid = vp, within = list(graphtype), between = list(group))
ezdat %>% ezPlot(dv  = avg, wid = vp, within = list(graphtype), between = list(group),
                 x=graphtype, col=group, do_lines=F)
print(erg1)
print(erg2)


#
#
# # Fig. S1B
# ezp1 = ezdat %>% ezANOVA(dv = avg, wid = vp, within = list(cfs, graphtype), detailed=T)
# ezp  = ezdat %>% ezStats(dv = avg, wid = vp, within = list(cfs, graphtype), within_full = c(cfs, masktype, graphtype))
# SE = sqrt((ezp1$ANOVA$SSd[4]/ezp1$ANOVA$DFd[4])/18) # compute SSerror for target effect, then MSerror; see Loftus & Masson, 1994
# ezp$SE = rep(SE,4)
# fs1b = ggplot(data=ezp, aes(x=graphtype, y=Mean, fill = cfs)) +
#   geom_bar(stat="identity", color='black', position=position_dodge(), width=0.7) +
#   scale_fill_grey() +
#   geom_errorbar(aes(ymin=Mean-SE, ymax=Mean+SE), width=.2,
#                 position=position_dodge(.7)) +
#   coord_cartesian(ylim = c(0, 4)) +
#   theme_classic()
# print(fs1b)
#
# # Fig S1C
# ezp1 = ezdat %>% ezANOVA(dv = avg, wid = vp, within = list(cfs, masktype), detailed=T)
# ezp  = ezdat %>% ezStats(dv = avg, wid = vp, within = list(cfs, masktype), within_full = c(cfs, masktype, graphtype))
# SE = sqrt((ezp1$ANOVA$SSd[4]/ezp1$ANOVA$DFd[4])/18)
# ezp$SE = rep(SE,4)
# fs1c = ggplot(data=ezp, aes(x=masktype, y=Mean, fill = cfs)) +
#   geom_bar(stat="identity", color='black', position=position_dodge(), width=0.7) +
#   scale_fill_grey() +
#   geom_errorbar(aes(ymin=Mean-SE, ymax=Mean+SE), width=.2,
#                 position=position_dodge(.7)) +
#   coord_cartesian(ylim = c(0, 4)) +
#   theme_classic()
# print(fs1c)
#
# # Fig S1D
# ezp1 = ezdat %>% ezANOVA(dv = avg, wid = vp, within = list(graphtype, masktype), detailed=T)
# ezp  = ezdat %>% ezStats(dv = avg, wid = vp, within = list(graphtype, masktype), within_full = c(cfs, masktype, graphtype))
# SE = sqrt((ezp1$ANOVA$SSd[4]/ezp1$ANOVA$DFd[4])/18)
# ezp$SE = rep(SE,4)
# fs1d = ggplot(data=ezp, aes(x=masktype, y=Mean, fill = graphtype), ylim=c(0,3.5)) +
#   geom_bar(stat="identity", color='black', position=position_dodge(), width=0.7) +
#   scale_fill_grey() +
#   geom_errorbar(aes(ymin=Mean-SE, ymax=Mean+SE), width=.2,
#                 position=position_dodge(.7)) +
#   coord_cartesian(ylim = c(0, 4)) +
#   theme_classic()
# print(fs1d)
