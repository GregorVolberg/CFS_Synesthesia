library(R.matlab)
library(tidyverse)

fnames <- list.files('./raw', 
           recursive = TRUE,
           pattern = '*cfs_synesthesia.mat',
           full.names = TRUE)

df = NULL
for (nfile in 1:length(fnames)){
  
  tmp = readMat(fnames[nfile])[[1]][1] %>%
  data.frame() %>%
  add_column(vp  = str_sub(fnames[nfile], start=-23, end=-21),
                   .before = 1) %>%
  add_column(domEye = pluck(readMat(fnames[nfile])[[1]][5],1,1)) %>%
  add_column(trial = 1:nrow(.), .before = 2) %>%
  add_column(grapheme = c(unlist((readMat(fnames[nfile])[[1]][2])), # inducer    
                          unlist((readMat(fnames[nfile])[[1]][3])))[.$X3+(.$X7-1)*4]) # noninducer)  
  df = rbind(df, tmp)
}    

data <- df %>% 
  as_tibble() %>%
  select(vp, trial, X1, X3, X6:X15, domEye, grapheme) %>%
  rename(gaborRotation  = "X6",
         SOA            = "X9",
         RT             = "X10",
         ISI            = "X12",
         xCoord         = "X13",
         yCoord         = "X14",
         gaborWlength   = "X15") %>%
  mutate(targetVF   = if_else(is.element(X1, c(1, 3)), "LVF", "RVF"),
         stimulus   = case_when(
           X7 == 1 ~ "inducer",
           X7 == 2 ~ "non-inducer",
           X7 == 3 ~ "gabor"),
         group      = if_else(str_detect(vp, "K"), "control", "synesthete"),
         response   = case_when(
           X11 == 37 ~ "left",
           X11 == 39 ~ "right"),
         fontType   = case_when(
           X8 == 1 ~ 'arial',
           X8 == 2 ~ 'comicsans',
           X8 == 3 ~ 'verdana',
           X8 == 4 ~ 'times')) %>%
    select(trial, vp, group, stimulus, grapheme, targetVF, response, RT, SOA, ISI, 
           domEye, gaborRotation, gaborWlength,xCoord, yCoord, fontType) %>%
  write_csv(file = "cfs_synesthesia.csv")

# from matlab stimulation script:

# % A 288 x 15 numerical matrix. Each row is one trial.
# % outm(:,1) Position of target. 1 = left side and LVF, 2 = left side and
# %           RVF, 3 = right side and LVF, 4 = right side and RVF. Side depends on
# %           dominant eye.
# % outm(:,2) Mask Type. 1 = colored, 2 = achromatic. Was always 1 in this
# %           experiment.
# % outm(:,3) Grapheme number. Used as an index into cell arrays 'inducer' or
# %           'noninducer' as found in the results structure.
# % outm(:,4) CFS condition. 1 = CFS, 2 = control. Was always CFS in this
# %           experiment.
# % outm(:,5) Trial Type. 1 = normal, 2 = catch trial (without target). Was
# %           always normal in this experiment.
# % outm(:,6) Rotation of target, only for Gabor patches. Given in degrees, counterclockwise from
# %           horizontal orientation.
# % outm(:,7) Stimulus Type. 1 = inducer, 2 = non-inducer, 3 = Gabor patch.
# % outm(:,8) Font Type. 1 = 'Arial', 2 = 'Comic Sans MS', 3 = 'Verdana', 4 = 'Times'
# % outm(:,9) Time of target onset relative to trial onset (in seconds).
# % outm(:,10) Reaction time. Relative to target onset (in seconds).
# % outm(:,11) Key pressed for response. 37 = left arrow, 39 = right arrow.
# % outm(:,12) Inter-stimulus interval. I. e., pause after the trial (in seconds).
# % outm(:,13) X-coordinate of the center of the target stimulus (in pixels). 
# % outm(:,14) Y-coordinate of the center of the target stimulus (in pixels). 
# % outm(:,15) Wavelength lambda of the Gabor patch (in pixels).


# bb <- 
#     map(vp, ~readMat(.)[[1]][1]) %>%
#     map_dfr(~ as_tibble(data.frame(.))) %>%
# 
#   bb <- 
#   map(vp, ~readMat(.)[[1]][1]) %>%
#   map(~ as_tibble(data.frame(.))) %>%
#   map(~ mutate(vpname = pluck(readMat(vp[[1]])[[1]][4],1,1)))
