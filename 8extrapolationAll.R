source('libsAndMore.R')

### input ####
fitsAll <- read.table('logdata/fitsAll.txt')
fitsBootExtr <- read.table('logdata/fitsBootExtr.txt')
fitsBootExtrFreq <- read.table('logdata/fitsBootExtrFreq.txt')
fitsBootExtrTf <- read.table('logdata/fitsBootExtrTf.txt')

fitsExtrAll <- fitsAll %>%
  group_by(radius, number, durationTest) %>%
  do(extrapol(., x = 'speed', y = 'pse', xout = c(speeds,speedsExtr)))

fitsExtrAllFreq <- fitsAll %>%
  group_by(radius, number, durationTest) %>%
  do(extrapol(., x = 'freq', y = 'pse', xout = freqs))

fitsExtrAllTf <- fitsAll %>%
  group_by(radius, number, durationTest) %>%
  do(extrapol(., x = 'tf', y = 'pse', xout = tfs))

### For speed ####
difSpeedRadiusAll <- function(prob){
  fitsBootExtr %>%
    group_by(speed, number, durationTest) %>%
    do(bootdifAll(., 
               y = 'psesample',
               sample = 'sample',
               prob = prob,
               onlySigni = TRUE))
}
fitsBootExtrDifAll <- difSpeedRadiusAll(.025)
fitsBootExtrDif2All <- difSpeedRadiusAll(.005)

difSpeedNumberAll <- function(prob){
  fitsBootExtr %>%
    group_by(speed, radius, durationTest) %>%
    do(bootdifAll(., 
               y = 'psesample',
               sample = 'sample',
               prob = prob,
               onlySigni = TRUE))
}
fitsBootExtrDifNumberAll <- difSpeedNumberAll(.025)
fitsBootExtrDifNumber2All <- difSpeedNumberAll(.005)

### For frequency ####
difFreqRadiusAll <- function(prob){
  fitsBootExtrFreq %>%
    group_by(number, durationTest) %>%
    do(overlaping(., v = 'freq', var = 'radius')) %>%
    group_by(freq, number, durationTest) %>%
    do(bootdifAll(.,
               y = 'psesample',
               sample = 'sample',
               prob = prob,
               onlySigni = TRUE))
}
fitsBootExtrDifFreqAll <- difFreqRadiusAll(.025)
fitsBootExtrDif2FreqAll <- difFreqRadiusAll(.005)

difFreqNumberAll <- function(prob){
  fitsBootExtrFreq %>%
    group_by(radius, durationTest) %>%
    do(overlaping(., v = 'freq', var = 'number')) %>%
    group_by(freq, radius, durationTest) %>%
    do(bootdifAll(.,
               y = 'psesample',
               sample = 'sample',
               prob = prob,
               onlySigni = TRUE))
}
fitsBootExtrDifNumberFreqAll <- difFreqNumberAll(.025)
fitsBootExtrDifNumber2FreqAll <- difFreqNumberAll(.005)

### For local temporal frequency ####
difTfRadiusAll <- function(prob){
  fitsBootExtrTf %>%
    group_by(number, durationTest) %>%
    do(overlaping(., v = 'tf', var = 'radius')) %>%
    group_by(tf, number, durationTest) %>%
    do(bootdifAll(.,
               y = 'psesample',
               sample = 'sample',
               prob = prob,
               onlySigni = TRUE))
}
fitsBootExtrDifTfAll <- difTfRadiusAll(.025)
fitsBootExtrDif2TfAll <- difTfRadiusAll(.005)

difTfNumberAll <- function(prob){
  fitsBootExtrTf %>%
    group_by(radius, durationTest) %>%
    do(overlaping(., v = 'tf', var = 'number')) %>%
    group_by(tf, radius, durationTest) %>%
    do(bootdifAll(.,
               y = 'psesample',
               sample = 'sample',
               prob = prob,
               onlySigni = TRUE))
}
fitsBootExtrDifNumberTfAll <- difTfNumberAll(.025)
fitsBootExtrDifNumber2TfAll <- difTfNumberAll(.005)

### output ####
write.table(fitsExtrAll, 'logdata/fitsExtrAll.txt')
write.table(fitsExtrAllFreq, 'logdata/fitsExtrAllFreq.txt')
write.table(fitsExtrAllTf, 'logdata/fitsExtrAllTf.txt')


write.table(fitsBootExtrDifAll,
            'logdata/fitsBootExtrDifAll.txt')
write.table(fitsBootExtrDif2All,
            'logdata/fitsBootExtrDif2All.txt')
write.table(fitsBootExtrDifNumberAll,
            'logdata/fitsBootExtrDifNumberAll.txt')
write.table(fitsBootExtrDifNumber2All,
            'logdata/fitsBootExtrDifNumber2All.txt')

write.table(fitsBootExtrDifFreqAll,
            'logdata/fitsBootExtrDifFreqAll.txt')
write.table(fitsBootExtrDif2FreqAll,
            'logdata/fitsBootExtrDif2FreqAll.txt')
write.table(fitsBootExtrDifNumberFreqAll,
            'logdata/fitsBootExtrDifNumberFreqAll.txt')
write.table(fitsBootExtrDifNumber2FreqAll,
            'logdata/fitsBootExtrDifNumber2FreqAll.txt')


write.table(fitsBootExtrDifTfAll,
            'logdata/fitsBootExtrDifTfAll.txt')
write.table(fitsBootExtrDif2TfAll,
            'logdata/fitsBootExtrDif2TfAll.txt')
write.table(fitsBootExtrDifNumberTfAll,
            'logdata/fitsBootExtrDifNumberTfAll.txt')
write.table(fitsBootExtrDifNumber2TfAll,
            'logdata/fitsBootExtrDifNumber2TfAll.txt')
