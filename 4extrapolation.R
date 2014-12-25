source('libsAndMore.R')

### inputs ####
fits <- read.table('logdata/fits.txt')
fitsBoot <- read.table('logdata/fitsBoot.txt')
speedMatch <- read.table('logdata/speedMatch.txt')
freqMatch <- read.table('logdata/freqMatch.txt')

### For speed ####
speeds <- sort(unique(fits$speed))
logSpeeds <- log(speeds)
logSpeedsExtr <- logSpeeds + .5 * first(diff(log(speeds)))
speedsExtr <- exp(logSpeedsExtr)
speedsExtr <- head(speedsExtr,-1)

fitsExtr <- fits %>%
  group_by(subject, radius, number, durationTest) %>%
  do(extrapol(., x = 'speed', y = 'pse', xout = c(speeds,speedsExtr)))

fitsExtrFreq <- fits %>%
  group_by(subject, radius, number, durationTest) %>%
  do(extrapol(., x = 'freq', y = 'pse', xout =freqs))

fitsBootExtr <- fitsBoot %>%
  group_by(subject, radius, number, durationTest,sample) %>%
  do(extrapol(., x = 'speed', y = 'psesample', xout = c(speeds,speedsExtr)))

difSpeedRadius <- function(prob){
  fitsBootExtr %>%
    group_by(subject, speed, number, durationTest) %>%
    do(bootdif(., y = 'psesample',sample = 'sample',
               prob = prob, onlySigni = TRUE))
}
fitsBootExtrDif <- difSpeedRadius(.025)
fitsBootExtrDif2 <- difSpeedRadius(.005)

difSpeedNumber <- function(prob){
  fitsBootExtr %>%
    group_by(subject, speed, radius, durationTest) %>%
    do(bootdif(., y = 'psesample', sample = 'sample',
               prob = prob, onlySigni = TRUE))
}
fitsBootExtrDifNumber <- difSpeedNumber(.025)
fitsBootExtrDifNumber2 <- difSpeedNumber(.005)

### For frequency ####
freqs <- sort(unique(fits$freq))

fitsBootExtrFreq <- fitsBoot %>%
  group_by(subject, radius, number, durationTest,sample) %>%
  do(extrapol(., x = 'freq', y = 'psesample', xout = freqs))

difFreqRadius <- function(prob){
  fitsBootExtrFreq %>%
    group_by(subject, number, durationTest) %>%
    do(overlaping(., v = 'freq', var = 'radius')) %>%
    group_by(subject, freq, number, durationTest) %>%
    do(bootdif(.,
               y = 'psesample',
               sample = 'sample',
               prob = prob,
               onlySigni = TRUE))
}
fitsBootExtrDifFreq <- difFreqRadius(.025)
fitsBootExtrDif2Freq <- difFreqRadius(.005)

difFreqNumber <- function(prob){
  fitsBootExtrFreq %>%
    group_by(subject, radius, durationTest) %>%
    do(overlaping(., v = 'freq', var = 'number')) %>%
    group_by(subject, freq, radius, durationTest) %>%
    do(bootdif(.,
               y = 'psesample',
               sample = 'sample',
               prob = prob,
               onlySigni = TRUE))
}
fitsBootExtrDifNumberFreq <- difFreqNumber(.025)
fitsBootExtrDifNumber2Freq <- difFreqNumber(.005)

### For local temporal frequency ####
tfs <- sort(unique(fits$tf))

fitsBootExtrTf <- fitsBoot %>%
  group_by(subject, radius, number, durationTest,sample) %>%
  do(extrapol(., x = 'tf', y = 'psesample', xout = tfs))

difTfRadius <- function(prob){
  fitsBootExtrTf %>%
    group_by(subject, number, durationTest) %>%
    do(overlaping(., v = 'tf', var = 'radius')) %>%
    group_by(subject, tf, number, durationTest) %>%
    do(bootdif(.,
               y = 'psesample',
               sample = 'sample',
               prob = prob,
               onlySigni = TRUE))
}
fitsBootExtrDifTf <- difTfRadius(.025)
fitsBootExtrDif2Tf <- difTfRadius(.005)

difTfNumber <- function(prob){
  fitsBootExtrTf %>%
    group_by(subject, radius, durationTest) %>%
    do(overlaping(., v = 'tf', var = 'number')) %>%
    group_by(subject, tf, radius, durationTest) %>%
    do(bootdif(.,
               y = 'psesample',
               sample = 'sample',
               prob = prob,
               onlySigni = TRUE))
}
fitsBootExtrDifNumberTf <- difTfNumber(.025)
fitsBootExtrDifNumber2Tf <- difTfNumber(.005)

### For matched speed ####
speedMatchAll <- rbind_list(speedMatch, freqMatch)

fitsBootspeedMatch <- merge(fitsBoot,speedMatchAll)

fitsExtrSpeedMatch <- merge(fits, speedMatchAll) %>%
  group_by(subject, radius, number, durationTest) %>%
  do(extrapolSpeedMatch(., x = 'speedMatch', y = 'pse', dSpeed =speedMatchAll))

fitsBootExtrSpeedMatch <- fitsBootspeedMatch %>%
  group_by(subject, radius, number, durationTest, sample) %>%
  do(extrapolSpeedMatch(., x = 'speedMatch', y = 'psesample', 
                        dSpeed = speedMatchAll ))


difSpeedMatchRadius <- function(prob){
  fitsBootExtrSpeedMatch %>%
    group_by(subject, number, durationTest) %>%
    do(overlaping(., v = 'speedMatch', var = 'radius')) %>%
    group_by(subject, speedMatch, number, durationTest) %>%
    do(bootdif(.,
               y = 'psesample',
               sample = 'sample',
               prob = prob,
               onlySigni = TRUE))
}
fitsBootExtrDifSpeedMatch <- difSpeedMatchRadius(.025)
fitsBootExtrDif2SpeedMatch <- difSpeedMatchRadius(.005)

fitsBootExtrSpeedMatchNumber <- fitsBootspeedMatch %>%
  group_by(subject, radius, number, durationTest, sample) %>%
  do(extrapolSpeedMatchNumber(., x = 'speedMatch', y = 'psesample', 
                        dSpeed = speedMatchAll))

difSpeedMatchNumber <- function(prob){
  fitsBootExtrSpeedMatchNumber %>%
    group_by(subject, radius, durationTest) %>%
    do(overlaping(., v = 'speedMatch', var = 'number')) %>%
    group_by(subject, speedMatch, radius, durationTest) %>%
    do(bootdif(.,
               y = 'psesample',
               sample = 'sample',
               prob = prob,
               onlySigni = TRUE))
}
fitsBootExtrDifNumberSpeedMatch <- difSpeedMatchNumber(.025)
fitsBootExtrDifNumber2SpeedMatch <- difSpeedMatchNumber(.005)

### For matched frequency ####
fitsBootfreqMatch <- merge(fitsBoot,freqMatch)

fitsExtrFreqMatch <- merge(fits, fitsMatchFreq) %>%
  group_by(subject, radius, number, durationTest) %>%
  do(extrapolSpeedMatch(., x = 'freqMatch', y = 'pse', dSpeed =freqMatch))

fitsBootExtrFreqdMatch <- fitsBootfreqMatch %>%
  group_by(subject, radius, number, durationTest, sample) %>%
  do(extrapolSpeedMatch(., x = 'freqMatch', y = 'psesample', 
                        dSpeed = freqMatch ))

difFreqMatchRadius <- function(prob){
  fitsBootExtrFreqdMatch %>%
    group_by(subject, number, durationTest) %>%
    do(overlaping(., v = 'freqMatch', var = 'radius')) %>%
    group_by(subject, freqMatch, number, durationTest) %>%
    do(bootdif(.,
               y = 'psesample',
               sample = 'sample',
               prob = prob,
               onlySigni = TRUE))
}
fitsBootExtrDifFreqMatch <- difFreqMatchRadius(.025)
fitsBootExtrDif2FreqMatch <- difFreqMatchRadius(.005)

fitsBootExtrFreqMatchNumber <- fitsBootfreqMatch %>%
  group_by(subject, radius, number, durationTest, sample) %>%
  do(extrapolSpeedMatchNumber(., x = 'freqMatch', y = 'psesample', 
                              dSpeed = freqMatch))

difFreqMatchNumber <- function(prob){
  fitsBootExtrFreqMatchNumber %>%
    group_by(subject, radius, durationTest) %>%
    do(overlaping(., v = 'freqMatch', var = 'number')) %>%
    group_by(subject, freqMatch, radius, durationTest) %>%
    do(bootdif(.,
               y = 'psesample',
               sample = 'sample',
               prob = prob,
               onlySigni = TRUE))
}
fitsBootExtrDifNumberFreqMatch <- difFreqMatchNumber(.025)
fitsBootExtrDifNumber2FreqMatch <- difFreqMatchNumber(.005)

### output ####

# write.table(fitsBootExtr, 'logdata/fitsBootExtr.txt')
# write.table(fitsBootExtrFreq, 'logdata/fitsBootExtrFreq.txt')
# write.table(fitsBootExtrTf, 'logdata/fitsBootExtrTf.txt')

write.table(fitsExtr, 'logdata/fitsExtr.txt')
write.table(fitsExtrFreq, 'logdata/fitsExtrFreq.txt')
write.table(fitsExtrSpeedMatch, 'logdata/fitsExtrSpeedMatch.txt')
write.table(fitsExtrFreqMatch, 'logdata/fitsExtrFreqMatch.txt')


write.table(fitsBootExtrDif,
            'logdata/fitsBootExtrDif.txt')
write.table(fitsBootExtrDif2,
            'logdata/fitsBootExtrDif2.txt')
write.table(fitsBootExtrDifNumber,
            'logdata/fitsBootExtrDifNumber.txt')
write.table(fitsBootExtrDifNumber2,
            'logdata/fitsBootExtrDifNumber2.txt')

write.table(fitsBootExtrDifFreq,
            'logdata/fitsBootExtrDifFreq.txt')
write.table(fitsBootExtrDif2Freq,
            'logdata/fitsBootExtrDif2Freq.txt')
write.table(fitsBootExtrDifNumberFreq,
            'logdata/fitsBootExtrDifNumberFreq.txt')
write.table(fitsBootExtrDifNumber2Freq,
            'logdata/fitsBootExtrDifNumber2Freq.txt')

write.table(fitsBootExtrDifTf,
            'logdata/fitsBootExtrDifTf.txt')
write.table(fitsBootExtrDif2Tf,
            'logdata/fitsBootExtrDif2Tf.txt')
write.table(fitsBootExtrDifNumberTf,
            'logdata/fitsBootExtrDifNumberTf.txt')
write.table(fitsBootExtrDifNumber2Tf,
            'logdata/fitsBootExtrDifNumber2Tf.txt')

write.table(fitsBootExtrDifSpeedMatch,
            'logdata/fitsBootExtrDifSpeedMatch.txt')
write.table(fitsBootExtrDif2SpeedMatch,
            'logdata/fitsBootExtrDif2SpeedMatch.txt')
write.table(fitsBootExtrDifNumberSpeedMatch,
            'logdata/fitsBootExtrDifNumberSpeedMatch.txt')
write.table(fitsBootExtrDifNumber2SpeedMatch,
            'logdata/fitsBootExtrDifNumber2SpeedMatch.txt')

write.table(fitsBootExtrDifFreqMatch,
            'logdata/fitsBootExtrDifFreqMatch.txt')
write.table(fitsBootExtrDif2FreqMatch,
            'logdata/fitsBootExtrDif2FreqMatch.txt')
write.table(fitsBootExtrDifNumberFreqMatch,
            'logdata/fitsBootExtrDifNumberFreqMatch.txt')
write.table(fitsBootExtrDifNumber2FreqMatch,
            'logdata/fitsBootExtrDifNumber2FreqMatch.txt')

