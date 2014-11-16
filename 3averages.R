source('libsAndMore.R')

### input ####
speedMatch <- read.table('logdata/speedMatch.txt')
freqMatch <- read.table('logdata/freqMatch.txt')
datDur <- readfiles(path ='data', exp = 'DurSingleObject')


### number of trials ####
datDur %>% group_by(subject, durationTest,radius,number,speed,durComp) %>% summarise(n=n()) # n trials

### averages ####
avDur <- datDur %>% 
  group_by(subject, radius, speed, number, 
           durationTest, durComp) %>%
  summarise(n = n(), nyes = sum(response), nno = n - nyes, y = nyes / n)  %>% 
  mutate(durTestMs = durationTest * 1000 / p$hz, 
         durCompMs = durComp * 1000 / p$hz,
         durTestLog = log(durTestMs), 
         durCompLog = log(durCompMs), 
         cond = paste('n: ',number,'r: ', radius))

### psychometric functions ####
curves <- avDur %>% group_by(subject, cond, speed, durationTest) %>%
  do(curvepsy(., x = 'durCompLog', pini = p$durIni))

### thresholds ####
fits <- avDur %>% group_by(subject, radius, speed, number, durationTest) %>%
  do(fitpsy(., x = 'durCompLog', pini = p$durIni, B = p$B))

meandur <- datDur %>% group_by(subject, durationTest) %>%
  summarise(meandur = exp(mean(log(durComp))) * 1000 / p$hz)

fits <- merge(fits,meandur)

fits <- fits %>% mutate(pse = 2 - exp(x) / meandur,
                        psemin = 2 - exp(xmax) / meandur ,
                        psemax = 2 - exp(xmin) / meandur,
                        freq = speed / (2 * pi * radius),
                        tf = freq *number,
                        angle = freq* durationTest / p$hz)

fitsMatchSpeed <- merge(fits, speedMatch)
fitsMatchFreq <- merge(fits, freqMatch)

### bootstrap samples ####
fitsBoot <- avDur %>% 
  group_by(subject, speed, number, radius, durationTest) %>%
  do(fitpsyboot(., x = 'durCompLog', ythre = .5, m = Inf, p = p$durIni, B = p$B))

fitsBoot <- merge(fitsBoot,meandur)
fitsBoot <- fitsBoot %>% mutate(psesample = 2 - exp(xsample) / meandur,
                                freq = speed / (2 * pi * radius),
                                tf = freq *number,
                                angle = freq* durationTest / p$hz)


### Thresholds across participants ####
fitsAllPse <- fits %>% group_by(speed, radius, number, durationTest) %>%
  summarise(pse = exp(mean(log(pse))) ) 

fitsAllCi <- merge(fitsBoot, meandur) %>% 
  group_by(subject, speed, radius, number, durationTest,sample) %>%
  mutate(psesample = 2  - exp(xsample)/meandur) %>%
  group_by(speed, radius, number, durationTest, sample) %>%
  summarise(psesample=exp(mean(log(psesample)))) %>%
  group_by(speed, radius, number, durationTest) %>%
  summarise(psemin = quantile(psesample, .025),
            psemax = quantile(psesample, 1-.025))

fitsAll <- merge(fitsAllPse, fitsAllCi) %>%
  mutate(subject = 'ALL', 
         freq = speed / (2 * pi * radius),
         tf = freq *number, 
         angle = freq* durationTest / p$hz)

### output ####
write.table(avDur,'logdata/avDur.txt')
write.table(curves,'logdata/curves.txt')
write.table(fits,'logdata/fits.txt')
write.table(fitsBoot,'logdata/fitsBoot.txt')
write.table(fitsMatchSpeed,'logdata/fitsMatchSpeed.txt')
write.table(fitsMatchFreq,'logdata/fitsMatchFreq.txt')
write.table(fitsAll,'logdata/fitsAll.txt')
