setwd("~/Desktop/determinaciondeparametros/nomarl")
files <- dir()
load(file=as.character(files[1]))
normal <- forecast.mean
for (i in 2:length(files))
{
    print(i)
    load(file=files[i])
    normal <- rbind( normal, forecast.mean)
}
precisiones <- normal[, names(normal)[6]]
hist(precisiones)
summary(precisiones)

setwd("~/Desktop/determinaciondeparametros/stress")
files <- dir()
load(file=as.character(files[1]))
stress <- forecast.mean
for (i in 2:length(files))
{
    print(i)
    load(file=files[i])
    stress <- rbind( stress, forecast.mean)
}
precisiones.stress <- stress[, names(stress)[6]]
hist(precisiones.stress)
summary(precisiones.stress)


plot(precisiones, precisiones.stress, )
hist( precisiones.stress- precisiones)
summary(precisiones.stress- precisiones)
