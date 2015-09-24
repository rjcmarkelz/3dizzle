library(qtl)
?effectscan

data(hyper)

fake.f2 <- sim.geno(hyper, step=2.5, n.draws=16)
test <- effectscan(fake.f2)
test
head(fake.f2)

plot(scanone(hyper))
