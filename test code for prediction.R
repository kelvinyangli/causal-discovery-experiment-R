par(mfrow=c(1,2))
dagTrue = generateDag(30,5)
cptsTrue = generateCPTs(dagTrue, 4, 10)
dataTraining = rbn(cptsTrue, 10000)
dataTesting = rbn(cptsTrue, 1000)
dataTesting2 = rbn(cptsTrue,10000)

dagLearned = mmhc(dataTraining)
cptsLearned = bn.fit(dagLearned, dataTraining, method = "bayes")
graphviz.plot(dagTrue)
graphviz.plot(dagLearned)

dataTestingNumeric = toNumeric(dataTesting)
dataTestingNumeric2 = toNumeric(dataTesting2)

d1=prediction(cptsTrue, dataTraining, dataTestingNumeric, maxNumParents = 5, maxNumValues = 4)
d2=prediction(cptsTrue, dataTraining, dataTestingNumeric2, maxNumParents = 5, maxNumValues = 4)
d3 = prediction(cptsLearned, dataTraining, dataTestingNumeric, maxNumParents = 5, maxNumValues = 4)

system.time(prediction(cptsLearned, dataTraining, dataTestingNumeric, maxNumParents = 5, maxNumValues = 4))
# 15 second for 1000 data points 

Rprof("mml.out")
y = mmlCPT("V3","V1",data=data)
Rprof(NULL)
proftable("mml.out")
