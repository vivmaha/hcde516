D.Pop = D[D$Topic == "PopCulture",]
D.Pop.Serif <- D.Pop[D.Pop$Font == "serif", ]
D.Pop.Sans <- D.Pop[D.Pop$Font == "sans-serif", ]

t.test(D.Pop.Serif$Agreement, D.Pop.Sans$Agreement)

D.Science = D[D$Topic == "Science",]
D.Science.Serif <- D.Science[D.Science$Font == "serif", ]
D.Science.Sans <- D.Science[D.Science$Font == "sans-serif", ]

t.test(D.Science.Serif$Agreement, D.Science.Sans$Agreement)

library(pwr)