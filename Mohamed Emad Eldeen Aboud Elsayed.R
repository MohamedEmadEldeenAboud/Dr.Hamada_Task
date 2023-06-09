
trees <-read.csv("G1_Allometry.csv")

View(trees)

# We Will Start With Data cleaning

# Recoding The Height Feature To Tall And Short

trees$heightDescribtion[trees$height >= 30] = "Tall"
trees$heightDescribtion[trees$height < 30]="Short"

# Recoding the diameter feature to Thick and Thin

trees$dimeterDescribtion[trees$diameter >= 50] = "Thick"
trees$dimeterDescribtion[trees$diameter < 50]= "Thin"

# show all rows that have NA values or missing values

trees[!complete.cases(trees) , ]


# Replace NA Values In Height Field According To The Mean Of
# Heights Of The Same Type

# Make Sure Height Field Is Numeric
trees$height <- as.numeric(trees$height)

# We Will Test It On PIPO Type
pipoMean<-mean(trees[trees$species=="PIPO" , 'height' ], na.rm=T)
trees[is.na(trees$height) & trees$species=="PIPO"  , 'height'] = pipoMean


#On PIMO Type
pimoMean<-mean(trees[trees$species=="PIMO" , 'height' ], na.rm=T)
trees[is.na(trees$height) & trees$species=="PIMO"  , 'height'] = pimoMean

# Can't apply it to PSME Cause All Values Are NULL

# Recoding leafarea Field Using ifelse

# Make Sure leafarea Field Is Numeric
trees$leafarea <- as.numeric(trees$leafarea)

m = mean(trees$leafarea)
trees$leafareaAmount = as.factor(ifelse(trees$leafarea >= m, "dense tree" 
                                   ,"sparse tree"))

#Recoding A Code

trees$isDenseTree[trees$leafareaAmount == "dense tree"] = T
trees$isDenseTree[trees$leafareaAmount == "sparse tree"]= F
trees$isDenseTree = as.factor(trees$isDenseTree)


#Get The Mean Of Dense Trees And Sparse Trees

mDense <-mean(trees$isDenseTree==T)
MSparse <-mean(trees$isDenseTree==F)
mDense
MSparse

# Subset Only Tall Trees

tallTrees = trees [trees$heightDescribtion=="Tall", ]

# Subset Only Short Trees

shortTrees = trees [trees$heightDescribtion=="Short", ]


# Subset Trees That Are Thick And Tall 


thickAndTallTrees = trees[trees$heightDescribtion=="Tall" & trees$dimeterDescribtion=="Thick", ]

# Sort Or Order Our Data Set ascending Based On height and dimeter

sortedData = trees[order(trees$height ,trees$diameter) , ]


#Get Only First 15 Rows
headOfData <-head(trees ,15)

#Get Only last 15 Rows
tailOfData <-tail(trees ,15)


# Data Visualization

library(ggplot2)

# Visualize The Effect of The height On branchmass Using Scatter Plot

draw1 = ggplot(trees , aes(x=branchmass  , y= height))
draw1 + geom_point() + ggtitle("Co_relation Between height branchmass")


# Visualize The Effect of The height On branchmass colored by the groups of diameter using scatterplot

draw2 = ggplot(trees , aes(branchmass , height))
draw2 + geom_point(aes(color=diameter)) + stat_smooth(se=FALSE)  


# Visualize Distribution Of branchmass Using Histogram

draw3 <-ggplot(trees , aes(branchmass))
draw3 + geom_histogram(binwidth = 8)
draw3 + geom_histogram(fill = "orange")+ ggtitle("Tree's Branch Mass Distribution")+labs(x="Branch Mass" , y="Frequency")

# Visualize Distribution Of branchmass Using Histogram

draw4 = ggplot(trees , aes(height))
draw4 + geom_histogram(binwidth = 8)
draw4 + geom_histogram(fill = "red")+ ggtitle("Tree's Height Distribution") 

# Summarizing isDenseTree T or F To Species And Height Groups Using Bar Chart

draw5 = ggplot(trees , aes(x=isDenseTree  ,fill = species))
draw5 + geom_bar() +labs(y=" DenseTree count" ,title="Dense Category Rating")
draw5 + geom_bar() +theme_light()+facet_wrap(~height)
















