## Senior Thesis and museR

Contains my senior thesis from Reed College with a major in mathematics with a concentration in statistics required for graduation (May 2018). Includes R package museR I authored for the analysis of sheet music. 



## About museR:

A package for analyzing sheet music 


## Thesis

### Abstract
 The use of quantitative methods for attribution of pieces of classical music to composers is a relatively underexplored field. In theory, composers leave behind unconscious (or perhaps conscious) signals that indicate a piece of music as their own. Ideally these signals or features can be extracted and used to build a model to predict the composer of a piece of music. For instance, the siblings Fanny Hensel and Felix Mendelssohn, although very similar in compositional style, likely have features that distinguish their music. It is speculated that there are at least six pieces that Fanny wrote that were published by Felix. This historical setting motivates the construction of a model to determine which sibling was the true author.  

Low level features, including frequencies of chords and scale degrees, were extracted from 31 lieder known to be written by Felix and 35 pieces known to be written by Fanny. These features were extracted using museR, an R package written for the purpose of this thesis. Additionally, to check the validity of features used, 27 solo piano pieces from J.S. Bach's Well Tempered Clavier were also included in the analysis. Logistic lasso, $k-$nearest neighbors, random forests, naive Bayes, and linear discriminant analysis (LDA) were performed on the feature space in order to classify the pieces. In addition, those same models were fit on the principal components. The LDA model fit on the principal components comparing Bach and Mendelssohn resulted in the lowest misclassification rate (0.049). The logistic lasso model on the principal components between Felix and Fanny resulted in the lowest misclassification rate (0.45).  Finaly, each model was used to predict the authorship of the the six disputed pieces. As misclassification rates were so high on the models, we cannot trust the predictions for these disputed pieces. 





