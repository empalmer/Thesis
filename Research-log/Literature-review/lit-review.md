---
title: "Literature Review"
output: github_document
---


# Literature Review

* * *

- **Author:** Andrew Brinkman, Daniel Shanahan, Craig Sapp
- **Year:** 2016
- **Title:** Musical Stylometry, Machine Learning, and Attribution Studies: A Semi-Supervised Approach to the Works of Josquin. 
- **Summary:**
- **Notes:**

- hard for humans to preform unbiased analysis of authorial attribution
- uses "high-level features" along with low-level features. high level for example 9-8 suspensions, lowlevel note to note transition probabilities. 
- looking at features that are easier to quantify - not always what music historians look at. 
- Compares Josquin to other composers in a range of styles (identify features that represent differences between styles, are common to a particular style, or are unique to a particular composer)
- 
- Initial test- compare to JS Bach four part chorales
- Next compares to Ockeghem and Du Fay
- Finally compares de Orto and La Rue

- use PCA to reduce 53 input features
- Chose first 5 components to account for 85% of the variance
- Can see clear seperation between Bach and Josquin with only two PCs
- (Does not talk about what the model actually is... )
- repetition of the fifth is often a distinguishing feature
- using the first two pCs there is very little seperation, same for La Rue
- 

- uses results of the principal component analysis as a way of training a multi-composer classifier. 
- train three classifiers
- 1) K nearest neighbor classifier. Uses 27 pcs 
- 2) Support Vector Machine 
- 3) Decision tree

* * * 

- **Author** Jacqueline Speiser, Vishesh Gupta
- **Year**
- **Title** Composer Style Attributino
- **Notes** 

- goal to analyze music of Josquin and Ockeghem and la Rue to find a model that can classify unknown works as Josquin or not
- four categories: frequencies of individual notes, frequencies of pairwise interval combinations between each voice, markov transition matrices for rhythms of pieces, and markov transition matrices of pitches in each piece
- used mutual information to score 3000 features
- used top 50 features
- PCA and selected top 50 features
- use Naive Bayes for initial learning algorithm, had 30-40 percent training error- music not independent
- used 10-fold cv on support vector machine and gaussian discriminant analysis. 
- chose svm with a gaussian kernel
- 


* * *

- **Author** Francoise Tillard
- **Year**
- **Title** Fanny Mendelssohn
- **Notes**

pg Op8 and Op9 has two lieder by Fanny, no2, no3, no12, no9,10,12
Mentions that Felix admitted that fanny wrote op8 no3 but doesn't mention how the rest are thought to be atrributed to her. 
(Includes list of publications by Fanny)

* * *

- **Author:** Eric Backer, Peter van Kranenburg
- **Year:** 2016
- **Title:** On musical stylometry -- a patern recognition approach
- **Summary:**
- **Notes:**

- pg 1
- Uses 20 features - style markers to describ different sonorities in compositions
- 200 different compositions of Bach, Handel, Telemann, Mozer and Haydn. 
-Eve with a few features, the styles of the various composers could be seperated with leave one out error rates varying from 4 to 9 percent. 
- A second experiment included 30 fugues from JS Bach, WF BAch and JL KRebs of differnet style and character. Then the f minor fugue for organ which Bach's authorship is disputed was analyze. There was experimental evidence that JL Krebs wrote it 

-pg2
- Problem with stylometry is the lack of an underlying theory. Until the study is done, it is not known which of the style markers will be the discriminator. 
- Have to generate a large number of potentially interesting features wich it is hoped will be suitable for stylometric studies. 


-pg 3
- Sometimes they have to transpose the parts - I wonder why? 
- Uses overlapping windowing over entire composition - I like that they use the whole piece
- Tradeoff between the number of fragments (as high as possible) and the variance of the feature values (as small as possible) 


- pg 4
- Cool graph plotting variance as a function of the number of bars in a window - I wonder if this changes with time signature? 
- Chose 30 bars per fragment
- Decompositng a composition (windowing) results in a number of related data points, enabling representation of a composition as a cloud of data points on the basis of which global densities can be estimated
- Data points are ordered in time
- Note how many bars offset
- 20 veatures are computed, most are low level properties of counter point
- expected to be consistent from different genres and dates 
- high level features, key, modulations, development of theme expected to reflect characteristics of individual compositions. 


- pg 5
- List of the features used: 
- Stability - normalizes to compare different time signatures 
- Fraction of the score that consists of dissonatnt sonorities
- Fraction of bars that begin with a dissonant sonority
- Sonority Entropy - Manson(1985) sonority is a type of chord with a number reprosenting each sonority - total duration of all occurrences for each sonority computed, then probabilities of the occurance are estimated by weighted frequencies
- Harmony Entropy similar to sonority entropy but considers pitch of chord
- pitch entropy - list of occurrences of all the pitches made
- voice density - only takes into account bars that are strictly polyphonic
- part seconds to part octave - measures amount of a number of intervals between the different voice pairs
- parallel thirs fourths sixths 
- step suspension


- pg 6 Analysis
- Uses Matlab toolbox PRTools
- feature selection uses Floating Forward Slection 


- pg 7 
- for two class and five class problems uses k nearest neighbor classifier
- what is a leave one out error 
- Find that Bach's style can be isolated from the style of the other composers



- likely that JL Krebs is the composer

* * *


* * *

- **Author:** Dannenberg, Thom, Watson
- **Year:** 1997
- **Title:** A Machine Learning Approach to Musical Style Recognition
- **Summary:** They use naieve basiean classifiers, linear classifiers and neural networks to see if a MIDI recording of an improvized piece of four and eight options. 
- **Notes:**

-pg 1


- Uses machine learning to build effective style classifiers for interactive performance systems instead of focusing on low-level perceptual features such as pitch and tempo. 
- Relatively simple stylistic features such as playing energetically, playing lyrically, or playing with syncopation are difficult to detect reliably. 
- Issues of contrast - energetic pieces contain silence
-higher level musical intent appears chaotic and unstructured when presented in low - level terms such as MIDI performance data
-This paper is studying hte feasibility of applying machine learning techniques to build musical style classifiers
- initial problem to classify an improvisation as one of four styles, lyrical, frantic, syncopated or pointillistic
- classifier operates within five seconds - important to recognize in real time


- pg2
- Uses MIDI files from a performer, does new styles every fifteen seconds
- 1200 five second training examples
- Used naive Bayesian, linear, and neural network approaces 
- naive Bayesian classifier assumes that features are uncorrelated and normally distributed. Given a vector of features F, want to know which classification C is most likely. The most likely class is the one whose mean feature vector has the least normalized distance to f. 
$$ \delta c = \sqrt{\sum_{i = 1}^n \big( \frac{F_i - \mu_{C,i}}{\sigma_{C,i}}\big)^2}$$
- 3.2 Linear classifier - computed a weighted sum of features where a differnt set of weights is used for each class. Assumes features are normally distributed but not uncorrelated. It tries to sepaerate memebers of the class from non-members by cutting the feature vector space with a hyperplane. 
- 3.3 Neural Networks. The most powerful because they incorporate non linear terms and do not make strong assumptions about the feature probability distributions. Used a Cascade-Correlation architecture wich consists initially of only input and output units 
- Had high percentage of correct classifications 
- They wonder why these systems worked well when hand-coded approaches have failed. 


- **How does this relate?**
I dont think it relates exactly, as I dont want to deal with sound, but sheet music. I wonder if I could use some of the stylistic ideas on sheet music? It seems to somewhat dismiss the lower level stuff i maybe wanted to do. 

* * *


* * *

- **Author:** M.A. Crerar
- **Year:** 1985
- **Title:** Elements of a Statistical Approach to the Question of Authorship of Music
- **Summary:** This paper uses methods previous paper (Paisely 1964) that has some issues to examine the work of three italian composers, two foreign composers, and one non-contemporary to see differnces. They use the first 20 notes for each sample. They use four methods, first examining frequency of note value, second examining frequency of opening interval, next the opening interval keeping in mind the first note, and fourth looking at 2x2 note transitions. 

- **Notes:** 
*pg 1 
  *Follows from a paper regarding Valentini, a student of Corelli. Notes how much musicological investigation uses a combined technical details and characterization of style. 

*pg2 
  *Computational musicology helps give evidence for previous claims. Use markers whose usage remains invariant under changes of topic. Refering to literature: Analysis devoid of both syntatic and semantic considerations - subconscious stylistic traits - wouldnt be copied by an adept imitator, also ignoring musical syntax which woudl be shared by contemporaries. Operate at micro level of intervallic movement, convention-bound but offers space for individuality. 

  *Things to avoid - focus on characteristics which are conscious products of musical convention and considering too many parameters simultaneously

  *Intersting note about influence - Valentini was a student of Corelli 

*pg3
  *used method from Paisley 1964
  *Method - random sample of 105 incipits coded for corelli, coded random samples of Handel and JS Bach to see if they are closer in style to each other than to a foreign contemporary, random sample from the instrumental works of Beethoven - different period

  *Samples were approx 20 pitches in length
  *Transposed into C, assigned a numerical value to each note in the chromatic progression C-B
  *Paisely did two test, first looked at two samles of 800 two note transitions for each composer

*pg 4
  *Can also do frequency of note in each piece - all composers basically the same for this
  *Claim that the three composers of interest are indistinguishable by simple statistical methods- cant do easy word count like in literature

  *second test - examine opening interval in each incipit - uniformity in composers

  *Apparently some funky stuff happening with Paisleys chi squre tests

  *Comparing h and b, the expected frequencies are found by calculating the arithmetic mean for each set of two observations and then using all the actual values as obbservations
$$x^2 = \sum{frac{(b_i-e_i)^2}{e_i}} + \sum{\frac{(h_i - e_i)^2}{e_i}}$$

*pg 5 Paisly ignored interval direction, two notes can be separated by at most six semitones and then classfied 7 (0   to 6) - chi-square values in table 4 

  *Each composer rejects the others work - I think this means they are distiguishable by this

  *Paisley second analysis looked at two transitions of the type tonic to tonic, third to fifth, other diatonic to tonic, chromatic to fifth etc. 

  *They represented minor incipits in the relative major, so used different groups - a little confusing

  *The italian composers reject each others work

*Pg 6)
  *Third test, examined 1800 two note transitions recorded info on what note the leap was made from
  *All six composers are thus shown to reject each other's work. 

  *Final test - 1600 sets of consecutive 2X2 note transitions - 7X7 matrix occurences of all permutations of intervals [0-16]x[o-16]
 
 
*7) Consistency for one composer

  *issues - use start, which is perhaps not as unique 

  *future directions - which a work was started by one composer and finished by anoterh 
  *early work of a composer looks like the later work of the composer, or if is more similar to a different composer ie beethoven , mozart

Questions: What is an incipit? Is this a music or math word? 


- **How does this relate?** Has some good ideas for simple tests I could run. Also warnigns on things to avoid. 

* * *










