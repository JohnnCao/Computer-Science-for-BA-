# Computer-Science-for-BA-
Duplicate Detection on Web shop products 

This repository is used for the individual assignment of the course "Computer Science for Business Analytics" of the Erasmus School of Economics. 
The programming language used is R. 

The code is used to perform a straightforward implementation duplicate detection with Locality-Sensitive Hashing (LSH) on a data set of televisions. 
After extracting title words and creating a binary product vector the required steps of LSH will be implemented. The code is filled with commented blocks 
such that each individual step can be executed separately. 

Note that after creating the signature matrix with Min Hashing a list of possible thresholds can be given such that classification can executed for each individual threshold.
For fast execution of a single iteration (with 1 single threshold value) adjust "threshold_list". 

The collected evaluation metrics are: Pair Quality, Pair Completeness, F1 and F1* 
