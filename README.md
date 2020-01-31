This is a case study in making R code more efficient. This shows various different complementary
approaches. We also show how to explore and become familiar with somebody else's code. We use a
variety of meta-programming and instrumentation tools to explore and measure the characteristics of
the code.

This is authentic code from a problem of finding the optimal water release strategy for
a reservoir releasing cold and warm water at different times of the year to positively
impact fish populations down-stream of the dam, while maintaining adequate water resources
for potential droughts in the summer and avoiding exceeding capacity in rainy seasons.

Sincere thanks to Dr. Lauren Adams for sharing this code with us.

## Focal File
The key initial file is 

+  TOY.R


## Challenges
The high-level challenges are to:

+ Find which data (CSV) files this script needs
+ Get the script to run without trying repeatedly run it up to an error and fixing that one issue,
  and running it again.
+ Make the code more efficient so that it runs faster.
+ Deal with the BIG.R file to make that version run at all! (Memory.)
   + What's the difference between BIG.R and TOY.R?


+ What information would you want to have about the code for each of these steps?
+ How can we get that information?


## Useful packages

+   [CallCounter](https://github.com/duncantl/CallCounter)
+   [CodeDepends](https://github.com/duncantl/CodeDepends)
+   [CodeAnalysis](https://github.com/duncantl/CodeAnalysis)
+   [rstatic](https://github.com/nick-ulle/rstatic)
+   [codetools](https://cran.r-project.org/web/packages/codetools/index.html)

