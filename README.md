This is a case study in making R code more efficient. This shows various different complementary
approaches. We also show how to explore and become familiar with somebody else's code. We use a
variety of meta-programming and instrumentation tools to explore and measure the characteristics of
the code.

This is authentic code from a problem of finding the optimal water release strategy for
a reservoir releasing cold and warm water at different times of the year to positively
impact fish populations down-stream of the dam, while maintaining adequate water resources
for potential droughts in the summer and avoiding exceeding capacity in rainy seasons.

The key initial file is 

+  TOY.R

Sincere thanks to Dr. Lauren Adams for sharing this code with us.

Related packages

+   [CallCounter](https://github.com/duncantl/CallCounter)
+   [CodeDepends](https://github.com/duncantl/CodeDepends)
+   [CodeAnalysis](https://github.com/duncantl/CodeAnalysis)
+   [rstatic](https://github.com/nick-ulle/rstatic)
+   [codetools](https://cran.r-project.org/web/packages/codetools/index.html)

