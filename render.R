rmarkdown::render('Main.Rmd')






mat <- matrix(
  c(
c(0.000246914 , -0.000123457 , -0.000000000 , -0.000000000),
c(-0.000123457 , 0.000246914 , -0.000000000 , -0.000000000),
c(-0.000000000 , -0.000000000 , 0.000246914 , -0.000123457),
c(-0.000000000 , -0.000000000 , -0.000123457 , 0.000246914) ),
nrow = 4,
ncol = 4
)


fpeff=det(mat)
deffic=fpeff^(-1/ncol(mat))
deffic

image(matrix(runif(1000000), nrow  = 10000, ncol = 10000))
image


```{r echo=FALSE, message = FALSE, child='C:\\Users\\grigo\\Dropbox\\projects\\BTS_CONJOINT_THESIS\\2_dp_and_analysis.R'}


```