# persuasio
persuasio: The R module to estimate the persuasion effect and conduct inference, using the estimators in Jun and Lee (2023, Journal of Political Economy, https://doi.org/10.1086/724114). A Stata package with the same name is posted on the Statistical Software Components (SSC) archive.

```
persuasio/
│
├── R/
│
│   # 1. Main user interface
│   persuasio.R
│
│   # 2. Estimator wrapper
│   persuasio4ytz.R
│   persuasio4ytz2lpr.R
│   persuasio4yz.R
│
│   # 3. Base functions
│   aprlb.R
│   aprub.R
│   lpr4ytz.R
│   calc4persuasio.R
│   
│   # 4. Print methods
│   aprlb_print.R
│   aprub_print.R
│   lpr4ytz_print.R
│   calc4persuasio_print.R
│   persuasio4yz_print.R
│   persuasio4ytz_print.R
│
├── man/
├── tests/
├── DESCRIPTION
├── NAMESPACE
└── README.md
```
