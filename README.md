# persuasio
persuasio: R module to estimate the effect of persuasion and conduct inference, using some of identification results obtained in Jun and Lee (2022, arXiv, https://arxiv.org/abs/1812.02276). This R package follows the 2022 eponymous Stata package posted at the Statistical Software Components (SSC) archive.

persuasio/
│
│
├── R/
│
│   # 1. Main user interface
│   persuasio.R
│
│
│   # 2. APR family
│   aprlb.R
│   aprub.R
│   persuasio4ytz.R
│
│
│   # 3. LPR family
│   lpr4ytz.R
│   persuasio4ytz2lpr.R
│
│
│   # 4. YZ family
│   persuasio4yz.R
│
│   
│   # 5. Summary statistics calculator
│   calc4persuasio.R
│
│
│   # 6. Shared utilities
│   utils_binary.R
│   utils_matrix.R
│   utils_bootstrap.R
│   utils_clipping.R
│
│   
│   # 7. Print methods
│   print_aprlb.R
│   print_aprub.R
│   print_lpr4ytz.R
│   print_calc4persuasio.R
│   print_persuasio4yz.R
│   print_persuasio4ytz.R
│
│   
│   # 8. Internal helpers
│   internal_apr_helpers.R
│   internal_lpr_helpers.R
│
│  
│   # 9. Package utils
│   zzz.R
│
├── man/
├── tests/
├── DESCRIPTION
├── NAMESPACE
└── README.md
