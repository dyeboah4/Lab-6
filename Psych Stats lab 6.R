> brfss22$Educ_number <- fct_recode(brfss22$EDUCA, 
                                    +                                   "0" = "Never attended school or only kindergarten", 
                                    +                                   "4.5" = "Grades 1 through 8 (Elementary)",
                                    +                                   "10" = "Grades 9 through 11 (Some high school)",
                                    +                                   "12" = "Grade 12 or GED (High school graduate)",
                                    +                                   "14" = "College 1 year to 3 years (Some college or technical school)",
                                    +                                   "16" = "College 4 years or more (College graduate)",
                                    +                                   NULL = "Refused" )
> brfss22$Educ_number <- as.numeric(levels(brfss22$Educ_number))[brfss22$Educ_number]
> brfss22$Age_midpt <- fct_recode(brfss22$X_AGEG5YR, "21" = "Age 18 to 24",
                                  +                                 "27" = "Age 25 to 29", "32" = "Age 30 to 34",
                                  +                                 "37" = "Age 35 to 39", "42" = "Age 40 to 44",
                                  +                                 "47" = "Age 45 to 49", "52" = "Age 50 to 54",
                                  +                                 "57" = "Age 55 to 59", "62" = "Age 60 to 64",
                                  +                                 "67" = "Age 65 to 69", "72" = "Age 70 to 74",
                                  +                                 "77" = "Age 75 to 79", "82" = "Age 80 or older",
                                  +                                 NULL = "Dont know/Refused/Missing")
> brfss22$Age_midpt <- as.numeric(levels(brfss22$Age_midpt))[brfss22$Age_midpt]
> brfss22$income_midpoint <- fct_recode(brfss22$INCOME3, 
                                        +                                       "7500" = "Household income less than $10,000",
                                        +                                       "12500" = "Less than $15,000 ($10,000 to less than $15,000)",
                                        +                                       "17500" = "Less than $20,000 ($15,000 to less than $20,000) ",
                                        +                                       "22500" = "Less than $25,000 ($20,000 to less than $25,000) ",
                                        +                                       "30000" = "Less than $35,000 ($25,000 to less than $35,000) ",
                                        +                                       "42500" = "Less than $50,000 ($35,000 to less than $50,000) ",
                                        +                                       "62500" = "Less than $75,000 ($50,000 to less than $75,000)",
                                        +                                       "87500" = "Less than $100,000 ($75,000 to less than $100,000)",
                                        +                                       "125000" = "Less than $150,000 ($100,000 to less than $150,000)",
                                        +                                       "175000" = "Less than $200,000 ($150,000 to less than $200,000)",
                                        +                                       "210000" = "$200,000 or more",
                                        +                                       NULL = "Dont know/Not sure",
                                        +                                       NULL = "Refused")
> brfss22$income_midpoint <- as.numeric(levels(brfss22$income_midpoint))[brfss22$income_midpoint]
> 
  > brfss22$Educ_number <- fct_recode(brfss22$EDUCA, 
                                      +                                   "0" = "Never attended school or only kindergarten", 
                                      +                                   "4.5" = "Grades 1 through 8 (Elementary)",
                                      +                                   "10" = "Grades 9 through 11 (Some high school)",
                                      +                                   "12" = "Grade 12 or GED (High school graduate)",
                                      +                                   "14" = "College 1 year to 3 years (Some college or technical school)",
                                      +                                   "16" = "College 4 years or more (College graduate)",
                                      +                                   NULL = "Refused" )
  > brfss22$Educ_number <- as.numeric(levels(brfss22$Educ_number))[brfss22$Educ_number] 
  > levels(brfss_marijan$X_PRACE2)[7] <- "dont know not sure"
  > 
    > select1 <- !is.na(brfss22$MARIJAN1)
    > brfss_marijan <- subset(brfss22, select1 )
    > 
      > 
      > p_cannabis_age <- ggplot(data = brfss_marijan,
                                 +                          mapping = aes(x = Age_midpt,
                                                                          +                                        y = MARIJAN1))
      > p_cannabis_age + geom_smooth() 
      `geom_smooth()` using method = 'gam' and formula = 'y ~ s(x, bs = "cs")'
      Warning message:
        Removed 1357 rows containing non-finite values (`stat_smooth()`). 
      > model_1 <- lm(MARIJAN1 ~ Age_midpt, data = brfss_marijan)
      > summary(model_1) 
      
      Call:
        lm(formula = MARIJAN1 ~ Age_midpt, data = brfss_marijan)
      
      Residuals:
        Min     1Q Median     3Q    Max 
      -4.383 -2.670 -1.682 -0.694 29.635 
      
      Coefficients:
        Estimate Std. Error t value Pr(>|t|)    
      (Intercept)  5.766272   0.074888   77.00   <2e-16 ***
        Age_midpt   -0.065873   0.001268  -51.94   <2e-16 ***
        ---
        Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
      
      Residual standard error: 6.848 on 92376 degrees of freedom
      (1357 observations deleted due to missingness)
      Multiple R-squared:  0.02837,	Adjusted R-squared:  0.02836 
      F-statistic:  2697 on 1 and 92376 DF,  p-value: < 2.2e-16
      
      > model_2 <- lm(MARIJAN1 ~ Age_midpt + X_PRACE2 + X_HISPANC + EDUCA, data = brfss_marijan)
      > summary(model_2)
      
      Call:
        lm(formula = MARIJAN1 ~ Age_midpt + X_PRACE2 + X_HISPANC + EDUCA, 
           data = brfss_marijan)
      
      Residuals:
        Min      1Q  Median      3Q     Max 
      -7.5630 -2.6542 -1.6487 -0.6594 31.2237 
      
      Coefficients:
        Estimate Std. Error t value Pr(>|t|)
      (Intercept)                                                        5.096888   0.677485   7.523 5.39e-14
      Age_midpt                                                         -0.067031   0.001292 -51.882  < 2e-16
      X_PRACE2Black or African American                                 -0.042721   0.084100  -0.508 0.611471
      X_PRACE2American Indian or Alaskan Native                          1.557556   0.160643   9.696  < 2e-16
      X_PRACE2Asian                                                     -1.393354   0.105982 -13.147  < 2e-16
      X_PRACE2Native Hawaiian or other Pacific Islander                 -0.227623   0.140967  -1.615 0.106374
      X_PRACE2Multiracial but no preferred race                          1.570928   0.334752   4.693 2.70e-06
      X_PRACE2Don\x92t know/Not sure                                    -0.567359   0.248434  -2.284 0.022389
      X_PRACE2No race choice given                                      -0.678235   0.200407  -3.384 0.000714
      X_PRACE2Refused                                                    0.173510   0.195293   0.888 0.374296
      X_HISPANCno                                                        0.663137   0.095251   6.962 3.38e-12
      X_HISPANCdont know refused missing                                 1.404075   0.275795   5.091 3.57e-07
      EDUCAGrades 1 through 8 (Elementary)                              -0.196586   0.693073  -0.284 0.776684
      EDUCAGrades 9 through 11 (Some high school)                        1.639713   0.683020   2.401 0.016367
      EDUCAGrade 12 or GED (High school graduate)                        0.714901   0.674568   1.060 0.289243
      EDUCACollege 1 year to 3 years (Some college or technical school)  0.395906   0.674549   0.587 0.557259
      EDUCACollege 4 years or more (College graduate)                   -0.428980   0.674231  -0.636 0.524615
      EDUCARefused                                                      -0.610511   0.826101  -0.739 0.459892
      
      (Intercept)                                                       ***
        Age_midpt                                                         ***
        X_PRACE2Black or African American                                    
      X_PRACE2American Indian or Alaskan Native                         ***
        X_PRACE2Asian                                                     ***
        X_PRACE2Native Hawaiian or other Pacific Islander                    
      X_PRACE2Multiracial but no preferred race                         ***
        X_PRACE2Don\x92t know/Not sure                                    *  
        X_PRACE2No race choice given                                      ***
        X_PRACE2Refused                                                      
      X_HISPANCno                                                       ***
        X_HISPANCdont know refused missing                                ***
        EDUCAGrades 1 through 8 (Elementary)                                 
      EDUCAGrades 9 through 11 (Some high school)                       *  
        EDUCAGrade 12 or GED (High school graduate)                          
      EDUCACollege 1 year to 3 years (Some college or technical school)    
      EDUCACollege 4 years or more (College graduate)                      
      EDUCARefused                                                         
      ---
        Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
      
      Residual standard error: 6.81 on 92356 degrees of freedom
      (1361 observations deleted due to missingness)
      Multiple R-squared:  0.03916,	Adjusted R-squared:  0.03898 
      F-statistic: 221.4 on 17 and 92356 DF,  p-value: < 2.2e-16
      Call:
        lm(formula = MARIJAN1 ~ Age_midpt + X_PRACE2 + INCOME3 + EDUCA, 
           data = brfss_marijan)
      
      Residuals:
        Min      1Q  Median      3Q     Max 
      -7.9690 -2.6399 -1.5737 -0.5832 31.5561 
      
      Coefficients:
        Estimate Std. Error t value Pr(>|t|)
      (Intercept)                                                        6.244188   0.686031   9.102  < 2e-16
      Age_midpt                                                         -0.068300   0.001292 -52.855  < 2e-16
      X_PRACE2Black or African American                                 -0.207406   0.084220  -2.463  0.01379
      X_PRACE2American Indian or Alaskan Native                          1.218726   0.160309   7.602 2.93e-14
      X_PRACE2Asian                                                     -1.443476   0.105695 -13.657  < 2e-16
      X_PRACE2Native Hawaiian or other Pacific Islander                 -0.354284   0.140663  -2.519  0.01178
      X_PRACE2Multiracial but no preferred race                          1.367469   0.333767   4.097 4.19e-05
      X_PRACE2Don\x92t know/Not sure                                    -1.128369   0.240863  -4.685 2.81e-06
      X_PRACE2No race choice given                                      -1.106126   0.195155  -5.668 1.45e-08
      X_PRACE2Refused                                                    0.223999   0.182051   1.230  0.21854
      INCOME3Less than $15,000 ($10,000 to less than $15,000)            0.479683   0.204398   2.347  0.01894
      INCOME3Less than $20,000 ($15,000 to less than $20,000)           -0.420189   0.191727  -2.192  0.02841
      INCOME3Less than $25,000 ($20,000 to less than $25,000)           -0.175597   0.179291  -0.979  0.32739
      INCOME3Less than $35,000 ($25,000 to less than $35,000)           -0.655337   0.164598  -3.981 6.85e-05
      INCOME3Less than $50,000 ($35,000 to less than $50,000)           -0.837639   0.163478  -5.124 3.00e-07
      INCOME3Less than $75,000 ($50,000 to less than $75,000)           -1.137511   0.161452  -7.045 1.86e-12
      INCOME3Less than $100,000 ($75,000 to less than $100,000)         -1.595350   0.164589  -9.693  < 2e-16
      INCOME3Less than $150,000 ($100,000 to less than $150,000)        -1.766228   0.165430 -10.677  < 2e-16
      INCOME3Less than $200,000 ($150,000 to less than $200,000)        -2.009649   0.182513 -11.011  < 2e-16
      INCOME3$200,000 or more                                           -1.942165   0.184115 -10.549  < 2e-16
      INCOME3Dont know/Not sure                                         -1.073781   0.169473  -6.336 2.37e-10
      INCOME3Refused                                                    -1.756333   0.169669 -10.352  < 2e-16
      EDUCAGrades 1 through 8 (Elementary)                              -0.170392   0.691007  -0.247  0.80523
      EDUCAGrades 9 through 11 (Some high school)                        1.870505   0.680439   2.749  0.00598
      EDUCAGrade 12 or GED (High school graduate)                        1.246637   0.671835   1.856  0.06352
      EDUCACollege 1 year to 3 years (Some college or technical school)  1.084762   0.671878   1.615  0.10642
      EDUCACollege 4 years or more (College graduate)                    0.570441   0.671894   0.849  0.39588
      EDUCARefused                                                       0.267046   0.823078   0.324  0.74560
      
      (Intercept)                                                       ***
        Age_midpt                                                         ***
        X_PRACE2Black or African American                                 *  
        X_PRACE2American Indian or Alaskan Native                         ***
        X_PRACE2Asian                                                     ***
        X_PRACE2Native Hawaiian or other Pacific Islander                 *  
        X_PRACE2Multiracial but no preferred race                         ***
        X_PRACE2Don\x92t know/Not sure                                    ***
        X_PRACE2No race choice given                                      ***
        X_PRACE2Refused                                                      
      INCOME3Less than $15,000 ($10,000 to less than $15,000)           *  
        INCOME3Less than $20,000 ($15,000 to less than $20,000)           *  
        INCOME3Less than $25,000 ($20,000 to less than $25,000)              
      INCOME3Less than $35,000 ($25,000 to less than $35,000)           ***
        INCOME3Less than $50,000 ($35,000 to less than $50,000)           ***
        INCOME3Less than $75,000 ($50,000 to less than $75,000)           ***
        INCOME3Less than $100,000 ($75,000 to less than $100,000)         ***
        INCOME3Less than $150,000 ($100,000 to less than $150,000)        ***
        INCOME3Less than $200,000 ($150,000 to less than $200,000)        ***
        INCOME3$200,000 or more                                           ***
        INCOME3Dont know/Not sure                                         ***
        INCOME3Refused                                                    ***
        EDUCAGrades 1 through 8 (Elementary)                                 
      EDUCAGrades 9 through 11 (Some high school)                       ** 
        EDUCAGrade 12 or GED (High school graduate)                       .  
      EDUCACollege 1 year to 3 years (Some college or technical school)    
      EDUCACollege 4 years or more (College graduate)                      
      EDUCARefused                                                         
      ---
        Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
      
      Residual standard error: 6.79 on 92346 degrees of freedom
      (1361 observations deleted due to missingness)
      Multiple R-squared:  0.04498,	Adjusted R-squared:  0.0447 
      F-statistic: 161.1 on 27 and 92346 DF,  p-value: < 2.2e-16 
      