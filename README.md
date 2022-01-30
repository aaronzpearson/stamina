# stamina: An R package that models an athlete's player load and energy expenditure

stamina is a lightweight package that is complementary to fvp (github.com/aaronzpearson/fvp). stamina helps coaches model an athlete's aerobic and anaerobic fitness from positional tracking data. 

**This package contains minimal documentation.**

Models include:        
* VO2 max     
* Maximal aerobic speed    
* Critical speed    
* D' balance    
* Metabolic power    
* Player load    

**Note** The VO2 max and critical speed models are approximations. The VO2 max and maximal aerobic speed models are not validated and have no physiological justifications.    
**Note** All values must be in m/s (meters per second). You can use the `convert.to.metric()` function from *fvp* to help with conversions.    

## Installing the Package

To install the package, copy-and-paste the following code into your R console. The package is very small and should download quickly.

```
devtools::install_github("aaronzpearson/stamina")
library(stamina)
```

*See stamina-vignette.pdf for sample code and model outputs*
