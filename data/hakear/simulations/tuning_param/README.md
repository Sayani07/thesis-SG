**Objective:** Choice of optimal tuning parameter $\lambda$ with the help of two alternate designs (vary_x and vary_f)  for different nx and nfacets. The lambda for which the two designs intersect have to be chosen to put appropriate weights for within-facet and between-facet distances.

**Assumptions:** The initial combination has a $N(\mu,\sigma)$ distribution and then every combination increases as follows:  
- $\mu_{j.} = \mu + j\omega$ (for design vary_x)  
- $\mu_{.k} = \mu + k\omega$ (for design vary_f)  
Value of the parameters chosen for this simulation are $\mu = 0$, $\sigma = 1$ and $\omega = {1, 2, \dots, 10}$

**Questions:** 
- Fixing $\omega$, which is the value of $\lambda$ where the design interacts  
- How value of $\lambda$ changes with increasing $\omega$  
- How value of $\lambda$ changes with increasing $\omega$ and mean for fixed nx, nfacet and standard deviation?  
- How value of $\lambda$ changes with increasing $\omega$ and sd for fixed nx, nfacet and mean?  