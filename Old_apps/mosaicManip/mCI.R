mCI = function(){
  if (!require(manipulate) | !require(lattice) | !require(grid)) stop("Must have manipulate package.")
  Wald=function(p.hat, n, conf.level, sd=sqrt(p.hat*(1-p.hat))){
    error = (qnorm(.5+conf.level/2)*sd)/(sqrt(n))
    return(list(lower = p.hat-error, upper = p.hat+error))
  }
  Agresti= function(p.hat, n, conf.level){
    sd=sqrt(p.hat*(1-p.hat))
    z = qnorm(.5+conf.level/2)
    ntilde = n+z^2
    ptilde = (p.hat*n + (z^2)/2)/ntilde
    error = z*sqrt(ptilde*(1-ptilde)/ntilde)
    return(list(lower=p.hat-error, upper=p.hat+error))
  }

 myFun=function(n=n, conf.level=0.95,p=p,ntrials=10,seed=125, int.type=Wald){
  set.seed(seed)
  # === start of panel function
  mypanel=function(x,y){
   outside = 0
   for (k in (ntrials:1) ) {
    p.hat <- rbinom(1,size=n,prob=p)/n
    int <- int.type(p.hat=p.hat,n=n,conf.level=conf.level)
    lower.bound <- int[1]
    upper.bound <- int[2]
    panel.abline(v=p, col = "red")
    lsegments(0, k, 1, k)
    lsegments(x0=lower.bound,y0=k,x1=upper.bound,y1=k, lwd=5)
    ltext(c(lower.bound, upper.bound), c(k,k), c("(",")"))    #OPTIONAL PARENTHENSES ARROWS
    lpoints(p.hat, k, pch = 16)
    if(p<lower.bound|p>upper.bound){
      lpoints(1.02, k, pch = 8, col = "chocolate1", cex = 1.5)
      outside=outside+1
    }
      
  }
  popViewport()
  grid.text(paste("Total failed CIs:", outside), 
            x=unit(.5, "npc"),
            y=unit(.98, "npc"),
            just = "center",
            gp = gpar(fontsize=15, col="chocolate1"))
 }
 # === end of panel function
 xyplot(0:1 ~ 0:1, panel = mypanel, type="n",
       ylim=rev(c(0,max(ntrials+1,20))),
       xlim=c(-.1, 1.1),
       ylab="Trial Number",
       xlab="Probability")

}


#==========
manipulate(myFun(n=n, conf.level=conf.level,p=p, ntrials=ntrials, seed=seed, int.type=int.type),
           n = slider(5, 500, step = 1, initial=100, label = "Sample Size"),
           conf.level = slider(.01, 1.00, step = .01, initial=0.95, label = "Confidence Level"),
           p = slider(0,1, step = .01, initial=0.8, label = "True Mean"),
           ntrials = slider(1, 100, step = 1, initial = 1, label = "Number of Trials"),
           seed = slider(100,200, step=1, initial=125, label = "Random Seed"),
           int.type = picker("Agresti"=Agresti,"Wald"=Wald, label = "Type of CI")
           )
}
