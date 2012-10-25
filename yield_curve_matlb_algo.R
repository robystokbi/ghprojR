dati4_5a_porting <- function( mydata, curvetype = 'ns', makeplot = 'no' )
{
  #
  # Commenti Originali Matlab dati4_5a_fun_new
  #
  # %
  # % mydata = [ptqrea; matpag; matsca]
  # % ptqrea = prezzi osservati sul mercato dei titoli utilizzati nella stima
  # % matpag = matrice dei pagamenti dei titoli
  # % matsca = matrice delle date dei pagamenti dei titoli
  # % curvetype = 'ns' oppure 'nss'
  # % makeplot  = 'si' oppure 'no'
  #
  # % Nel modello 'nss':
  # %
  # %    b1 = tasso a lungo termine
  # %    b2 = pendenza della curva (con segno cambiato)
  # %    b1 + b2 = tasso overnight
  # %    b3 = altezza primo max/min
  # %    b4 = altezza secondo max/min
  # %    tau1 = posizione primo max/min
  # %    tau2 = posizione secondo max/min
  # %
  #
  ##########################################################################
  #
  # % ptqrea = prezzi osservati sul mercato dei titoli utilizzati nella stima
  #
  ptqrea = mydata[1,] 
  #
  ##########################################################################
  # 
  #  temporaneo - mydata senza prezzi
  #                                                  tmp = mydata(2:end,:);
  tmp = mydata[2:nrow(mydata),]
  
  #
  ##########################################################################
  #
  # % matpag = matrice dei pagamenti dei titoli
  #
  #                                         matpag = tmp(1:size(tmp,1)/2,:);  
  matpag = tmp[1:nrow(tmp)/2,]
  #
  ##########################################################################
  #

  #
  ##########################################################################
  #
  # % matsca = matrice delle date dei pagamenti dei titoli
  #  
  #                                     matsca = tmp(size(tmp,1)/2+1:end,:);
  #  
  matsca = tmp[(nrow(tmp)/2+1):nrow(tmp),]
  #
  ##########################################################################
  #
  #
  #
  ##
  ## OptionsUnc = optimset('LargeScale','off','Display','notify');
  ## % OptionsCon = optimset('MaxFunEvals',1e5,'MaxIter',1e5,'Algorithm','sqp','Display','notify');
  ## if isequal(curvetype,'ns')
  ## lb = [0.01 -0.10 -5 0.001];
  ## ub = [0.50  0.10  5   Inf];
  ## x0 = [0.07 -0.05  1     1];
  ## %
  ## %
  ## % x = fminunc(fun,x0,options)
  ## %
  ## % fun can also be a function handle for an anonymous function.
  ## %
  ## % x = fminunc(@(x)norm(x)^2,x0,options);
  ## %
  ## %
  ## %
  ## [parcrv,fval] = fminunc(@(x)minfun(x,ptqrea,matsca,matpag,curvetype),x0,OptionsUnc);
  #
  #
  #########################################
  #
  # Ovviamente implemento solo 'ns'
  #
  x0 = c(0.07 , -0.05 ,  1 ,   1)
  #
 
  minfun_matlb <- function(x, ptqrea, matsca, matpag, curvetype)
  {
    #  %
    #  [yldtmp,<tilde>] = yldfun(matsca,x,curvetype);
    #  fatsco = exp(-matsca.*yldtmp);
    #  %
    #  %
    #   ptqteo = nansum(matpag.*fatsco);
    #  dur = nansum(matsca.*matpag.*fatsco)./ptqteo;
    #  wtd = (1./dur)/sum(1./dur);
    #  wtd = wtd.^(1/2);
    #  %
    #  y = (ptqrea-ptqteo).*wtd;
    #  output = y*y';
    #  %
    #  %
    
    # %
    # function [yldspot, yldfrwd] = yldfun(maturity, parcrv, curvetype)
    # %
    # tmp = num2cell(parcrv);
    # if isequal(curvetype,'ns')
    #    [b1 b2 b3 tau] = deal(tmp{:});
    #    yldspot = b1+tau*(b2+b3)*(1-exp(-maturity/tau))./maturity-b3*exp(-maturity/tau);
    #  #### yldfrwd = b1+b2*exp(-maturity/tau)+b3*((maturity/tau).*exp(-maturity/tau));
    #  #### elseif isequal(curvetype,'nss')
    #  #### [b1 b2 b3 b4 tau1 tau2] = deal(tmp{:});
    #  #### yldspot = b1+tau1*(b2+b3)*(1-exp(-maturity/tau1))./maturity-b3*exp(-maturity/tau1)+b4*(tau2-exp(-maturity/tau2).*(maturity+tau2))./maturity;
    #  #### yldfrwd = b1+b2*exp(-maturity/tau1)+b3*((maturity/tau1).*exp(-maturity/tau1))+b4*((maturity/tau2).*exp(-maturity/tau2));
    # end
    
    yldfun_matlb <- function(maturity, parcrv)
    {
      b1 = parcrv[1]
      b2 = parcrv[2]
      b3 = parcrv[3]
      tau = parcrv[4]
      #
      #### matlab
      #
      # yldspot = b1+tau*(b2+b3)*(1-exp(-maturity/tau))./maturity-b3*exp(-maturity/tau)
      #
      # Array right division. A./B is the matrix with elements A(i,j)/B(i,j).
      # A and B must have the same size, unless one of them is a scalar.
      #
      # Array multiplication. A.*B is the element-by-element product of the arrays A and B.
      # A and B must have the same size, unless one of them is a scalar.
      #
      # in R basta dividere : l'operazione viene fatta elem by elem
      # in R basta moltiplicare : l'operazione viene fatta elem by elem
      #
      
      #
      return( b1+tau*(b2+b3)*(1-exp(-maturity/tau))/maturity-b3*exp(-maturity/tau) )
    }

    
    #  %
    #  [yldtmp,<tilde>] = yldfun(matsca,x,curvetype);
    #  fatsco = exp(-matsca.*yldtmp);
    
    yldtmp <- yldfun(matsca,x)
    
    fatsco = exp(-matsca * yldtmp)
    
    
    #  %
    #  %
    #   ptqteo = nansum(matpag.*fatsco);
    #
    #  For vectors x, nansum(x) is the sum of the remaining elements,
    #  once NaN values are removed. For matrices X, nansum(X) is a row vector of column
    #  sums, once NaN values are removed.
    #
    
    #  dur = nansum(matsca.*matpag.*fatsco)./ptqteo;
    #  wtd = (1./dur)/sum(1./dur);
    #  wtd = wtd.^(1/2);
    #  %
    #  y = (ptqrea-ptqteo).*wtd;
    #  output = y*y';
    #  %
    #  %
    
  }
  
  ################################################
  #
  # Esempio
  #
  # f <- function(x, a)
  # {
  #  res <- sum((x-a)^2)
  #  attr(res, "gradient") <- 2*(x-a)
  #  res
  # }
  #
  # resf = nlm(f, c(10,10), a=c(3,5))
  # return(resf)

    
  #  %
  #  function output = minfun(x, ptqrea, matsca, matpag, curvetype)
  #  %
  #  [yldtmp] = yldfun(matsca,x,curvetype);
  #  fatsco = exp(-matsca.*yldtmp);
  #  %
  #  ptqteo = nansum(matpag.*fatsco);
  #  dur = nansum(matsca.*matpag.*fatsco)./ptqteo;
  #  wtd = (1./dur)/sum(1./dur);
  #  wtd = wtd.^(1/2);
  #  %
  #  y = (ptqrea-ptqteo).*wtd;
  #  output = y*y';
  #  %
  #
  #######################################################
    
  
  return( list( ptqrea=ptqrea, matpag=matpag, matsca=matsca ) )
  
  
  
  
  
}

parbribtp <- read.csv("parbribtp_20120130.txt", head=FALSE)
matbribtp <- data.matrix(parbribtp)

res_roby <- dati4_5a_porting( matbribtp )

#####################
#
# Test modify per github
#
#####################
