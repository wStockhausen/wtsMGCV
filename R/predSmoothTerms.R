predSmoothTerms<-function(model,grids){
  terms = getSmoothTerms(model);       #--vector of smooth term names
  vars  = getVarsForSmoothTerms(terms);#--list of variable names 
  lst = list();
  for (trm in terms){
    #--testing: trm = terms[1];
    vrs = vars[[trm]];
    lst_grds = list();
    for (vr in vrs) {
      #--testng vr = vrs[1];
      lst_grds[[vr]] = grids[[vr]];
    }
    grdTbl = createGridTbl(lst_grds);
    lst[[trm]] = predSmoothTerm(model,grdTbl,trm,include_intercept=trm==terms[1]) |> 
                   dplyr::mutate(term=trm,.before=1);
  }
  return(lst);
}
#--lstTermTbls = predSmoothTerms(model,grids);
