#!/bin/bash

directory=$(pwd)

#ultimo mes para pronostico
declare -a meses=("1" "2" "3" "4" "5" "6" "7" "8" "9" "10" "11" "12")
meseslength=${#meses[@]}
  
  #horizonte
  declare -a horizonte=("1" "3" "6")
  horizontelength=${#horizonte[@]}

  #horizonte
  declare -a anio=("2017" "2018")
    
    echo  `date`
    for (( j=1; j<${horizontelength}+1; j++ ));
    do
    fore=${horizonte[$j-1]}
    
    ###################################
    ### Parallel version
    for ID in ${meses[*]}; do echo $ID; done  | xargs -I{} --max-procs 22 bash -c "
      echo $fore {};
      Rscript /home/foo/ObsPronostico/obsPronostico/modelo_autoInflacionMax1.r $fore {}"
    echo "Exit code for xargs = $?"
    ###################################
    
    done    
    echo  `date`