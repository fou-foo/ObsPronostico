#!/bin/bash

directory=$(pwd)

#ultimo mes para pronostico SOLO SE TIENE INFORMACION COMLETA HASTA MARZO
declare -a meses=("1" "2" "3")
meseslength=${#meses[@]}
  
  #horizonte
  declare -a horizonte=("1" "3" "6")
  horizontelength=${#horizonte[@]}

  #horizonte
  declare -a anio=("2019")
    
    echo  `date`
    for (( j=1; j<${horizontelength}+1; j++ ));
    do
    fore=${horizonte[$j-1]}
    
    ###################################
    ### Parallel version
    for ID in ${meses[*]}; do echo $ID; done  | xargs -I{} --max-procs 6 bash -c "
      echo $fore {};
      Rscript  /home/foo/ObsPronostico/obsPronostico/ActualizacionMensual/modelo_autoInflacionMax1Mensual.r $fore {}"
    echo "Exit code for xargs = $?"
    ###################################
    
    done    
    echo  `date`