#!/bin/bash

COMPUTER=$(cat /tmp/computer)

(
  flock -n 200 || exit 1

  source /etc/environment

  echo
  echo
  echo
  echo
  echo "****START****SYKDOMSPULS****"

  /usr/local/bin/Rscript /r/sykdomspuls/src/RunGetData.R

  echo "****END****SYKDOMSPULS****"

) 200>/var/lock/.sykdomspulsdata.exclusivelock
