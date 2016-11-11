#!/bin/bash

dateDiff_attence=$((($(date +"%s")-$(date -d "$1" +"%s"))/60))
echo "=== attence ==="
grep "$1" ~/recommend/attence/log|~/Scripts/awkTest.awk -v var=$dateDiff_attence
echo ""
dateDiff_credit=$(($(date +"%H")/3 + 1))
echo "=== credit ==="
grep "$1" ~/recommend/credit/log|~/Scripts/credit_log.awk -v var=$dateDiff_credit
