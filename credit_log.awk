#!/bin/awk -f

BEGIN {
  FS="2016"
}

{
  if ($2 ~ /Opening/) {open_count++}
  if ($2 ~ /credit caculate finished/) {credit_count++}
  if ($2 ~ /failed/) {fail_count++}
}

END {
  print "should have: "var
  print "total times: "open_count
  print "last line: "$1
  printf("credit caculation: %d, failed: %d\n", 
          credit_count, fail_count)
}