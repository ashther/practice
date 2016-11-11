#!/bin/awk -f

BEGIN {
  FS="2016"
}

{
  if ($2 ~ /cluster finished/) {cluster_count++}
  if ($2 ~ /data modify finished/) {modify_count++}
  if ($2 ~ /failed/) {fail_count++}
}

END {
  print "total lines: "NR
  print "last line: "$1
  printf("cluster caculation: %d, data modify: %d, failed: %d\n", 
          cluster_count, modify_count, fail_count)
}