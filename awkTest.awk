#!/bin/awk -f

BEGIN {
  FS="  "
}

{
  if ($3 ~ /cluster finished/) {cluster_count++}
  if ($3 ~ /data modify finished/) {modify_count++}
  if ($3 ~ /failed/) {fail_count++}
}

END {
  print "total lines: "NR
  print "last line: "$1, $2
  printf("cluster caculation: %d, data modify: %d, failed: %d\n", 
          cluster_count, modify_count, fail_count)
}