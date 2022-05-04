awk -v OFS=, -F' ' \
  'BEGIN {print "module,hexid,month"} NR>8 { print $1, $3, $6 }'
