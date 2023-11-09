awk -v OFS=, -F' ' \
  'BEGIN {print "module,fips,month"} NR>8 { print $1, $3, $6 }'
