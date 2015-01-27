BEGIN { total = 0 }
{ split($0,a,"/"); total += a[1]; }
END { print total / NR; }


