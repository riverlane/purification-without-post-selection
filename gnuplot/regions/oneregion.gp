set term epslatex
set output "7-4-1-region.tex"
set xlabel "CNOT depolarisation rate" offset 0,-1.5
set ylabel "$\\displaystyle\\frac{\\text{Toffoli rate}}{\\text{CNOT rate}}$" offset 1,-1
set zlabel "Preparation error rate" rotate
set logscale x 10
set logscale z 10
set xyplane 0

set xrange [0.0005:0.05]
set yrange [1:3]
set ytics 1,1
set xtics offset 0,-0.5

set view 80,10

set output "3-1-1-region.tex"
splot "<awk 'length($2)==3' ../../data/goodTriples/3-1-1.trim" pt 7 ps 0.2 notitle
set output "5-1-1-region.tex"
splot "<awk 'length($2)==3' ../../data/goodTriples/5-1-1.trim" pt 7 ps 0.2 notitle
set output "5-1-2a-region.tex"
splot "<awk 'length($2)==3' ../../data/goodTriples/5-1-2a.trim" pt 7 ps 0.2  notitle
set output "5-1-2b-region.tex"
splot "<awk 'length($2)==3' ../../data/goodTriples/5-1-2b.trim" pt 7 ps 0.2  notitle
set output "7-1-2-region.tex"
splot "<awk 'length($2)==3' ../../data/goodTriples/7-1-2.trim" pt 7 ps 0.2 notitle
set output "7-4-1-region.tex"
splot "<awk 'length($2)==3' ../../data/goodTriples/7-4-1.trim" pt 7 ps 0.2  notitle
set output "8-2-2-region.tex"
splot "<awk 'length($2)==3' ../../data/goodTriples/8-2-2.trim" pt 7 ps 0.2 notitle
set output "9-1-3-region.tex"
splot "<awk 'length($2)==3' ../../data/goodTriples/9-1-3.trim" pt 7 ps 0.2  notitle
set output "small-light-cone-region.tex"
splot "<awk 'length($2)==3' ../../data/goodTriples/small-light-cone.trim" pt 7 ps 0.2 notitle
set output "large-light-cone-region.tex"
splot "<awk 'length($2)==3' ../../data/goodTriples/large-light-cone.trim" pt 7 ps 0.2 notitle
set output "small-tolerant-light-cone-region.tex"
splot "<awk 'length($2)==3' ../../data/goodTriples/small-tolerant-light-cone.trim" pt 7 ps 0.2 notitle
set output "large-tolerant-light-cone-region.tex"
splot "<awk 'length($2)==3' ../../data/goodTriples/large-tolerant-light-cone.trim" pt 7 ps 0.2 notitle


set output "7-4-1-region.tex"
set arrow from 0.003,1,0.001 to 0.003,1,0.5 nohead dt 2
set label 1 "$a$" center at 0.003,1,0.019
set label 2 "$b$" center at 0.003,1,0.08
set label 3 "$c$" center at 0.003,1,0.5
splot "<awk 'length($2)==3' ../../data/goodTriples/7-4-1.trim" pt 7 ps 0.2 notitle
