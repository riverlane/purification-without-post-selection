set term epslatex color size 5in,5in
set xlabel "Preparation error rate"
set ylabel "Output error rate"
set size square
set key top left reverse Left
set xrange [0:0.1]
set yrange [0:0.1]

set output "all-noisy.tex"
plot x title "bare" lw 3,\
     "../../data/0.3/8-2-2.out" w lines title "$(8,2,2)$" dt 3 lw 3, \
     "../../data/0.3/5-1-2a.out" w lines title "$(5,1,2)$a" dt 3 lw 3,\
     "../../data/0.3/5-1-2b.out" w lines title "$(5,1,2)$b" dt 3 lw 3,\
     "../../data/0.3/large-tolerant-light-cone.out" w lines title "fault-tolerant cycle (large light cone)" dt 4 lw 3, \
     "../../data/0.3/small-tolerant-light-cone.out" w lines title "fault-tolerant cycle (small light cone)" dt 4 lw 3, \
     "../../data/0.3/large-light-cone.out" w lines title "cycle (large light cone)" dt 2 lw 3,\
     "../../data/0.3/small-light-cone.out" w lines title "cycle (small light cone)" dt 2 lw 3,\
     "../../data/0.3/7-4-1.out" w lines title "$(7,4,1)$" dt 2 lw 3, \
     "../../data/0.3/3-1-1.out" w lines title "$(3,1,1)$" dt 1 lw 3,\
     "../../data/0.3/5-1-1.out" w lines title "$(5,1,1)$" dt 1 lw 3, \
     "../../data/0.3/7-1-2.out" w lines title "$(7,1,2)$" dt 1 lw 3, \
     "../../data/0.3/9-1-3.out" w lines title "$(9,1,3)$" dt 1 lw 3
