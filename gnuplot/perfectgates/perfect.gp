set term epslatex color size 5in,4in
set xlabel "Preparation error rate"
set ylabel "Output error rate"
set size square
set key top left reverse Left

set output "perfect.tex"
plot x                                                               title "bare"                      lw 3, \
     "../../data/perfectgates/small-light-cone.out"          w lines title "cycle"                dt 2 lw 3, \
     "../../data/perfectgates/7-4-1.out"                     w lines title "$(7,4,1)$"            dt 2 lw 3, \
     "../../data/perfectgates/small-tolerant-light-cone.out" w lines title "fault-tolerant cycle" dt 2 lw 3, \
     "../../data/perfectgates/3-1-1.out"                     w lines title "$(3,1,1)$"            dt 1 lw 3, \
     "../../data/perfectgates/5-1-1.out"                     w lines title "$(5,1,1)$"            dt 1 lw 3, \
     "../../data/perfectgates/8-2-2.out"                     w lines title "$(8,2,2)$"            dt 3 lw 3, \
     "../../data/perfectgates/5-1-2a.out"                    w lines title "$(5,1,2)$a/b"         dt 3 lw 3, \
     "../../data/perfectgates/7-1-2.out"                     w lines title "$(7,1,2)$"            dt 1 lw 3, \
     "../../data/perfectgates/9-1-3.out"                     w lines title "$(9,1,3)$"            dt 1 lw 3

set xrange [0:0.1]
set yrange [0:0.1]
set output "perfect2.tex"
plot x                                                               title "bare"                      lw 3, \
     "../../data/perfectgates/small-light-cone.out"          w lines title "cycle"                dt 2 lw 3, \
     "../../data/perfectgates/7-4-1.out"                     w lines title "$(7,4,1)$"            dt 2 lw 3, \
     "../../data/perfectgates/small-tolerant-light-cone.out" w lines title "fault-tolerant cycle" dt 2 lw 3, \
     "../../data/perfectgates/3-1-1.out"                     w lines title "$(3,1,1)$"            dt 1 lw 3, \
     "../../data/perfectgates/5-1-1.out"                     w lines title "$(5,1,1)$"            dt 1 lw 3, \
     "../../data/perfectgates/8-2-2.out"                     w lines title "$(8,2,2)$"            dt 3 lw 3, \
     "../../data/perfectgates/5-1-2a.out"                    w lines title "$(5,1,2)$a/b"         dt 3 lw 3, \
     "../../data/perfectgates/7-1-2.out"                     w lines title "$(7,1,2)$"            dt 1 lw 3, \
     "../../data/perfectgates/9-1-3.out"                     w lines title "$(9,1,3)$"            dt 1 lw 3
