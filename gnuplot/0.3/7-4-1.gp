set term epslatex color #size #5in,4in
set xlabel "Preparation error rate"
set ylabel "Output error rate"
set size square
set key top left reverse Left
set xrange [0.001:1]
set yrange [0.001:1]

set logscale x 10
set logscale y 10

set output "7-4-1-line.tex"
set label 1 "$a$" center at 0.019,0.019
set label 2 "$b$" center at 0.08,0.08
set label 3 "$c$" center at 0.5,0.5
plot x title "bare" lw 2,\
     "../../data/0.3/7-4-1.out" w lines title "$(7,4,1)$" lw 2
