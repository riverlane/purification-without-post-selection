set term epslatex color
set contour base
set view map
set cntrparam levels discrete 0.003, 0.01, 0.03, 0.1
set xlabel "CNOT depolarisation rate"
set ylabel "Toffoli rate / CNOT rate"
set key noautotitle invert
set linetype 2 dt 1 linecolor "black" lw 3
set linetype 3 dt 2 linecolor "black" lw 3
set linetype 4 dt 3 linecolor "black" lw 3
set linetype 5 dt 4 linecolor "black" lw 3
set palette defined (0 "#ffffff", 1 "#00968f")

set logscale x 10
set logscale cb 10

set xrange [0.0005:0.05]
set cbrange [0.005:0.5]

set output "3-1-1.tex"
splot "../../data/careful/3-1-1.out" w pm3d

set output "5-1-1.tex"
splot "../../data/careful/5-1-1.out" w pm3d

set output "7-1-2.tex"
splot "../../data/careful/7-1-2.out" w pm3d

set output "9-1-3.tex"
splot "../../data/careful/9-1-3.out" w pm3d

set output "5-1-2a.tex"
splot "../../data/careful/5-1-2a.out" w pm3d

set output "5-1-2b.tex"
splot "../../data/careful/5-1-2b.out" w pm3d

set output "7-4-1.tex"
splot "../../data/careful/7-4-1.out" w pm3d

set output "8-2-2.tex"
splot "../../data/careful/8-2-2.out" w pm3d

set output "large-light-cone.tex"
splot "../../data/careful/large-light-cone.out" w pm3d

set output "small-light-cone.tex"
splot "../../data/careful/small-light-cone.out" w pm3d

set output "small-tolerant-light-cone.tex"
splot "../../data/careful/small-tolerant-light-cone.out" w pm3d

set output "large-tolerant-light-cone.tex"
splot "../../data/careful/large-tolerant-light-cone.out" w pm3d
