set term epslatex color size 6in,9in
set output "fast.tex"
#set term pdfcairo size 5in,8in
#set output "~/winhome/multiplot.pdf" 
set contour base
set view map
set cntrparam levels discrete 0.003, 0.01, 0.03, 0.1
# set xlabel "CNOT depolarisation rate"
# set ylabel "Toffoli rate / CNOT rate"
# set key noautotitle invert
unset key
set linetype 2 dt 1 linecolor "black" lw 3
set linetype 3 dt 2 linecolor "black" lw 3
set linetype 4 dt 3 linecolor "black" lw 3
set linetype 5 dt 4 linecolor "black" lw 3
#set palette defined (0 "#ffffff", 1 "#1d3c34")
# set palette defined (0 "#ffffff", 1 "#006f62")
set palette defined (0 "#ffffff", 1 "#00968f")

set logscale x 10
set logscale cb 10

set xrange [0.0005:0.05]
set cbrange [0.005:0.5]

set macros


set multiplot 

FIRSTCOLUMN = "set lmargin at screen 0.1; set rmargin at screen 0.5; set ytics ('1.5' 1.5, '2' 2, '2.5' 2.5); unset colorbox"
SECONDCOLUMN = "set lmargin at screen 0.5; set rmargin at screen 0.9; set ytics ('' 1, '' 1.5, '' 2, '' 2.5); unset ylabel; set colorbox"

FIRSTROW = "set tmargin at screen 1; set bmargin at screen 0.77; set format x ''; unset xlabel"
SECONDROW = "set tmargin at screen 0.77; set bmargin at screen 0.54"
THIRDROW = "set tmargin at screen 0.54; set bmargin at screen 0.31"
FOURTHROW = "set tmargin at screen 0.31; set bmargin at screen 0.08; set format x '%g'"#"; set xlabel 'CNOT depolarisation rate'"# ; set xtics ('5e-4' 0.0005, '1e-3' 0.001, '5e-3' 0.005, '1e-2' 0.01, '5e-2' 0.05)

PLACE = "at 0.0006,2.85 front"


@FIRSTCOLUMN
@FIRSTROW
set label 1 "$(5,1,2)$a" @PLACE
splot "../../data/fast/5-1-2a.out" w pm3d
@SECONDROW
set label 1 "$(8,2,2)$" @PLACE
splot "../../data/fast/8-2-2.out" w pm3d
@THIRDROW
set label 1 "large-light-cone" @PLACE
splot "../../data/fast/large-light-cone.out" w pm3d
@FOURTHROW
set label 1 "large-tolerant-light-cone" @PLACE
splot "../../data/fast/large-tolerant-light-cone.out" w pm3d

@SECONDCOLUMN
@FIRSTROW
set label 1 "$(5,1,2)$b" @PLACE
splot "../../data/fast/5-1-2b.out" w pm3d
@SECONDROW
set label 1 "$(7,4,1)$a" @PLACE
splot "../../data/fast/7-4-1.out" w pm3d
@THIRDROW
set label 1 "small-light-cone" @PLACE
splot "../../data/fast/small-light-cone.out" w pm3d
@FOURTHROW
set label 1 "small-tolerant-light-cone" @PLACE
set key noautotitle invert bottom
set label 2 "CNOT depolarisation rate" center at screen 0.5, screen 0.03
set label 3 "Toffoli rate / CNOT rate" center rotate at screen 0.01, screen 0.54
splot "../../data/fast/small-tolerant-light-cone.out" w pm3d