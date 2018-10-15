set terminal epslatex color
set output "EvenOdd500Cumulative.tex"
set format xy "$%g$"
set title "(a) Cumulative Probability of Success for EvenOdd"
set xrange [0.0:500]
set yrange [0.0:1.0]
set xlabel "Generation"
set ylabel "CPS"
plot "EvenOdd500Cumulative.data" using 1:2 title "Constructive" with lines lw 3 lt rgb "#0000FF", \
     "EvenOdd500Cumulative.data" using 1:3 title "Destructive" with lines lw 3 lt rgb "#FF0000"
