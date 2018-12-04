set terminal epslatex color
set output "Ackerman500Cumulative.tex"
set format xy "$%g$"
set title "(c) Cumulative Probability of Success for Ackerman"
set xrange [0.0:500]
set yrange [0.0:1.0]
set xlabel "Generation"
set ylabel "CPS"
plot "Ackerman500Cumulative.data" using 1:2 title "Constructive" with lines lw 3 lt rgb "#0000FF", \
     "Ackerman500Cumulative.data" using 1:3 title "Destructive" with lines lw 3 lt rgb "#FF0000"
