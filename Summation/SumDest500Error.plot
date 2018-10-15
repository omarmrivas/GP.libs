set terminal epslatex color
set output "SumDest500Error.tex"
set format xy "$%g$"
set title "(b) Best-of-generation error for Sum (destructive)"
set xrange [0.0:28]
set yrange [0.0:1.0]
set xlabel "Generation"
set ylabel "Minimum error"
plot "SumDest500Error.data" using 1:2 title "" with lines lw 3 lt rgb "#000000", \
     "SumDest500Error.data" using 1:3 title "" with lines lt rgb "#7F7F7F", \
     "SumDest500Error.data" using 1:4 title "" with lines lt rgb "#7F7F7F", \
     "SumDest500Error.data" using 1:5 title "" with lines lt rgb "#7F7F7F", \
     "SumDest500Error.data" using 1:6 title "" with lines lt rgb "#7F7F7F", \
     "SumDest500Error.data" using 1:7 title "" with lines lt rgb "#7F7F7F", \