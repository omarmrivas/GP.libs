set terminal epslatex color
set output "AckermanConsts500Error.tex"
set format xy "$%g$"
set title "(e) Best-of-generation error for Ackerman (constructive)"
set xrange [0.0:23]
set yrange [0.0:1.0]
set xlabel "Generation"
set ylabel "Minimum error"
plot "AckermanConsts500Error.data" using 1:2 title "" with lines lw 3 lt rgb "#000000", \
     "AckermanConsts500Error.data" using 1:3 title "" with lines lt rgb "#7F7F7F", \
     "AckermanConsts500Error.data" using 1:4 title "" with lines lt rgb "#7F7F7F", \
     "AckermanConsts500Error.data" using 1:5 title "" with lines lt rgb "#7F7F7F", \
     "AckermanConsts500Error.data" using 1:6 title "" with lines lt rgb "#7F7F7F", \
     "AckermanConsts500Error.data" using 1:7 title "" with lines lt rgb "#7F7F7F", \
     "AckermanConsts500Error.data" using 1:8 title "" with lines lt rgb "#7F7F7F", \
     "AckermanConsts500Error.data" using 1:9 title "" with lines lt rgb "#7F7F7F", \
     "AckermanConsts500Error.data" using 1:10 title "" with lines lt rgb "#7F7F7F", \
     "AckermanConsts500Error.data" using 1:11 title "" with lines lt rgb "#7F7F7F", \
     "AckermanConsts500Error.data" using 1:12 title "" with lines lt rgb "#7F7F7F", \
     "AckermanConsts500Error.data" using 1:13 title "" with lines lt rgb "#7F7F7F", \
     "AckermanConsts500Error.data" using 1:14 title "" with lines lt rgb "#7F7F7F", \
     "AckermanConsts500Error.data" using 1:15 title "" with lines lt rgb "#7F7F7F", \
     "AckermanConsts500Error.data" using 1:16 title "" with lines lt rgb "#7F7F7F", \
     "AckermanConsts500Error.data" using 1:17 title "" with lines lt rgb "#7F7F7F", \
     "AckermanConsts500Error.data" using 1:18 title "" with lines lt rgb "#7F7F7F", \
     "AckermanConsts500Error.data" using 1:19 title "" with lines lt rgb "#7F7F7F", \
     "AckermanConsts500Error.data" using 1:20 title "" with lines lt rgb "#7F7F7F", \
     "AckermanConsts500Error.data" using 1:21 title "" with lines lt rgb "#7F7F7F", \
     "AckermanConsts500Error.data" using 1:22 title "" with lines lt rgb "#7F7F7F", \
