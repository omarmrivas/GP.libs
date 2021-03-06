set terminal epslatex color
set output "Sum500LambdaTerms.tex"
set format y "$10^{%T}$"
set title "Number of typed $\\lambda$-terms for Sum"
set xrange [1.0:40]
set logscale y
set xlabel "Size"
set ylabel "$\\lambda$-terms"
plot "Sum500LambdaTerms.data" using 1:2 title "Constructive Style" with lines lw 3 lt rgb "#0000FF", \
     "Sum500LambdaTerms.data" using 1:3 title "Destructive Style" with lines lw 3 lt rgb "#FF0000"
