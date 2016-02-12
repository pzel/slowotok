# slowotok development notes

## parallel speedup 

    nfiles      sequential      parList (-N4)        -N8   chunk 1000   chunk 100
         1       0m3.751s           0m2.020s           
         2       0m4.936s           0m2.842s
         3       0m11.381s          0m6.796s
         4       0m12.010s          0m6.784s
         5       0m15.568s          0m7.662s
        10       1m18.009s          0m36.221s  0m47.277s    0m50.396s   0m52.166s
        18       5m38.299s          2m58.528s



## to fetch the source data:

`for url in sklepy-cynamonowe-traktat-o-manekinach-dokonczenie.txt schulz-sanatorium-pod-klepsydra-ksiega.txt schulz-sanatorium-pod-klepsydra-genialna-epoka.txt schulz-sanatorium-pod-klepsydra-wiosna.txt schulz-sanatorium-pod-klepsydra-noc-lipcowa.txt schulz-sanatorium-pod-klepsydra-moj-ojciec-wstepuje-do-strazakow.txt schulz-sanatorium-pod-klepsydra-druga-jesien.txt schulz-sanatorium-pod-klepsydra-martwy-sezon.txt schulz-sanatorium-pod-klepsydra-sanatorium-pod-klepsydra.txt schulz-sanatorium-pod-klepsydra-dodo.txt schulz-sanatorium-pod-klepsydra-edzio.txt lord-jim.txt lalka-tom-pierwszy.txt lalka-tom-drugi.txt schulz-jesien.txt kariera-nikodema-dyzmy.txt wierna-rzeka.txt witkacy-narkotyki.txt ; do curl -O https://wolnelektury.pl/media/book/txt/$url; done`
