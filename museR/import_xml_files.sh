
for file in *
do
  extract -f 1 "$file" > v_$file
  extract -f 2 "$file" > pR_$file
  extract -f 3 "$file" > pL_$file
done

# to get to humdrum file ~/humdrum-tools/humdrum
rcheck c1.krn | grep = | tail -5
rcheck c2.krn | grep = | tail -5
rcheck c3.krn | grep = | tail -5

# Function to convert entire folder
convert_krn(){
for file in *.xml
do
    xml2hum -s1 "$file" > a.krn
    xml2hum -s2 "$file" > b.krn
    xml2hum -s3 "$file" > c.krn

    extract -i '**kern' a.krn > d.krn
    extract -i '**kern' b.krn > e.krn
    extract -i '**kern' c.krn > f.krn

    minrhy d.krn e.krn f.krn > g.txt
    grep -h 'all:' g.txt > r.txt
    rh=$(egrep -h -o '[0-9]{1,2}' r.txt)

    echo "$file"
    timebase -t "$rh" d.krn > $file.a.krn
    timebase -t "$rh" e.krn > $file.b.krn
    timebase -t "$rh" f.krn > $file.c.krn

   assemble $file.a.krn $file.b.krn $file.c.krn | rid -d > $file.krn
   rm g.txt
   rm a.krn
   rm b.krn
   rm f.krn
   rm c.krn
   rm d.krn
   rm e.krn
   rm r.txt
   rm $file.a.krn
   rm $file.b.krn
   rm $file.c.krn
done
}

convert_one_krn(){

    xml2hum -s1 $1 > a.krn
    xml2hum -s2_ $1 > b.krn
    xml2hum -s3 $1 > c.krn

    extract -i '**kern' a.krn > d.krn
    extract -i '**kern' b.krn > e.krn
    extract -i '**kern' c.krn > z.krn

    minrhy d.krn e.krn z.krn > g.txt
    grep -h 'all:' g.txt > r.txt
    rh=$(egrep -h -o '[0-9]{1,2}' r.txt)

    timebase -t "$rh" d.krn > $1.a.krn
    timebase -t "$rh" e.krn > $1.b.krn
    timebase -t "$rh" z.krn > $1.c.krn

    assemble j.krn i.krn h.krn | rid -d > $file.krn

}


mv *.xml.krn Kern_files



