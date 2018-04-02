
# to get to humdrum file ~/humdrum-tools/humdrum

### Individual files: 
xml2hum -s1 h13.xml > c1.krn
xml2hum -s2 h13.xml > c2.krn
xml2hum -s3 h13.xml > c3.krn

rcheck c1.krn | grep = | tail -5
rcheck c2.krn | grep = | tail -5
rcheck c3.krn | grep = | tail -5

extract -i '**kern' c1.krn > tc1.krn
extract -i '**kern' c3.krn > tc3.krn
minrhy c2.krn tc3.krn > ry.txt
grep -h 'all:' ry.txt > r.txt
egrep -h -o '[0-9]{1,2}' r.txt


timebase -t 16 tc1.krn > h13.xml-a.krn
timebase -t 16 c2.krn > h13.xml-b.krn
timebase -t 16 tc3.krn > h13.xml-c.krn

#assemble x3.krn x2.krn x1.krn | rid -d > h3.krn

# Function to convert entire file (still error when rhythem is wrong)
convert_krn

convert_krn(){
for file in *.xml
do
    xml2hum -s1 "$file" > a.krn
    xml2hum -s2 "$file" > b.krn
    xml2hum -s3 "$file" > c.krn

    extract -i '**kern' a.krn > d.krn
    extract -i '**kern' b.krn > e.krn
    extract -i '**kern' c.krn > z.krn

    minrhy d.krn e.krn z.krn > g.txt
    grep -h 'all:' g.txt > r.txt
    rh=$(egrep -h -o '[0-9]{1,2}' r.txt)

    echo "$file"
    timebase -t "$rh" d.krn > $file.a.krn
    timebase -t "$rh" e.krn > $file.b.krn
    timebase -t "$rh" z.krn > $file.c.krn
    
   assemble $file.a.krn $file.b.krn $file.c.krn | rid -d > $file.krn
   rm g.txt
   rm a.krn
   rm b.krn
   rm c.krn
   rm d.krn
   rm e.krn
   rm z.krn
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
    
#    assemble j.krn i.krn h.krn | rid -d > $file.krn

}


mv *.xml.krn Kern_files



