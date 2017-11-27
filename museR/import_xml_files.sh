cd ~emilypalmer/Desktop/Thesis/Data

# to get to humdrum file ~/humdrum-tools/humdrum
# For folder
for file in *.xml; do
xml2hum "$file" > "$file.krn"; done

mv *.krn Kern_files   # After conversion, move to new file... still working

### Individual files: 
xml2hum -s1 h21.xml > c1.krn
xml2hum -s2 h21.xml > c2.krn
xml2hum -s3 h21.xml > c3.krn

rcheck c1.krn | grep = | tail -5
rcheck c2.krn | grep = | tail -5
rcheck c3.krn | grep = | tail -5

minrhy c2.krn c3.krn > ry.txt
grep -h 'all:' ry.txt > r.txt
egrep -h -o '[0-9]{1,2}' r.txt
extract -i '**kern' c1.krn > tc1.krn

timebase -t 24 tc1.krn > x1.krn
timebase -t 24 c2.krn > x2.krn
timebase -t 24 c3.krn > x3.krn

assemble x3.krn x2.krn x1.krn | rid -d > h1.krn

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
    extract -i '**kern' c.krn > f.krn

    minrhy d.krn e.krn f.krn > g.txt
    grep -h 'all:' g.txt > r.txt
    rh=$(egrep -h -o '[0-9]{1,2}' r.txt)

    echo "$file"
    timebase -t "$rh" d.krn > $file.a.krn
    timebase -t "$rh" e.krn > $file.b.krn
    timebase -t "$rh" f.krn > $file.c.krn
    
#    assemble j.krn i.krn h.krn | rid -d > $file.krn

done
}

convert_one_krn(){

    xml2hum -s1 $1 > a.krn
    xml2hum -s2 $1 > b.krn
    xml2hum -s3 $1 > c.krn

    extract -i '**kern' a.krn > d.krn
    extract -i '**kern' b.krn > e.krn
    extract -i '**kern' c.krn > f.krn

    minrhy d.krn e.krn f.krn > g.txt
    grep -h 'all:' g.txt > r.txt
    rh=$(egrep -h -o '[0-9]{1,2}' r.txt)

    timebase -t "$rh" d.krn > $1.a.krn
    timebase -t "$rh" e.krn > $1.b.krn
    timebase -t "$rh" f.krn > $1.c.krn
    
#    assemble j.krn i.krn h.krn | rid -d > $file.krn

}


mv *.xml.krn Kern_files



