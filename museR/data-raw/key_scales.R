#==============================================================
key_sigs <- c("nosf","f#","f#c#","f#c#g#","f#c#g#d#","f#c#g#d#a#",
              "f#c#g#d#a#e#","f#c#g#d#a#e#b#",
              "b-e-a-d-g-c-f-","b-e-a-d-g-c-","b-e-a-d-g-",
              "b-e-a-d-","b-e-a-","b-e-","b-")
major_keys <- c("C","G","D","A","E","B","F#","C#",
                "C-","G-","D-","A-","E-","B-","F")
minor_keys <- c("a","e","b","f#","c#","g#","d#","a#","a-",
                "e-","b-","f","c","g","d")
notes <- as.factor(c("A","A-","A#","B","B-","B#","C","C-","C#",
                     "D","D-","D#","E","E-","E#","F","F-","F#",
                     "G","G-","G#","rest"))
key <- t(data.frame(major_keys,minor_keys))
colnames(key) <- key_sigs


#==============================================================

scale_degree_names <- c("Tonic","Supertonic","Mediant",
                        "Subdominant",
                        "Dominant","Submediant",
                        "Leading Tone")
solfege <- c("Do","Di","Ra","Re","Ri","Me","Mi","Fa",
             "Fi","Se","Sol","Si","Le","La","Li","Te","Ti")

scale_degree_roman_major <- c("I","ii","iii","IV","V","vi","viid")
scale_degree_roman_minor <- c("i","iid","III","iv","V","VI","viid")

# major scales
C <- c("C","D","E","F","G","A","B")
G <- c("G","A","B", "C","D","E","F#")
D <- c("D","E","F#","G","A","B","C#")
A <- c("A","B","C#","D","E","F#","G#")
E <- c("E","F#","G#","A","B","C#","D#")
B <- c("B","C#","D#","E","F#","G#","A#")
Fs <- c("F#","G#","A#","B","C#","D#","E#")
Cs <- c("C#","D#","E#","F#","G#","A#","B#")
Cb <- c("C-","D-","E-","F-","G-","A-","B-")
Gb <- c("G-","A-","B-","C-","D-","E-","F")
Db <- c("D-","E-","F","G-","A-","B-","C")
Ab <- c("A-","B-","C","D-","E-","F","G")
Eb <- c("E-","F","G","A-","B-","C","D")
Bb <- c("B-","C","D","E-","F","G","A")
FM <- c("F","G","A","B-","C","D","E")


# minor scales - natrual
a <- c("A","B","C","D","E","F","G")
e <- c("E","F#","G","A","B","C","D")
b <- c("B","C#","D","E","F#","G","A")
fs <- c("F#","G#","A","B","C#","D","E")
cs <- c("C#","D#","E","F#","G#","A","B")
gs <- c("G#","A#","B","C","D#","E","F#")
ds <- c("D#","E#","F#","G#","A#","B","C#")
as <- c("A#","B#","C#","D#","E#","F#","G#")
ab <- c("A-","B-","C-","D-","E-","F-","G-")
eb <- c("E-","F","G-","A-","B-","C-","D-")
bb <- c("B-","C","D-","E-","F","G-","A-")
f <- c("F","G","A-","B-","C","D-","E-")
c <- c("C","D","E-","F","G","A-","B-")
g <- c("G","A","B-","C","D","E-","F")
d <- c("D","E","F","G","A","B-","C")

scales <- data.frame(scale_degree_names,scale_degree_roman_major,
                     C,G,D,A,E,B,Fs,Cs,
                     Cb,Gb,Db,Ab,Eb,Bb,FM,scale_degree_roman_minor,
                     a,e,b,fs,cs,gs,ds,as,ab,eb,bb,f,c,g,d)
colnames(scales) <- c("scale_degree_names","scale_degree_roman_major",
                      "C","G","D","A","E","B","F#","C#",
                      "C-","G-","D-","A-","E-","B-","F",
                      "scale_degree_roman_minor",
                      "a","e","b","f#","c#","g#","d#","a#",
                      "a-","e-","b-","f","c","g","d")




#==============================================================

solfege_names <- c("Do","Di","Ra","Re","Ri","Me","Mi",
                   "Fa","Fi","Se","Sol","Si","Le","La",
                   "Li","Te","Ti")

m2a <- list(c("Do","Di"),c("Do","Ra"),c("Di","Re"),c("Ra","Re"),
            c("Re","Ri"),c("Re","Me"),c("Ri","Mi"),c("Me","Mi"),
            c("Mi","Fa"),c("Fa","Fi"),c("Fa","Se"),c("Fi","Sol"),
            c("Se","Sol"),c("Sol","Si"),c("Sol","Le"), c("Si","La"),
            c("Le","La"),c("La","Li"),c("La","Te"),c("Li","Ti"),
            c("Te","Ti"),c("Ti","Do"))
m2a <- as.data.frame(m2a) %>% t()
m2a_n <- rep("minor 2nd",length(m2a))
M2a <- list(c("Do","Re"),c("Di","Ri"),c("Ra","Ri"),c("Re","Mi"),
            c("Ri","Fa"),c("Me","Fa"),c("Mi","Fi"),c("Mi","Se"),
            c("Fa","Sol"),c("Fi","Si"),c("Fi","Le"),c("Se","Si"),
            c("Sol","La"),c("Si","Li"),c("Si","Te"), c("Le","Li"),
            c("Le","Te"),c("La","Ti"),c("Li","Do"),c("Te","Do"),
            c("Ti","Di"),c("Ti","Ra"))
M2a_n <- rep("Major 2nd", length(M2a))

m3a <- list(c("Do","Ri"),c("Do","Me"),c("Di","Mi"),c("Ra","Mi"),
            c("Re","Fa"),c("Ri","Fi"),c("Me","Fi"),c("Ri","Se"),
            c("Me","Se"),c("Mi","Sol"),c("Fa","Si"),c("Fa","Le"),
            c("Fi","La"),c("Se","La"),c("Sol","Li"),c("Sol","Te"),
            c("Si","Ti"),c("Le","Ti"),c("La","Do"),c("Li","Di"),
            c("Li","Ra"),c("Te","Di"),c("Te","Ra"),c("Ti","Re"))
m3a_n <- rep("Minor 3rd", length(m3a))

M3a <- list(c("Do","Mi"),c("Di","Fa"),c("Ra","Fa"),c("Re","Fi"),
            c("Re","Se"),c("Ri","Sol"),c("Me","Sol"),c("Mi","Si"),
            c("Mi","Le"),c("Fa","La"),c("Fi","Li"),c("Fi","Te"),
            c("Se","Li"),c("Se","Te"),c(""))




#==============================================================


# major scales
C <- c("C","D","E","F","G","A","B")
G <- c("G","A","B", "C","D","E","F#")
D <- c("D","E","F#","G","A","B","C#")
A <- c("A","B","C#","D","E","F#","G#")
E <- c("E","F#","G#","A","B","C#","D#")
B <- c("B","C#","D#","E","F#","G#","A#")
Fs <- c("F#","G#","A#","B","C#","D#","E#")
Cs <- c("C#","D#","E#","F#","G#","A#","B#")
Cb <- c("C-","D-","E-","F-","G-","A-","B-")
Gb <- c("G-","A-","B-","C-","D-","E-","F")
Db <- c("D-","E-","F","G-","A-","B-","C")
Ab <- c("A-","B-","C","D-","E-","F","G")
Eb <- c("E-","F","G","A-","B-","C","D")
Bb <- c("B-","C","D","E-","F","G","A")
FM <- c("F","G","A","B-","C","D","E")


# minor scales - natrual
a <- c("A","B","C","D","E","F","G")
e <- c("E","F#","G","A","B","C","D")
b <- c("B","C#","D","E","F#","G","A")
fs <- c("F#","G#","A","B","C#","D","E")
cs <- c("C#","D#","E","F#","G#","A","B")
gs <- c("G#","A#","B","C","D#","E","F#")
ds <- c("D#","E#","F#","G#","A#","B","C#")
as <- c("A#","B#","C#","D#","E#","F#","G#")
ab <- c("A-","B-","C-","D-","E-","F-","G-")
eb <- c("E-","F","G-","A-","B-","C-","D-")
bb <- c("B-","C","D-","E-","F","G-","A-")
f <- c("F","G","A-","B-","C","D-","E-")
c <- c("C","D","E-","F","G","A-","B-")
g <- c("G","A","B-","C","D","E-","F")
d <- c("D","E","F","G","A","B-","C")


