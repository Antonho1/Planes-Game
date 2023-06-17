000001 IDENTIFICATION DIVISION.
000002 PROGRAM-ID. Avioanele.
000003 ENVIRONMENT DIVISION.
000004 CONFIGURATION SECTION.
000005 DATA DIVISION.
       Working-Storage Section.
       01  qwe pic xx.
       01  rty pic xx.
       01  dir-mort pic 9.
       01  tura pic 9.
       01  numarmeu pic 99 value 0.
       01  numarmadv pic 99 value 0.
       01  raspuns pic xx.
       01  lovitura pic xxx.
       01  ok pic 99 value 0.
       01  xc pic x.
       01  yc pic xx.
       01  xc-nr pic 99.
       01  alf pic x(11) value " ABCDEFGHIJ".
       01  numere pic x(11) value " 12345678910".
       01  aux pic 99.
       01  i pic 99 value 1.
       01  j pic 99 value 1.
       01  lungime-row pic 99.
       01  lungime-col pic 99.
       01  c pic 9 value 0.
       01  Matrix.
         02 Row occurs 11 times.
           03 Coll occurs 11 times pic xx.
           
       01  Matrix2.
         02 Row2 occurs 11 times.
           03 Coll2 occurs 11 times pic xx.
       
       01  Avion
         02  cap PIC xxx value "B3".
         02  dir PIC 9 value 2.
         02  ebun pic 9 value 0.
         
       01  A1 pic xxxxx.
       01  A2 pic xxxxx.
       01  A3 pic xxxxx.
            
           
       
       
000006 PROCEDURE DIVISION.
           
           perform main.
           
           STOP RUN.
           
           
       main.
      
           perform ijinitiere.
      
           perform lungime.
          
           perform resetgrafic until ok=30.
           
           perform graficshow.
           
           Display "R - Pune-le in alte pozitii".
           Display "S - Start"
           
           perform ijinitiere.
           move 0 to ok.
           perform raspunsuri until ok>=1
           
           if ok=1 then
           perform main;
           else
           perform staart;
           end-if.
     
           
       staart.
       
           Display"Cine incepe? 1-Eu, 2-Adversarul.".
           Accept tura FROM CONSOLE.
           
           if tura = 1 then;
           perform eulovesc;
           end-if.
           
           if tura = 2 then;
           perform adversarloveste;
           else 
           Display "Nu e o optiune valida. Mai incearca o data.";
           Display" ";
           perform staart;
           end-if.
           
       eulovesc.
       
           Display"Unde vrei sa lovesti? Exemplu: A8".
           Accept lovitura FROM CONSOLE.
           move FUNCTION UPPER-CASE(lovitura) to lovitura.
           perform ijinitiere.
           
           move lovitura(1:1) to xc.
       		 move lovitura(2:2) to yc.
           
           perform forconversie until i>=12.
           
           move FUNCTION NUMVAL(xc-nr) to i.
           move FUNCTION NUMVAL(yc) to j.
           add 1 to j.
           
           perform spuneadv until lovitura ="l"or"m"or"n"or"N"OR"L"OR"M".
           
           if lovitura = "N" or "n" then; 
           move "*" to Coll2(i,j);
           end-if.
           
           if lovitura = "L" or "l" then;
           move "x" to Coll2(i,j);
           end-if.
           
           if lovitura = "M" or "m" then;
           Display"Si directie este?(1-jos, 2-stanga, 3-sus,4-dreapta)";
           Accept dir-mort FROM CONSOLE;
           perform addavionmort-adv;
           end-if.
           
           
           perform cineacastigat.
       
       spuneadv.
           
           Display"Ce a spus adversarul?(N-Nimic, L-Lovit, M-Mort)".
           Accept lovitura FROM CONSOLE.
       
       adversarloveste.
       
           Display"Unde a lovit adversarul? Exemplu: A8".
           Accept lovitura FROM CONSOLE.
           move FUNCTION UPPER-CASE(lovitura) to lovitura.
           Display" ".
           Display" ".
           perform ijinitiere.
           
           move lovitura(1:1) to xc.
       		 move lovitura(2:2) to yc.
           
           perform forconversie until i>=12.
           
           move FUNCTION NUMVAL(xc-nr) to i.
           move FUNCTION NUMVAL(yc) to j.
           add 1 to j.
           
           if Coll(i,j) = " " then;
           Display"Spune-i adversarului: NIMIC";
           move "*" to Coll(i,j);
           end-if.
           
           if Coll(i,j) = "^" or ">" or "v" or "<" then;
           Display"Spune-i adversarului: LOVIT";
           move "x" to Coll(i,j);
           end-if.
           
           if Coll(i,j) = "O" then;
             
             if A1(2:2) =  i and A1(4:2) = j then;
             move A1(1:1) to c;
             Display"Spune-i adversarului: MORT si directia este " c;
             end-if;
             if A2(2:2) =  i and A2(4:2) = j then;
             move A2(1:1) to c;
             Display"Spune-i adversarului: MORT si directia este " c;
             end-if;
             if A3(2:2) =  i and A3(4:2) = j then;
             move A3(1:1) to c;
             Display"Spune-i adversarului: MORT si directia este " c;
             end-if;
             perform addavionmort-eu;
           end-if.
           
           perform cineacastigat.
           
       cineacastigat.
       
           perform ijinitiere.
           Display" ".
           
           perform graficshow. 
           
           if numarmeu=3 then;
           Display "Ai pierdut";
           Stop Run;
           end-if.
           if numarmadv=3 then;
           Display "Ai castigat";
           Stop Run;
           end-if.
           
           if tura = 1 then;
           move 2 to tura;
           perform adversarloveste;
           else
           move 1 to tura;
           perform eulovesc;
           end-if.
           
       raspunsuri.
            
           ACCEPT raspuns FROM CONSOLE.
           
           if raspuns = "r" or "R" then;
           move 1 to ok;
           else
             if raspuns = "s" or "S" then;
             move 2 to ok;
             else
             move 0 to ok;
             Display " Nu e o optiune valida";
             end-if;
           end-if.
           
       resetgrafic.
       
           Display " ".
           DISPLAY "Adauga cele 3 avioane:".
           Display " ".
           move 0 to ok.
           
           perform ijinitiere.
           
           perform foriaddspatii until i>= lungime-row.
       
           perform ijinitiere.
           
           perform foriaddlitere until i>= lungime-row.
          
           perform ijinitiere.
           
           perform foriaddnr until i>= lungime-row.
           
           perform ijinitiere.
           
           perform forishow until i>= lungime-row.
           DISPLAY "----------------------".
           Display " ".
           
           Display" Asa arata un avion cu directia 1:".
           Display"              O".
           Display"          ^ ^ ^ ^ ^".
           Display"              ^".
           Display"            ^ ^ ^".
           
           perform ijinitiere.
           move 0 to ebun.
           
           perform verificavionu until ebun=1.
           
           move dir to A1(1:1).
           move qwe to A1(2:2).
           move rty to A1(4:2).
           
           perform ijinitiere.
           move 0 to ebun.
           
           perform verificavionu until ebun=1.
           
           move dir to A2(1:1).
           move qwe to A2(2:2).
           move rty to A2(4:2).
           
           perform ijinitiere.
           move 0 to ebun.
           
           perform verificavionu until ebun=1.
           
           move dir to A3(1:1).
           move qwe to A3(2:2).
           move rty to A3(4:2).
           
           move 2 to i.
           
           perform forinravioane until i>=lungime-row.
           if ok <30 then;
           Display "Nu ai pus bine avioanele, mai incearca o data:"
           end-if.
           
       verificavionu.
       		 
       		 DISPLAY "Adauga capul:".
       		 ACCEPT cap FROM CONSOLE.
       		 move FUNCTION UPPER-CASE(cap) to cap.
       		 Display"Cu directia 1-jos,2-stanga,3-sus,4-dreapta".
       		 
       		 ACCEPT dir FROM CONSOLE.
       		 
       		 move 1 to ebun;
       		 
       		 if dir > 4 then
       		 move 0 to ebun;
       		 end-if.
       		 
       		 if dir = 0 then
       		 move 0 to ebun;
       		 end-if.
       		 
           if dir = 1 and (cap(2:2)<3 or cap(2:2)>8) then;
           move 0 to ebun;
           end-if.
           
           if dir = 2 and cap(2:2)<4 then;
           move 0 to ebun;
           end-if.
           
           
           if dir = 3 and (cap(2:2)<3 or cap(2:2)>8) then;
           move 0 to ebun;
           end-if.
           
           if dir = 4 and cap(2:2)>7
           move 0 to ebun;
           end-if.
           
           if dir = 1 and cap(1:1)>"G"
           move 0 to ebun;
           end-if.
           
           if dir = 2 and (cap(1:1)<"C" or cap(1:1)>"H")
           move 0 to ebun;
           end-if.
           
           if dir = 3 and cap(1:1)<"D"
           move 0 to ebun;
           end-if.
           
           if dir = 4 and (cap(1:1)<"C" or cap(1:1)>"H")
           move 0 to ebun;
           end-if.
           
           if dir = 2 and cap(2:2)="10" then;
           move 1 to ebun;
           end-if.
           
           if ebun = 0 then;
           Display "Nu e bine";
           else
           perform addavion;
           end-if.
           
       addavionmort-adv.
           
           add 1 to numarmadv.
       
           if dir-mort=1 then;                                                  
            move "^" to Coll2(i,j);
            add 1 to i;
            move "x" to Coll2(i,j); 
            add 1 to i;
            move "x" to Coll2(i,j); 
            add 1 to i;
            move "x" to Coll2(i,j); 
            subtract 2 from i;
            add 2 to j;
            move "x" to Coll2(i,j); 
            subtract 1 from j;
            move "x" to Coll2(i,j); 
            subtract 2 from j;
            move "x" to Coll2(i,j); 
            subtract 1 from j;
            move "x" to Coll2(i,j); 
            add 2 to i;
            add 1 to j;
            move "x" to Coll2(i,j); 
            add 2 to j;
            move "x" to Coll2(i,j); 
           end-if.
           
           if dir-mort=3 then;                                                  
            move "v" to Coll2(i,j);
            subtract 1 from i;
            move "x" to Coll2(i,j); 
            subtract 1 from i;
            move "x" to Coll2(i,j); 
            subtract 1 from i;
            move "x" to Coll2(i,j); 
            add 2 to i;
            subtract 2 from j;
            move "x" to Coll2(i,j); 
            add 1 to j;
            move "x" to Coll2(i,j); 
            add 2 to j;
            move "x" to Coll2(i,j); 
            add 1 to j;
            move "x" to Coll2(i,j); 
            subtract 2 from i;
            subtract 1 from j;
            move "x" to Coll2(i,j); 
            subtract 2 from j;
            move "x" to Coll2(i,j); 
           end-if.
           
           if dir-mort=2 then;                                                  
            move ">" to Coll2(i,j);
            subtract 1 from j;
            move "x" to Coll2(i,j); 
            subtract 1 from j;
            move "x" to Coll2(i,j); 
            subtract 1 from j;
            move "x" to Coll2(i,j); 
            add 2 to j;
            add 2 to i;
            move "x" to Coll2(i,j); 
            subtract 1 from i;
            move "x" to Coll2(i,j); 
            subtract 2 from i;
            move "x" to Coll2(i,j); 
            subtract 1 from i;
            move "x" to Coll2(i,j); 
            subtract 2 from j;
            add 1 to i;
            move "x" to Coll2(i,j); 
            add 2 to i;
            move "x" to Coll2(i,j); 
           end-if.
           
           if dir-mort=4 then;                                                  
            move "<" to Coll2(i,j);
            add 1 to j;
            move "x" to Coll2(i,j); 
            add 1 to j;
            move "x" to Coll2(i,j); 
            add 1 to j;
            move "x" to Coll2(i,j); 
            subtract 2 from j;
            add 2 to i;
            move "x" to Coll2(i,j); 
            subtract 1 from i;
            move "x" to Coll2(i,j); 
            subtract 2 from i;
            move "x" to Coll2(i,j); 
            subtract 1 from i;
            move "x" to Coll2(i,j); 
            add 2 to j;
            add 1 to i;
            move "x" to Coll2(i,j); 
            add 2 to i;
            move "x" to Coll2(i,j); 
           end-if.  
           
       addavionmort-eu.
           
           add 1 to numarmeu.
           move c to dir-mort.
           
           if dir-mort=1 then;                                                  
            move "x" to Coll(i,j);
            add 1 to i;
            move "x" to Coll(i,j); 
            add 1 to i;
            move "x" to Coll(i,j); 
            add 1 to i;
            move "x" to Coll(i,j); 
            subtract 2 from i;
            add 2 to j;
            move "x" to Coll(i,j); 
            subtract 1 from j;
            move "x" to Coll(i,j); 
            subtract 2 from j;
            move "x" to Coll(i,j); 
            subtract 1 from j;
            move "x" to Coll(i,j); 
            add 2 to i;
            add 1 to j;
            move "x" to Coll(i,j); 
            add 2 to j;
            move "x" to Coll(i,j); 
           end-if.
           
           if dir-mort=3 then;                                                  
            move "x" to Coll(i,j);
            subtract 1 from i;
            move "x" to Coll(i,j); 
            subtract 1 from i;
            move "x" to Coll(i,j); 
            subtract 1 from i;
            move "x" to Coll(i,j); 
            add 2 to i;
            subtract 2 from j;
            move "x" to Coll(i,j); 
            add 1 to j;
            move "x" to Coll(i,j); 
            add 2 to j;
            move "x" to Coll(i,j); 
            add 1 to j;
            move "x" to Coll(i,j); 
            subtract 2 from i;
            subtract 1 from j;
            move "x" to Coll(i,j); 
            subtract 2 from j;
            move "x" to Coll(i,j); 
           end-if.
           
           if dir-mort=2 then;                                                  
            move "x" to Coll(i,j);
            subtract 1 from j;
            move "x" to Coll(i,j); 
            subtract 1 from j;
            move "x" to Coll(i,j); 
            subtract 1 from j;
            move "x" to Coll(i,j); 
            add 2 to j;
            add 2 to i;
            move "x" to Coll(i,j); 
            subtract 1 from i;
            move "x" to Coll(i,j); 
            subtract 2 from i;
            move "x" to Coll(i,j); 
            subtract 1 from i;
            move "x" to Coll(i,j); 
            subtract 2 from j;
            add 1 to i;
            move "x" to Coll(i,j); 
            add 2 to i;
            move "x" to Coll(i,j); 
           end-if.
           
           if dir-mort=4 then;                                                  
            move "x" to Coll(i,j);
            add 1 to j;
            move "x" to Coll(i,j); 
            add 1 to j;
            move "x" to Coll(i,j); 
            add 1 to j;
            move "x" to Coll(i,j); 
            subtract 2 from j;
            add 2 to i;
            move "x" to Coll(i,j); 
            subtract 1 from i;
            move "x" to Coll(i,j); 
            subtract 2 from i;
            move "x" to Coll(i,j); 
            subtract 1 from i;
            move "x" to Coll(i,j); 
            add 2 to j;
            add 1 to i;
            move "x" to Coll(i,j); 
            add 2 to i;
            move "x" to Coll(i,j); 
           end-if.      
           
       addavion.
           
           move cap(1:1) to xc.
       		 move cap(2:2) to yc.
           
           perform forconversie until i>=12.
           
           move FUNCTION NUMVAL(xc-nr) to i.
           move FUNCTION NUMVAL(yc) to j.
           add 1 to j.
           
           move i to qwe.
           move j to rty.
           
           if dir=1 then;                                                  
            move "O" to Coll(i,j);
            add 1 to i;
            move "^" to Coll(i,j); 
            add 1 to i;
            move "^" to Coll(i,j); 
            add 1 to i;
            move "^" to Coll(i,j); 
            subtract 2 from i;
            add 2 to j;
            move "^" to Coll(i,j); 
            subtract 1 from j;
            move "^" to Coll(i,j); 
            subtract 2 from j;
            move "^" to Coll(i,j); 
            subtract 1 from j;
            move "^" to Coll(i,j); 
            add 2 to i;
            add 1 to j;
            move "^" to Coll(i,j); 
            add 2 to j;
            move "^" to Coll(i,j); 
           end-if.
           
           if dir=3 then;                                                  
            move "O" to Coll(i,j);
            subtract 1 from i;
            move "v" to Coll(i,j); 
            subtract 1 from i;
            move "v" to Coll(i,j); 
            subtract 1 from i;
            move "v" to Coll(i,j); 
            add 2 to i;
            subtract 2 from j;
            move "v" to Coll(i,j); 
            add 1 to j;
            move "v" to Coll(i,j); 
            add 2 to j;
            move "v" to Coll(i,j); 
            add 1 to j;
            move "v" to Coll(i,j); 
            subtract 2 from i;
            subtract 1 from j;
            move "v" to Coll(i,j); 
            subtract 2 from j;
            move "v" to Coll(i,j); 
           end-if.
           
           if dir=2 then;                                                  
            move "O" to Coll(i,j);
            subtract 1 from j;
            move ">" to Coll(i,j); 
            subtract 1 from j;
            move ">" to Coll(i,j); 
            subtract 1 from j;
            move ">" to Coll(i,j); 
            add 2 to j;
            add 2 to i;
            move ">" to Coll(i,j); 
            subtract 1 from i;
            move ">" to Coll(i,j); 
            subtract 2 from i;
            move ">" to Coll(i,j); 
            subtract 1 from i;
            move ">" to Coll(i,j); 
            subtract 2 from j;
            add 1 to i;
            move ">" to Coll(i,j); 
            add 2 to i;
            move ">" to Coll(i,j); 
           end-if.
           
           if dir=4 then;                                                  
            move "O" to Coll(i,j);
            add 1 to j;
            move "<" to Coll(i,j); 
            add 1 to j;
            move "<" to Coll(i,j); 
            add 1 to j;
            move "<" to Coll(i,j); 
            subtract 2 from j;
            add 2 to i;
            move "<" to Coll(i,j); 
            subtract 1 from i;
            move "<" to Coll(i,j); 
            subtract 2 from i;
            move "<" to Coll(i,j); 
            subtract 1 from i;
            move "<" to Coll(i,j); 
            add 2 to j;
            add 1 to i;
            move "<" to Coll(i,j); 
            add 2 to i;
            move "<" to Coll(i,j); 
           end-if.

           perform ijinitiere.
           
           perform forishow until i>= lungime-row.
           DISPLAY "----------------------".
           Display " ".
           
       forconversie.
       
           if xc=alf(i:1) then;
           move i to xc-nr;
           end-if
           
           add 1 to i.
           
       lungime.
       
           compute lungime-row=
           function length(Matrix)/function length (Row).
           
           compute lungime-col=
           function length(Row)/function length (Coll).
           
           ADD 1 TO lungime-row.
           ADD 1 TO lungime-col.
           
       ijinitiere.
       
           move 1 to i.
           move 1 to j.
           
       foriaddlitere.
           
           
           move alf(i:1) to Coll (i,1).
           move alf(i:1) to Coll2 (i,1).
       		 add 1 to i.
       		 
       		 
       foriaddnr.
       
       		 if i<11 then;
       		 move numere(i:1) to Coll (1,i);
       		 move numere(i:1) to Coll2 (1,i);
       		 else
       		 move "10" to Coll (1,11);
       		 move "10" to Coll2 (1,11);
           end-if.
           add 1 to i.
           
           
       foriaddspatii.    
           
           move 1 to j.
           
           perform forjaddspatii until j>= lungime-col.
           
           add 1 to i.
           
       forjaddspatii. 
       
           move "   " to Coll(i,j).
       
           add 1 to j.
           
           
       forishow.
       
	     		 Display Row(i) " ".
           add 1 to i.
           
       forjshow.
       
	         Display Coll(i,j).
	         add 1 to j.
	         
	     forishow2.
	     
	     		 Display Row(i) "     | " Row2(i) " ".
           add 1 to i.
           
       forjshow2.
       
	         Display Coll2(i,j).
	         add 1 to j.

       forinravioane.
           
           move 2 to j.
           
           perform forjnravioane until j>= lungime-col.
           
           add 1 to i.
           
       forjnravioane. 
       
           if Coll(i,j) not = " " then
           add 1 to ok;
           end-if.
       
           add 1 to j.

       graficshow.
       
           perform ijinitiere.
           
           Display "           EU              |        Adversar".
           Display"----------------------------------------------------------".
           perform forishow2 until i>= lungime-row.
           DISPLAY"----------------------------------------------------------".
           Display " ".

