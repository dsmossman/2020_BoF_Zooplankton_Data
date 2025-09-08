
%% associated with opc2; switch case ('1')%%%%
%%%to output the bug size into 64 difference bin size categories%%%;
%% take the sqaure root of the digital size and round it up to the nearest integer

bin=sqrt(value);
bin=round(bin);
switch (bin)
 case(1)
     bin1=bin1+1;
 case(2)
     bin2=bin2+1;
 case(3)
     bin3=bin3+1;
 case(4)
     bin4=bin4+1;
 case(5)
     bin5=bin5+1;
 case(6)
     bin6=bin6+1;
 case(7)
     bin7=bin7+1;
 case(8)
     bin8=bin8+1;
 case(9)
     bin9=bin9+1;
 case(10)
     bin10=bin10+1;
  case(11)
     bin11=bin11+1;
 case(12)
     bin12=bin12+1;
 case(13)
     bin13=bin13+1;
 case(14)
     bin14=bin14+1;
 case(15)
     bin15=bin15+1;
 case(16)
     bin16=bin16+1;
 case(17)
     bin17=bin17+1;
 case(18)
     bin18=bin18+1;
 case (19)
     bin19=bin19+1;
 case(20)
     bin20=bin20+1;
  case(21)
     bin21=bin21+1;
 case(22)
     bin22=bin22+1;
 case(23)
     bin23=bin23+1;
 case(24)
     bin24=bin24+1;
 case(25)
     bin25=bin25+1;
 case(26)
     bin26=bin26+1;
 case(27)
     bin27=bin27+1;
 case(28)
     bin28=bin28+1;
 case(29)
     bin29=bin29+1;
 case(30)
     bin30=bin30+1;
  case(31)
     bin31=bin31+1;
 case(32)
     bin32=bin32+1;
 case(33)
     bin33=bin33+1;
 case(34)
     bin34=bin34+1;
 case(35)
     bin35=bin35+1;
 case(36)
     bin36=bin36+1;
 case(37)
     bin37=bin37+1;
 case(38)
     bin38=bin38+1;
 case(39)
     bin39=bin39+1;
 case(40)
     bin40=bin40+1;
  case(41)
     bin41=bin41+1;
 case(42)
     bin42=bin42+1;
 case(43)
     bin43=bin43+1;
 case(44)
     bin44=bin44+1;
 case(45)
     bin45=bin45+1;
 case(46)
     bin46=bin46+1;
 case(47)
     bin47=bin47+1;
 case(48)
     bin48=bin48+1;
 case(49)
     bin49=bin49+1;
 case(50)
     bin50=bin50+1;
  case(51)
     bin51=bin51+1;
 case(52)
     bin52=bin52+1;
 case(53)
     bin53=bin53+1;
 case(54)
     bin54=bin54+1;
 case(55)
     bin55=bin55+1;
 case(56)
     bin56=bin56+1;
 case(57)
     bin57=bin57+1;
 case(58)
     bin58=bin58+1;
 case(59)
     bin59=bin59+1;
 case(60)
     bin60=bin60+1;
  case(61)
     bin61=bin61+1;
 case(62)
     bin62=bin62+1;
 case(63)
     bin63=bin63+1;
 case(64)
     bin64=bin64+1;
 otherwise
     fprintf(fp_out,'%u\t',linecount);
      fprintf(fp_out,'bin size error ');
     fprintf(fp_out,'%u\t',bin);
      fprintf(fp_out,'\n') ;
 end
     
     
  
     
 