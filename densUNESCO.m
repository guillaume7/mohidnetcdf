function x = densUNESCO(S,T,p,correc); 
%function x = densUNESCO(S,T,p,correc); 
%Density with pressure correction from the UNESCO equation of state
%S - salinity (psu)
%T - temperature (ºC)
%p - pressure (depth in (m))
%correc - boolean true or false
%teste

   T2 = T .* T;
   T3 = T.*T2;
   T4 = T2.*T2;
   T5 = T.*T4;
   S2 = S.*S;
   S3 = S.*S2;
   S15= sqrt(S3);

   x=999.842594+6.793952e-02*T-9.09529e-03*T2+1.001685e-04*T3;
   x=x-1.120083e-06*T4+6.536332e-09*T5;
   x=x+S .* (0.824493-4.0899e-03*T+7.6438e-05*T2-8.2467e-07*T3);
   x=x+ 5.3875e-09*T4 .* S;
   x=x+sqrt(S3) .* (-5.72466e-03 + 1.0227e-04*T - 1.6546e-06*T2);
   x=x+4.8314e-04*S2;

   if correc == true
     p2=p .* p;
         K= 19652.21 + 148.4206*T - 2.327105*T2 + 1.360477E-2*T3 - 5.155288E-5*T4;
         K1= 3.239908*p + 1.43713E-3*T.*p +  1.16092E-4*T2.*p - 5.77905E-7*T3.*p;
         K2= 8.50935E-5*p2 - 6.12293E-6*T.*p2 + 5.2787E-8*T2.*p2 + 54.6746*S - 0.603459*T.*S;
         K3= 1.09987E-2*T2.*S - 6.1670E-5*T3.*S + 7.944E-2*S15 + 1.6483E-2*T.*S15;
         K4= -5.3009E-4*T2 .* S15 + 2.2838E-3*p .* S - 1.0981E-5*T .*p .*S - 1.6078E-6*T2 .* p .* S;
         K5= 1.91075E-4*p .* S15 - 9.9348E-7*p2 .* S + 2.0816E-8*T .* p2 .* S + 9.1697E-10*T2 .* p2 .*S;
         K = K + K1 + K2 + K3 + K4 + K5;
     x = x ./ (1. - p ./ K);
   else
	 K= 19652.21 + 148.4206*T - 2.327105*T2 + 1.360477E-2*T3 - 5.155288E-5*T4;
         K1= 0;
         K2=  54.6746*S - 0.603459*T.*S;
         K3= 1.09987E-2*T2.*S - 6.1670E-5*T3.*S + 7.944E-2*S15 + 1.6483E-2*T.*S15;
         K4= -5.3009E-4*T2 .* S15;
         K5= 0;
         K = K + K1 + K2 + K3 + K4 + K5;

   end