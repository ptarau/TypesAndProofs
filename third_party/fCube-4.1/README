 fCube: an efficient prover for Intuitionistic propositional Logic

    Copyright (C) 2012  Mauro Ferrari,          email:  mauro.ferrari@uninsubria.it
                        Camillo Fiorentini      email:  fiorenti@dsi.unimi.it
                        Guido Fiorino           email: guido.fiorino@unimib.it

    This program is free software: you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation, either version 3 of the License, or
    (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this program.  If not, see <http://www.gnu.org/licenses/>.
                

        
                        INSTRUCTIONS
        Syntax:
        implication:    im(,);
        negation:       non();
        equivalence:    equiv(,);
        conjuncton:     and(,);
        disjunction:    or(,).
	        
        Example: ((a&b)->~c) is im(and(a,b),non(c)) 
        
        To decide a formula use the predicate 'decide'. 
        
        For example 'decide(im(a,non(a))).';
        



This version requires swi-prolog.
Latest version of SWI-Prolog used to develop fCube: SWI-Prolog (Multi-threaded, 64 bits, Version 6.2.6)
fCube-4 was developed on an Apple MacBook Pro.

To decide the formula the file 'filename.txt' the easiest way is to use 'fCube.bash':
   	    	    	 
			 fCube.bash filename.txt

To decide the formulas of the ILTP library run the bash script 'test.bash' from the root of fCube tree
or the command 

			swipl 

To decide large formulas it can be necessary to reserve more space for the global stack.
Use the option -G (for more information on the stack options see SWI-Prolog manual). 

As an example 
	
		swipl -G1g fCube < filename.txt

Use fCube-verbose to get more output. For the formulas which are unprovable,
fCube-verbose outputs a tree in a depth-first style, 
which information that rapresents the Kripke model.  
Siblings are aligned at the same column. In every world only the new forcing is printed. 
Example, 

		swipl -s fCube-verbose < ../ILTP/SYJ208+1.002.pitp

produces:

*** The Counter Model (see also the prolog term ) ***
-- root --
-- end world -- 
      F p3;
      F p5;
      T p0;
      FC p4;
      FC p2;
      FC p1;
-- end world -- 
         T p5;
-- end world -- 
            T p3;
-- end world --
 

which describe a the root, where nothing is forced. 
The root has one immediate successor that forces p0. 
Here we have also the information that p1, p2 and p4 will be not forced (read Fc A as T ~A).
Above this we have another world that extends the forcing of the previous world by forcing p5. Thus in 
this world p0 and p5 are forced.
Finally above this world we have another world that forces p0, p5 and also p3.
Note that in every branch the forcing of a  formula is explicitly showed only once. 


Another example:
	
		swipl -s fCube-verbose < ../ILTP/SYJ207+1.002.pitp

gives the following output:

*** The Counter Model (see also the prolog term ) ***
-- root --
-- end world -- 
      F p;
      F p4;
-- end world -- 
         T p4;
         T p3;
         F p2;
-- end world -- 
         T p3;
         F p2;
-- end world --

The root does not force any variable and has one immediate successor that does not force any variable.
This world has two immediate successors: one forces p4 and p3, the other p3.


As regards the proof, nested conjunctions or disjunctions are handled in a row as in this example:

      T (((p0|p1)&(p2|p3))&(p4|p5));
      F ((p0&p2)|((p0&p4)|((p2&p4)|((p1&p3)|((p1&p5)|(p3&p5))))));
 
Main SWFF: T (((p0|p1)&(p2|p3))&(p4|p5))

         T (p0|p1);
         T (p2|p3);
         T (p4|p5);
         F ((p0&p2)|((p0&p4)|((p2&p4)|((p1&p3)|((p1&p5)|(p3&p5))))));
 
Main SWFF: F ((p0&p2)|((p0&p4)|((p2&p4)|((p1&p3)|((p1&p5)|(p3&p5))))))

            F (p0&p2);
            F (p0&p4);
            F (p2&p4);
            F (p1&p3);
            F (p1&p5);
            F (p3&p5);
            T (p0|p1);
            T (p2|p3);
            T (p4|p5);



The structure of the directory is the following:
fCube/		contains 'fCube.bash' and the prolog code 'fCube' and 'fCube-verbose'. 
		'fCube-verbose' outputs the proof and the countermodel;

ILTP/   	contains the formulas of the ILTP library in the format suitable for fCube. 
        	As an example type:  
		      swipl -s fCube/fCube < ILTP/SYJ207+1.002.pitp 
		to decide the formula  SYJ207+1.002 of  ILTP library;

otherWffs/ 	contains some formulas characterizing intermediate formulas;
script/		some utilities. In particular test.bash is useful to run fCube on the formulas of ILTP library
paper/		some slides presented at Lpar'10 and Cilc'09  
