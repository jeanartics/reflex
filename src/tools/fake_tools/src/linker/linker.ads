--* FAKE * binder : copy source to dest if option -o
package Linker is
	procedure Link;
	
	
   -- command line arguments keys
   Arg_Obj   : constant String := "-o";
end Linker;
