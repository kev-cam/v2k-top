
module tryfact; // define function
   
   function [31:0] factorial;
      input  [3:0] operand;
      reg    [3:0] i;
      begin
	 factorial = 1;
         for (i = 2; i <= operand; i = i + 1)
	   factorial = i * factorial;
      end
   endfunction // factorial
   
   integer  result;
   integer  n;
   initial begin
      for (n = 0; n <= 7; n = n+1) begin
	 result = factorial(n);
         $display ("%0d factorial=%0d", n, result);
      end
   end

endmodule
