
module traffic_lights;
reg clock, red, amber, green;

parameter on = 1, off = 0, red_tics = 350,
          amber_tics = 30, green_tics = 200;

// initialize colors
   
initial  red = off;
initial  amber = off;
initial  green = off;

always begin : rga           // sequence to control the lights
   red = on;                 // turn red light on 
   light(red, red_tics);     // and wait. 
   green = on;               // turn green light on 
   light(green, green_tics); // and wait. 
   amber = on;               // turn amber light on 
   light(amber, amber_tics); // and wait.
end

// task to wait for 'tics' positive edge clocks 
// before turning 'color' light off
task light;
output color;
input  [31:0] tics;
begin
   repeat (tics) @ (posedge clock);
   color = off; // turn light off
end
endtask

always begin // waveform for the clock
   #100 clock = 0;
   #100 clock = 1;
end

initial begin
   #10000;
   $display("Test Complete");   
   $finish;   
end
   
endmodule // traffic_lights

