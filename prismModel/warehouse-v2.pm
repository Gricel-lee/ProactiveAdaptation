dtmc
//constants
const double t1;
const double t2;
const double t3;
const double t1_F;//=90;
const double t2_F;//=80;
const double t_r;//=70;
const double p1;
const double p2;
const double p3;
const double p4;
const double p_retry;//=0.7;
//labels
label "start" = s=0;
label "search" = s=0;
label "pickup" = s=1;
label "found" = s=1;
label "picked" = s=2;
label "bin" = s=2;
label "timeout" = s=3;
label "success" = s=4;
label "binned" = s=4;
label "find_fail" = s=10;
label "reposition" = s=6;
label "pickup_fail" = s=9;
label "binning_fail" =s=8;

module robot_process

// local state
s : [0..10] init 0;

[] s=0 -> p1 : (s'= 1) + (1-p1): (s'= 5);
[] s=1 -> p2 : (s'= 2) + p3 : (s'= 3)+ 1-p2-p3:(s'=7);
[a] s=2 -> p4 : (s'= 4) + (1-p4) : (s'= 8);
[] s=3 -> p_retry: (s'=6 ) + (1-p_retry):(s'= 9);
[] s=4 -> (s'= 4);
[] s=5 -> (s'= 10);
[] s=10 -> (s'= 10);
[] s=6 -> (s'= 1);
[] s=7 -> (s'= 9);
[] s=8 -> (s'= 8);
[] s=9 -> (s'= 9);

endmodule

rewards "total_time"
s=1:t1;
s=2:t2;
s=3:t2_F;
s=4:t3;
s=5:t1_F;
s=6:t_r;
s=7:t2;
[a] true:t3;
endrewards
