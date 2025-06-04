// -*-Faust-*-

declare name "tiltEQ";
declare version "1.0";
declare author "Klaus Scheuermann";
declare license "GPLv3";

import("stdfaust.lib");

Nch = 2; // number of channels

process = si.bus(Nch) : tilt_eq : si.bus(Nch);


// TILT EQ STEREO
  tilt_eq = par(i,Nch,_) : par(i,2, fi.lowshelf(N, -gain, freq) : fi.highshelf(N, gain, freq)) with{
    N = 1;
    gain = vslider("[1]eq tilt gain [unit:dB] [symbol:eq_tilt_gain]",0,-6,6,0.5):si.smoo;
    freq = 630; //vslider("v:master_me/t:expert/h:[3]eq/h:[2]tilt eq/[2]eq tilt freq [unit:Hz] [scale:log] [symbol:eq_tilt_freq]", 630, 200, 2000,1);
  };