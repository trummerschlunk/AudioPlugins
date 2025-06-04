// -*-Faust-*-

declare name "sideEQ";
declare version "1.0";
declare author "Klaus Scheuermann";
declare license "GPLv3"; 
 
import("stdfaust.lib");
 
process = si.bus(2) : side_eq_b : si.bus(2);
 
// stereo bypass with si.smoo fading
bp2(sw,pr) =  _,_ <: _,_,pr : (_*sm,_*sm),(_*(1-sm),_*(1-sm)) :> _,_ with {
    sm = sw : si.smoo;
};

// stereo to m/s encoder
ms_enc = _*0.5,_*0.5 <: +, -;

// m/s to stereo decoder
ms_dec = _,_ <: +, -;


// SIDE EQ
side_eq_b =  ms_enc : _,band_shelf(freq_low,freq_high,eq_side_gain) : ms_dec with{

    //band_shelf(freq1 ,freq2 ,gain) = fi.low_shelf(0-gain,freq1): fi.low_shelf(gain,freq2);
    band_shelf(freq1 ,freq2 ,gain) = fi.svf.ls(freq1,0.7,0-gain): fi.svf.ls(freq2,0.7,gain);

    freq_low = eq_side_freq - eq_side_freq*eq_side_width : max(50);
    freq_high = eq_side_freq + eq_side_freq*eq_side_width : min(8000);

    eq_side_gain = vslider("h:[3]side eq/[1]eq side gain [unit:dB] [symbol:eq_side_gain]",0,0,12,0.5):si.smoo;
    eq_side_freq = vslider("h:[3]side eq/[2]eq side freq [unit:Hz] [scale:log] [symbol:eq_side_freq]", 600,200,5000,1);
    eq_side_width = vslider("h:[3]side eq/[3]eq side bandwidth [symbol:eq_side_bandwidth]", 1,0.5,4,0.5);

};