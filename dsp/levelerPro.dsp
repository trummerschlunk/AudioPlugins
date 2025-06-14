// -*-Faust-*-

declare name "levelerPro";
declare version "1.0";
declare author "Klaus Scheuermann";
declare license "GPLv3";

ds = library("dynamicsmoothing.lib");
ebu = library("ebur128.lib");

import("stdfaust.lib");

Nch = 2;
maxSR = 96000;

init_leveler_target = -16;
init_leveler_maxboost = 30;
init_leveler_maxcut = 30;
init_leveler_brake_threshold = -22;
init_leveler_speed = 80;


process = si.bus(Nch) : pregain(Nch) : leveler : si.bus(Nch);

preGainSlider = vslider("h:[2]Leveler Controls/[0][unit:dB]PreGain", 0, -20, 20, 0.1);

target = vslider("h:[2]Leveler Controls/[1][symbol:target]target", -23, -60, 0, 1);
bp = checkbox("v:LevelerPro/[3][symbol:bypass_leveler]bypass_leveler"):si.smoo;
leveler_speed = vslider("h:[2]Leveler Controls/[4][unit:%][integer]speed", init_leveler_speed, 0, 100, 1) * 0.01;
leveler_brake_thresh = target + vslider("h:[2]Leveler Controls/[5][unit:dB]brake threshold", init_leveler_brake_threshold,-90,0,1)+32;
meter_leveler_brake = _*100 : vbargraph("h:[2]Leveler Controls/[6][unit:%][integer]brake",0,100);
limit_pos = vslider("h:[2]Leveler Controls/[7][unit:dB]max boost", init_leveler_maxboost, 0, 60, 1);
limit_neg = vslider("h:[2]Leveler Controls/[8][unit:dB]max cut", init_leveler_maxcut, 0, 60, 1) : ma.neg;
leveler_meter_gain = vbargraph("h:[2]Leveler Controls/[1][unit:dB][symbol:leveler_gain]gain",-50,50);

// utility functions

pregain(n) = par(i,n,gain) with {
    gain = _ * (preGainSlider : ba.db2linear : si.smoo);
};

// LEVELER

leveler(l,r) =

  ( ((l,r):leveler_sc(target)~(_,_)
                              :(
       (_*(1-bp))
      ,(_*(1-bp))
     ))
  , (l*bp,r*bp)
  ):>(_,_);

basefreq =
  it.interpolate_linear(leveler_speed
                        :pow(
                          2 // hslider("base freq power", 2, 0.1, 10, 0.1)
                        )
                       , 0.01
                       , 0.2 // hslider("base freq fast", 0.2, 0.1, 0.3, 0.001)
                       );

sensitivity =
  it.interpolate_linear(leveler_speed
                        :pow(
                          0.5 // hslider("sens power", 0.5, 0.1, 10, 0.1)
                        )
                       , 0.00000025
                       , 0.0000025 // hslider("sens fast", 0.0000025, 0.0000025, 0.000005, 0.0000001)
                       );

lk2_fixed(Tg)= par(i,2,kfilter : zi) :> 4.342944819 * log(max(1e-12)) : -(0.691) with {
  // maximum assumed sample rate is 192k
  sump(n) = ba.slidingSump(n, Tg*maxSR)/max(n,ma.EPSILON);
  envelope(period, x) = x * x :  sump(rint(period * ma.SR));
  zi = envelope(Tg); // mean square: average power = energy/Tg = integral of squared signal / Tg

  kfilter = ebu.ebur128;
};

lk2_var(Tg)= par(i,2,kfilter : zi) :> 4.342944819 * log(max(1e-12)) : -(0.691) with {
  // maximum assumed sample rate is 192k
  sump(n) = ba.slidingSump(n, 0.4 * maxSR)/max(n,ma.EPSILON);
  envelope(period, x) = x * x :  sump(rint(period * ma.SR));
  zi = envelope(Tg); // mean square: average power = energy/Tg = integral of squared signal / Tg

  kfilter = ebu.ebur128;
};
lk2 = lk2_fixed(3);
lk2_short = lk2_fixed(3);
lufs_out_meter(l,r) = l,r <: l, attach(r, (lk2_short : vbargraph("h:[6]PostStage/[symbol:lufs_out_meter][unit:dB]lufs",meters_minimum,0))) : _,_;

lk2_time =
  // 0.4;
  hslider("lk2 time", 0.01, 0.001, 3, 0.001);
// it.interpolate_linear(leveler_speed :pow(hslider("lk2 power", 2, 0.1, 10, 0.1))
//                      ,0.4 // hslider("lk2 time", 0.4, 0.001, 3, 0.001)
//                      , 0.04):max(0);
leveler_sc(target,fl,fr,l,r) =
  calc(lk2_fixed(0.01,fl,fr))
  // (calc(lk2_var(lk2_time,fl,fr))*(1-bp)+bp)
  <: (_*l,_*r)
with {
  // lp1p(cf) = si.smooth(ba.tau2pole(1/(2*ma.PI*cf)));
  calc(lufs) = FB(lufs)~_: ba.db2linear;
  FB(lufs,prev_gain) =
    (target - lufs)
    +(prev_gain )
    : ds.dynamicSmoothing(
      sensitivity * expander(abs(fl)+abs(fr))
    ,  basefreq * expander(abs(fl)+abs(fr))
    )
    :  limit(limit_neg,limit_pos)
    : leveler_meter_gain;

  limit(lo,hi) = min(hi) : max(lo);

  leveler_speed_brake(sc) = expander(sc) * leveler_speed;

  expander(x) = (co.peak_expansion_gain_mono_db(maxHold,strength,leveler_brake_thresh,range,gate_att,hold,gate_rel,knee,prePost,x)
                 : ba.db2linear
                 :max(0)
                 :min(1))
                <: attach(_, (1-_) : meter_leveler_brake) with{
                    maxHold = hold*maxSR;
                    strength = 1;
                    // hslider("gate strength", 1, 0.1, 10, 0.1);
                    range = 0-(ma.MAX);
                    gate_att =
                        0;
                    // hslider("gate att", 0.0, 0.0, 1, 0.001);
                    hold = 0.0001;
                    gate_rel =
                        0.1;
                    // hslider("gate rel", 0.1, 0.0, 1, 0.001);
                    knee =
                        ma.EPSILON;
                    // hslider("gate knee", 0, 0, 90, 1);
                    prePost = 1;
                };

  
};