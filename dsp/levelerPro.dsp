// -*-Faust-*-

declare name "levelerPro";
declare version "1.0";
declare author "Klaus Scheuermann";
declare license "GPLv3";

// ds = library("dynamicsmoothing.lib");
// ebu = library("ebur128.lib");

import("stdfaust.lib");

Nch = 2;
maxSR = 48000;

init_leveler_target = -16;
init_leveler_maxboost = 30;
init_leveler_maxcut = 30;
init_leveler_brake_threshold = -22;
init_leveler_speed = 80;
init_leveler_scale =100;


process = si.bus(Nch) : pregain(Nch) : leveler : si.bus(Nch);




bp = checkbox("h:LevelerPro/[0]bypass_leveler"):si.smoo;

preGainSlider = vslider("h:[2]Controls/[0][unit:dB]PreGain", 0, -20, 20, 0.1);
target = vslider("h:[2]Controls/[1][unit:dB]target", -23, -60, 0, 1);

limit_pos = vslider("h:[2]Controls/[2][unit:dB]max boost", init_leveler_maxboost, 0, 30, 1);
limit_neg = vslider("h:[2]Controls/[3][unit:dB]max cut", init_leveler_maxcut, 0, 30, 1) : ma.neg;
scale = vslider("h:[2]Controls/[4][unit:%]strength", init_leveler_scale, 0, 100, 1) * 0.01;

leveler_speed = vslider("h:[2]Controls/[5][unit:%]speed", init_leveler_speed, 0, 100, 1) * 0.01;
leveler_brake_thresh = target + vslider("h:[2]Controls/[6][unit:dB]brake threshold", init_leveler_brake_threshold,-90,0,1)+32;
meter_leveler_brake = _*100 : vbargraph("h:[2]Controls/[7][unit:%]brake",0,100);

leveler_meter_gain = vbargraph("h:[2]Controls/[8][unit:dB]gain",-50,50);

postGainSlider = vslider("h:[2]Controls/[9][unit:dB]PostGain", 0, -20, 20, 0.1);


// utility functions

pregain(n) = par(i,n,gain) with {
    gain = _ * (preGainSlider : ba.db2linear : si.smoo);
};

postgain(n) = par(i,n,gain) with {
    gain = _ * (postGainSlider : ba.db2linear : si.smoo);
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

  //kfilter = ebu.ebur128;
};

lk2_var(Tg)= par(i,2,kfilter : zi) :> 4.342944819 * log(max(1e-12)) : -(0.691) with {
  // maximum assumed sample rate is 192k
  sump(n) = ba.slidingSump(n, 0.4 * maxSR)/max(n,ma.EPSILON);
  envelope(period, x) = x * x :  sump(rint(period * ma.SR));
  zi = envelope(Tg); // mean square: average power = energy/Tg = integral of squared signal / Tg

  //kfilter = ebu.ebur128;
};


lk2_short = lk2_fixed(3);
lufs_out_meter(l,r) = l,r <: l, attach(r, (lk2_short : vbargraph("h:[6]PostStage/[unit:dB]lufs",meters_minimum,0))) : _,_;

lk2_time =
  // 0.4;
  hslider("lk2 time", 0.01, 0.001, 3, 0.001);
// it.interpolate_linear(leveler_speed :pow(hslider("lk2 power", 2, 0.1, 10, 0.1))
//                      ,0.4 // hslider("lk2 time", 0.4, 0.001, 3, 0.001)
//                      , 0.04):max(0);


kfilter = si.bus(1) <: (ebur128 * filterswitch), (speechfilter  * (1 - filterswitch)) :> si.bus(1) with{
    speechfilter = fi.highpass(2,200) : fi.fi.peak_eq_cq(3,2400,0.7);
    filterswitch = hslider("h:LevelerPro/[4][style:radio{'ebu':0;'speech':1}]filter",0,0,1,1);
};



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
    :  limit(limit_neg,limit_pos)
    : dynamicSmoothing(
      sensitivity * expander(abs(fl)+abs(fr))
    ,  basefreq * expander(abs(fl)+abs(fr))
    )
    
    * scale
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











freq2k(f_c) = tan((ma.PI * f_c)/ma.SR);

stage1 = fi.tf22t(b0,b1,b2,a1,a2)
with {
  f_c = 1681.7632251028442; // Hertz
  gain = 3.9997778685513232; // Decibel
  K = freq2k(f_c);
  V_0 = pow(10, (gain/20.0));

  denominator = 1.0 + sqrt(2.0)*K + K^2;
  b0 = (V_0 + sqrt((2.0*V_0))*K + K^2) / denominator;
  b1 = 2.0*(K^2 - V_0) / denominator;
  b2 = (V_0 - sqrt(2.0*V_0)*K + K^2) / denominator;

  a1 = 2*(K^2 - 1) / denominator;
  a2 = (1 - sqrt(2.0)*K + K^2) / denominator;
};

stage2 = fi.tf22t(b0,b1,b2,a1,a2)
with {
  f_c = 38.135470876002174; // Hertz
  Q = 0.5003270373223665;
  K = freq2k(f_c);

  denominator = (K^2) * Q + K + Q;
  b0 = Q / denominator;
  b1 = -2*Q / denominator;
  b2 = b0;

  a1 = (2*Q * (K^2 - 1)) / denominator;
  a2 = ((K^2) * Q - K + Q) / denominator;
};

prefilter = stage1 : stage2;


// Normalize such that 997Hz has unity gain 1.0.  Otherwise a sinewave
// of ~1000Hz would gain 0.66dB. This is additional to the
// ITU-recommendation biquads! 997Hz is the closest prime number to
// 1000Hz.
normalize997 = *(0.9273671710547968);

ebur128 = prefilter : normalize997;








// Dynamic Smoothing
PI = ma.PI;
SR = ma.SR;
NY = SR / 2.0;
T = 1.0 / SR;
PIT = PI * T;

SVF(Q, CF, x) = f ~ si.bus(2) : ! , ! , si.bus(3)
    with {
        g = tan(CF * PIT);
        R2 = 1.0 / Q;
        gPlusR2 = g + R2;
        f(s0, s1) = u0 , u1 , BP , HP , LP
            with {
                HP = (x - s0 * gPlusR2 - s1) / (1.0 + g * gPlusR2);
                v0 = HP * g;
                BP = s0 + v0;
                v1 = BP * g;
                LP = s1 + v1;
                u0 = v0 + BP;
                u1 = v1 + LP;
            };
    };
dynamicSmoothing(sensitivity, baseCF, x) = f ~ _ : ! , ! , _
    with {
        f(s) = SVF(.5, CF, x)
            with {
                CF = min(NY * .25, baseCF + sensitivity * abs(s) * NY);
            };
    };