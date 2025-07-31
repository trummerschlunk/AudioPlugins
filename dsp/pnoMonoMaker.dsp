// -*-Faust-*-

declare name "pnoMonoMaker";
declare version "0.01";
declare author "Klaus Scheuermann";
declare license "GPLv3";

import("stdfaust.lib");

gui_main(x) = hgroup("pnoMonoMaker",x);

process(l,r) = l,r
                
                : crossover
                : ro.interleave(2,2)

                : low, high

                : combine
                
                with{
                    
                    crossover = par(i,2,fi.crossover2LR4(freq));
                    freq = gui_main(vslider("crossover frequency[scale:log]", 200, 10, 2000,1));
                
                    high = si.bus(2);

                    low(l,r) = l * bal , r * (1-bal) :> si.bus(1) : gain_low : phaseLow <: si.bus(2);
                    bal = gui_main(vslider("ballance",0.5,0,1,0.1));
                    gain_low = _ * (gui_main(vslider("gain low",0,-6,6,0.1)) : ba.db2linear);
                    phaseLow = _ <: _,_ : _,(_:ma.neg) : _*(1-phase_switch),_*phase_switch :> _;
                    phase_switch = gui_main(checkbox("phase"));

                    combine = ro.interleave(2,2) : par(i,2,(_,_ :> _));

                    dryWet = gui_main(vslider("dry/wet", 1,0,1,0.1));
                
                };