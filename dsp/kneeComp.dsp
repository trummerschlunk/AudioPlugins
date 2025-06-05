// -*-Faust-*-

declare name "kneeComp";
declare version "1.0";
declare author "Klaus Scheuermann";
declare license "GPLv3";



import("stdfaust.lib");



process =

        
     
         ( sc_compressor)~(si.bus(2))

  
  
;

// stereo to m/s encoder
ms_enc = _*0.5,_*0.5 <: +, -;

// m/s to stereo decoder
ms_dec = _,_ <: +, -;



// SIDE CHAIN COMPRESSOR

sc_compressor(fl,fr,l,r) =
  (fl,fr,l,r)
  : feedforward_feedback
  : ((ms_enc : sc_filter),ms_enc):
  (((RMS_compression_gain_N_chan_db(strength,thresh,att,rel,knee,0,link,N)),si.bus(N) )
   : ro.interleave(N,2) : par(i,N,(post_gain : meter(i) :  ba.db2linear*(1-bypass)+bypass)*_))
  : ms_dec
  : ((l,_,r,_):par(i, 2, it.interpolate_linear(dw)))

with {
  N = 2;
  B = si.bus(2);
  bypass = checkbox("h:[5]kneeComp/[0][symbol:kneecomp_bypass]bypass"):si.smoo;
  strength = vslider("h:[5]kneeComp/[1][unit:%][integer][symbol:kneecomp_strength]strength", 10, 0, 100, 1) * 0.01;
  thresh = vslider("h:[5]kneeComp/[2][symbol:kneecomp_threshold][unit:dB]thresh",0,-40,0,1);
  att = vslider("h:[5]kneeComp/[3][symbol:kneecomp_attack][unit:ms]attack",50,1,100,1)*0.001;
  rel = vslider("h:[5]kneeComp/[4][symbol:kneecomp_release][unit:ms]release",2000,1,4000,1)*0.001;
  knee = vslider("h:[5]kneeComp/[5][unit:dB][symbol:kneecomp_knee]knee",24,0,30,1);
  link = vslider("h:[5]kneeComp/[6][unit:%][integer][symbol:kneecomp_link]link", 60, 0, 100, 1) *0.01;
  fffb = vslider ("h:[5]kneeComp/[7][unit:%][integer][symbol:kneecomp_fffb]ff-fb",50,0,100,1) *0.01;
  dw = vslider ("h:[5]kneeComp/[9][unit:%][integer][symbol:kneecomp_drywet]dry/wet",100,0,100,1) * 0.01:si.smoo;

  meter(i) =
    _<: attach(_, (max(-6):min(6):vbargraph(
                     "h:[5]kneeComp/[symbol:kneecomp_meter_%i][unit:dB]meter %i", -6, 6)
                  ));

  feedforward_feedback = B,(B<:B,B) : par(i,2,_*fffb), par(i,2,_* (1-fffb)),B : (_,_,_,_:>_,_),_,_;
    sc_filter = par(i,N,fi.highpass(1,sc_freq)) with{
        sc_freq = vslider("h:[5]kneeComp/[8][symbol:kneecomp_scfreq][unit:Hz][scale:log]sc_freq",20,10,600,1);
    };

  // dev version of faust has this in the libs, TODO, use co.RMS_compression_gain_N_chan_db
  RMS_compression_gain_N_chan_db(strength,thresh,att,rel,knee,prePost,link,1) =
    RMS_compression_gain_mono_db(strength,thresh,att,rel,knee,prePost);

  RMS_compression_gain_N_chan_db(strength,thresh,att,rel,knee,prePost,link,N) =
    par(i,N,RMS_compression_gain_mono_db(strength,thresh,att,rel,knee,prePost))
    <: (si.bus(N),(ba.parallelMin(N) <: si.bus(N))) : ro.interleave(N,2) : par(i,N,(it.interpolate_linear(link)));

  RMS_compression_gain_mono_db(strength,thresh,att,rel,knee,prePost) =
    RMS(rel) : ba.bypass1(prePost,si.onePoleSwitching(att,0)) : ba.linear2db : gain_computer(strength,thresh,knee) : ba.bypass1((prePost!=1),si.onePoleSwitching(0,att))
  with {
    gain_computer(strength,thresh,knee,level) =
      select3((level>(thresh-(knee/2)))+(level>(thresh+(knee/2))),
              0,
              ((level-thresh+(knee/2)) : pow(2)/(2*max(ma.EPSILON,knee))),
              (level-thresh))
      : max(0)*-strength;
    RMS(time) = ba.slidingRMS(s) with {
      s = ba.sec2samp(time):int:max(1);
    };
  };
  //post_gain
  post_gain =
    _+
    (vslider("h:[5]kneeComp/[8][unit:dB][symbol:kneecomp_makeup]makeup", 0,-10,+10,0.5) :si.smoo);



};
