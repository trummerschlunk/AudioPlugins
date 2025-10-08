// -*-Faust-*-

declare name "mbmsComp4";
declare version "1.0";
declare author "Klaus Scheuermann";
declare license "GPLv3";

add = library("addfaust.lib");


import("stdfaust.lib");



process = si.bus(Nch) : add.dryWetMixer(dryWet,mscomp_bp) : si.bus(Nch);

// stereo bypass with si.smoo fading
bp2(sw,pr) =  _,_ <: _,_,pr : (_*sm,_*sm),(_*(1-sm),_*(1-sm)) :> _,_ with {
    sm = sw : si.smoo;
};

// stereo to m/s encoder
ms_enc = _*0.5,_*0.5 <: +, -;

// m/s to stereo decoder
ms_dec = _,_ <: +, -;

Nch = 2; //number of channels
Nba = 4; //number of bands of the multiband compressor

global_thresh = vslider("v:master_me/h:[5]mscomp/[1]global_thresh[unit:dB][symbol:global_thresh]", 0,-50,0,0.5);
dryWet = vslider("v:master_me/h:[5]mscomp/[4]dry wet[unit:%][symbol:dryWet]", 100,0,100,1) / 100;

// MSCOMP Interpolated
mscomp_bp = bp2(checkbox("v:master_me/h:[5]mscomp/[0][symbol:mscomp_bypass]mscomp bypass"),
                ms_enc
                : B_band_Compressor_N_chan(Nba,Nch)
                : ms_dec
               ) ;

B_band_Compressor_N_chan(B,N) =
  si.bus (N) <: si.bus (2 * N)
  : ( (crossover:gain_calc), si.bus(N) )
  : apply_gain
  : outputGain
with {
  crossover =
    par(i, N, an.analyzer (6, crossoverFreqs)
              : ro.cross (B)
       );

  apply_gain =
    (ro.interleave(N, B+1))
    : par(i, N, ro.cross(B),_)
    : par(i, N, shelfcascade ((crossoverFreqs)))
  ;

  // TODO: use co.peak_compression_gain_N_chan_db when it arrives in the current faust version
  compressor(N,prePost,strength,thresh,att,rel,knee,link) = add.peak_compression_gain_N_chan_db(strength,thresh,att,rel,knee,prePost,link,N);

  gain_calc = (strength_array, thresh_array, att_array, rel_array, knee_array, link_array, si.bus(N*B))
              : ro.interleave(B,6+N)
              : par(i, B, compressor(N,prePost)) // : si.bus (N * Nr_bands)
              : par(b, B, par(c, N, meter(b+1, c+1)));


  outputGain = par(i, N, _*mscomp_outGain);

  /* TODO: separate %b%c in symbol name so that it is a valid C/C++ variable-name (ideally an underscore) %b_%c
   * meanwhile this is safe since there are only 8 bands (1..9) and 2 channels.
   */
  meter(b,c) =
    _<: attach(_, (max(-6):min(0):vbargraph(
                     // "v:master_me/h:[6]mscomp_meter/[%b.%c][unit:dB][tooltip: gain reduction in db][symbol:msredux%b%c]mscomp redux band %b chn %c", -3, 0)
                     "v:master_me/h:[7]mscomp_meter/[%b.%c][unit:dB][tooltip: gain reduction in db][symbol:msredux%b%c]", -6, 0)
                  ));

  /* higher order low, band and hi shelf filter primitives */
  ls3(f,g) = fi.svf.ls (f, .5, g3) : fi.svf.ls (f, .707, g3) : fi.svf.ls (f, 2, g3) with {g3 = g/3;};
  bs3(f1,f2,g) = ls3(f1,-g) : ls3(f2,g);
  hs3(f,g) = fi.svf.hs (f, .5, g3) : fi.svf.hs (f, .707, g3) : fi.svf.hs (f, 2, g3) with {g3 = g/3;};

  /* Cascade of shelving filters to apply gain per band.
   *
   * `lf` : list of frequencies
   * followed by (count(lf) +1) gain parameters
   */
  shelfcascade(lf) = fbus(lf), ls3(first(lf)) : sc(lf)
  with {
    sc((f1, f2, lf)) = fbus((f2,lf)), bs3(f1,f2) : sc((f2,lf)); // recursive pattern
    sc((f1, f2))     = _, bs3(f1,f2) : hs3(f2);                // halting pattern
    fbus(l)          = par(i, outputs(l), _);                  // a bus of the size of a list
    first((x,xs))    = x;                                      // first element of a list
  };

  /* Cross over frequency range */
  fl = vslider("v:master_me/h:[5]mscomp/h:[5]low band/[7][symbol:mscomp_low_crossover][scale:log][unit:Hz]low crossover", 100, 20, 4000, 1);
  fh = vslider("v:master_me/h:[5]mscomp/h:[6]high band/[7][symbol:mscomp_high_crossover][scale:log][unit:Hz]high crossover", 5000, 5000, 20000, 1);

  /* Compressor settings */
  strength_array = vslider("v:master_me/h:[5]mscomp/h:[5]low band/[1][unit:%][symbol:mscomp_low_strength]low strength", 10, 0, 100, 1)*0.01,vslider("v:master_me/h:[5]mscomp/h:[6]high band/[1][unit:%][symbol:mscomp_high_strength]high strength", 30, 0, 100, 1)*0.01:LinArray(B);
  thresh_array = global_thresh + vslider("v:master_me/h:[5]mscomp/h:[5]low band/[2][unit:dB][symbol:mscomp_low_threshold]low tar-thresh", 0, -12, 12, 0.5),global_thresh + vslider("v:master_me/h:[5]mscomp/h:[6]high band/[2][unit:dB][symbol:mscomp_high_threshold]high tar-thresh", 0, -12, 12, 0.5):LinArray(B);
  att_array = (vslider("v:master_me/h:[5]mscomp/h:[5]low band/[3][unit:ms][symbol:mscomp_low_attack]low attack", 30, 0, 100, 0.1)*0.001,vslider("v:master_me/h:[5]mscomp/h:[6]high band/[3][unit:ms][symbol:mscomp_high_attack]high attack", 4, 0, 100, 0.1)*0.001):LogArray(B);
  rel_array = (vslider("v:master_me/h:[5]mscomp/h:[5]low band/[4][unit:ms][symbol:mscomp_low_release]low release", 300, 1, 1000, 1)*0.001,vslider("v:master_me/h:[5]mscomp/h:[6]high band/[4][unit:ms][symbol:mscomp_high_release]high release", 40, 1, 1000, 1)*0.001):LogArray(B);
  knee_array = (vslider("v:master_me/h:[5]mscomp/h:[5]low band/[5][unit:dB][symbol:mscomp_low_knee]low knee", 6, 0, 30, 0.1),vslider("v:master_me/h:[5]mscomp/h:[6]high band/[5][unit:dB][symbol:mscomp_high_knee]high knee", 6, 0, 30, 0.1)):LinArray(B);
  link_array = (vslider("v:master_me/h:[5]mscomp/h:[5]low band/[6][unit:%][symbol:mscomp_low_link]low link", 60, 0, 100, 1)*0.01,vslider("v:master_me/h:[5]mscomp/h:[6]high band/[6][unit:%][symbol:mscomp_high_link]high link", 20, 0, 100, 1)*0.01):LinArray(B);
  crossoverFreqs = LogArray(B-1,fl,fh);
  mscomp_outGain = vslider("v:master_me/h:[5]mscomp/[3][unit:dB][symbol:mscomp_output_gain]makeup", 0, -6, 6, 0.5):ba.db2linear:si.smoo;

  // make a linear array of values, from bottom to top
  LinArray(N,bottom,top) = par(i,N,   ((top-bottom)*(i/(N-1)))+bottom);
  // make a log array of values, from bottom to top
  LogArray(N,bottom,top) = par(i,N,   pow((pow((t/b),1/(N-1))),i)*b)
  with {
    b = bottom:max(ma.EPSILON);
    t = top:max(ma.EPSILON);
  };


  prePost = 1;
};