'use client';

import { JSX, useCallback, useEffect, useRef, useState } from 'react';

// ═══════════════════════════════════════════════════════════════════════
//  TYPES
// ═══════════════════════════════════════════════════════════════════════

interface DanceProfile {
  id: string;
  name: string;
  short: string;
  meter: number;
  barsMin: [number, number];
  category: 'standard' | 'latin';
  accentTemplate: number[];
}
interface DanceResult {
  dance: DanceProfile;
  energy: number;
  barsPerMin: number;
  bpm: number;
  status: 'match' | 'fast' | 'slow' | 'none';
  deviation: number;
  meterConfidence: number;
  accentProfile: number[];
  barPhase: number;
  beatCount: number;
}

// ═══════════════════════════════════════════════════════════════════════
//  CONSTANTS
// ═══════════════════════════════════════════════════════════════════════

const SR = 44_100;
const FFT_SIZE = 2048;

const binOf = (hz: number): number => Math.round((hz / SR) * FFT_SIZE);

/**
 * Sub-bands defined by FFT bin ranges.
 * IMPORTANT: We work entirely in dB space. No linear conversion.
 * Weight here is applied to the final onset value per band.
 * floor_dB: ignore bins below this value (treat as silence/noise).
 */
const SUB_BANDS = [
  { name: 'kick', lo: binOf(40), hi: binOf(200), weight: 1.3, floor: -80 },
  { name: 'snare', lo: binOf(200), hi: binOf(1200), weight: 1, floor: -85 },
  { name: 'mid', lo: binOf(1200), hi: binOf(4500), weight: 0.7, floor: -90 },
  { name: 'high', lo: binOf(4500), hi: binOf(16_000), weight: 0.5, floor: -95 },
] as const;

const NUM_BANDS = SUB_BANDS.length;

// ═══════════════════════════════════════════════════════════════════════
//  WDSF DANCE PROFILES
// ═══════════════════════════════════════════════════════════════════════

const DANCES: DanceProfile[] = [
  {
    id: 'sw',
    name: 'Slow Waltz',
    short: 'SW',
    meter: 3,
    barsMin: [28, 30],
    category: 'standard',
    accentTemplate: [1, 0.25, 0.35],
  },
  {
    id: 'tg',
    name: 'Tango',
    short: 'TG',
    meter: 2,
    barsMin: [31, 33],
    category: 'standard',
    accentTemplate: [1, 0.7],
  },
  {
    id: 'vw',
    name: 'V. Waltz',
    short: 'VW',
    meter: 3,
    barsMin: [58, 60],
    category: 'standard',
    accentTemplate: [1, 0.25, 0.3],
  },
  {
    id: 'sf',
    name: 'Slow Foxtrot',
    short: 'SF',
    meter: 4,
    barsMin: [28, 30],
    category: 'standard',
    accentTemplate: [1, 0.35, 0.65, 0.35],
  },
  {
    id: 'qs',
    name: 'Quickstep',
    short: 'QS',
    meter: 4,
    barsMin: [50, 52],
    category: 'standard',
    accentTemplate: [1, 0.35, 0.7, 0.35],
  },
  {
    id: 'cc',
    name: 'Cha-Cha-Cha',
    short: 'CC',
    meter: 4,
    barsMin: [30, 32],
    category: 'latin',
    accentTemplate: [0.6, 0.3, 1, 0.55],
  },
  {
    id: 'sa',
    name: 'Samba',
    short: 'SA',
    meter: 2,
    barsMin: [50, 52],
    category: 'latin',
    accentTemplate: [1, 0.75],
  },
  {
    id: 'ru',
    name: 'Rumba',
    short: 'RU',
    meter: 4,
    barsMin: [25, 27],
    category: 'latin',
    accentTemplate: [0.4, 0.3, 0.65, 1],
  },
  {
    id: 'pd',
    name: 'Paso Doble',
    short: 'PD',
    meter: 2,
    barsMin: [60, 62],
    category: 'latin',
    accentTemplate: [1, 0.55],
  },
  {
    id: 'jv',
    name: 'Jive',
    short: 'JV',
    meter: 4,
    barsMin: [42, 44],
    category: 'latin',
    accentTemplate: [1, 0.35, 0.8, 0.35],
  },
];

function danceBeatPeriod(d: DanceProfile): number {
  return 60 / (((d.barsMin[0] + d.barsMin[1]) / 2) * d.meter);
}

// ═══════════════════════════════════════════════════════════════════════
//  DSP: SUB-BAND ONSET DETECTION — ALL IN dB DOMAIN
// ═══════════════════════════════════════════════════════════════════════

/**
 * Onset detection working entirely in dB space.
 *
 * For each sub-band:
 * 1. Compute mean dB level across the band's FFT bins (clamped to floor)
 * 2. Half-wave rectified flux = max(0, currentMeanDB - prevMeanDB)
 *    This is a dB increase — a 6dB rise is always "same onset strength"
 *    regardless of whether the band was at -30dB or -70dB.
 * 3. Normalize flux from dB rise to 0..1 range using a reference rise
 *    (e.g., 12dB rise = 1.0, below that scales linearly).
 * 4. Apply EMA smoothing and band weight.
 */
class OnsetDetector {
  private readonly prevMeanDB: Float32Array;
  private readonly smoothed: Float32Array;

  // How many dB of rise counts as "maximum onset"
  private static REFERENCE_RISE_DB = 15;

  constructor() {
    this.prevMeanDB = new Float32Array(NUM_BANDS).fill(-100);
    this.smoothed = new Float32Array(NUM_BANDS);
  }

  process(magDB: Float32Array): { onset: number; bandOnsets: Float32Array } {
    const bandOnsets = new Float32Array(NUM_BANDS);
    let combined = 0;

    for (let b = 0; b < NUM_BANDS; b++) {
      const { lo, hi, weight, floor } = SUB_BANDS[b]!;
      const clampHi = Math.min(hi, magDB.length - 1);
      const numBins = clampHi - lo + 1;
      if (numBins <= 0) continue;

      // Mean dB level in band (clamp bins to floor to ignore deep silence)
      let sumDB = 0;
      for (let k = lo; k <= clampHi; k++) {
        sumDB += Math.max(floor, magDB[k]!);
      }
      const meanDB = sumDB / numBins;

      // Half-wave rectified flux in dB
      const riseDB = Math.max(0, meanDB - this.prevMeanDB[b]!);
      this.prevMeanDB[b] = meanDB;

      // Normalize: map dB rise to 0..1
      const normalized = Math.min(1, riseDB / OnsetDetector.REFERENCE_RISE_DB);

      // EMA smooth
      this.smoothed[b] = this.smoothed[b]! * 0.65 + normalized * 0.35;

      bandOnsets[b] = this.smoothed[b]! * weight;
      combined += bandOnsets[b]!;
    }

    return { onset: combined, bandOnsets };
  }
}

// ═══════════════════════════════════════════════════════════════════════
//  DSP: COMB FILTER RESONATOR BANK
// ═══════════════════════════════════════════════════════════════════════

/**
 * Comb filter resonator bank — MULTI-PERIOD SWEEP + PHASE-ONLY PLL.
 *
 * Previous approach: single resonator per dance with PLL adjusting period.
 * Problem: onset detector's EMA smoothing introduces ~1-2 frame latency,
 * which biases the PLL toward always seeing onsets as "late," causing
 * systematic period increase (tempo underestimation).
 *
 * New approach: SEPARATE tempo estimation from beat tracking.
 *
 * TEMPO: For each dance, run N parallel comb filters at slightly different
 * periods spanning ±15% of the dance's center tempo. Each accumulates
 * energy independently. The period with the highest energy IS the measured
 * tempo. This is bias-free because comb filter energy is symmetric around
 * the true period — a period that's 2% too fast loses exactly as much
 * energy as one that's 2% too slow.
 *
 * BEAT TRACKING: A single phase oscillator at the best period, with
 * phase-only PLL (no frequency correction). The phase correction nudge
 * from onsets keeps beats aligned without affecting tempo measurement.
 *
 * The N-resonator sweep costs N× more per dance, but N=11 and the
 * per-resonator cost is ~5 multiplications, so total is ~550 muls/frame.
 */

const SWEEP_STEPS = 11; // periods per dance: center ± 5 steps
const SWEEP_RANGE = 0.15; // ±15% of center period
const PHASE_ALPHA = 0.06; // phase-only PLL correction gain
const COMB_DECAY = 0.988; // per-frame energy decay

interface SweepBin {
  period: number;
  energy: number;
  phase: number;
}

interface ResonatorState2 {
  bins: SweepBin[];
  bestIdx: number; // index of highest-energy bin
  // Beat tracking (uses bestIdx period)
  trackPhase: number; // 0..1 phase of the tracking oscillator
  beatCount: number;
  barPhase: number;
  accentBins: number[];
  accentCounts: number[];
  // Expose for type compat
  energy: number;
  phase: number;
  period: number;
  periodIntegrator: number; // unused
}

class ResonatorBank {
  private states: Map<string, ResonatorState2>;
  dt: number;

  constructor(analysisHz: number) {
    this.dt = 1 / analysisHz;
    this.states = new Map();
    for (const d of DANCES) this.states.set(d.id, this.fresh(d));
  }

  private fresh(d: DanceProfile): ResonatorState2 {
    const center = danceBeatPeriod(d);
    const bins: SweepBin[] = [];
    for (let i = 0; i < SWEEP_STEPS; i++) {
      // Spread from center*(1-range) to center*(1+range)
      const t = i / (SWEEP_STEPS - 1); // 0..1
      const factor = 1 - SWEEP_RANGE + 2 * SWEEP_RANGE * t; // 0.85..1.15
      bins.push({ period: center * factor, energy: 0, phase: Math.random() });
      // Random initial phase prevents all bins from being in lock-step
    }

    return {
      bins,
      bestIdx: Math.floor(SWEEP_STEPS / 2), // start at center
      trackPhase: 0,
      beatCount: 0,
      barPhase: 0,
      accentBins: Array.from<number>({ length: d.meter }).fill(0),
      accentCounts: Array.from<number>({ length: d.meter }).fill(0),
      // Compat fields
      energy: 0,
      phase: 0,
      period: center,
      periodIntegrator: 0,
    };
  }

  process(onset: number, kickOnset: number, _elapsed: number): void {
    for (const dance of DANCES) {
      const st = this.states.get(dance.id)!;

      // ── 1. Advance all sweep bins and accumulate comb energy ──
      let bestEnergy = -1;
      let bestIdx = st.bestIdx;

      for (let i = 0; i < st.bins.length; i++) {
        const bin = st.bins[i]!;

        // Advance phase
        bin.phase += this.dt / bin.period;
        while (bin.phase >= 1) bin.phase -= 1;

        // Comb filter: onset energy weighted by proximity to beat
        const dist = Math.abs(bin.phase - Math.round(bin.phase));
        const prox = 1 - 2 * dist; // 1 on beat, 0 at half-beat
        const gated = prox > 0.45 ? onset * prox * prox : 0;
        bin.energy = bin.energy * COMB_DECAY + gated;

        if (bin.energy > bestEnergy) {
          bestEnergy = bin.energy;
          bestIdx = i;
        }
      }
      st.bestIdx = bestIdx;
      st.energy = bestEnergy;
      st.period = st.bins[bestIdx]!.period;

      // ── 2. Beat tracking oscillator (uses best period, phase-only PLL) ──
      const trackPeriod = st.bins[bestIdx]!.period;
      st.trackPhase += this.dt / trackPeriod;

      // Beat crossing
      while (st.trackPhase >= 1) {
        st.trackPhase -= 1;
        st.beatCount++;

        const accentE = kickOnset * 0.6 + onset * 0.4;
        st.accentBins[st.barPhase]! += accentE;
        st.accentCounts[st.barPhase]!++;
        st.barPhase = (st.barPhase + 1) % dance.meter;
      }

      // Phase-only PLL: nudge tracking phase toward onsets (NO period change)
      if (onset > 0.02) {
        let phaseErr = st.trackPhase;
        if (phaseErr > 0.5) phaseErr -= 1;
        const weight = Math.min(1, onset / 0.2);
        st.trackPhase -= PHASE_ALPHA * phaseErr * weight;

        // Wrap
        while (st.trackPhase < 0) st.trackPhase += 1;
        while (st.trackPhase >= 1) {
          st.trackPhase -= 1;
          st.beatCount++;
          st.barPhase = (st.barPhase + 1) % dance.meter;
        }
      }

      st.phase = st.trackPhase;
    }
  }

  getResult(dance: DanceProfile): DanceResult {
    const st = this.states.get(dance.id)!;

    // BPM from best sweep bin — no PLL, no timestamps, no bias
    const measuredBPM = 60 / st.period;
    const measuredBars = measuredBPM / dance.meter;

    // Accent profile
    const prof = st.accentBins.map((s, i) =>
      st.accentCounts[i]! > 0 ? s / st.accentCounts[i]! : 0,
    );
    const pMax = Math.max(...prof, 1e-9);
    const normProf = prof.map((v) => v / pMax);

    // Meter confidence: best-rotation correlation
    let bestCorr = 0;
    for (let rot = 0; rot < dance.meter; rot++) {
      let dot = 0,
        nA = 0,
        nB = 0;
      for (let i = 0; i < dance.meter; i++) {
        const a = normProf[(i + rot) % dance.meter]!;
        const b = dance.accentTemplate[i]!;
        dot += a * b;
        nA += a * a;
        nB += b * b;
      }
      const denom = Math.sqrt(nA * nB);
      if (denom > 0) bestCorr = Math.max(bestCorr, dot / denom);
    }

    const center = (dance.barsMin[0] + dance.barsMin[1]) / 2;
    const dev = measuredBars - center;
    let status: DanceResult['status'] = 'none';
    if (st.energy > 0.008) {
      if (
        measuredBars >= dance.barsMin[0] - 0.5 &&
        measuredBars <= dance.barsMin[1] + 0.5
      )
        status = 'match';
      else if (measuredBars > dance.barsMin[1] + 0.5) status = 'fast';
      else status = 'slow';
    }

    return {
      dance,
      energy: st.energy,
      barsPerMin: measuredBars,
      bpm: measuredBPM,
      status,
      deviation: dev,
      meterConfidence: bestCorr,
      accentProfile: normProf,
      barPhase: st.barPhase,
      beatCount: st.beatCount,
    };
  }

  reset(): void {
    for (const d of DANCES) this.states.set(d.id, this.fresh(d));
  }
}

// ═══════════════════════════════════════════════════════════════════════
//  REACT APP
// ═══════════════════════════════════════════════════════════════════════

export default function WDSFLive(): JSX.Element {
  const [recording, setRecording] = useState(false);
  const [elapsed, setElapsed] = useState(0);
  const [results, setResults] = useState<DanceResult[]>([]);
  const [topResult, setTopResult] = useState<DanceResult | null>(null);
  const [level, setLevel] = useState(0);
  const [locked, setLocked] = useState<string | null>(null);
  const [micErr, setMicErr] = useState<string | null>(null);

  const ctxRef = useRef<AudioContext | null>(null);
  const analyserRef = useRef<AnalyserNode | null>(null);
  const streamRef = useRef<MediaStream | null>(null);
  const detRef = useRef<OnsetDetector | null>(null);
  const bankRef = useRef<ResonatorBank | null>(null);
  const magRef = useRef<Float32Array<ArrayBuffer> | null>(null);
  const animRef = useRef<number | null>(null);
  const lockedRef = useRef(locked);
  const bandHistRef = useRef<Float32Array[]>([]);
  const bandCanvasRef = useRef<HTMLCanvasElement | null>(null);

  useEffect(() => {
    lockedRef.current = locked;
  }, [locked]);

  // ── Band canvas ──
  const drawBands = useCallback(() => {
    const c = bandCanvasRef.current;
    if (!c) return;
    const g = c.getContext('2d')!;
    const W = c.width,
      H = c.height;
    g.fillStyle = '#07070c';
    g.fillRect(0, 0, W, H);

    const hist = bandHistRef.current;
    if (hist.length < 2) return;

    const colors = [
      'rgba(255,90,70,0.65)',
      'rgba(255,190,60,0.5)',
      'rgba(90,180,255,0.4)',
      'rgba(170,130,255,0.3)',
    ];
    const bH = H / NUM_BANDS;

    for (let b = 0; b < NUM_BANDS; b++) {
      let mx = 0;
      for (const f of hist) if (f[b]! > mx) mx = f[b]!;
      if (mx < 1e-6) mx = 1e-6;

      g.fillStyle = colors[b]!;
      for (const [i, element] of hist.entries()) {
        const x = (i / 350) * W;
        const h = (element[b]! / mx) * bH * 0.85;
        g.fillRect(x, bH * b + bH - h, Math.max(1, W / 350), h);
      }
      g.fillStyle = 'rgba(255,255,255,0.1)';
      g.font = '7px monospace';
      g.fillText(SUB_BANDS[b]!.name, 3, bH * b + 9);
    }
  }, []);

  // ── Start ──
  const start = useCallback(async () => {
    setMicErr(null);
    try {
      const stream = await navigator.mediaDevices.getUserMedia({
        audio: {
          echoCancellation: false,
          noiseSuppression: false,
          autoGainControl: false,
        },
      });
      streamRef.current = stream;

      const ctx = new AudioContext({ sampleRate: SR });
      ctxRef.current = ctx;
      const src = ctx.createMediaStreamSource(stream);

      const analyser = ctx.createAnalyser();
      analyser.fftSize = FFT_SIZE;
      analyser.smoothingTimeConstant = 0.08;
      src.connect(analyser);
      analyserRef.current = analyser;

      magRef.current = new Float32Array(analyser.frequencyBinCount);
      detRef.current = new OnsetDetector();

      // Estimate actual analysis rate — we'll measure it for accurate dt
      const measuredHz = 60; // initial estimate, refined below
      bankRef.current = new ResonatorBank(measuredHz);
      bandHistRef.current = [];

      setRecording(true);
      setResults([]);
      setTopResult(null);
      setElapsed(0);
      setLocked(null);
      lockedRef.current = null;

      const t0 = ctx.currentTime;
      let prevTime = t0;

      const tick = (): void => {
        const an = analyserRef.current;
        const det = detRef.current;
        const bnk = bankRef.current;
        const ac = ctxRef.current;
        if (!an || !det || !bnk || !ac) return;

        const now = ac.currentTime;
        const dt = now - t0;
        const frameDt = now - prevTime;
        prevTime = now;

        // Use actual frame dt for resonator (not assumed 60fps)
        // On first frame, frameDt may be 0 or huge — use estimate
        if (frameDt > 0.001 && frameDt < 0.1) {
          bnk.dt = frameDt;
        }

        setElapsed(dt);

        // Read native FFT (dB values)
        const mag = magRef.current!;
        an.getFloatFrequencyData(mag);

        // Level meter: average dB in low bins, mapped to 0..1
        let avgDB = 0;
        for (let k = 2; k < 80; k++) avgDB += mag[k]!;
        avgDB /= 78;
        // Map roughly: -60dB → 0, -10dB → 1
        setLevel(Math.max(0, Math.min(1, (avgDB + 60) / 50)));

        // Onset detection (all in dB domain)
        const { onset, bandOnsets } = det.process(mag);

        // Feed resonator bank
        bnk.process(onset, bandOnsets[0]!, dt);

        // Collect results
        const all = DANCES.map((d) => bnk.getResult(d));
        const scored = all.toSorted((a, b) => {
          const sa = a.energy * (0.5 + a.meterConfidence * 0.5);
          const sb = b.energy * (0.5 + b.meterConfidence * 0.5);
          return sb - sa;
        });
        setResults(scored);

        const lk = lockedRef.current;
        const eff = lk ? (scored.find((s) => s.dance.id === lk) ?? scored[0]) : scored[0];
        if (eff && eff.energy > 0.008) setTopResult(eff);
        else if (!lk) setTopResult(null);

        // Band viz
        const frame = new Float32Array(NUM_BANDS);
        frame.set(bandOnsets);
        bandHistRef.current.push(frame);
        if (bandHistRef.current.length > 350) bandHistRef.current.shift();
        drawBands();

        animRef.current = requestAnimationFrame(tick);
      };

      animRef.current = requestAnimationFrame(tick);
    } catch (error: any) {
      setMicErr(error?.message ?? 'Microphone access denied');
    }
  }, [drawBands]);

  const stop = useCallback(() => {
    if (animRef.current) cancelAnimationFrame(animRef.current);
    if (streamRef.current) for (const t of streamRef.current.getTracks()) t.stop();
    if (ctxRef.current) ctxRef.current.close();
    analyserRef.current = null;
    setRecording(false);
  }, []);

  useEffect(
    () => () => {
      if (animRef.current) cancelAnimationFrame(animRef.current);
      if (streamRef.current) for (const t of streamRef.current.getTracks()) t.stop();
    },
    [],
  );

  const fmt = (s: number) =>
    `${Math.floor(s / 60)}:${Math.floor(s % 60)
      .toString()
      .padStart(2, '0')}`;

  const gridStd = DANCES.filter((d) => d.category === 'standard').map((d) =>
    results.find((r) => r.dance.id === d.id),
  );
  const gridLat = DANCES.filter((d) => d.category === 'latin').map((d) =>
    results.find((r) => r.dance.id === d.id),
  );
  const topId = topResult?.dance.id;

  return (
    <div
      style={{
        background: '#0a0a10',
        color: '#d0d0d0',
        minHeight: '100vh',
        fontFamily: "'DM Mono','JetBrains Mono',monospace",
        padding: '20px 24px',
        boxSizing: 'border-box',
      }}
    >
      <style>{`
        @import url('https://fonts.googleapis.com/css2?family=DM+Mono:wght@300;400;500&family=Outfit:wght@300;400;500;600;700;800&display=swap');
        *{box-sizing:border-box}
        @keyframes lp{0%,100%{opacity:.5}50%{opacity:1}}
        @keyframes bg{0%{box-shadow:0 0 0 rgba(255,200,80,0)}20%{box-shadow:0 0 16px rgba(255,200,80,.45)}100%{box-shadow:0 0 0 rgba(255,200,80,0)}}
      `}</style>

      {/* Header */}
      <div
        style={{
          marginBottom: 20,
          display: 'flex',
          alignItems: 'baseline',
          gap: 12,
          flexWrap: 'wrap',
        }}
      >
        <h1
          style={{
            fontSize: 18,
            fontWeight: 700,
            margin: 0,
            fontFamily: "'Outfit',sans-serif",
          }}
        >
          <span style={{ color: '#ffc850' }}>WDSF</span>
          <span style={{ opacity: 0.12, margin: '0 6px' }}>·</span>
          <span style={{ color: '#8caae0' }}>LIVE TEMPO</span>
        </h1>
        {recording && (
          <div style={{ display: 'flex', alignItems: 'center', gap: 6 }}>
            <div
              style={{
                width: 7,
                height: 7,
                borderRadius: '50%',
                background: '#ff3b3b',
                animation: 'lp 1.2s ease infinite',
              }}
            />
            <span
              style={{
                fontSize: 10,
                color: '#ff3b3b',
                letterSpacing: '.06em',
                fontVariantNumeric: 'tabular-nums',
              }}
            >
              REC {fmt(elapsed)}
            </span>
          </div>
        )}
      </div>

      {/* Controls */}
      <div style={{ display: 'flex', gap: 10, marginBottom: 18, alignItems: 'center' }}>
        {!recording ? (
          <button
            onClick={start}
            style={{
              padding: '11px 26px',
              background: 'rgba(255,59,59,.07)',
              border: '1px solid rgba(255,59,59,.3)',
              color: '#ff3b3b',
              borderRadius: 6,
              fontSize: 13,
              fontFamily: "'Outfit',sans-serif",
              fontWeight: 600,
              cursor: 'pointer',
            }}
          >
            ● START
          </button>
        ) : (
          <button
            onClick={stop}
            style={{
              padding: '11px 26px',
              background: 'rgba(255,255,255,.03)',
              border: '1px solid rgba(255,255,255,.12)',
              color: '#aaa',
              borderRadius: 6,
              fontSize: 13,
              fontFamily: "'Outfit',sans-serif",
              fontWeight: 600,
              cursor: 'pointer',
            }}
          >
            ■ STOP
          </button>
        )}

        <div
          style={{
            flex: 1,
            maxWidth: 180,
            height: 6,
            background: 'rgba(255,255,255,.03)',
            borderRadius: 3,
            overflow: 'hidden',
          }}
        >
          <div
            style={{
              width: `${level * 100}%`,
              height: '100%',
              borderRadius: 3,
              background: level > 0.85 ? '#ff3b3b' : level > 0.5 ? '#ffc850' : '#40c870',
              transition: 'width .06s',
            }}
          />
        </div>

        {locked && (
          <button
            onClick={() => setLocked(null)}
            style={{
              padding: '5px 12px',
              background: 'rgba(255,200,80,.06)',
              border: '1px solid rgba(255,200,80,.25)',
              borderRadius: 4,
              color: '#ffc850',
              fontSize: 10,
              fontFamily: 'inherit',
              cursor: 'pointer',
            }}
          >
            UNLOCK {DANCES.find((d) => d.id === locked)?.short}
          </button>
        )}
      </div>

      {micErr && (
        <div
          style={{
            padding: '10px 14px',
            marginBottom: 14,
            borderRadius: 6,
            background: 'rgba(255,59,59,.06)',
            border: '1px solid rgba(255,59,59,.2)',
            fontSize: 12,
            color: '#ff6b6b',
          }}
        >
          {micErr}
        </div>
      )}

      {recording && elapsed < 3 && (
        <div
          style={{
            padding: '12px 16px',
            marginBottom: 16,
            borderRadius: 6,
            background: 'rgba(255,200,80,.03)',
            border: '1px solid rgba(255,200,80,.08)',
            fontSize: 12,
            color: 'rgba(255,200,80,.5)',
          }}
        >
          Listening… results in ~{Math.max(1, Math.ceil(3 - elapsed))}s
        </div>
      )}

      {/* ══ DANCE GRID ══ */}
      {results.length > 0 && elapsed >= 2 && (
        <div style={{ marginBottom: 14 }}>
          <GRow
            label="STANDARD"
            items={gridStd}
            topId={topId}
            locked={locked}
            setLocked={setLocked}
          />
          <GRow
            label="LATIN"
            items={gridLat}
            topId={topId}
            locked={locked}
            setLocked={setLocked}
          />
        </div>
      )}

      {/* Top banner */}
      {topResult && topResult.energy > 0.008 && (
        <Banner r={topResult} recording={recording} />
      )}

      {/* Sub-band viz */}
      <div style={{ marginBottom: 8 }}>
        <div
          style={{ fontSize: 8, opacity: 0.12, letterSpacing: '.08em', marginBottom: 2 }}
        >
          SUB-BAND ONSETS (dB FLUX)
        </div>
        <canvas
          ref={bandCanvasRef}
          width={900}
          height={68}
          style={{
            width: '100%',
            height: 68,
            borderRadius: 5,
            border: '1px solid rgba(255,255,255,.02)',
          }}
        />
      </div>
    </div>
  );
}

// ═══════════════════════════════════════════════════════════════════════
//  SUB-COMPONENTS
// ═══════════════════════════════════════════════════════════════════════

function GRow({
  label,
  items,
  topId,
  locked,
  setLocked,
}: {
  label: string;
  items: (DanceResult | undefined)[];
  topId: string | undefined;
  locked: string | null;
  setLocked: (id: string | null) => void;
}): JSX.Element {
  return (
    <>
      <div
        style={{
          fontSize: 8,
          opacity: 0.18,
          letterSpacing: '.1em',
          marginBottom: 4,
          marginTop: 6,
        }}
      >
        {label}
      </div>
      <div
        style={{
          display: 'grid',
          gridTemplateColumns: 'repeat(5,1fr)',
          gap: 5,
          marginBottom: 4,
        }}
      >
        {items.map((r, i) =>
          r ? (
            <Card
              key={r.dance.id}
              r={r}
              isTop={topId === r.dance.id}
              isLocked={locked === r.dance.id}
              onLock={() => setLocked(locked === r.dance.id ? null : r.dance.id)}
            />
          ) : (
            <div key={i} />
          ),
        )}
      </div>
    </>
  );
}

function Card({
  r,
  isTop,
  isLocked,
  onLock,
}: {
  r: DanceResult;
  isTop: boolean;
  isLocked: boolean;
  onLock: () => void;
}): JSX.Element {
  const {
    dance,
    energy,
    barsPerMin,
    status,
    deviation,
    meterConfidence,
    accentProfile,
    barPhase,
  } = r;
  const active = energy > 0.008;
  const sc =
    status === 'match'
      ? '#40c870'
      : status === 'fast'
        ? '#ff8855'
        : status === 'slow'
          ? '#5599ee'
          : 'rgba(255,255,255,.04)';
  const fill = Math.min(100, (energy / 0.12) * 100);

  return (
    <div
      onClick={onLock}
      style={{
        padding: '7px 9px',
        borderRadius: 5,
        position: 'relative',
        overflow: 'hidden',
        cursor: 'pointer',
        userSelect: 'none',
        background: isLocked
          ? 'rgba(255,200,80,.05)'
          : isTop
            ? 'rgba(255,200,80,.025)'
            : 'rgba(255,255,255,.008)',
        border: `1px solid ${isLocked ? 'rgba(255,200,80,.35)' : isTop ? 'rgba(255,200,80,.15)' : 'rgba(255,255,255,.02)'}`,
        transition: 'border-color .3s',
      }}
    >
      <div
        style={{
          position: 'absolute',
          left: 0,
          bottom: 0,
          width: `${fill}%`,
          height: 2,
          background: sc,
          opacity: 0.7,
          transition: 'width .15s',
        }}
      />

      <div
        style={{
          display: 'flex',
          justifyContent: 'space-between',
          alignItems: 'baseline',
          marginBottom: 3,
        }}
      >
        <span
          style={{
            fontFamily: "'Outfit',sans-serif",
            fontSize: 11,
            fontWeight: isTop || isLocked ? 700 : 400,
            color: isLocked
              ? '#ffc850'
              : isTop
                ? '#ffc850'
                : active
                  ? '#c0c0c0'
                  : 'rgba(255,255,255,.15)',
          }}
        >
          {dance.short}
        </span>
        <span style={{ fontSize: 7, opacity: 0.2 }}>
          {dance.meter === 2 ? '2/4' : dance.meter === 3 ? '3/4' : '4/4'}
        </span>
      </div>

      {active ? (
        <>
          <div
            style={{
              fontSize: 15,
              fontWeight: 600,
              color: sc,
              lineHeight: 1,
              fontVariantNumeric: 'tabular-nums',
            }}
          >
            {barsPerMin.toFixed(1)}
            <span style={{ fontSize: 7, opacity: 0.4, marginLeft: 2 }}>b/m</span>
          </div>
          <div
            style={{
              fontSize: 7,
              marginTop: 2,
              color: status === 'match' ? 'rgba(64,200,112,.5)' : 'rgba(255,255,255,.12)',
            }}
          >
            {status === 'match' ? '✓ ' : ''}
            {dance.barsMin[0]}–{dance.barsMin[1]}
            {status === 'fast' ? ` +${Math.abs(deviation).toFixed(1)}` : ''}
            {status === 'slow' ? ` ${deviation.toFixed(1)}` : ''}
          </div>

          {/* Beat pips */}
          <div style={{ display: 'flex', gap: 2, marginTop: 4 }}>
            {Array.from({ length: dance.meter }).map((_, i) => (
              <div
                key={i}
                style={{
                  flex: 1,
                  height: 3,
                  borderRadius: 1.5,
                  background:
                    barPhase === i ? (i === 0 ? '#ffc850' : sc) : 'rgba(255,255,255,.06)',
                  transition: 'background .06s',
                }}
              />
            ))}
          </div>

          {/* Accent bars */}
          {meterConfidence > 0.3 && (
            <div
              style={{
                display: 'flex',
                gap: 1,
                marginTop: 3,
                alignItems: 'flex-end',
                height: 12,
              }}
            >
              {accentProfile.map((v, i) => (
                <div
                  key={i}
                  style={{
                    flex: 1,
                    height: Math.max(1, v * 12),
                    borderRadius: 1,
                    background:
                      v === Math.max(...accentProfile)
                        ? 'rgba(255,200,80,.6)'
                        : 'rgba(255,255,255,.07)',
                  }}
                />
              ))}
            </div>
          )}
        </>
      ) : (
        <div style={{ fontSize: 9, opacity: 0.06, marginTop: 2 }}>—</div>
      )}

      {isLocked && (
        <div
          style={{
            position: 'absolute',
            top: 4,
            right: 5,
            fontSize: 7,
            color: '#ffc850',
          }}
        >
          🔒
        </div>
      )}
      {isTop && !isLocked && (
        <div
          style={{
            position: 'absolute',
            top: 5,
            right: 5,
            width: 4,
            height: 4,
            borderRadius: '50%',
            background: '#ffc850',
            boxShadow: '0 0 6px rgba(255,200,80,.4)',
          }}
        />
      )}
    </div>
  );
}

function Banner({ r, recording }: { r: DanceResult; recording: boolean }): JSX.Element {
  const {
    dance,
    barsPerMin,
    bpm,
    status,
    deviation,
    meterConfidence,
    accentProfile,
    barPhase,
    beatCount,
  } = r;
  const sc =
    status === 'match'
      ? '#40c870'
      : status === 'fast'
        ? '#ff8855'
        : status === 'slow'
          ? '#5599ee'
          : '#666';

  return (
    <div
      style={{
        padding: '14px 18px',
        marginBottom: 14,
        borderRadius: 8,
        background: status === 'match' ? 'rgba(64,200,112,.04)' : 'rgba(255,170,60,.04)',
        border: `1px solid ${status === 'match' ? 'rgba(64,200,112,.12)' : 'rgba(255,170,60,.12)'}`,
        display: 'flex',
        alignItems: 'center',
        gap: 18,
        flexWrap: 'wrap',
      }}
    >
      <div>
        <div
          style={{
            fontFamily: "'Outfit',sans-serif",
            fontSize: 22,
            fontWeight: 800,
            color: sc,
            lineHeight: 1,
          }}
        >
          {dance.name}
        </div>
        <div style={{ fontSize: 9, opacity: 0.3, marginTop: 3 }}>
          {dance.meter === 2 ? '2/4' : dance.meter === 3 ? '3/4' : '4/4'} ·{' '}
          {dance.barsMin[0]}–{dance.barsMin[1]} bars/min
        </div>
      </div>

      <NS value={barsPerMin.toFixed(1)} label="BARS/MIN" color="#ffc850" />
      <NS value={bpm.toFixed(1)} label="BPM" color="#8caae0" />

      {meterConfidence > 0.2 && (
        <div style={{ display: 'flex', gap: 3, alignItems: 'flex-end', height: 28 }}>
          {accentProfile.map((v, i) => (
            <div
              key={i}
              style={{
                width: 10,
                height: Math.max(3, v * 28),
                borderRadius: 2,
                background:
                  v === Math.max(...accentProfile) ? '#ffc850' : 'rgba(255,255,255,.08)',
                transition: 'height .2s',
              }}
            />
          ))}
          <span style={{ fontSize: 7, opacity: 0.2, marginLeft: 3, alignSelf: 'center' }}>
            {(meterConfidence * 100).toFixed(0)}%
          </span>
        </div>
      )}

      <div
        style={{
          padding: '5px 13px',
          borderRadius: 4,
          fontSize: 11,
          fontWeight: 700,
          fontFamily: "'Outfit',sans-serif",
          background: `${sc}15`,
          color: sc,
        }}
      >
        {status === 'match'
          ? '✓ IN RANGE'
          : `⚠ ${deviation > 0 ? 'FAST' : 'SLOW'} ${Math.abs(deviation).toFixed(1)} b/m`}
      </div>

      {recording && (
        <div style={{ display: 'flex', gap: 5, marginLeft: 'auto' }}>
          {Array.from({ length: dance.meter }).map((_, i) => (
            <div
              key={i}
              style={{
                width: 30,
                height: 30,
                borderRadius: 5,
                background:
                  barPhase === i
                    ? i === 0
                      ? '#ffc850'
                      : `${sc}99`
                    : 'rgba(255,255,255,.02)',
                border: `1px solid ${barPhase === i ? (i === 0 ? 'rgba(255,200,80,.6)' : `${sc}44`) : 'rgba(255,255,255,.03)'}`,
                display: 'flex',
                alignItems: 'center',
                justifyContent: 'center',
                fontSize: 12,
                fontWeight: 800,
                fontFamily: "'Outfit',sans-serif",
                color:
                  barPhase === i
                    ? i === 0
                      ? '#0a0a10'
                      : '#fff'
                    : 'rgba(255,255,255,.06)',
                transition: 'all .05s',
                animation: barPhase === i ? 'bg .3s ease-out' : 'none',
              }}
            >
              {i + 1}
            </div>
          ))}
          <div style={{ alignSelf: 'center', marginLeft: 4 }}>
            <div
              style={{ fontSize: 10, opacity: 0.2, fontVariantNumeric: 'tabular-nums' }}
            >
              BAR {Math.floor(beatCount / dance.meter) + 1}
            </div>
          </div>
        </div>
      )}
    </div>
  );
}

function NS({
  value,
  label,
  color,
}: {
  value: string;
  label: string;
  color: string;
}): JSX.Element {
  return (
    <div style={{ textAlign: 'center' }}>
      <div
        style={{
          fontSize: 28,
          fontWeight: 700,
          color,
          fontVariantNumeric: 'tabular-nums',
          lineHeight: 1,
        }}
      >
        {value}
      </div>
      <div style={{ fontSize: 8, opacity: 0.25, marginTop: 2 }}>{label}</div>
    </div>
  );
}
