import { useState, useRef, useCallback, useEffect, useMemo } from "react";

/* ── Types ── */

interface Dance {
  name: string;
  bMin: number;
  bMax: number;
  time: string;
  bpb: number;
}

interface OctaveResult {
  barsPerMin: number;
  factor: number;
  beatsPerMin: number;
}

interface AlgoState {
  raw: number;
  bars: number;
  factor: number;
  beats: number;
  conf: number;
}

interface OnsetResult {
  rawBpm: number;
  confidence: number;
  intervals: number;
}

interface OnsetAnalyzer {
  process: (freqData: Uint8Array, now: number) => OnsetResult | null;
  getLastOnset: () => number;
  reset: () => void;
}

interface PendingState {
  level: number;
  algo: AlgoState | null;
  hist: number | null;
}

// Phase tracker: keeps a continuously advancing beat phase synced to onsets
interface PhaseTracker {
  update: (beatsPerMin: number, onsetTime: number, now: number) => void;
  getPhase: (now: number) => number; // 0..bpb (fractional beat position in bar)
  getBeatInterval: () => number;     // ms per beat
  reset: () => void;
}

type Mode = "mic" | "tap";

/* ── Theme ── */

const CSS_VARS: Record<string, string> = {
  "--bpm-bg": "#f8f8f8",
  "--bpm-surface": "#ffffff",
  "--bpm-border": "#e5e5e5",
  "--bpm-text": "#1a1a1a",
  "--bpm-text-muted": "#737373",
  "--bpm-text-faint": "#a3a3a3",
  "--bpm-ok": "#3b82f6",
  "--bpm-ok-bg": "#eff6ff",
  "--bpm-warn": "#92400e",
  "--bpm-warn-bg": "#fefce8",
  "--bpm-danger": "#b91c1c",
  "--bpm-danger-bg": "#fef2f2",
  "--bpm-accent": "#1a1a1a",
};

/* ── Data ── */

const DANCES: Dance[] = [
  { name: "Waltz", bMin: 28, bMax: 30, time: "3/4", bpb: 3 },
  { name: "Tango", bMin: 31, bMax: 33, time: "2/4", bpb: 2 },
  { name: "Viennese Waltz", bMin: 58, bMax: 60, time: "3/4", bpb: 3 },
  { name: "Slow Foxtrot", bMin: 28, bMax: 30, time: "4/4", bpb: 4 },
  { name: "Quickstep", bMin: 50, bMax: 52, time: "4/4", bpb: 4 },
  { name: "Cha-Cha-Cha", bMin: 30, bMax: 32, time: "4/4", bpb: 4 },
  { name: "Samba", bMin: 50, bMax: 52, time: "2/4", bpb: 2 },
  { name: "Rumba", bMin: 25, bMax: 27, time: "4/4", bpb: 4 },
  { name: "Paso Doble", bMin: 60, bMax: 62, time: "2/4", bpb: 2 },
  { name: "Jive", bMin: 42, bMax: 44, time: "4/4", bpb: 4 },
];

/* ── Octave correction ── */

function octaveCorrectToBars(rawBpm: number, dance: Dance): OctaveResult {
  if (rawBpm <= 0) return { barsPerMin: 0, factor: 1, beatsPerMin: 0 };
  const beatsMin = dance.bMin * dance.bpb;
  const beatsMax = dance.bMax * dance.bpb;
  const mid = (beatsMin + beatsMax) / 2;

  let best = rawBpm, bestDist = Infinity, bestFactor = 1;
  for (let f = 0.125; f <= 16; f *= 2) {
    const candidate = rawBpm * f;
    const dist = Math.abs(candidate - mid);
    if (candidate >= beatsMin * 0.85 && candidate <= beatsMax * 1.15 && dist < bestDist) {
      best = candidate; bestDist = dist; bestFactor = f;
    }
  }
  if (bestDist === Infinity) {
    for (let f = 0.125; f <= 16; f *= 2) {
      const candidate = rawBpm * f;
      const dist = Math.abs(candidate - mid);
      if (dist < bestDist) { best = candidate; bestDist = dist; bestFactor = f; }
    }
  }

  const correctedBeats = Math.round(best * 10) / 10;
  return {
    barsPerMin: Math.round((correctedBeats / dance.bpb) * 10) / 10,
    factor: bestFactor,
    beatsPerMin: correctedBeats,
  };
}

/* ── Energy / Onset Analyzer ── */

function createOnsetAnalyzer(): OnsetAnalyzer {
  const s = {
    energyHistory: [] as number[],
    onsets: [] as number[],
    lastOnset: 0,
    bpmBuffer: [] as number[],
  };

  function process(freqData: Uint8Array, now: number): OnsetResult | null {
    let energy = 0;
    for (let i = 0; i < freqData.length; i++) energy += freqData[i]!;
    energy /= freqData.length;

    s.energyHistory.push(energy);
    if (s.energyHistory.length > 80) s.energyHistory.shift();

    const avg = s.energyHistory.reduce((a, b) => a + b, 0) / s.energyHistory.length;

    if (energy > avg * 1.4 && now - s.lastOnset > 180) {
      s.onsets.push(now);
      s.lastOnset = now;
      if (s.onsets.length > 50) s.onsets.shift();
    }

    if (s.onsets.length < 4) return null;

    const intervals: number[] = [];
    for (let i = 1; i < s.onsets.length; i++) {
      const d = s.onsets[i]! - s.onsets[i - 1]!;
      if (d > 150 && d < 2500) intervals.push(d);
    }
    if (intervals.length < 3) return null;

    intervals.sort((a, b) => a - b);
    const q1 = intervals[Math.floor(intervals.length * 0.25)]!;
    const q3 = intervals[Math.floor(intervals.length * 0.75)]!;
    const iqr = q3 - q1;
    const filtered = intervals.filter((v) => v >= q1 - 1.5 * iqr && v <= q3 + 1.5 * iqr);
    if (filtered.length < 2) return null;

    const avgInterval = filtered.reduce((a, b) => a + b, 0) / filtered.length;
    const bpm = 60000 / avgInterval;
    if (bpm < 30 || bpm > 400) return null;

    const sd = Math.sqrt(filtered.reduce((x, v) => x + (v - avgInterval) ** 2, 0) / filtered.length);
    const confidence = Math.max(0, Math.min(1, 1 - (sd / avgInterval) * 3));

    s.bpmBuffer.push(bpm);
    if (s.bpmBuffer.length > 20) s.bpmBuffer.shift();
    const sorted = [...s.bpmBuffer].sort((a, b) => a - b);

    return {
      rawBpm: Math.round(sorted[Math.floor(sorted.length / 2)]! * 10) / 10,
      confidence,
      intervals: filtered.length,
    };
  }

  function getLastOnset(): number { return s.lastOnset; }

  function reset(): void {
    s.energyHistory = []; s.onsets = []; s.lastOnset = 0; s.bpmBuffer = [];
  }

  return { process, getLastOnset, reset };
}

/* ── Phase Tracker ── */

function createPhaseTracker(bpb: number): PhaseTracker {
  let beatInterval = 0;   // ms per beat
  let anchorTime = 0;     // timestamp of last phase anchor
  let anchorPhase = 0;    // phase at anchor (0..bpb)

  function update(beatsPerMin: number, onsetTime: number, now: number): void {
    if (beatsPerMin <= 0) return;
    beatInterval = 60000 / beatsPerMin;

    if (anchorTime === 0) {
      // First sync: anchor at beat 0
      anchorTime = onsetTime;
      anchorPhase = 0;
      return;
    }

    // Where does the current phase model think we are at onsetTime?
    const elapsed = onsetTime - anchorTime;
    const rawPhase = (anchorPhase + elapsed / beatInterval) % bpb;

    // Snap to nearest integer beat
    const nearestBeat = Math.round(rawPhase) % bpb;

    // Re-anchor: this onset is at nearestBeat
    anchorTime = onsetTime;
    anchorPhase = nearestBeat;
  }

  function getPhase(now: number): number {
    if (beatInterval <= 0 || anchorTime === 0) return 0;
    const elapsed = now - anchorTime;
    const phase = (anchorPhase + elapsed / beatInterval) % bpb;
    return phase < 0 ? phase + bpb : phase;
  }

  function getBeatInterval(): number { return beatInterval; }

  function reset(): void {
    beatInterval = 0; anchorTime = 0; anchorPhase = 0;
  }

  return { update, getPhase, getBeatInterval, reset };
}

/* ── Helpers ── */

function arrayMinMax(arr: number[]): [number, number] {
  let min = Infinity, max = -Infinity;
  for (let i = 0; i < arr.length; i++) {
    if (arr[i]! < min) min = arr[i]!;
    if (arr[i]! > max) max = arr[i]!;
  }
  return [min, max];
}

/* ── Visualization: Circular Beat Display ── */

interface BeatDisplayProps {
  dance: Dance;
  phase: number;       // 0..bpb
  barsPerMin: number;
  confidence: number;
  active: boolean;      // whether we're listening
}

function BeatDisplay({ dance, phase, barsPerMin, confidence, active }: BeatDisplayProps) {
  const { bpb, bMin, bMax } = dance;
  const inRange = barsPerMin >= bMin && barsPerMin <= bMax;

  const cx = 150, cy = 150, r = 120;
  const beatR = 108; // radius for beat markers

  // Beat positions around the circle (12 o'clock = beat 1)
  const beatAngles = Array.from({ length: bpb }, (_, i) => (i / bpb) * Math.PI * 2 - Math.PI / 2);

  // Sweep hand angle
  const sweepAngle = active && barsPerMin > 0
    ? (phase / bpb) * Math.PI * 2 - Math.PI / 2
    : -Math.PI / 2;

  // Which beat are we nearest? (for highlighting)
  const currentBeat = active && barsPerMin > 0 ? Math.floor(phase) % bpb : -1;
  const beatProximity = phase - Math.floor(phase); // 0..1 how far past the beat

  const handX = cx + Math.cos(sweepAngle) * (r - 8);
  const handY = cy + Math.sin(sweepAngle) * (r - 8);

  // Dev from range
  const dev = barsPerMin > 0
    ? (barsPerMin < bMin ? barsPerMin - bMin : barsPerMin > bMax ? barsPerMin - bMax : 0)
    : 0;

  return (
    <div className="flex justify-center mb-4">
      <svg viewBox="0 0 300 300" width="300" height="300">
        {/* Outer ring */}
        <circle cx={cx} cy={cy} r={r}
                fill="none" stroke="var(--bpm-border)" strokeWidth="2" />

        {/* Subdivision ticks */}
        {Array.from({ length: bpb * 4 }, (_, i) => {
          const angle = (i / (bpb * 4)) * Math.PI * 2 - Math.PI / 2;
          const isBeat = i % 4 === 0;
          const innerR = isBeat ? r - 10 : r - 5;
          return (
            <line key={i}
                  x1={cx + Math.cos(angle) * innerR}
                  y1={cy + Math.sin(angle) * innerR}
                  x2={cx + Math.cos(angle) * r}
                  y2={cy + Math.sin(angle) * r}
                  stroke={isBeat ? "var(--bpm-text-muted)" : "var(--bpm-border)"}
                  strokeWidth={isBeat ? 2 : 1}
            />
          );
        })}

        {/* Beat markers */}
        {beatAngles.map((angle, i) => {
          const bx = cx + Math.cos(angle) * beatR;
          const by = cy + Math.sin(angle) * beatR;
          const isActive = currentBeat === i;
          // Pulse: full at beat, fade over first 30% of interval
          const pulse = isActive && beatProximity < 0.3
            ? 1 - beatProximity / 0.3
            : 0;
          const dotR = 14 + pulse * 6;

          return (
            <g key={i}>
              {/* Glow */}
              {pulse > 0 && (
                <circle cx={bx} cy={by} r={dotR + 4}
                        fill={inRange ? "var(--bpm-ok)" : "var(--bpm-warn)"}
                        opacity={pulse * 0.2}
                />
              )}
              {/* Dot */}
              <circle cx={bx} cy={by} r={dotR}
                      fill={isActive ? (inRange ? "var(--bpm-ok)" : "var(--bpm-warn)") : "var(--bpm-bg)"}
                      stroke={isActive ? (inRange ? "var(--bpm-ok)" : "var(--bpm-warn)") : "var(--bpm-border)"}
                      strokeWidth="2"
                      style={{ transition: "fill 0.08s, r 0.08s" }}
              />
              {/* Beat number */}
              <text x={bx} y={by}
                    textAnchor="middle" dominantBaseline="central"
                    fontSize="13" fontWeight="700"
                    fill={isActive ? "var(--bpm-surface)" : "var(--bpm-text-muted)"}
                    style={{ transition: "fill 0.08s", fontVariantNumeric: "tabular-nums" }}
              >
                {i + 1}
              </text>
            </g>
          );
        })}

        {/* Sweep hand */}
        {active && barsPerMin > 0 && (
          <>
            <line x1={cx} y1={cy} x2={handX} y2={handY}
                  stroke={inRange ? "var(--bpm-ok)" : "var(--bpm-warn)"}
                  strokeWidth="2" strokeLinecap="round"
                  opacity="0.6"
            />
            <circle cx={handX} cy={handY} r="4"
                    fill={inRange ? "var(--bpm-ok)" : "var(--bpm-warn)"}
            />
          </>
        )}

        {/* Center: BPM readout */}
        <text x={cx} y={cy - 14}
              textAnchor="middle" dominantBaseline="central"
              fontSize="38" fontWeight="800"
              fill={!active || barsPerMin === 0
                ? "var(--bpm-border)"
                : inRange ? "var(--bpm-ok)" : "var(--bpm-warn)"}
              style={{ fontVariantNumeric: "tabular-nums", transition: "fill 0.3s" }}
        >
          {active && barsPerMin > 0 ? barsPerMin.toFixed(1) : "—"}
        </text>
        <text x={cx} y={cy + 12}
              textAnchor="middle" dominantBaseline="central"
              fontSize="11" fill="var(--bpm-text-faint)"
        >
          bars / min
        </text>
        {active && barsPerMin > 0 && (
          <text x={cx} y={cy + 28}
                textAnchor="middle" dominantBaseline="central"
                fontSize="12"
                fill={inRange ? "var(--bpm-ok)" : "var(--bpm-warn)"}
          >
            {inRange ? "In range" : `${dev > 0 ? "+" : ""}${dev.toFixed(1)}`}
          </text>
        )}
        {active && confidence > 0 && (
          <text x={cx} y={cy + 44}
                textAnchor="middle" dominantBaseline="central"
                fontSize="10" fill="var(--bpm-text-faint)"
          >
            {Math.round(confidence * 100)}% confidence
          </text>
        )}
      </svg>
    </div>
  );
}

/* ── Sparkline ── */

function Sparkline({ history, dance, height = 36 }: { history: number[]; dance: Dance; height?: number }) {
  if (history.length < 3) return null;
  const [hMin, hMax] = arrayMinMax(history);
  const lo = Math.min(hMin, dance.bMin) - 0.5;
  const hi = Math.max(hMax, dance.bMax) + 0.5;
  const y = (v: number) => height - ((v - lo) / (hi - lo)) * height;

  return (
    <div className="rounded-lg overflow-hidden" style={{ background: "var(--bpm-surface)", border: "1px solid var(--bpm-border)" }}>
      <svg viewBox={`0 0 ${history.length} ${height}`} className="w-full" style={{ height, display: "block" }} preserveAspectRatio="none">
        <rect x="0" y={y(dance.bMax)} width={history.length} height={y(dance.bMin) - y(dance.bMax)}
              fill="var(--bpm-ok-bg)" opacity="0.5" />
        <polyline
          fill="none" stroke="var(--bpm-text-faint)" strokeWidth="1.5" vectorEffect="non-scaling-stroke"
          points={history.map((b, i) => `${i},${y(b)}`).join(" ")}
        />
      </svg>
    </div>
  );
}

/* ── Main component ── */

const ALGO_INIT: AlgoState = { raw: 0, bars: 0, factor: 1, beats: 0, conf: 0 };
const UI_FLUSH_INTERVAL = 5;

export default function TempoDetector() {
  const [dance, setDance] = useState<Dance>(DANCES[0]!);
  const [listening, setListening] = useState(false);
  const [error, setError] = useState<string | null>(null);
  const [level, setLevel] = useState(0);
  const [algo, setAlgo] = useState<AlgoState>(ALGO_INIT);
  const [hist, setHist] = useState<number[]>([]);
  const [mode, setMode] = useState<Mode>("mic");
  const [tapTimes, setTapTimes] = useState<number[]>([]);

  // Phase state for visualization (updated via rAF, not React state for perf)
  const [visPhase, setVisPhase] = useState(0);

  const audioCtxRef = useRef<AudioContext | null>(null);
  const streamRef = useRef<MediaStream | null>(null);
  const rafRef = useRef<number | null>(null);
  const analyzerRef = useRef<OnsetAnalyzer | null>(null);
  const phaseRef = useRef<PhaseTracker | null>(null);
  const danceRef = useRef<Dance>(dance);
  const frameRef = useRef(0);
  const startIdRef = useRef(0);
  const pendingRef = useRef<PendingState>({ level: 0, algo: null, hist: null });
  const lastOnsetRef = useRef(0); // track last onset we synced phase to

  useEffect(() => {
    danceRef.current = dance;
    // Rebuild phase tracker when dance (bpb) changes
    if (phaseRef.current) {
      phaseRef.current.reset();
    }
    phaseRef.current = createPhaseTracker(dance.bpb);
  }, [dance]);

  const stopAudio = useCallback(() => {
    startIdRef.current++;
    if (rafRef.current !== null) { cancelAnimationFrame(rafRef.current); rafRef.current = null; }
    if (streamRef.current) { streamRef.current.getTracks().forEach((t) => t.stop()); streamRef.current = null; }
    if (audioCtxRef.current) { audioCtxRef.current.close(); audioCtxRef.current = null; }
    analyzerRef.current = null;
  }, []);

  const start = useCallback(async () => {
    if (audioCtxRef.current) return;
    setError(null);
    const id = ++startIdRef.current;

    try {
      const stream = await navigator.mediaDevices.getUserMedia({ audio: true });

      if (startIdRef.current !== id) {
        stream.getTracks().forEach((t) => t.stop());
        return;
      }
      streamRef.current = stream;

      const ctx = new AudioContext();
      audioCtxRef.current = ctx;

      const source = ctx.createMediaStreamSource(stream);
      const analyserNode = ctx.createAnalyser();
      analyserNode.fftSize = 2048;
      source.connect(analyserNode);

      analyzerRef.current = createOnsetAnalyzer();
      phaseRef.current = createPhaseTracker(danceRef.current.bpb);
      lastOnsetRef.current = 0;

      setListening(true);
      setHist([]);
      setAlgo(ALGO_INIT);
      setVisPhase(0);
      frameRef.current = 0;
      pendingRef.current = { level: 0, algo: null, hist: null };

      const freqBuf = new Uint8Array(analyserNode.frequencyBinCount);
      const timeBuf = new Float32Array(analyserNode.fftSize);

      const loop = () => {
        analyserNode.getByteFrequencyData(freqBuf);
        analyserNode.getFloatTimeDomainData(timeBuf);
        const now = performance.now();
        const d = danceRef.current;
        const frame = frameRef.current++;
        const pending = pendingRef.current;

        // Level meter
        let rms = 0;
        for (let i = 0; i < timeBuf.length; i++) rms += timeBuf[i]! * timeBuf[i]!;
        pending.level = Math.min(1, Math.sqrt(rms / timeBuf.length) * 5);

        // Onset analysis
        const result = analyzerRef.current!.process(freqBuf, now);
        if (result) {
          const c = octaveCorrectToBars(result.rawBpm, d);
          pending.algo = {
            raw: result.rawBpm, bars: c.barsPerMin, factor: c.factor,
            beats: c.beatsPerMin, conf: result.confidence,
          };
          if (frame % 3 === 0) pending.hist = c.barsPerMin;

          // Sync phase tracker on new onsets
          const lastOnset = analyzerRef.current!.getLastOnset();
          if (lastOnset > lastOnsetRef.current && c.beatsPerMin > 0) {
            phaseRef.current!.update(c.beatsPerMin, lastOnset, now);
            lastOnsetRef.current = lastOnset;
          }
        }

        // Update visualization phase at ~20fps (every 3 frames)
        if (frame % 3 === 0 && phaseRef.current && phaseRef.current.getBeatInterval() > 0) {
          setVisPhase(phaseRef.current.getPhase(now));
        }

        // Flush other state at ~12fps
        if (frame % UI_FLUSH_INTERVAL === 0) {
          setLevel(pending.level);
          if (pending.algo) { setAlgo(pending.algo); pending.algo = null; }
          if (pending.hist !== null) {
            const val = pending.hist;
            setHist((h) => [...h, val].slice(-80));
            pending.hist = null;
          }
        }

        rafRef.current = requestAnimationFrame(loop);
      };
      loop();
    } catch (e: unknown) {
      console.error("Mic error", e);
      const err = e instanceof DOMException ? e : null;
      setError(err?.name === "NotAllowedError" ? "Microphone access denied" : "Could not access microphone");
      stopAudio();
    }
  }, [stopAudio]);

  const stop = useCallback(() => {
    stopAudio();
    setListening(false);
  }, [stopAudio]);

  useEffect(() => () => stopAudio(), [stopAudio]);

  // Re-map when dance changes
  useEffect(() => {
    setAlgo((prev) => {
      if (prev.raw <= 0) return prev;
      const c = octaveCorrectToBars(prev.raw, dance);
      return { ...prev, bars: c.barsPerMin, factor: c.factor, beats: c.beatsPerMin };
    });
    setHist([]);
  }, [dance]);

  // Tap tempo
  const handleTap = useCallback(() => {
    setTapTimes((prev) => {
      const now = performance.now();
      return [...prev, now].filter((t) => now - t < 6000);
    });
  }, []);

  const tapBars = useMemo((): number | null => {
    if (tapTimes.length < 3) return null;
    const intervals: number[] = [];
    for (let i = 1; i < tapTimes.length; i++) intervals.push(tapTimes[i]! - tapTimes[i - 1]!);
    const avg = intervals.reduce((a, b) => a + b, 0) / intervals.length;
    return octaveCorrectToBars(60000 / avg, dance).barsPerMin;
  }, [tapTimes, dance]);

  const tapInRange = tapBars !== null && tapBars >= dance.bMin && tapBars <= dance.bMax;

  return (
    <div className="min-h-screen p-4" style={{ ...CSS_VARS, background: "var(--bpm-bg)", color: "var(--bpm-text)" }}>
      <div className="max-w-md mx-auto">

        {/* Header */}
        <div className="text-center mb-4">
          <h1 className="text-lg font-bold tracking-tight" style={{ color: "var(--bpm-text)" }}>Tempo Detector</h1>
          <p className="text-xs mt-0.5" style={{ color: "var(--bpm-text-faint)" }}>
            bars per minute · octave-corrected
          </p>
        </div>

        {/* Dance selector */}
        <div className="grid grid-cols-2 gap-1.5 mb-4">
          {DANCES.map((d) => {
            const active = dance.name === d.name;
            return (
              <button
                key={d.name}
                onClick={() => { setDance(d); setTapTimes([]); }}
                className="text-left rounded-lg px-3 py-2 transition-all text-sm border"
                style={{
                  background: active ? "var(--bpm-surface)" : "transparent",
                  borderColor: active ? "var(--bpm-border)" : "transparent",
                  boxShadow: active ? "0 1px 2px rgba(0,0,0,0.04)" : "none",
                }}
              >
                <div className="font-medium text-sm" style={{ color: active ? "var(--bpm-text)" : "var(--bpm-text-muted)" }}>
                  {d.name}
                </div>
                <div className="mt-0.5" style={{ fontSize: 11, color: "var(--bpm-text-faint)" }}>
                  {d.bMin}–{d.bMax} bars/min · {d.time}
                </div>
              </button>
            );
          })}
        </div>

        {/* Mode toggle */}
        <div className="flex gap-1.5 mb-4 rounded-lg border p-1" style={{ background: "var(--bpm-surface)", borderColor: "var(--bpm-border)" }}>
          {(["mic", "tap"] as const).map((key) => (
            <button
              key={key}
              onClick={() => { setMode(key); setError(null); }}
              className="flex-1 py-2 rounded-md text-xs font-medium transition-all"
              style={{
                background: mode === key ? "var(--bpm-accent)" : "transparent",
                color: mode === key ? "var(--bpm-surface)" : "var(--bpm-text-faint)",
              }}
            >
              {key === "mic" ? "Microphone" : "Tap Tempo"}
            </button>
          ))}
        </div>

        {error && (
          <div className="text-center text-xs py-2 px-3 mb-3 rounded-lg border"
               style={{ background: "var(--bpm-danger-bg)", borderColor: "var(--bpm-danger)", color: "var(--bpm-danger)" }}>
            {error}
          </div>
        )}

        {mode === "mic" ? (
          <>
            {/* Beat visualization */}
            <BeatDisplay
              dance={dance}
              phase={visPhase}
              barsPerMin={algo.bars}
              confidence={algo.conf}
              active={listening}
            />

            {/* Level meter */}
            {listening && (
              <div className="mb-3">
                <div className="h-1 rounded-full overflow-hidden" style={{ background: "var(--bpm-border)" }}>
                  <div
                    className="h-full rounded-full transition-all duration-75"
                    style={{ width: `${level * 100}%`, background: "var(--bpm-text-faint)" }}
                  />
                </div>
              </div>
            )}

            {/* Tempo history sparkline */}
            {hist.length >= 3 && (
              <div className="mb-4">
                <Sparkline history={hist} dance={dance} />
              </div>
            )}

            {/* Stats row */}
            {algo.bars > 0 && (
              <div className="grid grid-cols-3 gap-2 mb-4 text-center rounded-lg p-3"
                   style={{ background: "var(--bpm-surface)", border: "1px solid var(--bpm-border)" }}>
                <div>
                  <div className="uppercase tracking-wider" style={{ fontSize: 10, color: "var(--bpm-text-faint)" }}>Raw</div>
                  <div className="font-semibold tabular-nums" style={{ fontSize: 14, color: "var(--bpm-text-muted)" }}>
                    {algo.raw} <span style={{ fontSize: 10, color: "var(--bpm-text-faint)" }}>bpm</span>
                  </div>
                </div>
                <div>
                  <div className="uppercase tracking-wider" style={{ fontSize: 10, color: "var(--bpm-text-faint)" }}>Corrected</div>
                  <div className="font-semibold tabular-nums" style={{ fontSize: 14, color: "var(--bpm-text-muted)" }}>
                    {algo.beats} <span style={{ fontSize: 10, color: "var(--bpm-text-faint)" }}>bpm</span>
                  </div>
                </div>
                <div>
                  <div className="uppercase tracking-wider" style={{ fontSize: 10, color: "var(--bpm-text-faint)" }}>Octave</div>
                  <div className="font-semibold tabular-nums" style={{ fontSize: 14, color: "var(--bpm-text-muted)" }}>
                    ×{algo.factor}
                  </div>
                </div>
              </div>
            )}

            {/* Start / Stop */}
            <button
              onClick={listening ? stop : start}
              className="w-full py-3.5 rounded-xl text-sm font-semibold transition-all border"
              style={listening
                ? { background: "var(--bpm-surface)", borderColor: "var(--bpm-danger)", color: "var(--bpm-danger)" }
                : { background: "var(--bpm-accent)", borderColor: "var(--bpm-accent)", color: "var(--bpm-surface)" }
              }
            >
              {listening ? "Stop listening" : "Start listening"}
            </button>

            <p className="text-center mt-3 leading-relaxed" style={{ fontSize: 11, color: "var(--bpm-text-faint)" }}>
              detect beats/min → octave-correct to {dance.time} ({dance.bMin * dance.bpb}–{dance.bMax * dance.bpb} bpm)
              → ÷{dance.bpb} = {dance.bMin}–{dance.bMax} bars/min
            </p>
          </>
        ) : (
          <>
            <div className="rounded-xl border p-6 text-center mb-3" style={{ background: "var(--bpm-surface)", borderColor: "var(--bpm-border)" }}>
              <div className="uppercase tracking-widest mb-2" style={{ fontSize: 11, color: "var(--bpm-text-faint)" }}>
                Tap — bars/min
              </div>
              <div
                className="font-bold tabular-nums leading-none transition-colors"
                style={{
                  fontSize: 48,
                  color: tapBars === null ? "var(--bpm-border)" : tapInRange ? "var(--bpm-ok)" : "var(--bpm-warn)",
                }}
              >
                {tapBars !== null ? tapBars.toFixed(1) : "—"}
              </div>
              {tapBars !== null && (
                <div className="mt-2" style={{ fontSize: 12, color: tapInRange ? "var(--bpm-ok)" : "var(--bpm-warn)" }}>
                  {tapInRange ? `In range for ${dance.name}` : `Outside ${dance.bMin}–${dance.bMax}`}
                </div>
              )}
              <p className="mt-2" style={{ fontSize: 11, color: "var(--bpm-text-faint)" }}>
                Tap each <span className="font-medium">beat</span> — converts via {dance.time} to bars/min
              </p>
            </div>

            <button
              onClick={handleTap}
              className="w-full py-14 rounded-xl border text-lg font-bold tracking-wide active:scale-[0.98] transition-transform select-none"
              style={{ background: "var(--bpm-surface)", borderColor: "var(--bpm-border)", color: "var(--bpm-text)" }}
            >
              TAP
            </button>

            <button
              onClick={() => setTapTimes([])}
              className="w-full mt-2 py-2.5 rounded-lg text-xs transition-all"
              style={{ color: "var(--bpm-text-faint)" }}
            >
              Reset
            </button>
          </>
        )}
      </div>
    </div>
  );
}
