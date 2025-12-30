type IdtRange = {
  readonly start: number;
  readonly end: number;
};

const IDT_RANGES: readonly IdtRange[] = [
  // Legacy allocation block.
  { start: 18_000_000, end: 18_092_599 },
  // Newer allocations restarted at 10 600 000 before wrapping back above 18M.
  { start: 10_600_000, end: 17_999_999 },
  // Future growth beyond the legacy block.
  { start: 18_095_000, end: 19_999_000 },
];

export const getNextIdt = (idt: number) => {
  if (idt === 0) return IDT_RANGES[0].start;

  const currentBlockIdx = IDT_RANGES.findIndex(
    ({ start, end }) => idt >= start && idt <= end,
  );
  if (currentBlockIdx === -1) return null;

  const nextBase = Math.floor(idt / 10) + 1;
  const nextIdt = nextBase * 10 + computeEan8CheckDigit(nextBase);

  if (nextIdt <= IDT_RANGES[currentBlockIdx].end) return nextIdt;
  return IDT_RANGES[currentBlockIdx + 1]?.start ?? null;
};

export function* iterateRange({ start, end }: IdtRange): Generator<number> {
  const baseStart = Math.floor(start / 10);
  const baseEnd = Math.floor(end / 10);

  for (let base = baseStart; base <= baseEnd; base += 1) {
    const checkDigit = computeEan8CheckDigit(base);
    const idt = base * 10 + checkDigit;

    if (idt < start || idt > end) {
      continue;
    }

    yield idt;
  }
}

function toDigits(value: number, length: number): number[] {
  const digits = value.toString().padStart(length, '0').split('');
  return digits.map((digit) => {
    const parsed = Number.parseInt(digit, 10);
    if (Number.isNaN(parsed)) {
      throw new TypeError(`Invalid digit "${digit}" in value ${value}`);
    }
    return parsed;
  });
}

function computeEan8CheckDigit(base: number): number {
  if (!Number.isInteger(base)) {
    throw new TypeError('The EAN-8 base must be an integer.');
  }

  const digits = toDigits(base, 7);

  const weightedSum = digits.reduce((sum, digit, index) => {
    const position = index + 1; // Positions are counted from the left starting at 1.
    const weight = position % 2 === 1 ? 3 : 1;
    return sum + digit * weight;
  }, 0);

  const remainder = weightedSum % 10;
  return remainder === 0 ? 0 : 10 - remainder;
}
