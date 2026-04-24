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

  const blockIdx = IDT_RANGES.findIndex(({ start, end }) => idt >= start && idt <= end);
  if (blockIdx === -1) throw new Error(`Invalid IDT input in getNextIdt: ${idt}`);

  const nextBase = Math.floor(idt / 10) + 1;
  const nextIdt = nextBase * 10 + computeEan8CheckDigit(nextBase);

  if (nextIdt > IDT_RANGES[blockIdx].end && blockIdx + 1 < IDT_RANGES.length)
    return IDT_RANGES[blockIdx + 1].start;
  return nextIdt;
};

function computeEan8CheckDigit(base: number): number {
  if (!Number.isInteger(base)) {
    throw new TypeError('The EAN-8 base must be an integer.');
  }
  const digits = base
    .toString()
    .padStart(7, '0')
    .split('')
    .map((digit) => Number.parseInt(digit, 10));

  const weightedSum = digits.reduce((sum, digit, index) => {
    const position = index + 1; // Positions are counted from the left starting at 1.
    const weight = position % 2 === 1 ? 3 : 1;
    return sum + digit * weight;
  }, 0);

  const remainder = weightedSum % 10;
  return remainder === 0 ? 0 : 10 - remainder;
}
