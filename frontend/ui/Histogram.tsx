import { max, scaleBand, scaleLinear } from 'd3';

export function Histogram({
  label,
  data,
  bucketWidth = 18,
  formatLabel = (value) => value,
  formatTitle = (value) => value,
}: {
  label: string;
  data: readonly (readonly [string, number])[];
  bucketWidth?: number;
  formatLabel?: (value: string) => string;
  formatTitle?: (value: string) => string;
}) {
  const width = Math.max(48, data.length * bucketWidth);
  const height = 48;
  const x = scaleBand<string>()
    .domain(data.map(([value]) => value))
    .rangeRound([0, width])
    .padding(0.4);
  const y = scaleLinear([0, max(data, ([, count]) => count) || 1], [height, 12]);

  return (
    <figure className="min-w-0">
      <figcaption className="mb-1 text-xs text-neutral-11">{label}</figcaption>
      <svg
        role="img"
        aria-label={`${label}: ${data.map(([value, count]) => `${formatTitle(value)}: ${count}`).join(', ') || '?'}`}
        width={width}
        height={68}
        className="max-w-full"
        viewBox={`0 0 ${width} 68`}
      >
        <path d={`M0 ${height}H${width}`} className="stroke-neutral-5" />
        {data.map(([value, count]) => (
          <g key={value} transform={`translate(${x(value)}, 0)`}>
            <title>{`${formatTitle(value)}: ${count}`}</title>
            <rect
              y={y(count)}
              width={x.bandwidth()}
              height={height - y(count)}
              rx={2}
              className="fill-accent-9"
            />
            <text
              x={x.bandwidth() / 2}
              y={y(count) - 3}
              textAnchor="middle"
              fontSize={10}
              className="fill-neutral-11"
            >
              {count}
            </text>
            <text
              x={x.bandwidth() / 2}
              y={height + 14}
              textAnchor="middle"
              fontSize={10}
              className="fill-neutral-12"
            >
              {formatLabel(value)}
            </text>
          </g>
        ))}
      </svg>
    </figure>
  );
}
