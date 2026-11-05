import { max, scaleBand, scaleLinear } from 'd3';

export function Histogram({
  label,
  data,
  bucketWidth = 18,
  onSelect,
  formatLabel = (value) => value,
  formatTitle = (value) => value,
}: {
  label?: string;
  data: readonly (readonly [
    string,
    number,
    (readonly { label: string; count: number; className: string }[])?,
  ])[];
  bucketWidth?: number;
  onSelect?: (value: string) => void;
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
      {label && <figcaption className="mb-1 text-xs text-neutral-11">{label}</figcaption>}
      <svg width={width} height={68} className="max-w-full" viewBox={`0 0 ${width} 68`}>
        <path d={`M0 ${height}H${width}`} className="stroke-neutral-5" />
        {data.map(([value, count, segments]) => {
          let cumulative = 0;
          return (
            <g
              key={value}
              transform={`translate(${x(value)}, 0)`}
              onClick={onSelect && count ? () => onSelect(value) : undefined}
              className={
                onSelect && count
                  ? 'cursor-pointer outline-accent-9 hover:opacity-70'
                  : undefined
              }
            >
              <title>{`${formatTitle(value)}: ${count}`}</title>
              {onSelect && count && (
                <rect y={0} width={x.bandwidth()} height={68} fill="transparent" />
              )}
              {(
                segments ?? [
                  { label: formatTitle(value), count, className: 'fill-accent-9' },
                ]
              ).map((segment) => {
                const bottom = y(cumulative);
                cumulative += segment.count;
                return (
                  <rect
                    key={segment.label}
                    y={y(cumulative)}
                    width={x.bandwidth()}
                    height={bottom - y(cumulative)}
                    rx={segments ? 0 : 2}
                    className={segment.className}
                  >
                    {segments && (
                      <title>
                        {`${segment.label}: ${segment.count}`}
                      </title>
                    )}
                  </rect>
                );
              })}
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
          );
        })}
      </svg>
    </figure>
  );
}
