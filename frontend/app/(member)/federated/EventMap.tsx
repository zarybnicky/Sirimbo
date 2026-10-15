'use client';

import React from 'react';
import * as Plot from '@observablehq/plot';
import { json, type ExtendedFeatureCollection } from 'd3';
import { geoRobinson } from 'd3-geo-projection';
import type { FederationMapEventFragment } from '@/graphql/Federation';
import { FormError } from '@/ui/form';
import { formatDateOnly } from '@/ui/format';

export const eventMonths = [
  'leden',
  'únor',
  'březen',
  'duben',
  'květen',
  'červen',
  'červenec',
  'srpen',
  'září',
  'říjen',
  'listopad',
  'prosinec',
];

export const mapShapes = [
  { value: 'czechia', label: 'Česko' },
  { value: 'europe', label: 'Evropa' },
  { value: 'world', label: 'Svět' },
] as const;
type MapShape = (typeof mapShapes)[number]['value'];
export const defaultMapShapes: Record<string, MapShape> = {
  csts: 'czechia', szts: 'europe', wdsf: 'world',
};

export default function EventMap({ events, shape }: { events: FederationMapEventFragment[]; shape: MapShape }) {
  const [geography, setGeography] = React.useState<ExtendedFeatureCollection>();
  const [error, setError] = React.useState<Error>();
  const containerRef = React.useRef<HTMLDivElement | null>(null);
  React.useEffect(() => {
    const controller = new AbortController();
    json<ExtendedFeatureCollection>(shape === 'czechia' ? '/cz-admin1.json' : '/world-countries.json', {
      signal: controller.signal,
    }).then(setGeography).catch((error) => {
      if (!controller.signal.aborted) setError(error);
    });
    return () => controller.abort();
  }, [shape]);
  React.useEffect(() => {
    if (!geography || !containerRef.current) return;
    const projections: Record<MapShape, Plot.ProjectionOptions> = {
      czechia: {
        type: 'azimuthal-equidistant',
        domain: geography,
        rotate: [-20, 0],
      },
      europe: {
        type: geoRobinson,
        domain: { type: 'MultiPoint', coordinates: [[-25, 34], [-25, 72], [45, 34], [45, 72]] },
      },
      world: { type: geoRobinson, domain: { type: 'Sphere' } },
    };
    const plot = Plot.plot({
      height: 400,
      projection: { ...projections[shape], inset: 5 },
      axis: null,
      color: { type: 'categorical', scheme: 'sinebow', domain: eventMonths, legend: true },
      style: { fontFamily: 'Georgia' },
      marks: [
        Plot.geo(geography, {
          fill: 'var(--color-neutral-2)',
          stroke: 'var(--color-neutral-8)',
          strokeWidth: 0.6,
        }),
        Plot.graticule({ stroke: 'var(--color-neutral-6)', strokeWidth: 0.5 }),
        Plot.dot(
          events.filter((e) => e.venueLat !== null && e.venueLng !== null),
          {
            y: 'venueLat',
            x: 'venueLng',
            fill: (x) => eventMonths[Number(x.startDate.slice(5, 7)) - 1],
            r: 2,
            channels: {
              title: (x) => [formatDateOnly(x.startDate), x.name, x.location].join('\n'),
            },
            tip: true,
          },
        ),
      ],
    });
    containerRef.current.append(plot);
    return () => plot.remove();
  }, [geography, events, shape]);
  return error ? <FormError error={error} /> : <div ref={containerRef} />;
}
