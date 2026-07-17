'use client';

import React from 'react';
import * as Plot from '@observablehq/plot';
import type { ExtendedFeatureCollection } from 'd3';
import type { ICompetitionEventLocationsResult } from '../tools.queries';

export default function EventMap({
  events,
}: {
  events: ICompetitionEventLocationsResult[];
}) {
  const [cz, setCz] = React.useState<ExtendedFeatureCollection | null>(null);
  const containerRef = React.useRef<HTMLDivElement | null>(null);

  React.useEffect(() => {
    fetch('/cz-admin1.json').then((x) => x.json()).then(setCz);
  }, []);

  React.useEffect(() => {
    if (!cz || !containerRef.current) return;

    const months = ['leden', 'únor', 'březen', 'duben', 'květen', 'červen', 'červenec', 'srpen', 'září', 'říjen', 'listopad', 'prosinec'];
    const plot = Plot.plot({
      projection: {
        type: "azimuthal-equidistant",
        domain: cz,
        inset: 5,
        rotate: [-20, 0],
      },
      axis: null,
      color: {
        type: 'categorical',
        scheme: 'sinebow',
        domain: months,
        legend: true,
      },
      style: {
        fontFamily: "Georgia",
      },
      marks: [
        Plot.geo(cz, { stroke: '#aaa' }),
        Plot.dot(events, {
          y: "venueLat",
          x: "venueLng",
          fill: x => months[Number.parseInt(x.startDate.slice(5, 7)) - 1],
          r: 2,
          fy: x => x.startDate.slice(0, 4),
          channels: {
            title: x => [new Date(x.startDate).toLocaleDateString(), x.name, x.location].join('\n'),
          },
          tip: true,
        }),
        Plot.text(events, Plot.selectFirst({
          dx: 20,
          dy: 20,
          frameAnchor: "top-left",
          text: x => x.startDate.slice(0, 4),
          fy: x => x.startDate.slice(0, 4),
        })),
      ]
    });
    containerRef.current.append(plot);
    return () => plot.remove();
  }, [cz, events]);

  return <div ref={containerRef} />
}
