'use client';

import React from "react";
import * as Plot from '@observablehq/plot';
import { ExtendedFeatureCollection } from "d3";

export default function EventMap() {
  const [cz, setCz] = React.useState<ExtendedFeatureCollection | null>(null);
  const [events, setEvents] = React.useState<Array<{ start_date: string; venue_lat: number; venue_lng: number; }> | null>(null);
  const containerRef = React.useRef<HTMLDivElement | null>(null);

  React.useEffect(() => {
    Promise.all([
      fetch('/cz-admin1.json').then(x => x.json()).then(setCz),
      fetch('https://api.rozpisovnik.cz/federated/event?federation=eq.csts&start_date=lte.' + new Date().toISOString().slice(0, 4) + '-12-31&start_date=gte.' + (Number.parseInt(new Date().toISOString().slice(0, 4)) - 3) + '-12-31')
        .then(x => x.json()).then(setEvents),
    ]);
  }, []);

  React.useEffect(() => {
    if (!cz || !events || !containerRef.current) return;

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
          y: "venue_lat",
          x: "venue_lng",
          fill: x => months[Number.parseInt(x.start_date.slice(5, 7)) - 1],
          r: 2,
          fy: x => x.start_date.slice(0, 4),
          channels: {
            title: x => [new Date(x.start_date).toLocaleDateString(), x.name, x.location].join('\n'),
          },
          tip: true,
        }),
        Plot.text(events, Plot.selectFirst({
          dx: 20,
          dy: 20,
          frameAnchor: "top-left",
          text: x => x.start_date.slice(0, 4),
          fy: x => x.start_date.slice(0, 4),
        })),
      ]
    });
    containerRef.current.append(plot);
    return () => plot.remove();
  }, [cz, events]);

  return <div ref={containerRef} />
}
